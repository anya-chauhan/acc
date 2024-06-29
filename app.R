library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(networkD3)
library(RColorBrewer)
library(shinyjs)
library(ggplot2)
library(plotly)
library(scales)
library(viridis)

# Load the dataset
df <- read.csv('ML-final.csv')

create_graph <- function(df) {
  # Check for required columns
  required_cols <- c("Skill", "JobPostings", "CompaniesHiring", "TopJobTitles", "TopCompanies")
  missing_cols <- required_cols[!required_cols %in% colnames(df)]
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns in the dataset: ", paste(missing_cols, collapse=", "))
  }
  
  # Create empty graph
  g <- make_empty_graph(directed = FALSE)
  
  # Add skill nodes and set attributes
  skill_nodes <- df$Skill
  g <- add_vertices(g, length(skill_nodes), name = skill_nodes)
  V(g)$type <- "Skill"
  V(g)$job_postings <- as.numeric(gsub("%", "", df$JobPostings))
  V(g)$companies_hiring <- as.numeric(gsub("%", "", df$CompaniesHiring))
  
  # Function to add nodes and edges
  add_nodes_and_edges <- function(g, from, to_list, type) {
    for (to in to_list) {
      if (!(to %in% V(g)$name)) {
        g <- add_vertices(g, 1, name = to)
        V(g)[to]$type <- type
      }
      g <- add_edges(g, c(from, to))
    }
    return(g)
  }
  
  # Add job title and company nodes and edges
  for (i in 1:nrow(df)) {
    skill <- df$Skill[i]
    
    # Job titles
    job_titles <- strsplit(as.character(df$TopJobTitles[i]), ", ")[[1]]
    g <- add_nodes_and_edges(g, skill, job_titles, "Job Title")
    
    # Companies
    companies <- strsplit(as.character(df$TopCompanies[i]), ", ")[[1]]
    g <- add_nodes_and_edges(g, skill, companies, "Company")
  }
  
  # Remove isolated nodes
  isolated_nodes <- V(g)[degree(g) == 0]
  g <- delete_vertices(g, isolated_nodes)
  
  # Pre-compute the ego graphs for each skill node
  skill_nodes <- V(g)[V(g)$type == "Skill"]
  ego_graphs <- lapply(skill_nodes, function(node) {
    make_ego_graph(g, order = 1, nodes = node, mode = "all")[[1]]
  })
  
  list(full_graph = g, ego_graphs = ego_graphs)
}

graph_data <- create_graph(df)

ui <- dashboardPage(
  dashboardHeader(title = "AI Career Explorer"),
  dashboardSidebar(
    selectInput("view_selector", "Select View:", choices = c("Interactive Network", "Quick Insights")),
    conditionalPanel(
      condition = "input.view_selector == 'Interactive Network'",
      tags$div(
        h4("Display New Graph", style = "padding: 10px;"),
        textInput("search_current", "Search nodes in current view"),
        actionButton("search_current_button", "Find & Update", title = "Update the graph based on your selection from the current view"),
        textInput("search_all", "Search any node"),
        actionButton("search_all_button", "Find & Update", title = "Find and focus on a new skill or job from the entire dataset"),
        style = "margin-bottom: 20px;"
      ),
      numericInput("num_nodes", "Number of Related Items to Show:", value = 50, min = 1, max = 1000),
      sliderInput("job_postings_filter", "Filter by Job Demand (%):", 
                  min = 0, max = 100, value = c(0, 100), step = 1)
    ),
    conditionalPanel(
      condition = "input.view_selector == 'Quick Insights'",
      radioButtons("plot_selector", "Select Plot:", 
                   choices = c("Top 10 Most In-Demand Skills", "Demand for Programming Languages"),
                   selected = "Top 10 Most In-Demand Skills")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$script("
      $(document).on('shiny:busy', function() {
        $('#loading').show();
      });
      $(document).on('shiny:idle', function() {
        $('#loading').hide();
      });
    ")),
    div(id = "loading", "Loading...", style = "display: none;"),
    fluidRow(
      column(12, uiOutput("view_output"))
    )
  )
)


server <- function(input, output, session) {
  
  central_node <- reactiveVal("Research")
  
  graph <- reactive({
    job_postings_range <- input$job_postings_filter
    
    # Start with the full graph
    sub_g <- graph_data$full_graph
    
    # Filter skills based on job postings
    skill_nodes <- V(sub_g)[V(sub_g)$type == "Skill" & 
                              V(sub_g)$job_postings >= job_postings_range[1] & 
                              V(sub_g)$job_postings <= job_postings_range[2]]
    
    # If central node is specified and valid, create its ego network
    if (central_node() %in% V(sub_g)$name) {
      central_node_id <- which(V(sub_g)$name == central_node())
      sub_g <- make_ego_graph(sub_g, order = 1, nodes = central_node_id, mode = "all")[[1]]
    } else {
      # If no valid central node, use the filtered skill nodes
      sub_g <- induced_subgraph(sub_g, c(skill_nodes, neighbors(sub_g, skill_nodes)))
    }
    
    # Increase the neighborhood size to allow for more nodes if needed
    if (vcount(sub_g) < input$num_nodes) {
      expanded_nodes <- unlist(neighborhood(sub_g, order = 2, nodes = V(sub_g)))
      sub_g <- induced_subgraph(sub_g, expanded_nodes)
    }
    
    # Limit the number of nodes if necessary
    if (vcount(sub_g) > input$num_nodes) {
      sampled_nodes <- sample(V(sub_g), input$num_nodes)
      sub_g <- induced_subgraph(sub_g, sampled_nodes)
    }
    
    sub_g
  })
  
  # Search within current graph
  observeEvent(input$search_current_button, {
    search_term <- input$search_current
    sub_g <- graph()
    
    matching_nodes <- V(sub_g)$name[grep(search_term, V(sub_g)$name, ignore.case = TRUE)]
    if (length(matching_nodes) > 0) {
      central_node(matching_nodes[1])
      showNotification(paste("Found matching node:", matching_nodes[1]), type = "message")
    } else {
      showNotification("No matching nodes found in current graph", type = "warning")
    }
  })
  
  # Search all nodes
  observeEvent(input$search_all_button, {
    search_term <- input$search_all
    full_g <- graph_data$full_graph
    
    matching_nodes <- V(full_g)$name[grep(search_term, V(full_g)$name, ignore.case = TRUE)]
    if (length(matching_nodes) > 0) {
      central_node(matching_nodes[1])
      showNotification(paste("Found matching node:", matching_nodes[1]), type = "message")
    } else {
      showNotification("No matching nodes found", type = "warning")
    }
  })
  
  output$skills_network_plot <- renderForceNetwork({
    sub_g <- graph()
    
    if (vcount(sub_g) == 0) {
      return(NULL)
    }
    
    nodes <- data.frame(
      id = seq_len(vcount(sub_g)) - 1,
      name = V(sub_g)$name,
      group = V(sub_g)$type,
      job_postings = V(sub_g)$job_postings,
      companies_hiring = V(sub_g)$companies_hiring,
      stringsAsFactors = FALSE
    )
    
    links <- as.data.frame(as_edgelist(sub_g, names = FALSE) - 1)
    names(links) <- c("source", "target")
    links$value <- 1
    
    color_palette <- c("Skill" = "#4CAF50", "Job Title" = "#2196F3", "Company" = "#FFC107")
    nodes$color <- color_palette[nodes$group]
    
    # Normalize job_postings for node size
    min_size <- 5
    max_size <- 20
    nodes$node_size <- ifelse(nodes$group == "Skill" & !is.na(nodes$job_postings), 
                              pmin(pmax(nodes$job_postings / 2, min_size), max_size), 
                              min_size)
    
    forceNetwork(Links = links, Nodes = nodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", Nodesize = "node_size",
                 opacity = 0.8,
                 linkDistance = 100, charge = -300,
                 fontSize = 14, zoom = TRUE,
                 colourScale = JS("d3.scaleOrdinal().domain(['Skill', 'Job Title', 'Company']).range(['#4CAF50', '#2196F3', '#FFC107'])"),
                 legend = TRUE,
                 opacityNoHover = 0.5,
                 arrows = FALSE,
                 bounded = TRUE,
                 clickAction = "Shiny.setInputValue('selected_node', d.name);")
  })
  
  output$node_info <- renderUI({
    selected_node <- input$selected_node
    if (is.null(selected_node)) {
      return(HTML("<p>Click on a node to see its information.</p>"))
    }
    
    sub_g <- graph()
    node_data <- V(sub_g)[selected_node]
    
    if (length(node_data) == 0) {
      return(HTML("<p>No information available for this node.</p>"))
    }
    
    if (node_data$type == "Skill") {
      HTML(paste(
        "<p><strong>", node_data$name, "</strong><br>",
        "Type: Skill<br>",
        "Job Postings: ", node_data$job_postings, "%<br>",
        "Companies Hiring: ", node_data$companies_hiring, "%</p>"
      ))
    } else {
      HTML(paste("<p><strong>", node_data$name, "</strong><br>",
                 "Type:", node_data$type, "</p>"))
    }
  })
  
  # Network statistics
  output$network_stats <- renderText({
    sub_g <- graph()
    if (vcount(sub_g) == 0) return("No graph data available")
    
    paste(" Number of Nodes:", vcount(sub_g),"\n",
          "Number of Edges:", ecount(sub_g),"\n",
          "Network Density:", round(edge_density(sub_g), 4),"\n",
          "Average Degree:", round(mean(degree(sub_g)), 2))
  })
  
  output$view_output <- renderUI({
    if (input$view_selector == "Interactive Network") {
      tagList(
        fluidRow(
          column(12, forceNetworkOutput("skills_network_plot", height = "600px"))
        ),
        fluidRow(
          column(6, box(title = "Network Statistics", status = "primary", solidHeader = TRUE,
                        verbatimTextOutput("network_stats"), width = 12)),
          column(6, box(title = "Node Information", status = "info", solidHeader = TRUE,
                        uiOutput("node_info"), width = 12))
        )
      )
    } else {
      tagList(
        fluidRow(
          column(12, plotlyOutput("overview_plot"))
        )
      )
    }
  })
  
  output$overview_plot <- renderPlotly({
    plot_data <- switch(input$plot_selector,
                        "Top 10 Most In-Demand Skills" = df %>% top_n(10, JobPostings),
                        "Demand for Programming Languages" = df %>% filter(Skill %in% c('Python', 'R', 'Java', 'C++', 'JavaScript', 'C', 'Scala'))
    )
    
    plot <- switch(input$plot_selector,
                   "Top 10 Most In-Demand Skills" = ggplot(plot_data, aes(x = reorder(Skill, JobPostings), y = JobPostings, fill = Skill)) +
                     geom_bar(stat = 'identity') + coord_flip() + theme_minimal() +
                     scale_fill_viridis_d() + 
                     ggtitle("Top 10 Most In-Demand Skills"),
                   "Demand for Programming Languages" = ggplot(plot_data, aes(x = reorder(Skill, JobPostings), y = JobPostings, fill = Skill)) +
                     geom_bar(stat = 'identity') + coord_flip() + theme_minimal() + 
                     scale_fill_viridis_d() + 
                     ggtitle("Demand for Programming Languages")
    )
    
    ggplotly(plot)
  })
  
}

shinyApp(ui, server)
