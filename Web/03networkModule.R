# networkModule.R
library(shiny)
library(igraph)
library(ggraph)
library(tidyverse)
library(visNetwork)
library(DT)
library(colourpicker)

networkUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 自定义CSS样式
    tags$style(HTML("
      .network-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px;
        padding: 20px;
        margin-bottom: 20px;
        color: white;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border: none;
      }
      
      .stats-card {
        background: white;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-left: 4px solid #3498DB;
      }
      
      .sidebar-panel {
        background: #f8f9fa;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        margin-bottom: 20px;
      }
      
      .plot-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      
      .btn-primary {
        background: linear-gradient(45deg, #3498DB, #2C3E50);
        border: none;
        border-radius: 25px;
        font-weight: 600;
        padding: 10px 25px;
        transition: all 0.3s ease;
        width: 100%;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
      }
      
      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        font-weight: 600;
        color: #2C3E50;
        transition: all 0.3s ease;
      }
      
      .nav-tabs > li.active > a {
        background: linear-gradient(45deg, #3498DB, #2C3E50);
        color: white;
        border: none;
      }
      
      .slider-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.05);
      }
      
      .network-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .color-picker-container {
        background: white;
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 10px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.05);
      }
    ")),
    
    fluidPage(
      # 顶部标题卡片
      div(class = "network-header",
          h2("🌐 Clinical Network Analysis", style = "margin: 0; font-weight: 700;"),
          p("Explore complex relationships between clinical departments and diseases through network visualization", 
            style = "margin: 5px 0 0 0; opacity: 0.9;")
      ),
      
      fluidRow(
        # 侧边栏
        column(
          width = 3,
          # 数据加载面板
          div(class = "sidebar-panel",
              h4("📁 Data Loading", style = "color: #2C3E50; margin-top: 0;"),
              
              fileInput(ns("file"), 
                        label = NULL,
                        buttonLabel = "📂 Upload CSV",
                        placeholder = "No file selected",
                        accept = c(".csv")),
              
              div(style = "text-align: center;",
                  checkboxInput(ns("use_sample"), "Use Sample Data", value = TRUE)
              )
          ),
          
          # 网络修剪设置面板
          div(class = "sidebar-panel",
              h4("✂️ Network Pruning", style = "color: #2C3E50; margin-top: 0;"),
              
              div(class = "slider-container",
                  sliderInput(ns("min_connection"), "Minimum Connection Strength:",
                              min = 1, max = 100, value = 5, step = 1)
              ),
              
              div(class = "slider-container",
                  sliderInput(ns("min_degree"), "Minimum Node Degree:",
                              min = 1, max = 20, value = 2, step = 1)
              ),
              
              div(class = "slider-container",
                  sliderInput(ns("node_quantile"), "Keep Top Nodes by Degree (%):",
                              min = 10, max = 100, value = 70, step = 5)
              ),
              
              div(class = "slider-container",
                  sliderInput(ns("max_nodes"), "Maximum Number of Nodes:",
                              min = 10, max = 500, value = 100, step = 10)
              )
          ),
          
          # 可视化参数面板
          div(class = "sidebar-panel",
              h4("🎨 Visualization", style = "color: #2C3E50; margin-top: 0;"),
              
              div(class = "slider-container",
                  sliderInput(ns("node_size"), "Node Size Scaling:",
                              min = 1, max = 10, value = 3, step = 0.5)
              ),
              
              div(class = "slider-container",
                  sliderInput(ns("edge_width"), "Edge Width Scaling:",
                              min = 0.5, max = 5, value = 1, step = 0.5)
              ),
              
              selectizeInput(
                ns("layout"), 
                "Network Layout Algorithm:",
                choices = c(
                  "Fruchterman-Reingold" = "fr",
                  "Kamada-Kawai" = "kk", 
                  "Force Atlas" = "lgl",
                  "Random" = "random",
                  "Circle" = "circle"
                ),
                selected = "fr"
              ),
              
              # 颜色选择器
              div(class = "color-picker-container",
                  colourpicker::colourInput(ns("dept_color"), "Department Node Color:", value = "#313695")
              ),
              
              div(class = "color-picker-container",
                  colourpicker::colourInput(ns("disease_color"), "Disease Node Color:", value = "#A50026")
              ),
              
              div(class = "color-picker-container",
                  colourpicker::colourInput(ns("edge_color"), "Edge Color:", value = "#4D4D4D")
              ),
              
              checkboxInput(ns("show_labels"), "Show Node Labels", value = TRUE),
              
              div(class = "slider-container",
                  sliderInput(ns("label_size"), "Label Size:",
                              min = 1, max = 5, value = 2.5, step = 0.5)
              ),
              
              actionButton(ns("update_plot"), "🚀 Apply Pruning & Update", class = "btn-primary")
          )
        ),
        
        # 主内容区
        column(
          width = 9,
          tabsetPanel(
            id = ns("network_tabs"),
            type = "tabs",
            
            tabPanel(
              "🌐 Network Visualization",
              icon = icon("project-diagram"),
              br(),
              fluidRow(
                column(12,
                       div(class = "plot-container",
                           h4("🖱️ Interactive Network", style = "color: #2C3E50;"),
                           visNetworkOutput(ns("vis_network"), height = "600px")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(class = "plot-container",
                           h4("🖼️ Static Network", style = "color: #2C3E50;"),
                           plotOutput(ns("static_plot"), height = "600px")
                       )
                )
              )
            ),
            
            tabPanel(
              "📊 Data Summary", 
              icon = icon("table"),
              br(),
              fluidRow(
                column(6, 
                       div(class = "stats-card",
                           h4("📈 Original Data Statistics", style = "color: #2C3E50;"),
                           tableOutput(ns("data_stats"))
                       )),
                column(6,
                       div(class = "stats-card",
                           h4("✂️ Pruning Summary", style = "color: #2C3E50;"),
                           tableOutput(ns("pruning_stats"))
                       ))
              ),
              div(class = "plot-container",
                  h4("👀 Data Preview", style = "color: #2C3E50;"),
                  DTOutput(ns("data_preview"))
              )
            ),
            
            tabPanel(
              "📈 Network Statistics",
              icon = icon("chart-bar"),
              br(),
              fluidRow(
                column(6, 
                       div(class = "stats-card",
                           h4("🔧 Network Properties", style = "color: #2C3E50;"),
                           tableOutput(ns("network_stats"))
                       )),
                column(6,
                       div(class = "stats-card",
                           h4("⭐ Top Central Nodes", style = "color: #2C3E50;"),
                           tableOutput(ns("centrality_table"))
                       ))
              ),
              div(class = "plot-container",
                  plotOutput(ns("degree_distribution"), height = "350px")
              )
            ),
            
            tabPanel(
              "📋 Study Details",
              icon = icon("search"),
              br(),
              div(class = "plot-container",
                  DTOutput(ns("study_table"))
              )
            )
          )
        )
      )
    )
  )
}

networkServer <- function(id, meta_db_network = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 美化后的图表主题
    pretty_theme <- function() {
      theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2C3E50"),
          plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#7F8C8D"),
          axis.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
          axis.text = element_text(size = 10, color = "#34495E"),
          legend.title = element_text(face = "bold", color = "#2C3E50"),
          legend.text = element_text(color = "#34495E"),
          panel.grid.major = element_line(color = "#ECF0F1", linewidth = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # Reactive data loading
    loaded_data <- reactive({
      if (!is.null(input$file)) {
        # Read uploaded file
        read.csv(input$file$datapath, stringsAsFactors = FALSE)
      } else if (input$use_sample) {
        # Use provided meta_db_network or generate sample data
        if (!is.null(meta_db_network)) {
          # Transform meta_db_network to match expected format
          data <- meta_db_network %>%
            mutate(
              Clinical_Department = ifelse(!is.na(Clinical_Department), Clinical_Department, "Unknown"),
              Disease = ifelse(!is.na(Disease), Disease, "General"),
              Connection_Strength = ifelse(!is.na(Num_Studies), Num_Studies * 10, 
                                           ifelse(!is.na(Total_Sample_Size), log10(Total_Sample_Size) * 10, 10))
            ) %>%
            select(Clinical_Department, Disease, Connection_Strength, everything())
          
          return(data)
        } else {
          # Generate sample data as fallback
          set.seed(123)
          n_studies <- 200
          
          departments <- c("Cardiology", "Neurology", "Oncology", "Psychiatry", 
                           "Surgery", "Pediatrics", "Radiology", "Pathology",
                           "Endocrinology", "Gastroenterology", "Nephrology",
                           "Pulmonology", "Dermatology", "Orthopedics", "Urology")
          
          diseases <- c("Hypertension", "Diabetes", "Cancer", "Depression", 
                        "Arthritis", "Asthma", "Stroke", "Heart Failure",
                        "COPD", "Osteoporosis", "Alzheimer's", "Parkinson's",
                        "HIV", "Hepatitis", "Sepsis", "Pneumonia", "Tuberculosis",
                        "Malaria", "Dengue", "COVID-19")
          
          data <- data.frame(
            Clinical_Department = sample(departments, n_studies, replace = TRUE),
            Disease = sample(diseases, n_studies, replace = TRUE),
            PMID = sample(30000000:40000000, n_studies),
            Effect_Size = rnorm(n_studies, mean = 0, sd = 2),
            P_Value = runif(n_studies, 0, 0.1),
            Num_Studies = sample(1:20, n_studies, replace = TRUE),
            Total_Sample_Size = sample(50:5000, n_studies, replace = TRUE)
          )
          
          data <- data %>%
            mutate(
              CI_Lower = Effect_Size - abs(rnorm(n(), 0.5, 0.3)),
              CI_Upper = Effect_Size + abs(rnorm(n(), 0.5, 0.3)),
              Literature_Abbreviation = paste0("Study_", 1:n()),
              Effect_Direction = case_when(
                Effect_Size > 0 & P_Value < 0.05 ~ "Positive",
                Effect_Size < 0 & P_Value < 0.05 ~ "Negative",
                TRUE ~ "Not Significant"
              ),
              CI_Width = CI_Upper - CI_Lower,
              Connection_Strength = ifelse(is.na(Num_Studies), 
                                           log10(Total_Sample_Size) * 10, 
                                           Num_Studies * 10)
            )
          
          return(data)
        }
      }
    })
    
    # Data statistics
    output$data_stats <- renderTable({
      data <- loaded_data()
      if (is.null(data)) return()
      
      data.frame(
        Metric = c("📚 Total Studies", "🏥 Unique Departments", "🩺 Unique Diseases", 
                   "📊 Average Effect Size", "👥 Average Sample Size"),
        Value = c(nrow(data),
                  length(unique(data$Clinical_Department)),
                  length(unique(data$Disease)),
                  round(mean(data$Effect_Size, na.rm = TRUE), 3),
                  round(mean(data$Total_Sample_Size, na.rm = TRUE), 1))
      )
    }, bordered = TRUE, align = 'lc')
    
    # Data preview
    output$data_preview <- renderDT({
      data <- loaded_data()
      if (is.null(data)) return()
      
      datatable(head(data, 10), 
                options = list(
                  scrollX = TRUE, 
                  pageLength = 5,
                  dom = 'tip',
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#3498DB', 'color': 'white'});",
                    "}")
                )) %>%
        formatStyle(names(data), backgroundColor = 'white')
    })
    
    # Build network with pruning (with debouncing)
    build_network <- eventReactive(input$update_plot, {
      data <- loaded_data()
      if (is.null(data)) return(make_empty_graph())
      
      # Apply pruning thresholds
      dept_disease_network <- data %>%
        select(Clinical_Department, Disease, Connection_Strength) %>%
        filter(Connection_Strength >= input$min_connection) %>%
        group_by(Clinical_Department, Disease) %>%
        summarize(Connection_Strength = sum(Connection_Strength), .groups = 'drop')
      
      if(nrow(dept_disease_network) == 0) {
        return(make_empty_graph())
      }
      
      # Create initial network
      g <- graph_from_data_frame(dept_disease_network, directed = FALSE)
      
      # Remove nodes with degree below threshold
      if (input$min_degree > 1) {
        g <- delete_vertices(g, which(degree(g) < input$min_degree))
      }
      
      # Keep only top nodes by degree
      if (vcount(g) > 0) {
        degree_threshold <- quantile(degree(g), (100 - input$node_quantile) / 100)
        g <- induced_subgraph(g, which(degree(g) >= degree_threshold))
      }
      
      # Limit maximum number of nodes
      if (vcount(g) > input$max_nodes) {
        top_nodes <- names(sort(degree(g), decreasing = TRUE)[1:input$max_nodes])
        g <- induced_subgraph(g, top_nodes)
      }
      
      return(g)
    }, ignoreNULL = FALSE)
    
    # Initial network build
    build_network_initial <- reactive({
      data <- loaded_data()
      if (is.null(data)) return(make_empty_graph())
      
      dept_disease_network <- data %>%
        select(Clinical_Department, Disease, Connection_Strength) %>%
        filter(Connection_Strength >= 5) %>%  # Default value
        group_by(Clinical_Department, Disease) %>%
        summarize(Connection_Strength = sum(Connection_Strength), .groups = 'drop')
      
      if(nrow(dept_disease_network) == 0) {
        return(make_empty_graph())
      }
      
      g <- graph_from_data_frame(dept_disease_network, directed = FALSE)
      return(g)
    })
    
    # Pruning statistics
    output$pruning_stats <- renderTable({
      data <- loaded_data()
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if (is.null(data)) return()
      
      original_edges <- data %>%
        select(Clinical_Department, Disease) %>%
        distinct() %>%
        nrow()
      
      data.frame(
        Metric = c("🟢 Original Nodes", "✂️ Pruned Nodes", "🔗 Original Edges", "✂️ Pruned Edges",
                   "📉 Reduction (%)"),
        Value = c(length(unique(c(data$Clinical_Department, data$Disease))),
                  ifelse(vcount(g) == 0, 0, vcount(g)),
                  original_edges,
                  ifelse(vcount(g) == 0, 0, ecount(g)),
                  ifelse(vcount(g) == 0, 100, 
                         round((1 - vcount(g)/length(unique(c(data$Clinical_Department, data$Disease)))) * 100, 1)))
      )
    }, bordered = TRUE, align = 'lc')
    
    # Static network visualization
    output$static_plot <- renderPlot({
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if(vcount(g) == 0) {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "🌐 Network is empty\nPlease adjust pruning thresholds", 
                   size = 8, color = "#BDC3C7") +
          theme_void() +
          pretty_theme()
        return()
      }
      
      # Prepare node types
      data <- loaded_data()
      node_types <- ifelse(V(g)$name %in% data$Clinical_Department, 
                           "Clinical Department", "Disease")
      
      p <- ggraph(g, layout = input$layout) +
        geom_edge_link(aes(width = Connection_Strength), 
                       color = input$edge_color, alpha = 0.7) +
        scale_edge_width_continuous(range = c(0.5, 3 * input$edge_width), 
                                    name = "Connection Strength") +
        
        geom_node_point(aes(color = node_types, size = degree(g)), alpha = 0.8) +
        scale_color_manual(values = c("Clinical Department" = input$dept_color, 
                                      "Disease" = input$disease_color),
                           name = "Node Type") +
        scale_size_continuous(range = c(2, 8 * input$node_size), name = "Node Degree") +
        
        labs(
          title = "Clinical Department-Disease Association Network",
          subtitle = paste("Network with", vcount(g), "nodes and", ecount(g), "edges"),
          caption = if(input$update_plot > 0) {
            paste("Pruning: min connection =", input$min_connection, 
                  "| min degree =", input$min_degree,
                  "| top", input$node_quantile, "% nodes")
          } else {
            "Default network view - click 'Apply Pruning & Update' to apply filters"
          }
        ) +
        pretty_theme() +
        theme(
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
          plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50")
        )
      
      if(input$show_labels) {
        p <- p + geom_node_text(aes(label = name), repel = TRUE, 
                                size = input$label_size, 
                                max.overlaps = 20, 
                                box.padding = 0.3,
                                color = "#2C3E50",
                                fontface = "bold")
      }
      
      p
    })
    
    # Interactive network visualization
    output$vis_network <- renderVisNetwork({
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if(vcount(g) == 0) {
        return(visNetwork(nodes = data.frame(id = 1, label = "🌐 Network is empty\nAdjust pruning thresholds"), 
                          edges = data.frame()) %>%
                 visOptions(manipulation = TRUE) %>%
                 visNodes(color = list(background = "#3498DB", border = "#2C3E50")))
      }
      
      data <- loaded_data()
      
      # Prepare node data
      nodes <- data.frame(
        id = 1:vcount(g),
        label = V(g)$name,
        group = ifelse(V(g)$name %in% data$Clinical_Department, 
                       "Clinical Department", "Disease"),
        value = degree(g),
        title = paste0("Node: ", V(g)$name, "<br>Degree: ", degree(g)),
        font.size = 20
      )
      
      # Prepare edge data
      edges <- get.edgelist(g)
      edge_weights <- E(g)$Connection_Strength
      
      edges_df <- data.frame(
        from = match(edges[,1], V(g)$name),
        to = match(edges[,2], V(g)$name),
        value = edge_weights,
        title = paste0("Connection Strength: ", round(edge_weights, 2)),
        width = edge_weights * 0.1 * input$edge_width,
        color = input$edge_color
      )
      
      # Create interactive network
      visNetwork(nodes, edges_df) %>%
        visGroups(groupname = "Clinical Department", 
                  color = list(background = input$dept_color, border = "darkblue"),
                  shape = "dot") %>%
        visGroups(groupname = "Disease", 
                  color = list(background = input$disease_color, border = "darkred"),
                  shape = "dot") %>%
        visLegend() %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                   nodesIdSelection = TRUE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLayout(randomSeed = 123) %>%
        visPhysics(stabilization = FALSE) %>%
        visEdges(smooth = TRUE)
    })
    
    # Network statistics
    output$network_stats <- renderTable({
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if(vcount(g) == 0) {
        return(data.frame(Property = "🌐 Network is empty", Value = "Adjust pruning thresholds"))
      }
      
      data.frame(
        Property = c("🟢 Number of Nodes", "🔗 Number of Edges", "📊 Network Density", 
                     "📈 Average Degree", "📏 Diameter", "🔄 Clustering Coefficient"),
        Value = c(vcount(g), 
                  ecount(g),
                  round(edge_density(g), 3),
                  round(mean(degree(g)), 2),
                  ifelse(vcount(g) > 1, diameter(g), "N/A"),
                  round(transitivity(g), 3))
      )
    }, bordered = TRUE, align = 'lc')
    
    # Degree distribution plot
    output$degree_distribution <- renderPlot({
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if(vcount(g) == 0) {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "📊 No network data available", 
                   size = 8, color = "#BDC3C7") +
          theme_void() +
          pretty_theme()
        return()
      }
      
      degree_data <- data.frame(degree = degree(g))
      
      ggplot(degree_data, aes(x = degree)) +
        geom_histogram(binwidth = 1, fill = "#3498DB", alpha = 0.7, color = "white") +
        labs(
          title = "Node Degree Distribution",
          subtitle = "Distribution of connections per node in the network",
          x = "Degree", 
          y = "Frequency"
        ) +
        pretty_theme() +
        theme(
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
    })
    
    # Centrality ranking
    output$centrality_table <- renderTable({
      g <- if (input$update_plot > 0) build_network() else build_network_initial()
      
      if(vcount(g) == 0) {
        return(data.frame(Rank = "N/A", Node = "No nodes", Degree = "N/A"))
      }
      
      deg_centrality <- degree(g)
      top_nodes <- head(sort(deg_centrality, decreasing = TRUE), 10)
      
      data.frame(
        Rank = 1:length(top_nodes),
        Node = names(top_nodes),
        Degree = as.numeric(top_nodes)
      )
    }, bordered = TRUE)
    
    # Study details table
    output$study_table <- renderDT({
      data <- loaded_data()
      if (is.null(data)) return()
      
      datatable(data, 
                options = list(
                  pageLength = 10, 
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel'),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#3498DB', 'color': 'white'});",
                    "}")
                ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center; color: #2C3E50; font-size: 1.5em; font-weight: bold;',
                  '📋 Study Characteristics'
                )) %>%
        formatRound(columns = c('Effect_Size', 'P_Value'), digits = 3) %>%
        formatStyle(names(data), backgroundColor = 'white')
    })
    
  })
}

networkAnalysisUI <- function(id) {
  ns <- NS(id)
  networkUI(ns("network"))
}

networkAnalysisServer <- function(id, meta_db_network = NULL) {
  moduleServer(id, function(input, output, session) {
    networkServer("network", meta_db_network = meta_db_network)
  })
}