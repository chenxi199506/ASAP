# Create Enhanced Model Comparison Plot Function
create_enhanced_model_plot <- function(data, column_info, row_info, palettes) {
  
  # Set dimension parameters
  row_height <- 1.1
  row_space <- 0.1
  col_width <- 1.1
  col_space <- 0.2
  col_bigspace <- 0.5
  
  # Calculate row positions
  row_pos <- row_info %>%
    mutate(
      row_i = row_number(),
      colour_background = row_i %% 2 == 1,
      y = -row_i * (row_height + row_space),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )
  
  # Calculate column positions
  column_pos <- column_info %>%
    mutate(
      x = cumsum(c(0, head(width, -1)) + width/2 + c(0, rep(col_space, n()-1))),
      xmin = x - width/2,
      xmax = x + width/2
    )
  
  # Define metric groups and color mapping
  metric_groups <- list(
    group1 = list(
      circle_metrics = c("avg_response_time"),
      bar_metrics = c("match_success_rate"),
      color_palette = "Blues",
      group_name = "Matching\nPerformance"
    ),
    group2 = list(
      circle_metrics = c("studies_accuracy", "sample_accuracy"),
      bar_metrics = c("extract_accuracy"),
      color_palette = "Greens", 
      group_name = "Study\nAccuracy"
    ),
    group3 = list(
      circle_metrics = c("amstar_non_critical_accuracy", "amstar_critical_accuracy"),
      bar_metrics = c("amstar_accuracy"),
      color_palette = "Purples",
      group_name = "AMSTAR2\nAssessment"
    ),
    group4 = list(
      circle_metrics = c(),
      bar_metrics = c("overall_score"),
      color_palette = "RdYlBu",
      group_name = "Overall\nScore"
    )
  )
  
  # Create circle data
  circle_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$circle_metrics) {
      values <- data[[metric]]
      # Calculate circle sizes
      r_values <- row_height/2 * sqrt(values)
      r_values <- rescale(r_values, to = c(0.05, 0.55))
      
      # Use group-specific color mapping
      palette_colors <- colorRampPalette(rev(brewer.pal(9, palette_name)))(length(values))
      colors <- palette_colors[rank(values, ties.method = "average", na.last = "keep")]
      
      circle_data_list[[paste(metric, group_name)]] <- data.frame(
        model_name = data$model_name,
        metric = metric,
        group = group_name,
        x0 = column_pos$x[column_pos$id == metric],
        y0 = row_pos$y[match(data$model_name, row_pos$id)],
        r = r_values,
        colors = colors,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    }
  }
  circle_data <- bind_rows(circle_data_list)
  
  # Create bar data
  rect_data_list <- list()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    palette_name <- group$color_palette
    
    for(metric in group$bar_metrics) {
      values <- data[[metric]]
      palette_colors <- colorRampPalette(rev(brewer.pal(9, palette_name)))(length(values))
      bar_colors <- palette_colors[rank(values, ties.method = "average", na.last = "keep")]
      
      rect_data_list[[metric]] <- data.frame(
        model_name = data$model_name,
        metric = metric,
        group = group_name,
        value = values,
        xmin = column_pos$xmin[column_pos$id == metric],
        xmax = column_pos$xmax[column_pos$id == metric],
        ymin = row_pos$ymin[match(data$model_name, row_pos$id)] + 0.1,
        ymax = row_pos$ymax[match(data$model_name, row_pos$id)] - 0.1,
        colors = bar_colors,
        stringsAsFactors = FALSE
      ) %>% as_tibble()
    }
  }
  rect_data <- bind_rows(rect_data_list)
  
  # Create basic info text data
  text_metrics <- c("Developer", "Release_Year", "Total_Parameters_B", "Context_Length_K_tokens")
  
  text_data_list <- list()
  
  for(metric in text_metrics) {
    values <- data[[metric]]
    text_data_list[[metric]] <- data.frame(
      model_name = data$model_name,
      metric = metric,
      x = column_pos$x[column_pos$id == metric],
      y = row_pos$y[match(data$model_name, row_pos$id)],
      label = as.character(values),
      hjust = 0.5,
      vjust = 0.5,
      fontface = "plain",
      size = 3,
      color = "black",
      stringsAsFactors = FALSE
    ) %>% as_tibble()
  }
  text_data <- bind_rows(text_data_list)
  
  # Model name text
  model_text_data <- data.frame(
    x = column_pos$xmin[column_pos$id == "model_name"] + 0.3,
    y = row_pos$y,
    label = data$model_name,
    hjust = 0,
    fontface = "bold",
    size = 3.5,
    color = "black",
    stringsAsFactors = FALSE
  ) %>% as_tibble()
  
  # Add ranking text
  rank_data <- data.frame(
    x = column_pos$xmin[column_pos$id == "model_name"] - 0.5,
    y = row_pos$y,
    label = 1:nrow(data),
    hjust = 0.5,
    fontface = "bold",
    size = 4,
    color = "black",
    stringsAsFactors = FALSE
  ) %>% as_tibble()
  
  # Column titles with automatic line breaks
  column_text <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 1.2,
      label = case_when(
        id == "avg_response_time" ~ "Response\nTime (s)",
        id == "match_success_rate" ~ "Match\nSuccess Rate",
        id == "studies_accuracy" ~ "Studies\nAccuracy",
        id == "sample_accuracy" ~ "Sample\nAccuracy",
        id == "extract_accuracy" ~ "Extraction\nAccuracy",
        id == "amstar_non_critical_accuracy" ~ "AMSTAR2\nNon-critical",
        id == "amstar_critical_accuracy" ~ "AMSTAR2\nCritical",
        id == "amstar_accuracy" ~ "AMSTAR2\nAccuracy",
        id == "overall_score" ~ "Overall\nScore",
        id == "Total_Parameters_B" ~ "Parameters\n(B)",
        id == "Context_Length_K_tokens" ~ "Context\nLength (K)",
        id == "Release_Year" ~ "Release\nYear",
        id == "Developer" ~ "Developer",
        TRUE ~ name
      ),
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = 3.0,  # Slightly smaller for multi-line text
      color = "black"
    )
  
  # Add group headers
  group_header_data <- data.frame()
  for(group_name in names(metric_groups)) {
    group <- metric_groups[[group_name]]
    group_metrics <- c(group$circle_metrics, group$bar_metrics)
    
    if(length(group_metrics) > 0) {
      group_xmin <- min(column_pos$xmin[column_pos$id %in% group_metrics])
      group_xmax <- max(column_pos$xmax[column_pos$id %in% group_metrics])
      
      group_header <- data.frame(
        x = (group_xmin + group_xmax) / 2,
        y = max(row_pos$ymax) + 2.0,
        label = group$group_name,
        hjust = 0.5,
        vjust = 0.5,
        fontface = "bold",
        size = 3.5,
        color = "black"
      )
      group_header_data <- bind_rows(group_header_data, group_header)
    }
  }
  
  # Tick marks below column titles
  segment_data <- column_pos %>%
    filter(id != "model_name") %>%
    mutate(
      y = max(row_pos$ymax) + 0.8,
      yend = max(row_pos$ymax) + 1.0,
      color = "black",
      size = 0.5
    )
  
  # Create the plot
  g <- ggplot() +
    # Background stripes
    geom_rect(
      data = row_pos %>% filter(colour_background),
      aes(xmin = min(column_pos$xmin) - 0.3, xmax = max(column_pos$xmax) + 0.3,
          ymin = ymin, ymax = ymax),
      fill = "#DDDDDD", alpha = 0.8
    ) +
    
    # Bar charts
    geom_rect(
      data = rect_data,
      aes(xmin = xmin, xmax = xmin + value * (xmax - xmin), 
          ymin = ymin, ymax = ymax, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # Circle charts
    ggforce::geom_circle(
      data = circle_data,
      aes(x0 = x0, y0 = y0, r = r, fill = colors),
      color = "black", size = 0.25
    ) +
    
    # Basic info text
    geom_text(
      data = text_data,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "plain", size = 3, color = "black"
    ) +
    
    # Ranking numbers
    geom_text(
      data = rank_data,
      aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 4, color = "black"
    ) +
    
    # Model names
    geom_text(
      data = model_text_data,
      aes(x = x, y = y, label = label, hjust = hjust),
      fontface = "bold", size = 3.5, color = "black"
    ) +
    
    # Column titles
    geom_text(
      data = column_text,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.0, color = "black", lineheight = 0.8
    ) +
    
    # Group headers
    geom_text(
      data = group_header_data,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
      fontface = "bold", size = 3.5, color = "black", lineheight = 0.8
    ) +
    
    # Tick marks
    geom_segment(
      data = segment_data,
      aes(x = x, xend = x, y = y, yend = yend),
      size = 0.5, color = "black"
    ) +
    
    # Use identity scales
    scale_fill_identity() +
    scale_color_identity() +
    scale_size_identity() +
    
    # Theme settings
    theme_void() +
    theme(
      legend.position = "none",
      plot.margin = margin(1, 1, 2, 1, "cm"),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Coordinate limits
    coord_equal(
      xlim = c(min(column_pos$xmin) - 1, max(column_pos$xmax) + 0.5),
      ylim = c(min(row_pos$ymin) - 0.5, max(row_pos$ymax) + 3.0)
    )
  
  return(g)
}

# LLM Benchmarking UI
llmBenchmarkingUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 自定义CSS样式 - 与部门分析模块保持一致
    tags$style(HTML("
      .benchmark-card {
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
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow:   4px 15px rgba(52, 152, 219, 0.3);
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
      
      .selectize-control .selectize-input {
        border-radius: 8px;
        border: 2px solid #e9ecef;
        transition: border-color 0.3s ease;
      }
      
      .selectize-control .selectize-input:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      
      .benchmark-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 15px 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
      
      .data-table-container {
        background: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        margin-top: 20px;
      }
    ")),
    
    div(style = "padding: 20px;",
        # 顶部标题卡片 - 与部门分析模块风格一致
        div(class = "benchmark-header",
            h2("🤖 LLM Benchmarking for Meta-Analysis", style = "margin: 0; font-weight: 700;"),
            p("Comparative evaluation of large language models on meta-analysis tasks", 
              style = "margin: 5px 0 0 0; opacity: 0.9;")
        ),
        
        # Control Panel
        fluidRow(
          box(
            title = "⚙️ Benchmark Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            fluidRow(
              column(3,
                     selectInput(ns("sort_by"), "Sort by:",
                                 choices = c("Overall Score" = "overall_score",
                                             "Match Success Rate" = "match_success_rate",
                                             "Response Time" = "avg_response_time",
                                             "AMSTAR Accuracy" = "amstar_accuracy"),
                                 selected = "overall_score")
              ),
              column(3,
                     selectInput(ns("sort_order"), "Sort order:",
                                 choices = c("Descending" = "desc", "Ascending" = "asc"),
                                 selected = "desc")
              ),
              column(3,
                     numericInput(ns("top_n"), "Show top N models:",
                                  value = 10, min = 1, max = 20, step = 1)
              ),
              column(3,
                     actionButton(ns("refresh_btn"), "🔄 Refresh Data", 
                                  icon = icon("refresh"),
                                  class = "btn-primary",
                                  style = "margin-top: 25px;")
              )
            )
          )
        ),
        
        # Visualization
        fluidRow(
          box(
            title = "📊 Model Performance Comparison",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput(ns("benchmark_plot"), height = "700px")
          )
        ),
        
        # Data Table - 默认展开，与部门分析模块保持一致
        fluidRow(
          box(
            title = "📋 Detailed Benchmark Results",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,  # 修改为默认展开
            div(class = "data-table-container",
                DTOutput(ns("benchmark_table"))
            )
          )
        )
    )
  )
}

# LLM Benchmarking Server
llmBenchmarkingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Use actual data with proper handling of TBA values
    load_llm_benchmark_data <- function() {
      data <- tibble::tribble(
        ~model_name, ~Developer, ~Release_Year, ~Total_Parameters_B, ~Context_Length_K_tokens, ~avg_response_time, ~match_success_rate, ~studies_accuracy, ~sample_accuracy, ~extract_accuracy, ~amstar_non_critical_accuracy, ~amstar_critical_accuracy, ~amstar_accuracy, ~overall_score,
        "GPT-4o", "OpenAI", 2024, "200", 128, 7.97, 1, 0.64, 0.6, 0.62, 0.727272727, 0.63, 0.69, 1.3,
        "Qwen-2.5", "Alibaba Cloud", 2024, "72", 128, 19.17, 0.98, 0.65, 0.61, 0.63, 0.724279835, 0.65, 0.69, 1.32,
        "Claude-3.5", "Anthropic", 2024, "175", 200, 11.46, 1, 0.58, 0.51, 0.545, 0.642424242, 0.61, 0.63, 1.17,
        "Mistral-8x22B", "Mistral AI", 2024, "1760", 64, 8.84, 0.91, 0.42, 0.38, 0.4, 0.466666667, 0.46, 0.46, 0.86,
        "Deepseek-3.1", "DeepSeek AI", 2025, "685", 128, 6.87, 1, 0.65, 0.64, 0.645, 0.761616162, 0.69, 0.73, 1.37,
        "Gemini-2.0", "Google DeepMind", 2025, "TBA", 1000, 5.85, 0.89, 0.78, 0.69, 0.735, 0.700680272, 0.6, 0.66, 1.39,
        "Grok-4", "xAI", 2025, "TBA", 128, 12.88, 1, 0.65, 0.64, 0.645, 0.717171717, 0.66, 0.69, 1.34,
        "Llama-3.3", "Meta", 2024, "70", 128, 12, 0.96, 0.64, 0.6, 0.62, 0.71278826, 0.61, 0.67, 1.29,
        "Mistral-nemo", "Mistral AI", 2024, "12", 128, 9.18, 0.95, 0.21, 0.17, 0.19, 0.395299145, 0.45, 0.42, 0.61,
        "Qwen-3", "Alibaba Cloud", 2025, "235", 128, 15.98, 1, 0.67, 0.62, 0.645, 0.76969697, 0.69, 0.74, 1.38
      )
      
      # Ensure numeric columns are correct type
      numeric_cols <- c("avg_response_time", "match_success_rate", "studies_accuracy", 
                        "sample_accuracy", "extract_accuracy", "amstar_non_critical_accuracy",
                        "amstar_critical_accuracy", "amstar_accuracy", "overall_score")
      
      for(col in numeric_cols) {
        data[[col]] <- as.numeric(data[[col]])
      }
      
      # Keep Total_Parameters_B as character to handle TBA values
      data$Total_Parameters_B <- as.character(data$Total_Parameters_B)
      
      return(data)
    }
    
    # Load data
    benchmark_data <- reactive({
      req(input$top_n)
      
      data <- load_llm_benchmark_data()
      
      # Ensure we don't request more samples than available rows
      available_models <- nrow(data)
      top_n <- min(input$top_n, available_models)
      
      # Sorting
      if (input$sort_order == "desc") {
        data <- data %>% arrange(desc(!!sym(input$sort_by)))
      } else {
        data <- data %>% arrange(!!sym(input$sort_by))
      }
      
      # Take only top_n
      data <- head(data, top_n)
      
      return(data)
    })
    
    # Create column information
    column_info <- reactive({
      tibble(
        id = c("model_name", "Developer", "Release_Year", 
               "Context_Length_K_tokens", "Total_Parameters_B", "avg_response_time", "match_success_rate", "studies_accuracy", 
               "sample_accuracy", "extract_accuracy", "amstar_non_critical_accuracy",
               "amstar_critical_accuracy", "amstar_accuracy", "overall_score"),
        name = c("Model", "Developer", "Release Year", 
                 "Context Length", "Parameters", "Response Time", "Match Success Rate", "Studies Accuracy", "Sample Accuracy", 
                 "Extraction Accuracy",  "AMSTAR2 Non-critical", "AMSTAR2 Critical", "AMSTAR2 Accuracy", "Overall Score"),
        group = c("model", "info", "info", "info", "info", "performance", "performance", "accuracy", "accuracy", 
                  "accuracy", "amstar", "amstar", "amstar", "overall"),
        width = c(2, 1.5, 1, 1, 1, 1, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5, 1.5, 1.5)
      )
    })
    
    # Create row information
    row_info <- reactive({
      data <- benchmark_data()
      tibble(
        id = data$model_name,
        group = "LLM Evaluation"
      )
    })
    
    # Color palette configuration
    palettes <- reactive({
      list(
        performance = "Blues",
        accuracy = "Greens", 
        amstar = "Purples",
        overall = "RdYlBu",
        info = "Greys"
      )
    })
    
    # Render plot
    output$benchmark_plot <- renderPlot({
      data <- benchmark_data()
      
      # Ensure data is not empty
      req(nrow(data) > 0)
      
      col_info <- column_info()
      row_info <- row_info()
      palette_list <- palettes()
      
      create_enhanced_model_plot(data, col_info, row_info, palette_list)
    }, res = 96)
    
    # Render data table
    output$benchmark_table <- renderDT({
      data <- benchmark_data()
      
      # Ensure data is not empty
      req(nrow(data) > 0)
      
      datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        rownames = FALSE,
        extensions = 'Buttons'
      ) %>%
        formatPercentage(c("match_success_rate", "studies_accuracy", "sample_accuracy", 
                           "extract_accuracy", "amstar_non_critical_accuracy", 
                           "amstar_critical_accuracy", "amstar_accuracy", "overall_score"), 
                         digits = 1) %>%
        formatRound(c("avg_response_time"), digits = 2)
    })
    
    # Refresh button event
    observeEvent(input$refresh_btn, {
      # Reload data
      benchmark_data()
    })
  })
}


shinyApp(ui, server)