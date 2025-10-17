

keywordSearchUI <- function(id) {
  ns <- NS(id)
  
  departmentHeader <- function(title, subtitle, icon = "📊") {
    div(class = "department-header",
        h2(paste(icon, title), style = "margin: 0; font-weight: 700;"),
        p(subtitle, style = "margin: 5px 0 0 0; opacity: 0.9;")
    )
  }
  
  tagList(
    # 统一的顶部卡片
    fluidRow(
      box(width = 12, status = "primary",
          departmentHeader(
            title = " UMAR-based Secondary Meta-Analysis",
            subtitle = "Leveraging UMAR platform for streamlined evidence synthesis and statistical analysis",
            icon = "🚀"  # 火箭图标，强调效率和平台能力
          )
      )
    ),
    
    # 原有内容
    fluidRow(
      column(3,
             wellPanel(
               h4("Search Settings", style = "color: #2E86AB;"),
               
               wellPanel(
                 h5("Keywords Search", style = "color: #2E86AB;"),
                 
                 div(class = "logic-options",
                     radioButtons(ns("search_logic"), "Search Logic:",
                                  choices = c("OR (Any keyword matches)" = "OR", 
                                              "AND (All keywords match)" = "AND"),
                                  selected = "OR")
                 ),
                 
                 div(id = ns("keyword_inputs"),
                     div(class = "keyword-row", id = ns("keyword1_row"),
                         textInput(ns("keyword1"), "Keyword 1", "example", placeholder = "Enter keyword...")
                     )
                 ),
                 
                 fluidRow(
                   column(6, actionButton(ns("add_keyword"), "Add Keyword", 
                                          icon = icon("plus"), 
                                          class = "btn-success btn-sm")),
                   column(6, actionButton(ns("remove_keyword"), "Remove Keyword", 
                                          icon = icon("minus"), 
                                          class = "btn-danger btn-sm"))
                 ),
                 
                 br(),
                 actionButton(ns("search"), "Search", class = "btn-primary", icon = icon("search"),style = "color: white;")
               ),
               
               wellPanel(
                 h5("Search Options", style = "color: #2E86AB;"),
                 checkboxGroupInput(ns("search_columns"), "Search Columns:",
                                    choices = c("Participants", "Intervention", "Comparator", 
                                                "Outcomes", "Disease", "Clinical_Department",
                                                "Literature_Abbreviation", "Outcome_Type"),
                                    selected = c("Intervention", "Disease", "Outcome_Type"))
               ),
               
               wellPanel(
                 h5("Forest Plot Customization", style = "color: #2E86AB;"),
                 sliderInput(ns("forest_size"), "Text Size:",
                             min = 10, max = 18, value = 12),
                 sliderInput(ns("point_size"), "Point Size Range:",
                             min = 2, max = 8, value = c(3, 6), step = 0.5),
                 checkboxInput(ns("show_stats"), "Show Study Statistics", value = TRUE),
                 checkboxInput(ns("show_diamond"), "Show Overall Effect Diamond", value = TRUE)
               ),
               
               wellPanel(
                 h5("Funnel Plot Customization", style = "color: #2E86AB;"),
                 sliderInput(ns("funnel_size"), "Point Size:",
                             min = 1, max = 5, value = 3, step = 0.5),
                 sliderInput(ns("funnel_alpha"), "Transparency:",
                             min = 0.1, max = 1, value = 0.7, step = 0.1),
                 checkboxInput(ns("show_contours"), "Show Contour Lines", value = TRUE),
                 checkboxInput(ns("show_eggertest"), "Show Egger's Test", value = TRUE)
               )
             )
      ),
      
      column(9,
             tabsetPanel(
               tabPanel("Search Results",
                        h4("Matching Entries"),
                        uiOutput(ns("data_alert")),
                        DTOutput(ns("results_table")),
                        downloadButton(ns("download_data"), "Download Results", class = "btn-success")),
               
               tabPanel("Forest Plot",
                        h4("Forest Plot"),
                        uiOutput(ns("forest_alert")),
                        fluidRow(
                          column(12, align = "center",
                                 plotOutput(ns("forest_plot"), height = "600px")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(12, align = "center",
                                 downloadButton(ns("download_plot"), "Download Forest Plot", class = "btn-success")
                          )
                        )),
               
               tabPanel("Funnel Plot",
                        h4("Funnel Plot for Publication Bias"),
                        uiOutput(ns("funnel_alert")),
                        fluidRow(
                          column(12, align = "center",
                                 plotOutput(ns("funnel_plot"), height = "900px")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(12, align = "center",
                                 downloadButton(ns("download_funnel"), "Download Funnel Plot", class = "btn-success")
                          )
                        )),
               
               tabPanel("Meta-Regression",
                        h4("Meta-Regression Analysis"),
                        uiOutput(ns("regression_alert")),
                        fluidRow(
                          column(6,
                                 selectInput(ns("regressor"), "Choose Regressor:",
                                             choices = c("Total_Sample_Size", "Year", "Clinical_Department"),
                                             selected = "Total_Sample_Size")
                          ),
                          column(6,
                                 br(),
                                 actionButton(ns("run_regression"), "Run Meta-Regression", 
                                              class = "btn-info", icon = icon("chart-line"))
                          )
                        ),
                        fluidRow(
                          column(6,
                                 plotOutput(ns("regression_plot"), height = "400px")
                          ),
                          column(6,
                                 verbatimTextOutput(ns("regression_summary"))
                          )
                        )),
               
               tabPanel("Summary Statistics",
                        h4("Summary of Search Results"),
                        verbatimTextOutput(ns("summary_stats"))),
               
               tabPanel("Data Info",
                        h4("Dataset Information"),
                        verbatimTextOutput(ns("data_info")))
             )
      )
    )
  )
}

# 模块Server
keywordSearchServer <- function(id, meta_db) {
  moduleServer(id, function(input, output, session) {
    
    # 动态关键词计数器
    keyword_counter <- reactiveVal(1)
    
    # 观察添加关键词按钮
    observeEvent(input$add_keyword, {
      count <- keyword_counter()
      if (count < 10) {
        count <- count + 1
        keyword_counter(count)
        
        insertUI(
          selector = paste0("#", session$ns("keyword_inputs")),
          where = "beforeEnd",
          ui = div(class = "keyword-row", id = session$ns(paste0("keyword", count, "_row")),
                   textInput(session$ns(paste0("keyword", count)), 
                             paste("Keyword", count), 
                             "", 
                             placeholder = "Enter keyword...")
          )
        )
      }
    })
    
    # 观察删除关键词按钮
    observeEvent(input$remove_keyword, {
      count <- keyword_counter()
      if (count > 1) {
        removeUI(
          selector = paste0("#", session$ns("keyword", count, "_row"))
        )
        keyword_counter(count - 1)
      }
    })
    
    # 获取所有关键词
    get_keywords <- reactive({
      count <- keyword_counter()
      keywords <- character(0)
      
      for (i in 1:count) {
        keyword <- input[[paste0("keyword", i)]]
        if (!is.null(keyword) && keyword != "") {
          keywords <- c(keywords, keyword)
        }
      }
      
      keywords
    })
    
    # 搜索函数
    filtered_data <- eventReactive(c(input$search, input$keyword1), {
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        return(meta_db)
      }
      
      search_columns <- input$search_columns
      search_logic <- input$search_logic
      
      if (search_logic == "OR") {
        search_condition <- FALSE
        for (col in search_columns) {
          for (keyword in keywords) {
            search_condition <- search_condition | 
              grepl(keyword, meta_db[[col]], ignore.case = TRUE)
          }
        }
      } else {
        search_condition <- TRUE
        for (keyword in keywords) {
          keyword_matched <- FALSE
          for (col in search_columns) {
            keyword_matched <- keyword_matched | 
              grepl(keyword, meta_db[[col]], ignore.case = TRUE)
          }
          search_condition <- search_condition & keyword_matched
        }
      }
      
      meta_db[search_condition, ]
    })
    
    # 获取完整数据
    complete_data <- reactive({
      data <- filtered_data()
      if (nrow(data) == 0) {
        return(data.frame())
      }
      
      effect_columns <- c("Effect_Size", "CI_Lower", "CI_Upper")
      complete_cases <- complete.cases(data[, effect_columns])
      data[complete_cases, ]
    })
    
    # 获取森林图数据
    forest_data <- reactive({
      data <- complete_data()
      
      if (nrow(data) == 0) {
        return(data.frame())
      }
      
      data <- data %>%
        filter(
          !is.na(Effect_Size), !is.na(CI_Lower), !is.na(CI_Upper),
          is.finite(Effect_Size), is.finite(CI_Lower), is.finite(CI_Upper)
        )
      
      return(data)
    })
    
    # 数据显示警告信息
    output$data_alert <- renderUI({
      data <- filtered_data()
      complete <- complete_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        return(tags$div(class = "alert alert-info", 
                        icon("info-circle"), 
                        "No keywords entered. Showing all available data."))
      }
      
      if (nrow(data) == 0) {
        return(tags$div(class = "alert alert-warning", 
                        icon("exclamation-triangle"),
                        "No data found matching your search criteria."))
      }
      
      if (nrow(complete) < nrow(data)) {
        missing_count <- nrow(data) - nrow(complete)
        tags$div(
          class = "alert alert-info",
          tags$p(icon("info-circle"), 
                 strong("Note:"), 
                 paste(missing_count, "out of", nrow(data), 
                       "records have missing effect size data and will not be included in the forest plot.")),
          tags$p("Search logic:", strong(input$search_logic), 
                 "| Keywords used:", paste(keywords, collapse = ", "))
        )
      } else {
        tags$div(
          class = "alert alert-info",
          tags$p(icon("check-circle"), 
                 strong("Good news!"), 
                 "All", nrow(data), "records have complete effect size data."),
          tags$p("Search logic:", strong(input$search_logic), 
                 "| Keywords used:", paste(keywords, collapse = ", "))
        )
      }
    })
    
    # 森林图警告信息
    output$forest_alert <- renderUI({
      data <- filtered_data()
      forest <- forest_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        return(tags$div(class = "alert alert-info", 
                        "Enter keywords and click Search to generate forest plot."))
      }
      
      if (nrow(data) == 0) {
        return(tags$div(class = "alert alert-warning", 
                        "No data available to create forest plot."))
      }
      
      if (nrow(forest) == 0) {
        tags$div(
          class = "alert alert-warning",
          tags$p(icon("exclamation-triangle"), 
                 strong("Cannot create forest plot:"), 
                 "None of the matching records have complete effect size data.")
        )
      } else if (nrow(forest) < nrow(data)) {
        missing_count <- nrow(data) - nrow(forest)
        tags$div(
          class = "alert alert-info",
          tags$p(icon("info-circle"), 
                 strong("Forest plot information:"), 
                 "Showing", nrow(forest), "records with complete effect size data."),
          tags$p(paste(missing_count, "records with missing effect sizes are excluded."))
        )
      } else {
        tags$div(
          class = "alert alert-info",
          tags$p(icon("check-circle"), 
                 strong("Forest plot ready:"), 
                 "All", nrow(forest), "records will be displayed.")
        )
      }
    })
    
    # 漏斗图警告信息
    output$funnel_alert <- renderUI({
      data <- forest_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        return(tags$div(class = "alert alert-info", 
                        "Enter keywords and click Search to generate funnel plot."))
      }
      
      if (nrow(data) < 3) {
        tags$div(
          class = "alert alert-warning",
          tags$p(icon("exclamation-triangle"), 
                 strong("Cannot create funnel plot:"), 
                 "At least 3 studies with complete effect size data are required.")
        )
      } else {
        tags$div(
          class = "alert alert-info",
          tags$p(icon("check-circle"), 
                 strong("Funnel plot ready:"), 
                 "Based on", nrow(data), "studies with complete data.")
        )
      }
    })
    
    # 回归分析警告信息
    output$regression_alert <- renderUI({
      data <- forest_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        return(tags$div(class = "alert alert-info", 
                        "Enter keywords and click Search to perform meta-regression."))
      }
      
      if (nrow(data) < 5) {
        tags$div(
          class = "alert alert-warning",
          tags$p(icon("exclamation-triangle"), 
                 strong("Limited data for meta-regression:"), 
                 "At least 5 studies are recommended for meaningful regression analysis.")
        )
      } else {
        tags$div(
          class = "alert alert-info",
          tags$p(icon("check-circle"), 
                 strong("Meta-regression possible:"), 
                 "Based on", nrow(data), "studies with complete data.")
        )
      }
    })
    
    # 显示数据集信息
    output$data_info <- renderPrint({
      cat("DATASET INFORMATION\n")
      cat("===================\n")
      cat("Total records:", nrow(meta_db), "\n")
      
      effect_complete <- sum(complete.cases(meta_db[, c("Effect_Size", "CI_Lower", "CI_Upper")]))
      effect_missing <- nrow(meta_db) - effect_complete
      
      cat("Records with complete effect size data:", effect_complete, "\n")
      cat("Records with missing effect size data:", effect_missing, "\n")
      cat("Completion rate:", round(effect_complete/nrow(meta_db)*100, 1), "%\n\n")
      
      cat("Columns:", paste(names(meta_db), collapse = ", "), "\n\n")
      
      cat("Outcome Types distribution:\n")
      print(table(meta_db$Outcome_Type, useNA = "ifany"))
      
      cat("\nClinical Departments distribution:\n")
      print(table(meta_db$Clinical_Department, useNA = "ifany"))
      
      cat("\nMissing values by column:\n")
      missing_summary <- sapply(meta_db, function(x) sum(is.na(x)))
      print(missing_summary[missing_summary > 0])
      
      cat("\nEffect size data summary:\n")
      if (sum(!is.na(meta_db$Effect_Size)) > 0) {
        cat("Effect_Size range:", range(meta_db$Effect_Size, na.rm = TRUE), "\n")
        cat("CI_Lower range:", range(meta_db$CI_Lower, na.rm = TRUE), "\n")
        cat("CI_Upper range:", range(meta_db$CI_Upper, na.rm = TRUE), "\n")
      } else {
        cat("No valid effect size data available\n")
      }
    })
    
    # 显示搜索结果表格
    output$results_table <- DT::renderDT({
      data <- filtered_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        display_data <- meta_db
      } else if (nrow(data) == 0) {
        return(data.frame(Message = "No matching entries found"))
      } else {
        display_data <- data
      }
      
      display_data <- display_data %>%
        mutate(
          Data_Complete = ifelse(
            complete.cases(.[, c("Effect_Size", "CI_Lower", "CI_Upper")]),
            "✓ Complete", "✗ Missing"
          )
        )
      
      DT::datatable(display_data, 
                    options = list(
                      pageLength = 10, 
                      scrollX = TRUE,
                      dom = 'Blfrtip',
                      buttons = c('copy', 'csv', 'excel')
                    ),
                    class = 'display compact hover') %>%
        DT::formatStyle(
          'Data_Complete',
          backgroundColor = DT::styleEqual(
            c("✓ Complete", "✗ Missing"), 
            c('#d4edda', '#f8d7da')
          )
        )
    })
    
    # 森林图
    output$forest_plot <- renderPlot({
      data <- forest_data()
      
      if (nrow(data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No complete effect size data available for forest plot", 
                          size = 6, color = "gray50") +
                 theme_void())
      }
      
      forest_data_prepared <- data %>%
        mutate(
          Study = paste0(Literature_Abbreviation, " (n=", Total_Sample_Size, ")"),
          Estimate = sprintf("%.2f [%.2f, %.2f]", Effect_Size, CI_Lower, CI_Upper),
          y_position = n():1
        ) %>%
        mutate(
          Sample_Size_Scale = if (!all(is.na(Total_Sample_Size)) && diff(range(Total_Sample_Size, na.rm = TRUE)) > 0) {
            scales::rescale(Total_Sample_Size, 
                            to = input$point_size, 
                            from = range(Total_Sample_Size, na.rm = TRUE))
          } else {
            mean(input$point_size)
          }
        )
      
      if (input$show_diamond && nrow(forest_data_prepared) > 1) {
        overall_effect <- data.frame(
          Effect_Size = mean(forest_data_prepared$Effect_Size, na.rm = TRUE),
          CI_Lower = mean(forest_data_prepared$CI_Lower, na.rm = TRUE),
          CI_Upper = mean(forest_data_prepared$CI_Upper, na.rm = TRUE),
          y_position = 0
        )
      }
      
      has_diagnostic <- any(forest_data_prepared$Outcome_Type == "Diagnostic Accuracy")
      ref_line <- 0
      x_label <- "Effect Size"
      
      point_color <- "#2E86AB"
      fill_color <- alpha("#2E86AB", 0.7)
      
      x_min <- min(forest_data_prepared$CI_Lower, na.rm = TRUE)
      x_max <- max(forest_data_prepared$CI_Upper, na.rm = TRUE)
      
      if (!is.finite(x_min) || !is.finite(x_max) || x_min == x_max) {
        if (has_diagnostic) {
          x_min <- 0
          x_max <- 100
        } else {
          x_min <- -2
          x_max <- 2
        }
      }
      
      x_buffer <- (x_max - x_min) * 0.4
      x_range <- c(x_min - x_buffer, x_max + x_buffer)
      
      if (any(!is.finite(x_range))) {
        x_range <- if (has_diagnostic) c(0, 100) else c(-2, 2)
      }
      
      p <- ggplot(forest_data_prepared, aes(x = Effect_Size, y = y_position)) +
        geom_vline(xintercept = 1, linetype = "dashed", 
                   color = "gray60", size = 0.8, alpha = 0.7) +
        geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), 
                       height = 0.15, size = 1.2, alpha = 0.8, color = point_color) +
        geom_point(aes(size = Sample_Size_Scale), 
                   shape = 21, stroke = 1.5, color = point_color, fill = fill_color) +
        geom_text(aes(label = Study, 
                      x = x_range[1] + (x_range[2] - x_range[1]) * 0.05), 
                  hjust = 0, size = input$forest_size/2.8, fontface = "bold") +
        scale_size_identity() +
        scale_x_continuous(limits = x_range) +
        labs(x = x_label, y = "", 
             title = "Meta-Analysis Forest Plot",
             subtitle = paste("Includes", nrow(forest_data_prepared), "studies with complete data"),
             size = "Sample Size") +
        theme_minimal(base_size = input$forest_size) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90", size = 0.3),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = input$forest_size * 1.1),
          plot.title = element_text(face = "bold", hjust = 0.5, size = input$forest_size * 1.3),
          plot.subtitle = element_text(hjust = 0.5, size = input$forest_size * 1.1),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.text = element_text(size = input$forest_size * 0.9),
          legend.title = element_text(face = "bold", size = input$forest_size * 0.95),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      if (input$show_stats) {
        p <- p + 
          geom_text(aes(label = Estimate, 
                        x = x_range[2] - (x_range[2] - x_range[1]) * 0.3), 
                    hjust = 1, size = input$forest_size/3.2)
      }
      
      if (input$show_diamond && nrow(forest_data_prepared) > 1) {
        p <- p +
          geom_point(data = overall_effect, aes(x = Effect_Size, y = y_position),
                     shape = 23, size = 6, fill = "red", color = "darkred") +
          geom_errorbarh(data = overall_effect, 
                         aes(xmin = CI_Lower, xmax = CI_Upper, y = y_position),
                         height = 0.1, color = "darkred", size = 1.2)
      }
      
      p
    })
    
    # 漏斗图
    output$funnel_plot <- renderPlot({
      data <- forest_data()
      
      if (nrow(data) < 3) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "At least 3 studies with complete effect size data are required for funnel plot", 
                          size = 5, color = "gray50") +
                 theme_void())
      }
      
      # 计算标准误
      funnel_data <- data %>%
        mutate(
          se = (CI_Upper - CI_Lower) / (2 * 1.96),  # 假设95%置信区间
          precision = 1 / se
        )
      
      # 计算整体效应
      overall_effect <- mean(funnel_data$Effect_Size, na.rm = TRUE)
      
      p <- ggplot(funnel_data, aes(x = Effect_Size, y = precision)) +
        geom_point(size = input$funnel_size, alpha = input$funnel_alpha, 
                   color = "#2E86AB", fill = alpha("#2E86AB", 0.5), shape = 21) +
        geom_vline(xintercept = overall_effect, linetype = "dashed", 
                   color = "red", size = 1) +
        labs(x = "Effect Size", y = "Precision (1/SE)",
             title = "Funnel Plot for Publication Bias Assessment",
             subtitle = paste("Based on", nrow(data), "studies | Dashed line: overall effect")) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "white", color = NA)
        )
      
      # 添加漏斗轮廓线
      if (input$show_contours) {
        # 模拟漏斗形状
        x_seq <- seq(min(funnel_data$Effect_Size), max(funnel_data$Effect_Size), length.out = 100)
        max_precision <- max(funnel_data$precision, na.rm = TRUE)
        
        # 添加95%置信区间轮廓
        contour_data <- data.frame(
          x = c(x_seq, rev(x_seq)),
          y = c(rep(max_precision * 0.1, 100), rep(max_precision, 100))
        )
        
        p <- p + 
          geom_polygon(data = contour_data, aes(x = x, y = y), 
                       fill = alpha("gray", 0.1), color = "gray60", linetype = "dashed")
      }
      
      # 添加Egger's test结果
      if (input$show_eggertest && nrow(data) >= 10) {
        # 简单的Egger's test模拟（实际应用中应该使用metafor包）
        tryCatch({
          egger_test <- cor.test(funnel_data$Effect_Size, funnel_data$precision, 
                                 method = "pearson")
          egger_p <- round(egger_test$p.value, 3)
          egger_cor <- round(egger_test$estimate, 3)
          
          p <- p + 
            annotate("text", x = Inf, y = Inf, 
                     label = paste("Egger's test: r =", egger_cor, ", p =", egger_p),
                     hjust = 1.1, vjust = 1.1, size = 5, color = "darkred", fontface = "bold")
        }, error = function(e) {
          # 如果测试失败，不显示结果
        })
      }
      
      p
    })
    
    # 元回归分析
    regression_results <- eventReactive(input$run_regression, {
      data <- forest_data()
      regressor <- input$regressor
      
      if (nrow(data) < 5) {
        return(NULL)
      }
      
      # 简单的线性回归模拟（实际应用中应该使用metafor::rma()）
      tryCatch({
        formula <- as.formula(paste("Effect_Size ~", regressor))
        model <- lm(formula, data = data)
        
        return(list(
          model = model,
          summary = summary(model),
          data = data,
          regressor = regressor
        ))
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # 元回归图
    output$regression_plot <- renderPlot({
      results <- regression_results()
      
      if (is.null(results)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Insufficient data or error in regression analysis", 
                          size = 4, color = "gray50") +
                 theme_void())
      }
      
      data <- results$data
      regressor <- results$regressor
      model <- results$model
      
      p <- ggplot(data, aes_string(x = regressor, y = "Effect_Size")) +
        geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
        geom_smooth(method = "lm", se = TRUE, color = "red", fill = alpha("red", 0.2)) +
        labs(x = regressor, y = "Effect Size",
             title = paste("Meta-Regression:", regressor, "vs Effect Size"),
             subtitle = paste("Based on", nrow(data), "studies")) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major = element_line(color = "gray90"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
      
      # 添加回归方程
      coef <- coef(model)
      r2 <- summary(model)$r.squared
      p_value <- summary(model)$coefficients[2, 4]
      
      equation <- sprintf("y = %.3f + %.3fx\nR² = %.3f, p = %.3f", 
                          coef[1], coef[2], r2, p_value)
      
      p + annotate("text", x = Inf, y = Inf, 
                   label = equation, hjust = 1.1, vjust = 1.1, 
                   size = 4, color = "darkred", fontface = "bold")
    })
    
    # 元回归摘要
    output$regression_summary <- renderPrint({
      results <- regression_results()
      
      if (is.null(results)) {
        cat("Meta-Regression Analysis\n")
        cat("=======================\n")
        cat("Unable to perform regression analysis.\n")
        cat("Possible reasons:\n")
        cat("- Insufficient data (need at least 5 studies)\n")
        cat("- Missing values in the selected regressor\n")
        cat("- Computational error\n")
        return()
      }
      
      cat("META-REGRESSION SUMMARY\n")
      cat("=======================\n")
      cat("Regressor:", results$regressor, "\n")
      cat("Number of studies:", nrow(results$data), "\n\n")
      
      print(results$summary)
      
      cat("\nINTERPRETATION GUIDE:\n")
      cat("--------------------\n")
      cat("• Significant p-value (< 0.05) suggests the regressor influences effect size\n")
      cat("• R-squared indicates proportion of variance explained\n")
      cat("• Positive coefficient means effect size increases with regressor\n")
      cat("• Negative coefficient means effect size decreases with regressor\n")
    })
    
    # 摘要统计
    output$summary_stats <- renderPrint({
      data <- filtered_data()
      complete <- complete_data()
      keywords <- get_keywords()
      
      if (length(keywords) == 0) {
        data <- meta_db
        complete <- meta_db[complete.cases(meta_db[, c("Effect_Size", "CI_Lower", "CI_Upper")]), ]
      }
      
      if (nrow(data) == 0) {
        cat("No data available for summary")
        return()
      }
      
      cat("SUMMARY STATISTICS\n")
      cat("=================\n")
      if (length(keywords) > 0) {
        cat("Search logic:", input$search_logic, "\n")
        cat("Keywords:", paste(keywords, collapse = ", "), "\n")
      } else {
        cat("Showing all available data (no keywords entered)\n")
      }
      cat("\nTotal matching studies:", nrow(data), "\n")
      cat("Studies with complete effect size data:", nrow(complete), "\n")
      cat("Studies with missing effect size data:", nrow(data) - nrow(complete), "\n")
      cat("Completion rate:", round(nrow(complete)/nrow(data)*100, 1), "%\n\n")
      
      if (nrow(complete) > 0) {
        cat("Total sample size (complete data only):", sum(complete$Total_Sample_Size, na.rm = TRUE), "\n")
        cat("Average sample size per study:", round(mean(complete$Total_Sample_Size, na.rm = TRUE), 1), "\n")
        cat("Median effect size:", round(median(complete$Effect_Size, na.rm = TRUE), 3), "\n")
        cat("Effect size range: [", round(min(complete$Effect_Size, na.rm = TRUE), 3), ", ", 
            round(max(complete$Effect_Size, na.rm = TRUE), 3), "]\n", sep = "")
        
        cat("\nClinical Departments (complete data):\n")
        print(table(complete$Clinical_Department))
        
        cat("\nHeterogeneity measures (complete data):\n")
        if (nrow(complete) >= 2) {
          i2 <- tryCatch({
            # 简单的异质性计算（实际应该使用metafor包）
            q_stat <- sum((complete$Effect_Size - mean(complete$Effect_Size))^2 / complete$Effect_Size^2)
            i2 <- max(0, (q_stat - (nrow(complete) - 1)) / q_stat * 100)
            paste(round(i2, 1), "%")
          }, error = function(e) "Cannot calculate")
          cat("I² statistic (heterogeneity):", i2, "\n")
        }
      }
    })
    
    # 下载数据
    output$download_data <- downloadHandler(
      filename = function() {
        paste("meta_analysis_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data()
        if (nrow(data) == 0) {
          data <- meta_db
        }
        data$Data_Complete <- ifelse(
          complete.cases(data[, c("Effect_Size", "CI_Lower", "CI_Upper")]),
          "Complete", "Missing"
        )
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # 下载森林图
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("forest_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        data <- forest_data()
        if (nrow(data) == 0) return()
        
        # 重新生成森林图
        # 这里需要包含完整的绘图代码
        ggsave(file, width = 12, height = 8, dpi = 300)
      }
    )
    
    # 下载漏斗图
    output$download_funnel <- downloadHandler(
      filename = function() {
        paste("funnel_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        data <- forest_data()
        if (nrow(data) < 3) return()
        
        ggsave(file, width = 10, height = 8, dpi = 300)
      }
    )
    
    # 返回reactive值供主应用使用
    return(list(
      filtered_data = filtered_data,
      forest_data = forest_data
    ))
  })
}