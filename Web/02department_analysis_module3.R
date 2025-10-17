# department_analysis_module.R
#' Department Analysis Module UI
#'
#' @param id Module ID
#' @return UI components for department analysis
departmentAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 自定义CSS样式
    tags$style(HTML("
      .department-card {
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
      
      .selectize-control .selectize-input {
        border-radius: 8px;
        border: 2px solid #e9ecef;
        transition: border-color 0.3s ease;
      }
      
      .selectize-control .selectize-input:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      
      .department-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 15px 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }
    ")),
    
    fluidPage(
      # 顶部标题卡片
      div(class = "department-header",
          h2("📊 Clinical Department Analysis", style = "margin: 0; font-weight: 700;"),
          p("Explore meta-analysis data across different clinical departments", 
            style = "margin: 5px 0 0 0; opacity: 0.9;")
      ),
      
      fluidRow(
        # 侧边栏
        column(
          width = 3,
          div(class = "sidebar-panel",
              h4("🔍 Department Selection", style = "color: #2C3E50; margin-top: 0;"),
              
              selectizeInput(
                ns("department"), 
                label = NULL,
                choices = NULL,
                selected = NULL,
                multiple = FALSE,
                options = list(
                  placeholder = 'Search department...',
                  maxOptions = 100,
                  create = FALSE
                )
              ),
              
              br(),
              
              actionButton(
                ns("plot_btn"), 
                "🚀 Generate Analysis", 
                class = "btn-primary",
                width = "100%"
              ),
              
              br(), br(),
              
              # 统计信息卡片
              uiOutput(ns("stats_cards")),
              
              br(),
              
              div(style = "background: #e8f4fc; padding: 12px; border-radius: 8px;",
                  p("💡 Departments are sorted by publication count", 
                    style = "margin: 0; font-size: 12px; color: #2C3E50;")
              )
          )
        ),
        
        # 主内容区
        column(
          width = 9,
          # 当前部门信息
          uiOutput(ns("current_department")),
          
          # 图表标签页
          div(style = "margin-top: 20px;",
              tabsetPanel(
                id = ns("plot_tabs"),
                type = "tabs",
                
                tabPanel(
                  "📈 Disease Distribution",
                  icon = icon("chart-bar"),
                  br(),
                  div(class = "plot-container",
                      plotOutput(ns("bar_plot"), height = "550px")
                  )
                ),
                
                tabPanel(
                  "🫧 Study Characteristics", 
                  icon = icon("bubble-chart"),
                  br(),
                  div(class = "plot-container",
                      plotOutput(ns("bubble_plot"), height = "550px")
                  )
                ),
                
                tabPanel(
                  "⚖️ Funnel Plot",
                  icon = icon("balance-scale"),
                  br(),
                  div(class = "plot-container",
                      plotOutput(ns("funnel_plot"), height = "550px")
                  )
                ),
                
                tabPanel(
                  "📊 Effect Size Analysis",
                  icon = icon("project-diagram"),
                  br(),
                  div(class = "plot-container",
                      plotOutput(ns("effect_bubble_plot"), height = "550px")
                  )
                )
              )
          )
        )
      )
    )
  )
}

#' Department Analysis Module Server
#'
#' @param id Module ID
#' @param umar_data Clean UMAR data
#' @return Server logic for department analysis
departmentAnalysisServer <- function(id, umar_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 响应式数据处理
    processed_data <- reactive({
      
      umar_data 
    })
    
    # Preprocess data
    department_summary_enhanced <- reactive({
      req(processed_data())
      
      processed_data() %>%
        group_by(Clinical_Department) %>%
        summarise(
          Count = n(),
          Avg_Sample_Size = mean(Total_Sample_Size, na.rm = TRUE),
          Median_Sample_Size = median(Total_Sample_Size, na.rm = TRUE),
          Avg_Num_Studies = mean(Num_Studies, na.rm = TRUE),
          Median_Num_Studies = median(Num_Studies, na.rm = TRUE),
          Sig_Proportion = mean(P_Value < 0.05, na.rm = TRUE) * 100,
          .groups = 'drop'
        )
    })
    
    # Get sorted departments
    sorted_departments <- reactive({
      req(department_summary_enhanced())
      
      department_summary_enhanced() %>%
        arrange(desc(Count)) %>%
        pull(Clinical_Department)
    })
    
    # Get default department
    default_department <- reactive({
      req(sorted_departments())
      sorted_departments()[1]
    })
    
    # Update department selection
    observe({
      req(sorted_departments())
      
      updateSelectizeInput(
        session, 
        "department",
        choices = sorted_departments(),
        selected = default_department(),
        server = TRUE
      )
    })
    
    # 统计信息卡片
    output$stats_cards <- renderUI({
      req(default_department(), department_summary_enhanced())
      
      dept_stats <- department_summary_enhanced() %>%
        filter(Clinical_Department == default_department())
      
      if(nrow(dept_stats) > 0) {
        tagList(
          div(class = "stats-card",
              h5("📚 Total Publications", style = "color: #2C3E50; margin: 0 0 5px 0;"),
              h3(style = "color: #3498DB; margin: 0;", dept_stats$Count)
          ),
          div(class = "stats-card",
              h5("👥 Avg Sample Size", style = "color: #2C3E50; margin: 0 0 5px 0;"),
              h3(style = "color: #E74C3C; margin: 0;", round(dept_stats$Avg_Sample_Size, 0))
          ),
          div(class = "stats-card",
              h5("📊 Significant Results", style = "color: #2C3E50; margin: 0 0 5px 0;"),
              h3(style = "color: #27AE60; margin: 0;", paste0(round(dept_stats$Sig_Proportion, 1), "%"))
          )
        )
      }
    })
    
    # Display current selected department
    output$current_department <- renderUI({
      req(input$department, department_summary_enhanced())
      
      dept_stats <- department_summary_enhanced() %>%
        filter(Clinical_Department == input$department)
      
      if(nrow(dept_stats) > 0) {
        div(class = "department-card",
            fluidRow(
              column(8,
                     h3(paste("🏥", input$department), style = "margin: 0 0 5px 0;"),
                     p(paste("Comprehensive analysis of", dept_stats$Count, "meta-analyses"), 
                       style = "margin: 0; opacity: 0.9;")
              ),
              column(4, style = "text-align: right;",
                     div(style = "background: rgba(255,255,255,0.2); padding: 10px; border-radius: 8px;",
                         h4(dept_stats$Count, style = "margin: 0; color: white;"),
                         p("Studies", style = "margin: 0; font-size: 12px;")
                     )
              )
            )
        )
      }
    })
    
    # Reactive data filtering
    filtered_data <- reactive({
      req(input$department, processed_data())
      
      processed_data() %>%
        filter(Clinical_Department == input$department)
    })
    
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
    
    # 1. Disease Distribution Bar Plot
    output$bar_plot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      dept_disease_data <- data %>%
        group_by(Disease) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        slice_max(Count, n = 10)
      
      if (nrow(dept_disease_data) > 0) {
        ggplot(dept_disease_data, aes(x = reorder(Disease, Count), y = Count)) +
          ggpattern::geom_col_pattern(
            pattern = "gradient",
            pattern_fill = "#3498DB",
            pattern_fill2 = "#2C3E50",
            fill = "#3498DB",  # 基础填充色
            alpha = 0.9, 
            width = 0.7
          ) +
          geom_text(aes(label = Count), hjust = -0.2, size = 5, 
                    color = "#2C3E50", fontface = "bold") +
          coord_flip() +
          labs(
            title = paste("Top Diseases in", input$department),
            subtitle = "Distribution of Meta-analyses by Disease Area",
            x = NULL,
            y = "Number of Meta-analyses"
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
          pretty_theme() +
          theme(
            axis.text.y = element_text(size = 12, face = "bold", color = "#2C3E50"),
            panel.grid.major.y = element_blank()
          )
      } else {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "📊 No data available for this department", 
                   size = 8, color = "#BDC3C7") +
          theme_void()
      }
    })
    
    # 2. Sample Size vs Study Number Bubble Plot
    output$bubble_plot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      dept_disease_summary <- data %>%
        group_by(Disease) %>%
        summarise(
          Count = n(),
          Median_Sample_Size = median(Total_Sample_Size, na.rm = TRUE),
          Median_Num_Studies = median(Num_Studies, na.rm = TRUE),
          Sig_Proportion = mean(P_Value < 0.05, na.rm = TRUE) * 100,
          .groups = 'drop'
        ) %>%
        slice_max(Count, n = 15)
      
      if (nrow(dept_disease_summary) > 0) {
        ggplot(dept_disease_summary, 
               aes(x = Median_Num_Studies, y = Median_Sample_Size, size = Count)) +
          geom_point(aes(color = Sig_Proportion), alpha = 0.8, shape = 19) +
          ggrepel::geom_text_repel(
            aes(label = Disease), 
            size = 4.5, 
            max.overlaps = 15,
            box.padding = 0.6,
            point.padding = 0.2,
            color = "#2C3E50",
            fontface = "bold"
          ) +
          scale_x_log10(labels = scales::comma) +
          scale_y_log10(labels = scales::comma) +
          scale_size_continuous(
            name = "Number of Studies", 
            range = c(4, 12),
            breaks = pretty(dept_disease_summary$Count, n = 4),
            guide = guide_legend(override.aes = list(color = "#3498DB"))
          ) +
          scale_color_gradientn(
            name = "Significant (%)", 
            colors = c("#E74C3C", "#F39C12", "#27AE60"),
            breaks = seq(0, 100, by = 20),
            guide = guide_colorbar(barwidth = 15, barheight = 1)
          ) +
          labs(
            title = paste("Study Characteristics -", input$department),
            subtitle = "Sample Size vs Number of Studies (Top 15 Diseases)",
            x = "Median Number of Studies (Log10)",
            y = "Median Sample Size (Log10)"
          ) +
          pretty_theme()
      } else {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "🫧 No data available for bubble plot", 
                   size = 8, color = "#BDC3C7") +
          theme_void()
      }
    })
    
    # 3. Funnel Plot
    # 3. Funnel Plot
    output$funnel_plot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      # 直接去除异常值 - 使用分位数方法
      dept_data <- data %>%
        filter(!is.na(CI_Width) & !is.na(Effect_Size)) %>%
        filter(CI_Width > 0) %>%
        mutate(
          precision = 1/CI_Width
        ) %>%
        filter(
          # 去除Effect_Size的极端异常值 (去除前后1%)
          Effect_Size >= quantile(Effect_Size, 0.01, na.rm = TRUE) & 
            Effect_Size <= quantile(Effect_Size, 0.99, na.rm = TRUE) &
            # 去除precision的极端异常值 (去除前后1%)
            precision >= quantile(precision, 0.01, na.rm = TRUE) & 
            precision <= quantile(precision, 0.99, na.rm = TRUE)
        )
      
      if (nrow(dept_data) > 5) {
        # 进一步过滤确保数据质量
        dept_data <- dept_data %>%
          filter(
            abs(Effect_Size) < 10,  # 限制效应值范围
            precision < 100         # 限制精度范围
          )
      }
      
      if (nrow(dept_data) > 0) {
        # 动态计算坐标轴范围
        x_limit <- max(abs(dept_data$Effect_Size), na.rm = TRUE) * 1.2
        y_max <- max(dept_data$precision, na.rm = TRUE) * 1.1
        
        ggplot(dept_data, aes(x = Effect_Size, y = precision)) +
          geom_point(aes(color = Effect_Direction, size = Total_Sample_Size), 
                     alpha = 0.7, shape = 16) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "#95A5A6", linewidth = 1) +
          labs(
            title = paste("Publication Bias Assessment -", input$department),
            subtitle = paste("Funnel Plot (", nrow(dept_data), "studies after outlier removal)"),
            x = "Effect Size",
            y = "Precision (1/Confidence Interval Width)"
          ) +
          scale_color_manual(
            values = c(
              "Negative" = "#E74C3C",    # 红色表示负向
              "Positive" = "#27AE60",    # 绿色表示正向
              "Not Significant" = "#95A5A6"  # 灰色表示不显著
            ),
            name = "Effect Direction"
          ) +
          scale_size_continuous(
            name = "Sample Size",
            range = c(2, 6),
            breaks = c(100, 1000, 5000),
            labels = c("100", "1k", "5k+")
          ) +
          scale_x_continuous(limits = c(-x_limit, x_limit)) +
          scale_y_continuous(limits = c(0, y_max)) +
          pretty_theme() +
          theme(legend.position = "right")
      } else {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "⚖️ Insufficient data for funnel plot after outlier removal", 
                   size = 8, color = "#BDC3C7") +
          theme_void()
      }
    })
    
    # 4. Effect Size Bubble Plot
    output$effect_bubble_plot <- renderPlot({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      dept_data <- data %>%
        mutate(
          logP = -log10(pmax(P_Value, 1e-10)),
          logP = pmin(logP, 10),  # 限制最大值为10
          Effect_Size_trimmed = case_when(
            Effect_Size > 3 ~ 3,
            Effect_Size < -3 ~ -3,
            TRUE ~ Effect_Size
          ),
          Significance = case_when(
            P_Value < 0.001 ~ "p < 0.001",
            P_Value < 0.01 ~ "p < 0.01",
            P_Value < 0.05 ~ "p < 0.05",
            TRUE ~ "Not Significant"
          )
        ) %>%
        filter(!is.na(Effect_Size) & !is.na(logP))
      
      if (nrow(dept_data) > 0) {
        x_range <- max(abs(dept_data$Effect_Size_trimmed), na.rm = TRUE)
        
        ggplot(dept_data, aes(x = Effect_Size_trimmed, y = logP)) +
          geom_point(aes(size = sqrt(Total_Sample_Size), color = Significance), 
                     alpha = 0.7, shape = 16) +
          geom_hline(yintercept = -log10(0.05), linetype = "dashed", 
                     color = "#E74C3C", linewidth = 1, alpha = 0.8) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "#95A5A6", linewidth = 0.8) +
          labs(
            title = paste("Effect Size Distribution -", input$department),
            subtitle = "Association Strength and Statistical Significance",
            x = "Effect Size (trimmed at ±3)",
            y = "-log₁₀(P-value)"
          ) +
          scale_color_manual(
            values = c(
              "p < 0.001" = "#27AE60",
              "p < 0.01" = "#3498DB", 
              "p < 0.05" = "#F39C12",
              "Not Significant" = "#BDC3C7"
            ),
            name = "Significance Level"
          ) +
          scale_size_continuous(
            range = c(2, 8),
            name = "Sample Size\n(sqrt scale)",
            breaks = sqrt(c(100, 1000, 5000, 20000)),
            labels = c("100", "1k", "5k", "20k")
          ) +
          scale_x_continuous(limits = c(-x_range, x_range)) +
          coord_cartesian(ylim = c(0, 8)) +
          pretty_theme() +
          theme(legend.position = "right")
      } else {
        ggplot() +
          annotate("text", x = 1, y = 1, 
                   label = "📊 No valid data for effect size analysis", 
                   size = 8, color = "#BDC3C7") +
          theme_void()
      }
    })
    
    # Return reactive values if needed by other modules
    return(
      list(
        filtered_data = filtered_data,
        current_department = reactive({ input$department })
      )
    )
  })
}