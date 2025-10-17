evidenceMapUI <- function(id) {
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
            title = "Evidence Map Analysis",
            subtitle = "Interactive bubble plot visualization of clinical evidence",
            icon = "🗺️"
          )
      )
    ),
    
    # 原有内容
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
          title = "Evidence Overview Bubble Plot",
          plotlyOutput(ns("bubble_plot"), height = "600px")
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = "Selected Study Details",
          uiOutput(ns("study_details"))
      ),
      box(width = 6, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = "Effect Size Visualization",
          plotOutput(ns("mini_forest"))
      )
    )
  )
}

evidenceMapServer <- function(id, meta_db_example) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data - update based on filters
    filtered_data <- reactive({
      req(meta_db_example)
      data <- meta_db_example
      
      # 使用全局的 input 对象，因为过滤器在 sidebar
      if (!is.null(input$dept_filter)) {
        data <- data[data$Clinical_Department %in% input$dept_filter, ]
      }
      if (!is.null(input$outcome_filter)) {
        data <- data[data$Outcome_Type %in% input$outcome_filter, ]
      }
      
      data
    })
    
    selected_study <- reactiveVal(NULL)
    
    # Bubble plot
    output$bubble_plot <- renderPlotly({
      df <- filtered_data()
      if (nrow(df) == 0) {
        return(plotly_empty())
      }
      
      n_diseases <- length(unique(df$Disease))
      y_range <- c(0.5, n_diseases + 0.5)
      
      plot_ly(
        data = df,
        x = ~Intervention,
        y = ~Disease,
        type = "scatter",
        mode = "markers",
        size = ~Total_Sample_Size,
        color = ~Outcome_Type,
        key = ~seq_len(nrow(df)),
        text = ~paste(
          "</br><b>Study:</b>", Literature_Abbreviation,
          "</br><b>Intervention:</b>", Intervention,
          "</br><b>Disease:</b>", Disease,
          "</br><b>Effect Size:</b>", round(Effect_Size, 2),
          "</br><b>Sample Size:</b>", Total_Sample_Size
        ),
        hoverinfo = "text",
        marker = list(sizemode = "diameter", opacity = 0.7),
        source = ns("bubble")
      ) %>%
        layout(
          xaxis = list(title = "Intervention", tickangle = -45),
          yaxis = list(
            title = "Disease/Condition",
            type = "category",
            range = y_range
          ),
          margin = list(l = 200, r = 50, b = 100, t = 50),
          dragmode = "pan"
        ) %>%
        config(scrollZoom = TRUE, displayModeBar = TRUE)
    })
    
    # Capture click events
    observeEvent(event_data("plotly_click", source = ns("bubble")), {
      click <- event_data("plotly_click", source = ns("bubble"))
      if (is.null(click)) {
        selected_study(NULL)
        return()
      }
      
      idx <- NULL
      if (!is.null(click$key)) {
        idx <- as.numeric(click$key)
      } else if (!is.null(click$pointNumber)) {
        idx <- click$pointNumber + 1
      } else {
        selected_study(NULL)
        return()
      }
      
      df <- filtered_data()
      if (!is.null(df) && idx >= 1 && idx <= nrow(df)) {
        selected_study(df[idx, , drop = FALSE])
      } else {
        selected_study(NULL)
      }
    })
    
    # Study details
    output$study_details <- renderUI({
      s <- selected_study()
      if (is.null(s) || nrow(s) == 0) {
        return(tags$p("👆 Click a bubble in the plot to view study details"))
      }
      
      tagList(
        tags$h3(s$Literature_Abbreviation[1]),
        tags$p(tags$b("PMID: "), tags$a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", s$PMID[1]),
                                        target = "_blank", s$PMID[1])),
        tags$p(tags$b("Disease: "), s$Disease[1]),
        tags$p(tags$b("Intervention: "), s$Intervention[1]),
        tags$p(tags$b("Comparator: "), s$Comparator[1]),
        tags$p(tags$b("Outcome: "), s$Outcomes[1]),
        tags$p(tags$b("Department: "), s$Clinical_Department[1]),
        tags$p(tags$b("Effect Size: "), round(s$Effect_Size[1], 2),
               " (95% CI: ", round(s$CI_Lower[1], 2), ", ", round(s$CI_Upper[1], 2),
               "; p = ", s$P_Value[1], ")"),
        tags$p(tags$b("Number of Studies: "), s$Num_Studies[1]),
        tags$p(tags$b("Total Sample Size: "), s$Total_Sample_Size[1])
      )
    })
    
    # Mini forest plot
    output$mini_forest <- renderPlot({
      s <- selected_study()
      if (is.null(s) || nrow(s) == 0) {
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
        text(1, 1, "👆 Click a bubble in the plot to view study details", col = "gray40", cex = 1.2)
        return(NULL)
      }
      
      tryCatch({
        forest(x = s$Effect_Size, 
               ci.lb = s$CI_Lower, 
               ci.ub = s$CI_Upper, 
               slab = s$Literature_Abbreviation,
               main = paste("Summary Effect:", s$Intervention, "for", s$Disease),
               xlab = "Effect Size")
      }, error = function(e) {
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
        text(1, 1, "Unable to generate forest plot for this study", col = "red")
      })
    })
    
    # Return reactive values for other modules if needed
    return(list(
      selected_study = selected_study,
      filtered_data = filtered_data
    ))
  })
}

# Database 模块
databaseUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Meta-analysis Database",
          DTOutput(ns("meta_table"))
      )
    )
  )
}

databaseServer <- function(id, meta_db_example) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$meta_table <- renderDT({
      req(meta_db_example)
      datatable(meta_db_example[, c("Literature_Abbreviation", "Disease", "Intervention", 
                                    "Comparator", "Outcomes", "Clinical_Department", "PMID")],
                options = list(pageLength = 10, scrollX = TRUE),
                selection = 'single',
                rownames = FALSE)
    })
  })
}

