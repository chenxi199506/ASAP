

ManualMetaAnalysisUI <- function(id) {
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
            title = "Manual-based Secondary Meta-Analysis",
            subtitle = "Upload and analyze your meta-analysis data with comprehensive visualization and diagnostic tools",
            icon = "📤"
          )
      )
    ),
    
    fluidRow(
      column(3,
             wellPanel(
               h4("Data Upload", style = "color: #2E86AB;"),
               
               # File format selection
               selectInput(ns("file_type"), "File Format:",
                           choices = c("Excel" = "excel",
                                       "CSV" = "csv", 
                                       "Text (Tab-delimited)" = "txt",
                                       "Text (Comma-delimited)" = "csv2"),
                           selected = "excel"),
               
               # File upload
               fileInput(ns("file"), "Upload Data File",
                         accept = c(".csv", ".xlsx", ".xls", ".txt"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected"),
               
               # Auto detect button
               conditionalPanel(
                 condition = "output.file_uploaded", ns = ns,
                 actionButton(ns("auto_detect"), "Auto Detect Columns", 
                              class = "btn-info", icon = icon("search"))
               ),
               
               # Column mapping settings
               conditionalPanel(
                 condition = "output.file_uploaded", ns = ns,
                 wellPanel(
                   h5("Column Mapping", style = "color: #2E86AB;"),
                   selectInput(ns("effect_col"), "Effect Size Column:",
                               choices = NULL),
                   selectInput(ns("ci_lower_col"), "CI Lower Bound Column:",
                               choices = NULL),
                   selectInput(ns("ci_upper_col"), "CI Upper Bound Column:",
                               choices = NULL),
                   selectInput(ns("author_col"), "Author Column (Optional):",
                               choices = c("None" = "")),
                   selectInput(ns("year_col"), "Year Column (Optional):",
                               choices = c("None" = "")),
                   selectInput(ns("study_name_col"), "Study Name Column (Optional):",
                               choices = c("None" = "")),
                   selectInput(ns("sample_size_col"), "Sample Size Column (Optional):",
                               choices = c("None" = "")),
                   actionButton(ns("apply_mapping"), "Apply Column Mapping", 
                                class = "btn-primary", icon = icon("check"))
                 )
               ),
               
               actionButton(ns("load_example"), "Load Example Data", 
                            icon = icon("database"), class = "btn-info"),
               
               wellPanel(
                 h5("Data Requirements", style = "color: #2E86AB;"),
                 tags$ul(
                   tags$li("Required: effect size, confidence interval bounds"),
                   tags$li("Optional: study names, authors, year, sample size"),
                   tags$li("Column names will be automatically detected when possible")
                 )
               ),
               
               wellPanel(
                 h5("Meta-Analysis Settings", style = "color: #2E86AB;"),
                 selectInput(ns("method"), "Meta-Analysis Method:",
                             choices = c("Random Effects" = "REML",
                                         "Fixed Effect" = "FE",
                                         "Maximum Likelihood" = "ML",
                                         "DerSimonian-Laird" = "DL"),
                             selected = "REML"),
                 
                 checkboxInput(ns("test_heterogeneity"), 
                               "Test Heterogeneity", value = TRUE),
                 checkboxInput(ns("test_bias"), 
                               "Test Publication Bias", value = TRUE),
                 
                 actionButton(ns("run_analysis"), "Run Meta-Analysis", 
                              class = "btn-primary", icon = icon("play"))
               ),
               
               wellPanel(
                 h5("Plot Customization", style = "color: #2E86AB;"),
                 sliderInput(ns("text_size"), "Text Size:",
                             min = 10, max = 18, value = 12),
                 sliderInput(ns("point_size"), "Point Size:",
                             min = 2, max = 8, value = 3, step = 0.5),
                 selectInput(ns("color_scheme"), "Color Scheme:",
                             choices = c("Blue Theme" = "blue",
                                         "Green Theme" = "green",
                                         "Red Theme" = "red",
                                         "Custom" = "custom"),
                             selected = "blue")
               )
             )
      ),
      
      column(9,
             tabsetPanel(
               tabPanel("Data Overview",
                        h4("Uploaded Data Overview"),
                        uiOutput(ns("file_info")),
                        uiOutput(ns("data_alert")),
                        uiOutput(ns("mapping_info")),
                        DTOutput(ns("data_table")),
                        verbatimTextOutput(ns("data_summary")),
                        
                        # Example data format display
                        conditionalPanel(
                          condition = "input.load_example > 0 || output.data_uploaded == false",
                          ns = ns,
                          wellPanel(
                            h4("Example Data Format"),
                            tags$p("Your data file should contain these columns (names can vary):"),
                            DTOutput(ns("example_format"))
                          )
                        )),
               
               tabPanel("Mapped Data",
                        h4("Mapped and Standardized Data"),
                        uiOutput(ns("mapping_status")),
                        DTOutput(ns("mapped_data_table")),
                        verbatimTextOutput(ns("validation_output"))),
               
               tabPanel("Forest Plot",
                        h4("Forest Plot"),
                        uiOutput(ns("forest_alert")),
                        fluidRow(
                          column(12, align = "center",
                                 plotOutput(ns("forest_plot"), height = "700px")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h5("Forest Plot Options"),
                                   checkboxInput(ns("show_weights"), "Show Weights", value = TRUE),
                                   checkboxInput(ns("show_diamond"), "Show Overall Effect", value = TRUE),
                                   checkboxInput(ns("show_stats"), "Show Study Statistics", value = TRUE),
                                   checkboxInput(ns("show_ranking"), "Show Study Ranking", value = TRUE)
                                 )
                          ),
                          column(6, align = "center",
                                 downloadButton(ns("download_forest"), "Download Forest Plot", 
                                                class = "btn-success")
                          )
                        )),
               
               tabPanel("Funnel Plot",
                        h4("Funnel Plot"),
                        uiOutput(ns("funnel_alert")),
                        fluidRow(
                          column(12, align = "center",
                                 plotOutput(ns("funnel_plot"), height = "600px")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h5("Funnel Plot Options"),
                                   checkboxInput(ns("show_contours"), "Show Contour Lines", value = TRUE),
                                   checkboxInput(ns("show_egger"), "Show Egger's Test", value = TRUE),
                                   checkboxInput(ns("show_trimfill"), "Show Trim & Fill", value = FALSE)
                                 )
                          ),
                          column(6, align = "center",
                                 downloadButton(ns("download_funnel"), "Download Funnel Plot", 
                                                class = "btn-success")
                          )
                        )),
               
               tabPanel("Meta-Analysis Results",
                        h4("Meta-Analysis Results"),
                        verbatimTextOutput(ns("meta_summary")),
                        br(),
                        h4("Heterogeneity Analysis"),
                        verbatimTextOutput(ns("heterogeneity_summary")),
                        br(),
                        h4("Publication Bias Tests"),
                        verbatimTextOutput(ns("bias_tests"))),
               
               tabPanel("Additional Plots",
                        h4("Additional Diagnostic Plots"),
                        fluidRow(
                          column(6,
                                 h5("Cumulative Meta-Analysis"),
                                 plotOutput(ns("cumulative_plot"), height = "400px")
                          ),
                          column(6,
                                 h5("Influence Analysis"),
                                 plotOutput(ns("influence_plot"), height = "400px")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 h5("Baujat Plot"),
                                 plotOutput(ns("baujat_plot"), height = "400px")
                          ),
                          column(6,
                                 h5("Radial Plot"),
                                 plotOutput(ns("radial_plot"), height = "400px")
                          )
                        )),
               
               tabPanel("Download Results",
                        h4("Download Analysis Results"),
                        wellPanel(
                          h5("Download Data"),
                          downloadButton(ns("download_meta_data"), "Download Meta-Analysis Data", 
                                         class = "btn-success"),
                          br(), br(),
                          downloadButton(ns("download_full_results"), "Download Full Results", 
                                         class = "btn-info")
                        ),
                        wellPanel(
                          h5("Download Plots"),
                          fluidRow(
                            column(6,
                                   downloadButton(ns("download_all_plots"), "Download All Plots", 
                                                  class = "btn-warning")
                            ),
                            column(6,
                                   downloadButton(ns("download_report"), "Download Analysis Report", 
                                                  class = "btn-primary")
                            )
                          )
                        ))
             )
      )
    )
  )
}


ManualMetaAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing data and results
    manual_values <- reactiveValues(
      uploaded_data = NULL,
      mapped_data = NULL,
      meta_results = NULL,
      analysis_complete = FALSE,
      plot_data = NULL,
      using_example_data = FALSE,
      column_mapping = list(),
      file_info = NULL
    )
    
    # Auto detect column patterns
    auto_detect_columns <- function(data) {
      col_names <- names(data)
      mapping <- list()
      
      # Effect size column patterns
      effect_patterns <- c("effect", "es", "d\\(", "g\\(", "or", "rr", "hr", "coef", "estimate")
      mapping$effect_size <- find_matching_column(col_names, effect_patterns)
      
      # CI lower bound patterns
      ci_lower_patterns <- c("lower", "ci_lower", "lci", "ci.l", "lower_ci", "lower\\.ci")
      mapping$ci_lower <- find_matching_column(col_names, ci_lower_patterns)
      
      # CI upper bound patterns
      ci_upper_patterns <- c("upper", "ci_upper", "uci", "ci.u", "upper_ci", "upper\\.ci")
      mapping$ci_upper <- find_matching_column(col_names, ci_upper_patterns)
      
      # Author column patterns
      author_patterns <- c("author", "authors", "name", "names", "study", "id")
      mapping$authors <- find_matching_column(col_names, author_patterns)
      
      # Year column patterns
      year_patterns <- c("year", "yr", "date", "publication")
      mapping$year <- find_matching_column(col_names, year_patterns)
      
      # Study name column patterns
      study_name_patterns <- c("study", "study_name", "study_id", "id", "title")
      mapping$study_name <- find_matching_column(col_names, study_name_patterns)
      
      # Sample size column patterns
      sample_size_patterns <- c("n", "sample", "size", "sample_size", "n_total")
      mapping$sample_size <- find_matching_column(col_names, sample_size_patterns)
      
      return(mapping)
    }
    
    # Helper function: find matching column
    find_matching_column <- function(col_names, patterns) {
      for (pattern in patterns) {
        matches <- grep(pattern, col_names, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
          return(matches[1])
        }
      }
      return(NULL)
    }
    
    # Update column selectors
    update_column_selectors <- function(data) {
      col_names <- c("Please select" = "", names(data))
      
      # Get auto-detected mapping
      auto_mapping <- auto_detect_columns(data)
      
      updateSelectInput(session, "effect_col", 
                        choices = col_names,
                        selected = auto_mapping$effect_size %||% "")
      updateSelectInput(session, "ci_lower_col", 
                        choices = col_names,
                        selected = auto_mapping$ci_lower %||% "")
      updateSelectInput(session, "ci_upper_col", 
                        choices = col_names,
                        selected = auto_mapping$ci_upper %||% "")
      updateSelectInput(session, "author_col", 
                        choices = c("None" = "", col_names),
                        selected = auto_mapping$authors %||% "")
      updateSelectInput(session, "year_col", 
                        choices = c("None" = "", col_names),
                        selected = auto_mapping$year %||% "")
      updateSelectInput(session, "study_name_col", 
                        choices = c("None" = "", col_names),
                        selected = auto_mapping$study_name %||% "")
      updateSelectInput(session, "sample_size_col", 
                        choices = c("None" = "", col_names),
                        selected = auto_mapping$sample_size %||% "")
    }
    
    # Apply auto-detected mapping
    apply_auto_mapping <- function(auto_mapping) {
      if (!is.null(auto_mapping$effect_size)) {
        updateSelectInput(session, "effect_col", selected = auto_mapping$effect_size)
      }
      if (!is.null(auto_mapping$ci_lower)) {
        updateSelectInput(session, "ci_lower_col", selected = auto_mapping$ci_lower)
      }
      if (!is.null(auto_mapping$ci_upper)) {
        updateSelectInput(session, "ci_upper_col", selected = auto_mapping$ci_upper)
      }
      if (!is.null(auto_mapping$authors)) {
        updateSelectInput(session, "author_col", selected = auto_mapping$authors)
      }
      if (!is.null(auto_mapping$year)) {
        updateSelectInput(session, "year_col", selected = auto_mapping$year)
      }
      if (!is.null(auto_mapping$study_name)) {
        updateSelectInput(session, "study_name_col", selected = auto_mapping$study_name)
      }
      if (!is.null(auto_mapping$sample_size)) {
        updateSelectInput(session, "sample_size_col", selected = auto_mapping$sample_size)
      }
    }
    
    # Auto detect button
    observeEvent(input$auto_detect, {
      req(manual_values$uploaded_data)
      auto_mapping <- auto_detect_columns(manual_values$uploaded_data)
      apply_auto_mapping(auto_mapping)
      showNotification("Auto detection completed! Please check column mapping settings.", 
                       type = "message")
    })
    
    # Handle file upload
    observeEvent(input$file, {
      req(input$file)
      
      tryCatch({
        data <- NULL
        file_info <- list(
          name = input$file$name,
          size = input$file$size,
          type = input$file$type
        )
        
        # Read data based on file type
        if (input$file_type == "excel") {
          data <- readxl::read_excel(input$file$datapath)
          file_info$sheets <- excel_sheets(input$file$datapath)
        } else if (input$file_type == "csv") {
          data <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
        } else if (input$file_type == "txt") {
          data <- readr::read_tsv(input$file$datapath, show_col_types = FALSE)
        } else if (input$file_type == "csv2") {
          data <- readr::read_csv2(input$file$datapath, show_col_types = FALSE)
        }
        
        if (!is.null(data)) {
          manual_values$uploaded_data <- as.data.frame(data)
          manual_values$mapped_data <- NULL
          manual_values$using_example_data <- FALSE
          manual_values$analysis_complete <- FALSE
          manual_values$file_info <- file_info
          
          # Update column selectors
          update_column_selectors(manual_values$uploaded_data)
          
          # Auto apply detection
          auto_mapping <- auto_detect_columns(manual_values$uploaded_data)
          apply_auto_mapping(auto_mapping)
          
          showNotification(paste("File uploaded successfully! Detected", 
                                 ncol(manual_values$uploaded_data), "columns,",
                                 nrow(manual_values$uploaded_data), "rows."), 
                           type = "message")
        }
        
      }, error = function(e) {
        showNotification(paste("File reading error:", e$message), type = "error")
      })
    })
    
    # Apply column mapping
    observeEvent(input$apply_mapping, {
      req(manual_values$uploaded_data)
      
      data <- manual_values$uploaded_data
      
      # Validate required columns
      if (input$effect_col == "" || input$ci_lower_col == "" || input$ci_upper_col == "") {
        showNotification("Error: Please map all required columns (Effect Size, CI Lower, CI Upper)", 
                         type = "error")
        return()
      }
      
      # Check if columns exist
      required_cols <- c(input$effect_col, input$ci_lower_col, input$ci_upper_col)
      if (!all(required_cols %in% names(data))) {
        missing_cols <- required_cols[!required_cols %in% names(data)]
        showNotification(paste("Error: The following columns don't exist in data:", 
                               paste(missing_cols, collapse = ", ")), 
                         type = "error")
        return()
      }
      
      # Create new mapped data frame with renamed columns
      tryCatch({
        # Extract column data safely
        effect_vals <- data[[input$effect_col]]
        ci_lower_vals <- data[[input$ci_lower_col]]
        ci_upper_vals <- data[[input$ci_upper_col]]
        
        # Convert to numeric
        effect_vals <- as.numeric(effect_vals)
        ci_lower_vals <- as.numeric(ci_lower_vals)
        ci_upper_vals <- as.numeric(ci_upper_vals)
        
        # Check for conversion issues
        na_effect <- sum(is.na(effect_vals))
        na_ci_lower <- sum(is.na(ci_lower_vals))
        na_ci_upper <- sum(is.na(ci_upper_vals))
        
        if (na_effect > 0 || na_ci_lower > 0 || na_ci_upper > 0) {
          showNotification(paste("Warning: Data conversion produced", 
                                 na_effect + na_ci_lower + na_ci_upper, 
                                 "NA values. Please check data format."), 
                           type = "warning")
        }
        
        # Create mapped data frame with standardized column names
        mapped_data <- data.frame(
          effect_size = effect_vals,
          ci_lower = ci_lower_vals,
          ci_upper = ci_upper_vals,
          stringsAsFactors = FALSE
        )
        
        # Add optional columns
        if (input$author_col != "") {
          mapped_data$authors <- as.character(data[[input$author_col]])
        }
        
        if (input$year_col != "") {
          mapped_data$year <- as.numeric(data[[input$year_col]])
        }
        
        if (input$study_name_col != "") {
          mapped_data$study_name <- as.character(data[[input$study_name_col]])
        }
        
        if (input$sample_size_col != "") {
          mapped_data$sample_size <- as.numeric(data[[input$sample_size_col]])
        }
        
        # Create study label
        if ("authors" %in% names(mapped_data) && "year" %in% names(mapped_data)) {
          mapped_data$study_label <- paste(mapped_data$authors, "(", mapped_data$year, ")")
        } else if ("authors" %in% names(mapped_data)) {
          mapped_data$study_label <- mapped_data$authors
        } else if ("study_name" %in% names(mapped_data)) {
          mapped_data$study_label <- mapped_data$study_name
        } else {
          mapped_data$study_label <- paste("Study", 1:nrow(mapped_data))
        }
        
        # Calculate standard error
        mapped_data$se <- (mapped_data$ci_upper - mapped_data$ci_lower) / (2 * 1.96)
        
        # Save mapping information
        manual_values$column_mapping <- list(
          effect_size = input$effect_col,
          ci_lower = input$ci_lower_col,
          ci_upper = input$ci_upper_col,
          authors = input$author_col,
          year = input$year_col,
          study_name = input$study_name_col,
          sample_size = input$sample_size_col
        )
        
        manual_values$mapped_data <- mapped_data
        manual_values$analysis_complete <- FALSE
        
        showNotification("Column mapping applied successfully! Data standardized.", 
                         type = "message")
        
      }, error = function(e) {
        showNotification(paste("Mapping error:", e$message), type = "error")
      })
    })
    
    # Create example data function
    create_example_data <- function() {
      set.seed(123)
      
      example_data <- data.frame(
        StudyID = c("Smith2020", "Johnson2019", "Brown2021", "Wilson2018", "Davis2022"),
        Author = c("Smith", "Johnson", "Brown", "Wilson", "Davis"),
        PubYear = c(2020, 2019, 2021, 2018, 2022),
        N = c(100, 150, 80, 200, 120),
        EffectSize = round(c(0.75, 0.45, 0.60, 0.35, 0.80), 3),
        LowerCI = round(c(0.45, 0.25, 0.35, 0.15, 0.55), 3),
        UpperCI = round(c(1.05, 0.65, 0.85, 0.55, 1.05), 3),
        Intervention = c("CBT", "Medication", "Exercise", "Diet", "Mindfulness")
      )
      
      return(example_data)
    }
    
    # Display example data format
    output$example_format <- renderDT({
      example <- create_example_data()
      
      format_info <- data.frame(
        Column_Name = names(example),
        Required = c("No", "No", "No", "No", "Yes", "Yes", "Yes", "No"),
        Description = c(
          "Study identifier",
          "Author names", 
          "Publication year",
          "Sample size",
          "Effect size value",
          "Lower bound of 95% CI",
          "Upper bound of 95% CI",
          "Intervention type"
        ),
        Example_Value = as.character(example[1, ])
      )
      
      DT::datatable(format_info,
                    options = list(
                      pageLength = 10,
                      dom = 't',
                      scrollX = TRUE
                    ),
                    rownames = FALSE) %>%
        DT::formatStyle(
          'Required',
          backgroundColor = DT::styleEqual(
            c("Yes", "No"),
            c('#ffcccc', '#ccffcc')
          )
        )
    })
    
    # Load example data
    observeEvent(input$load_example, {
      example_data <- create_example_data()
      manual_values$uploaded_data <- example_data
      manual_values$using_example_data <- TRUE
      manual_values$analysis_complete = FALSE
      
      # Update selectors
      update_column_selectors(example_data)
      
      # Auto create mapped data
      mapped_data <- data.frame(
        effect_size = example_data$EffectSize,
        ci_lower = example_data$LowerCI,
        ci_upper = example_data$UpperCI,
        authors = example_data$Author,
        year = example_data$PubYear,
        study_name = example_data$StudyID,
        sample_size = example_data$N
      )
      
      # Create study label
      mapped_data$study_label <- paste(mapped_data$authors, "(", mapped_data$year, ")")
      
      # Calculate standard error
      mapped_data$se <- (mapped_data$ci_upper - mapped_data$ci_lower) / (2 * 1.96)
      
      manual_values$mapped_data <- mapped_data
      
      # Save mapping information
      manual_values$column_mapping <- list(
        effect_size = "EffectSize",
        ci_lower = "LowerCI",
        ci_upper = "UpperCI",
        authors = "Author",
        year = "PubYear",
        study_name = "StudyID",
        sample_size = "N"
      )
      
      showNotification("Example data loaded with automatic column mapping!", 
                       type = "message", duration = 5)
    })
    
    # Data preparation
    prepared_data <- reactive({
      req(manual_values$mapped_data)
      
      data <- manual_values$mapped_data
      
      # Validate required columns
      required_cols <- c("effect_size", "ci_lower", "ci_upper")
      missing_cols <- required_cols[!required_cols %in% names(data)]
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Missing required columns after mapping:", 
                               paste(missing_cols, collapse = ", ")), 
                         type = "error")
        return(NULL)
      }
      
      # Remove missing values
      complete_cases <- complete.cases(data[, c("effect_size", "ci_lower", "ci_upper")])
      data <- data[complete_cases, ]
      
      if (nrow(data) == 0) {
        showNotification("No studies with complete data after filtering!", type = "error")
        return(NULL)
      }
      
      return(data)
    })
    
    # Output file upload status
    output$file_uploaded <- reactive({
      !is.null(manual_values$uploaded_data)
    })
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    # Output data mapped status
    output$data_mapped <- reactive({
      !is.null(manual_values$mapped_data)
    })
    outputOptions(output, "data_mapped", suspendWhenHidden = FALSE)
    
    # Show file information
    output$file_info <- renderUI({
      if (is.null(manual_values$file_info)) return(NULL)
      
      info <- manual_values$file_info
      tags$div(
        class = "alert alert-info",
        tags$p(strong("File Information:")),
        tags$p(paste("File name:", info$name)),
        tags$p(paste("File size:", round(info$size/1024, 2), "KB")),
        tags$p(paste("File type:", info$type)),
        if (!is.null(info$sheets)) tags$p(paste("Excel sheets:", paste(info$sheets, collapse = ", ")))
      )
    })
    
    # Show mapping information
    output$mapping_info <- renderUI({
      if (is.null(manual_values$column_mapping)) return(NULL)
      
      mapping <- manual_values$column_mapping
      tags$div(
        class = "alert alert-info",
        h5("Current Column Mapping:"),
        tags$ul(
          tags$li(paste("Effect Size:", mapping$effect_size, "→ effect_size")),
          tags$li(paste("CI Lower:", mapping$ci_lower, "→ ci_lower")),
          tags$li(paste("CI Upper:", mapping$ci_upper, "→ ci_upper")),
          if (!is.null(mapping$authors) && mapping$authors != "") 
            tags$li(paste("Authors:", mapping$authors, "→ authors")),
          if (!is.null(mapping$year) && mapping$year != "") 
            tags$li(paste("Year:", mapping$year, "→ year")),
          if (!is.null(mapping$study_name) && mapping$study_name != "") 
            tags$li(paste("Study Name:", mapping$study_name, "→ study_name")),
          if (!is.null(mapping$sample_size) && mapping$sample_size != "") 
            tags$li(paste("Sample Size:", mapping$sample_size, "→ sample_size"))
        )
      )
    })
    
    # Show mapping status
    output$mapping_status <- renderUI({
      if (is.null(manual_values$mapped_data)) {
        tags$div(
          class = "alert alert-warning",
          tags$h4("Column Mapping Not Applied"),
          tags$p("Please set up column mapping in the left panel and click 'Apply Column Mapping'."),
          tags$p("You can also click 'Auto Detect Columns' to try automatic column detection.")
        )
      } else {
        tags$div(
          class = "alert alert-success",
          tags$h4("Column Mapping Applied"),
          tags$p(paste("Data rows:", nrow(manual_values$mapped_data))),
          tags$p(paste("Standardized columns:", paste(names(manual_values$mapped_data), collapse = ", ")))
        )
      }
    })
    
    # Display uploaded data - Simple and safe version
    output$data_table <- renderDT({
      req(manual_values$uploaded_data)
      
      # Simply display the uploaded data without adding extra columns
      display_data <- manual_values$uploaded_data
      
      DT::datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Blfrtip'
        ),
        rownames = FALSE,
        caption = if (manual_values$using_example_data) {
          "Example Dataset"
        } else {
          "Uploaded Dataset"
        }
      )
    })
    
    # Display mapped data
    output$mapped_data_table <- renderDT({
      req(manual_values$mapped_data)
      
      DT::datatable(
        manual_values$mapped_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Blfrtip'
        ),
        rownames = FALSE,
        caption = "Standardized Data After Column Mapping"
      )
    })
    
    # Data validation output
    output$validation_output <- renderPrint({
      if (is.null(manual_values$mapped_data)) {
        cat("Please apply column mapping first to view data validation report.\n")
        return()
      }
      
      data <- manual_values$mapped_data
      
      cat("=== DATA VALIDATION REPORT ===\n\n")
      cat("Data structure:\n")
      print(str(data))
      
      cat("\nMissing values statistics:\n")
      na_count <- colSums(is.na(data))
      for (col in names(na_count)) {
        if (na_count[col] > 0) {
          cat(sprintf("%-15s: %d missing values (%.1f%%)\n", 
                      col, na_count[col], na_count[col]/nrow(data)*100))
        }
      }
    })
    
    # Data overview alert
    output$data_alert <- renderUI({
      data <- manual_values$uploaded_data
      
      if (is.null(data)) {
        return(tags$div(class = "alert alert-info",
                        icon("info-circle"),
                        "Please upload a data file or click 'Load Example Data' to see demo analysis."))
      }
      
      if (is.null(manual_values$mapped_data)) {
        return(tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          "Data uploaded but column mapping not applied. ",
          "Please map your columns and click 'Apply Column Mapping'."
        ))
      }
      
      tags$div(
        class = "alert alert-success",
        if (manual_values$using_example_data) {
          tags$p(icon("database"), strong("Example Data Loaded:"), "Column mapping applied")
        } else {
          tags$p(icon("file-upload"), strong("User Data Loaded:"), "Column mapping applied")
        },
        tags$p(icon("check-circle"),
               strong("Data Status:"),
               paste("Successfully mapped", nrow(manual_values$mapped_data), "studies"))
      )
    })
    
    # Data summary
    output$data_summary <- renderPrint({
      data <- manual_values$uploaded_data
      
      if (is.null(data)) {
        cat("No data uploaded yet.\n")
        cat("Click 'Load Example Data' to see a demonstration or upload your own file.\n")
        return()
      }
      
      if (manual_values$using_example_data) {
        cat("=== EXAMPLE DATA ===\n\n")
      } else {
        cat("=== UPLOADED DATA ===\n\n")
      }
      
      cat("DATA SUMMARY\n")
      cat("============\n")
      cat("Total studies:", nrow(data), "\n")
      cat("Total columns:", ncol(data), "\n")
      cat("Column names:", paste(names(data), collapse = ", "), "\n\n")
      
      if (!is.null(manual_values$mapped_data)) {
        mapped <- manual_values$mapped_data
        complete_cases <- sum(complete.cases(mapped[, c("effect_size", "ci_lower", "ci_upper")]))
        cat("MAPPED DATA STATUS\n")
        cat("==================\n")
        cat("Studies with complete data:", complete_cases, "\n")
        cat("Studies with missing data:", nrow(mapped) - complete_cases, "\n")
      }
    })
    
    # Color schemes
    color_schemes <- reactive({
      switch(input$color_scheme,
             "blue" = list(main = "#2E86AB", light = "#A8DADC", dark = "#1B4F72"),
             "green" = list(main = "#27AE60", light = "#82E0AA", dark = "#196F3D"),
             "red" = list(main = "#E74C3C", light = "#F1948A", dark = "#B03A2E"),
             "custom" = list(main = "#8E44AD", light = "#C39BD3", dark = "#6C3483"))
    })
    
    # Run meta-analysis
    observeEvent(input$run_analysis, {
      req(prepared_data())
      
      data <- prepared_data()
      
      if (nrow(data) < 2) {
        showNotification("Need at least 2 studies with complete data for meta-analysis", 
                         type = "warning")
        return()
      }
      
      tryCatch({
        meta_model <- metafor::rma(
          yi = effect_size,
          sei = se,
          data = data,
          method = input$method,
          slab = study_label
        )
        
        manual_values$meta_results <- list(
          model = meta_model,
          data = data,
          method = input$method,
          mapping = manual_values$column_mapping
        )
        
        manual_values$analysis_complete <- TRUE
        
        showNotification("Meta-analysis completed successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Meta-analysis error:", e$message), type = "error")
      })
    })
    
    # Forest plot alert
    output$forest_alert <- renderUI({
      if (!manual_values$analysis_complete) {
        return(tags$div(class = "alert alert-info",
                        icon("info-circle"),
                        "Click 'Run Meta-Analysis' to generate the forest plot."))
      }
      
      data <- prepared_data()
      
      if (manual_values$using_example_data) {
        source_note <- "Displaying results from example data"
      } else {
        source_note <- "Displaying results from uploaded data"
      }
      
      tags$div(
        class = "alert alert-success",
        tags$p(icon("tree"),
               strong("Forest Plot Generated:"),
               "Showing", nrow(data), "studies with complete data."),
        tags$p(source_note)
      )
    })
    
###### 绘图代码#############
    # 1. 森林图函数
    create_forest_plot <- function(data, meta_model) {
      # 准备数据 - 二次Meta分析的数据是一次Meta分析的SMD结果
      plot_data <- data %>%
        mutate(
          name = study_label,
          effect_ci = sprintf("%.2f [%.2f, %.2f]", effect_size, ci_lower, ci_upper)
        )
      
      # 添加二次Meta分析的汇总结果 - 使用加权平均SMD
      overall_effect <- data.frame(
        name = "Pooled SMD",
        effect_size = meta_model$b[1],
        ci_lower = meta_model$ci.lb,
        ci_upper = meta_model$ci.ub,
        effect_ci = sprintf("%.2f [%.2f, %.2f]", 
                            meta_model$b[1], 
                            meta_model$ci.lb, 
                            meta_model$ci.ub)
      )
      
      # 合并数据
      final_data <- bind_rows(plot_data, overall_effect)
      
      # 配置信息 - 明确显示这是基于SMD的二次Meta分析
      column_info <- tibble(
        id = c("name", "forest_plot", "effect_ci"),
        name = c("Primary Meta-Analysis", "Forest Plot", "SMD [95% CI]"),
        group = c("treatment", "forest", "effect"),
        width = c(7, 12, 5)
      )
      
      row_info <- tibble(
        id = final_data$name,
        group = "Meta-Analysis"
      )
      
      # 设置尺寸参数
      row_height <- 1.1
      row_space <- 0.1
      col_space <- 0.2
      
      # 计算行位置
      row_pos <- row_info %>%
        mutate(
          row_i = row_number(),
          colour_background = row_i %% 2 == 1,
          y = -row_i * (row_height + row_space),
          ymin = y - row_height / 2,
          ymax = y + row_height / 2
        )
      
      # 计算列位置
      column_pos <- column_info %>%
        mutate(
          x = cumsum(c(0, head(width, -1)) + width/2 + c(0, rep(col_space, n()-1))),
          xmin = x - width/2,
          xmax = x + width/2
        )
      
      # 森林图数据准备
      forest_data <- final_data %>%
        mutate(
          model_name = name,
          # 计算在森林图列中的位置
          forest_xmin = column_pos$xmin[column_pos$id == "forest_plot"],
          forest_xmax = column_pos$xmax[column_pos$id == "forest_plot"],
          # 计算效应值在森林图列中的x坐标
          x_effect = forest_xmin + 
            (effect_size - min(final_data$ci_lower)) / 
            (max(final_data$ci_upper) - min(final_data$ci_lower)) * 
            (forest_xmax - forest_xmin),
          # 计算置信区间在森林图列中的位置
          x_left = forest_xmin + 
            (ci_lower - min(final_data$ci_lower)) / 
            (max(final_data$ci_upper) - min(final_data$ci_lower)) * 
            (forest_xmax - forest_xmin),
          x_right = forest_xmin + 
            (ci_upper - min(final_data$ci_lower)) / 
            (max(final_data$ci_upper) - min(final_data$ci_lower)) * 
            (forest_xmax - forest_xmin),
          y = row_pos$y[match(name, row_pos$id)]
        )
      
      # 创建效应值颜色
      effect_colors <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(nrow(final_data))
      forest_data$color <- effect_colors[rank(forest_data$effect_size, ties.method = "average")]
      
      # 创建文本数据
      text_data <- bind_rows(
        # 处理名称
        data.frame(
          model_name = final_data$name,
          metric = "name",
          x = column_pos$xmin[column_pos$id == "name"] + 0.3,
          y = row_pos$y[match(final_data$name, row_pos$id)],
          label = final_data$name,
          hjust = 0,
          vjust = 0.5,
          fontface = ifelse(final_data$name == "Pooled SMD", "bold", "plain"),
          size = ifelse(final_data$name == "Pooled SMD", 4, 3.5),
          color = ifelse(final_data$name == "Pooled SMD", "darkblue", "black"),
          stringsAsFactors = FALSE
        ),
        # 处理效应值和CI合并列
        data.frame(
          model_name = final_data$name,
          metric = "effect_ci",
          x = column_pos$x[column_pos$id == "effect_ci"],
          y = row_pos$y[match(final_data$name, row_pos$id)],
          label = final_data$effect_ci,
          hjust = 0.5,
          vjust = 0.5,
          fontface = ifelse(final_data$name == "Pooled SMD", "bold", "plain"),
          size = ifelse(final_data$name == "Pooled SMD", 3.5, 3.2),
          color = ifelse(final_data$name == "Pooled SMD", "darkblue", "black"),
          stringsAsFactors = FALSE
        )
      ) %>% as_tibble()
      
      # 添加排名文本（不包含汇总结果）
      rank_data <- data.frame(
        x = column_pos$xmin[column_pos$id == "name"] - 0.5,
        y = row_pos$y[1:(nrow(final_data)-1)],  # 排除汇总结果
        label = 1:(nrow(final_data)-1),
        hjust = 0.5,
        fontface = "bold",
        size = 4,
        color = "black",
        stringsAsFactors = FALSE
      ) %>% as_tibble()
      
      # 列标题
      column_text <- column_pos %>%
        mutate(
          y = max(row_pos$ymax) + 1.2,
          label = name,
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          fontface = "bold",
          size = 3.5,
          color = "black"
        )
      
      # SMD的参考线 - 通常为0（无效应）
      reference_value <- 0
      reference_x <- column_pos$xmin[column_pos$id == "forest_plot"] + 
        (reference_value - min(final_data$ci_lower)) / 
        (max(final_data$ci_upper) - min(final_data$ci_lower)) * 
        (column_pos$xmax[column_pos$id == "forest_plot"] - column_pos$xmin[column_pos$id == "forest_plot"])
      
      reference_line <- data.frame(
        x = reference_x,
        xend = reference_x,
        y = min(row_pos$ymin) - 0.2,
        yend = max(row_pos$ymax) + 0.2
      )
      
      # 添加效应方向标签
      effect_direction <- data.frame(
        x = c(reference_x - (column_pos$xmax[column_pos$id == "forest_plot"] - column_pos$xmin[column_pos$id == "forest_plot"]) * 0.1, 
              reference_x + (column_pos$xmax[column_pos$id == "forest_plot"] - column_pos$xmin[column_pos$id == "forest_plot"]) * 0.1),
        y = min(row_pos$ymin) - 0.8,
        label = c("Favors Control", "Favors physical activity"),
        hjust = c(1, 0),
        color = "darkred",
        stringsAsFactors = FALSE
      )
      
      # 创建图表
      g <- ggplot() +
        # 背景色带
        geom_rect(
          data = row_pos %>% filter(colour_background),
          aes(xmin = min(column_pos$xmin) - 0.3, xmax = max(column_pos$xmax) + 0.3,
              ymin = ymin, ymax = ymax),
          fill = "#DDDDDD", alpha = 0.8
        ) +
        
        # 参考线 - 实线
        geom_segment(
          data = reference_line,
          aes(x = x, xend = xend, y = y, yend = yend),
          color = "black", linetype = "solid", size = 1, alpha = 0.7
        ) +
        
        # 置信区间线段 - 粗线
        geom_segment(
          data = forest_data,
          aes(x = x_left, xend = x_right, y = y, yend = y),
          color = "black", size = 1, lineend = "butt"
        ) +
        
        # 置信区间T形末端 - 上端
        geom_segment(
          data = forest_data,
          aes(x = x_left, xend = x_left, 
              y = y - 0.15, yend = y + 0.15),
          color = "black", size = 1, lineend = "butt"
        ) +
        
        # 置信区间T形末端 - 下端
        geom_segment(
          data = forest_data,
          aes(x = x_right, xend = x_right, 
              y = y - 0.15, yend = y + 0.15),
          color = "black", size = 1, lineend = "butt"
        ) +
        
        # 效应值点
        geom_point(
          data = forest_data,
          aes(x = x_effect, y = y, fill = color),
          shape = 23, color = "black", size = 3, stroke = 1.2
        ) +
        
        # 所有文本
        geom_text(
          data = text_data,
          aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust, 
              fontface = fontface, size = size, color = color)
        ) +
        
        # 排名数字（不包含汇总结果）
        geom_text(
          data = rank_data,
          aes(x = x, y = y, label = label, hjust = hjust),
          fontface = "bold", size = 4, color = "black"
        ) +
        
        # 列标题
        geom_text(
          data = column_text,
          aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
          fontface = "bold", size = 3.5, color = "black"
        ) +
        
        # 效应方向标签
        geom_text(
          data = effect_direction,
          aes(x = x, y = y, label = label, hjust = hjust, color = color),
          size = 3, fontface = "bold"
        ) +
        
        # 使用identity scales
        scale_fill_identity() +
        scale_color_identity() +
        scale_size_identity() +
        
        # 主题设置
        theme_void() +
        theme(
          legend.position = "none",
          plot.margin = margin(1, 1, 2, 1, "cm"),
          panel.background = element_rect(fill = "white", color = NA),
          text = element_text(size = 10)
        ) +
        
        # 坐标限制
        coord_equal(
          xlim = c(min(column_pos$xmin) - 1, max(column_pos$xmax) + 0.5),
          ylim = c(min(row_pos$ymin) - 1.2, max(row_pos$ymax) + 2.0)
        )
      
      return(g)
    }
    
    
    # 2. 漏斗图函数
    create_funnel_plot <- function(data, meta_model) {
      if (nrow(data) < 2) {
        return(ggplot() +
                 labs(title = "Funnel Plot",
                      subtitle = "Insufficient data for funnel plot") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        # Prepare funnel plot data
        plot_data <- data.frame(
          study = 1:meta_model$k,
          effect_size = data$effect_size,
          se = sqrt(meta_model$vi),
          weight = weights(meta_model),
          precision = 1/sqrt(meta_model$vi)
        )
        
        # Calculate Egger's test
        egger_test <- tryCatch({
          metafor::regtest(meta_model)
        }, error = function(e) {
          NULL
        })
        
        # Create funnel plot using ggplot2
        p <- ggplot(plot_data, aes(x = effect_size, y = precision)) +
          
          # Add points with consistent styling
          geom_point(aes(size = weight, fill = weight), 
                     shape = 21, color = "white", stroke = 0.8, alpha = 0.9) +
          
          # Add overall effect line
          geom_vline(xintercept = meta_model$b[1], 
                     linetype = "solid", color = "#e74c3c", linewidth = 1) +
          
          # Add zero line
          geom_vline(xintercept = 0, 
                     linetype = "dashed", color = "grey", linewidth = 0.5) +
          
          # Add pseudo-confidence intervals (funnel shape)
          stat_summary_bin(fun = mean, geom = "line", 
                           aes(y = precision, x = effect_size),
                           color = "#3498db", alpha = 0.6, linewidth = 0.8) +
          
          # Size and fill scales (consistent with radial plot)
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Weight",
                              breaks = seq(0.1, 0.9, by = 0.2),
                              labels = scales::percent_format(accuracy = 1)) +
          scale_size_continuous(range = c(3, 8), name = "Weight",
                                breaks = seq(0.1, 0.9, by = 0.2),
                                labels = scales::percent_format(accuracy = 1)) +
          
          # Labels and titles
          labs(title = "Funnel Plot for Publication Bias Assessment",
               subtitle = "Asymmetry may indicate potential publication bias",
               x = "Effect Size",
               y = "Precision (1/SE)") +
          
          # Classic theme for publication (consistent with radial plot)
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13,
                                      margin = margin(b = 8)),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11,
                                         margin = margin(b = 12)),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "right",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9),
            legend.key.size = unit(0.6, "cm"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(15, 15, 15, 15)
          ) +
          
          # Axis scales
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
        
        # Add Egger's test results if available
        if (!is.null(egger_test)) {
          pval_text <- ifelse(egger_test$pval < 0.001, 
                              "Egger's test: p < 0.001", 
                              sprintf("Egger's test: p = %.3f", egger_test$pval))
          
          interpretation <- ifelse(egger_test$pval < 0.05,
                                   "Suggests potential publication bias", 
                                   "No significant publication bias detected")
          
          p <- p + 
            annotate("text", 
                     x = min(plot_data$effect_size), 
                     y = max(plot_data$precision) * 0.95,
                     label = pval_text,
                     hjust = 0, vjust = 1, size = 3.5,
                     color = ifelse(egger_test$pval < 0.05, "#e74c3c", "#2c3e50"),
                     fontface = ifelse(egger_test$pval < 0.05, "bold", "plain")) +
            annotate("text",
                     x = min(plot_data$effect_size),
                     y = max(plot_data$precision) * 0.88,
                     label = interpretation,
                     hjust = 0, vjust = 1, size = 3.2,
                     color = ifelse(egger_test$pval < 0.05, "#e74c3c", "#27ae60"),
                     fontface = ifelse(egger_test$pval < 0.05, "bold", "plain"))
        } else {
          p <- p + 
            annotate("text", 
                     x = min(plot_data$effect_size), 
                     y = max(plot_data$precision) * 0.95,
                     label = "Egger's test: Could not be computed",
                     hjust = 0, vjust = 1, size = 3.5,
                     color = "#7f8c8d")
        }
        
        # Add study count annotation
        p <- p +
          annotate("text",
                   x = max(plot_data$effect_size),
                   y = max(plot_data$precision) * 0.95,
                   label = sprintf("Number of studies: %d", nrow(plot_data)),
                   hjust = 1, vjust = 1, size = 3.5,
                   color = "#2c3e50", fontface = "bold")
        
        return(list(plot = p, egger_test = egger_test))
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Funnel Plot",
               subtitle = "Error in generating funnel plot") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      })
    }
    
    # 更精美的漏斗图版本 - 添加真实的漏斗形状
    create_enhanced_funnel_plot <- function(data, meta_model) {
      if (nrow(data) < 2) {
        return(ggplot() +
                 labs(title = "Funnel Plot",
                      subtitle = "Insufficient data for funnel plot") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        # Prepare funnel plot data
        plot_data <- data.frame(
          study = 1:meta_model$k,
          effect_size = data$effect_size,
          se = sqrt(meta_model$vi),
          weight = weights(meta_model),
          precision = 1/sqrt(meta_model$vi)
        )
        
        # Calculate Egger's test
        egger_test <- tryCatch({
          metafor::regtest(meta_model)
        }, error = function(e) {
          NULL
        })
        
        # Create funnel shape data
        funnel_lines <- data.frame(
          x = seq(min(plot_data$effect_size) - 1, max(plot_data$effect_size) + 1, length.out = 100)
        )
        funnel_lines$y_upper <- 1 / (1.96 * abs(funnel_lines$x - meta_model$b[1]))
        funnel_lines$y_lower <- 1 / (1.96 * abs(funnel_lines$x - meta_model$b[1]))
        
        # Create enhanced funnel plot
        p <- ggplot(plot_data, aes(x = effect_size, y = precision)) +
          
          # Add funnel shape
          geom_line(data = funnel_lines, aes(x = x, y = y_upper), 
                    color = "#3498db", linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
          geom_line(data = funnel_lines, aes(x = x, y = y_lower), 
                    color = "#3498db", linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
          
          # Add points with consistent styling
          geom_point(aes(size = weight, fill = weight), 
                     shape = 21, color = "white", stroke = 0.8, alpha = 0.9) +
          
          # Add overall effect line
          geom_vline(xintercept = meta_model$b[1], 
                     linetype = "solid", color = "#e74c3c", linewidth = 1.2) +
          
          # Add zero line
          geom_vline(xintercept = 0, 
                     linetype = "dashed", color = "grey", linewidth = 0.8, alpha = 0.8) +
          
          # Size and fill scales (consistent with radial plot)
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Weight",
                              breaks = seq(0.1, 0.9, by = 0.2),
                              labels = scales::percent_format(accuracy = 1)) +
          scale_size_continuous(range = c(3, 8), name = "Weight",
                                breaks = seq(0.1, 0.9, by = 0.2),
                                labels = scales::percent_format(accuracy = 1)) +
          
          # Labels and titles
          labs(title = "Enhanced Funnel Plot for Publication Bias Assessment",
               subtitle = "Dashed lines represent expected 95% confidence intervals under no bias",
               x = "Effect Size",
               y = "Precision (1/SE)") +
          
          # Classic theme for publication
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13,
                                      margin = margin(b = 8)),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11,
                                         margin = margin(b = 12)),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "right",
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9),
            legend.key.size = unit(0.6, "cm"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(15, 15, 15, 15)
          ) +
          
          # Axis scales
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
        
        # Add Egger's test results if available
        
        
        return(list(plot = p, egger_test = egger_test))
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Enhanced Funnel Plot",
               subtitle = "Error in generating enhanced funnel plot") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      })
    }
    
    # 3. 累积meta分析图
    create_cumulative_plot <- function(data, meta_model) {
      if (nrow(data) < 2) {
        return(ggplot() +
                 labs(title = "Cumulative Meta-Analysis",
                      subtitle = "Insufficient data for cumulative analysis") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        # Perform cumulative meta-analysis
        cumulative_model <- metafor::cumul(meta_model, order = order(data$year))
        
        # Prepare data for plotting
        plot_data <- data.frame(
          study = 1:meta_model$k,
          estimate = cumulative_model$estimate,
          ci_lower = cumulative_model$ci.lb,
          ci_upper = cumulative_model$ci.ub
        )
        
        ggplot(plot_data, aes(x = study, y = estimate)) +
          geom_hline(yintercept = meta_model$b[1], linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
          geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "lightblue") +
          geom_line(color = "darkblue", linewidth = 1) +
          geom_point(color = "darkblue", size = 2.5) +
          labs(title = "Cumulative Meta-Analysis",
               subtitle = "Shows how effect size changes as studies are added sequentially",
               x = "Number of Studies",
               y = "Cumulative Effect Size") +
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Cumulative Meta-Analysis",
               subtitle = "Error in generating cumulative analysis") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      })
    }
    
    # 4. 影响分析图（带标签优化）
    create_influence_plot <- function(data, meta_model) {
      if (nrow(data) < 3) {
        return(ggplot() +
                 labs(title = "Influence Analysis",
                      subtitle = "Need at least 3 studies for influence analysis") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        k <- meta_model$k
        influence_stats <- data.frame(
          study = 1:k,
          study_label = data$study_label,
          effect_size = data$effect_size,
          weight = weights(meta_model)
        )
        
        # 计算删除每个研究后的效应量变化
        influence_stats$effect_change <- sapply(1:k, function(i) {
          tryCatch({
            model_i <- metafor::rma(
              yi = effect_size[-i],
              sei = se[-i],
              data = data[-i, ],
              method = "REML"
            )
            abs(model_i$b[1] - meta_model$b[1])
          }, error = function(e) {
            NA
          })
        })
        
        # 计算标准化残差作为影响度量
        influence_stats$influence_measure <- abs(rstandard(meta_model)$z)
        
        # 移除NA值
        influence_stats <- influence_stats[complete.cases(influence_stats), ]
        
        # 使用ggrepel优化标签布局
        library(ggrepel)
        
        ggplot(influence_stats, aes(x = study, y = influence_measure)) +
          geom_hline(yintercept = 2, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.5) +
          geom_segment(aes(xend = study, yend = 0), color = "darkblue", alpha = 0.7, linewidth = 1) +
          geom_point(aes(size = effect_change, fill = influence_measure), 
                     shape = 21, color = "white", stroke = 0.8) +
          # 使用ggrepel添加标签，根据影响度调整位置
          geom_text_repel(
            aes(label = study_label),
            size = 3.2,
            color = "darkblue",
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.3,
            segment.color = "grey50",
            segment.size = 0.3,
            min.segment.length = 0.2,
            max.overlaps = 20,
            force = 2,
            direction = "both",
            nudge_y = ifelse(influence_stats$influence_measure > 2, 0.3, -0.3)
          ) +
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Influence") +
          scale_size_continuous(range = c(3, 8), name = "Effect Size\nChange") +
          labs(title = "Influence Analysis",
               subtitle = "Shows each study's influence on the overall results",
               x = "Study Number",
               y = "Influence Measure (|z-score|)") +
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom",
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          ) +
          scale_x_continuous(breaks = 1:k, labels = 1:k)
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Influence Analysis",
               subtitle = "Could not generate influence analysis") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "red"))
      })
    }
    
    # 5. Baujat图（带标签优化）
    create_baujat_plot <- function(data, meta_model) {
      if (nrow(data) < 3) {
        return(ggplot() +
                 labs(title = "Baujat Plot",
                      subtitle = "Need at least 3 studies for Baujat plot") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        # Create Baujat plot data
        Q_total <- meta_model$QE
        weights <- weights(meta_model)
        contributions <- (data$effect_size - meta_model$b[1])^2 / meta_model$vi
        
        plot_data <- data.frame(
          study = 1:meta_model$k,
          study_label = data$study_label,
          heterogeneity = contributions,
          influence = weights * (data$effect_size - meta_model$b[1])^2,
          weights = weights
        )
        
        # 识别异常值（前25%的影响度或异质性）
        influence_threshold <- quantile(plot_data$influence, 0.75)
        hetero_threshold <- quantile(plot_data$heterogeneity, 0.75)
        
        plot_data$is_outlier <- plot_data$influence > influence_threshold | 
          plot_data$heterogeneity > hetero_threshold
        
        library(ggrepel)
        
        ggplot(plot_data, aes(x = heterogeneity, y = influence)) +
          geom_point(aes(size = weights, fill = influence), 
                     shape = 21, color = "white", stroke = 0.8) +
          geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", 
                      linetype = "dashed", linewidth = 0.8) +
          # 为异常值点添加标签，其他点只在悬停时显示
          geom_text_repel(
            data = subset(plot_data, is_outlier),
            aes(label = study_label),
            size = 3.2,
            color = "darkred",
            fontface = "bold",
            box.padding = 0.6,
            point.padding = 0.4,
            segment.color = "red",
            segment.size = 0.4,
            min.segment.length = 0.1,
            max.overlaps = 15,
            force = 3,
            nudge_x = 0.1,
            nudge_y = 0.1
          ) +
          # 为非异常值点添加较轻的标签
          geom_text_repel(
            data = subset(plot_data, !is_outlier),
            aes(label = study_label),
            size = 2.8,
            color = "grey40",
            alpha = 0.7,
            box.padding = 0.3,
            point.padding = 0.2,
            segment.color = "grey",
            segment.size = 0.2,
            min.segment.length = 0.3,
            max.overlaps = 10,
            force = 1
          ) +
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Influence") +
          scale_size_continuous(range = c(3, 8), name = "Weight") +
          labs(title = "Baujat Plot",
               subtitle = "Identifies studies that contribute to heterogeneity",
               x = "Contribution to Q statistic",
               y = "Influence on overall estimate") +
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Baujat Plot",
               subtitle = "Error in generating Baujat plot") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      })
    }
    
    # 6. 径向图（带标签优化）
    create_radial_plot <- function(data, meta_model) {
      if (nrow(data) < 2) {
        return(ggplot() +
                 labs(title = "Radial Plot",
                      subtitle = "Insufficient data for radial plot") +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, face = "bold")))
      }
      
      tryCatch({
        # Prepare data for radial plot
        plot_data <- data.frame(
          study = 1:meta_model$k,
          study_label = data$study_label,
          precision = 1/sqrt(meta_model$vi),
          standardized_es = data$effect_size / sqrt(meta_model$vi),
          weight = weights(meta_model)
        )
        
        # 计算角度来分散标签
        plot_data$angle <- atan2(plot_data$standardized_es, plot_data$precision) * 180/pi
        
        # 根据位置分配标签方向
        plot_data$label_side <- ifelse(plot_data$standardized_es > 0, "top", "bottom")
        
        library(ggrepel)
        
        ggplot(plot_data, aes(x = precision, y = standardized_es)) +
          geom_point(aes(size = weight, fill = weight), 
                     shape = 21, color = "white", stroke = 0.8) +
          geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", 
                      linetype = "dashed", linewidth = 0.8) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.5) +
          # 使用角度优化的标签布局
          geom_text_repel(
            aes(label = study_label),
            size = 3.0,
            color = "darkblue",
            fontface = "bold",
            box.padding = 0.5,
            point.padding = 0.3,
            segment.color = "grey50",
            segment.size = 0.3,
            min.segment.length = 0.2,
            max.overlaps = 20,
            force = 2,
            direction = "both",
            # 根据象限调整标签位置
            nudge_x = ifelse(plot_data$precision > median(plot_data$precision), 0.2, -0.2),
            nudge_y = ifelse(plot_data$standardized_es > 0, 0.15, -0.15)
          ) +
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Weight") +
          scale_size_continuous(range = c(3, 8), name = "Weight") +
          labs(title = "Radial Plot",
               subtitle = "Alternative visualization of effect sizes and precision",
               x = "Precision (1/SE)",
               y = "Standardized Effect Size") +
          theme_classic(base_size = 12) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
            plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
      }, error = function(e) {
        ggplot() +
          labs(title = "Radial Plot",
               subtitle = "Error in generating radial plot") +
          theme_void() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      })
    }
    
    # Funnel plot alert
    output$funnel_alert <- renderUI({
      if (!manual_values$analysis_complete) {
        return(tags$div(class = "alert alert-info",
                        "Run meta-analysis first to generate funnel plot."))
      }
      
      data <- prepared_data()
      if (nrow(data) < 10) {
        tags$div(
          class = "alert alert-warning",
          tags$p(icon("exclamation-triangle"),
                 "Note: Funnel plot interpretation requires at least 10 studies for reliable assessment of publication bias.")
        )
      } else {
        tags$div(
          class = "alert alert-info",
          tags$p(icon("funnel"),
                 "Funnel plot for assessing publication bias based on", nrow(data), "studies.")
        )
      }
    })
    
    # 使用新的森林图函数
    output$forest_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      # 使用新的森林图函数
      create_forest_plot(data, meta_model)
    })
    
    # 使用新的漏斗图函数
    output$funnel_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      # 使用增强版漏斗图函数
      result <- create_enhanced_funnel_plot(data, meta_model)
      result$plot
    })
    
    # Meta-analysis summary
    output$meta_summary <- renderPrint({
      req(manual_values$meta_results)
      
      model <- manual_values$meta_results$model
      data <- manual_values$meta_results$data
      method <- manual_values$meta_results$method
      
      if (manual_values$using_example_data) {
        cat("=== EXAMPLE META-ANALYSIS RESULTS ===\n\n")
      } else {
        cat("=== META-ANALYSIS RESULTS ===\n\n")
      }
      
      cat("META-ANALYSIS RESULTS\n")
      cat("=====================\n")
      cat("Method:", method, "\n")
      cat("Number of studies:", model$k, "\n")
      cat("Overall effect size:", round(model$b[1], 3), "\n")
      cat("95% CI: [", round(model$ci.lb, 3), ", ", round(model$ci.ub, 3), "]\n", sep = "")
      cat("z-value:", round(model$zval, 3), "\n")
      cat("p-value:", round(model$pval, 4), "\n")
      cat("\n")
      
      if (!all(is.na(data$sample_size))) {
        total_n <- sum(data$sample_size, na.rm = TRUE)
        cat("Total sample size:", total_n, "\n")
        cat("Average sample size per study:", round(mean(data$sample_size, na.rm = TRUE), 1), "\n")
      }
      
      if (manual_values$using_example_data) {
        cat("\nNOTE: These results are based on example data for demonstration purposes.\n")
      }
    })
    
    # Heterogeneity analysis
    output$heterogeneity_summary <- renderPrint({
      req(manual_values$meta_results)
      
      model <- manual_values$meta_results$model
      
      cat("HETEROGENEITY ANALYSIS\n")
      cat("======================\n")
      cat("I² (total heterogeneity):", round(model$I2, 1), "%\n")
      cat("Tau² (between-study variance):", round(model$tau2, 3), "\n")
      cat("Tau (standard deviation):", round(sqrt(model$tau2), 3), "\n")
      cat("Q statistic:", round(model$QE, 3), "\n")
      cat("Degrees of freedom:", model$k - 1, "\n")
      cat("p-value for heterogeneity:", round(model$QEp, 4), "\n")
      cat("\n")
      
      if (model$QEp < 0.05) {
        cat("Interpretation: Significant heterogeneity detected\n")
      } else {
        cat("Interpretation: No significant heterogeneity\n")
      }
    })
    
    # Publication bias tests
    output$bias_tests <- renderPrint({
      req(manual_values$meta_results)
      
      model <- manual_values$meta_results$model
      
      cat("PUBLICATION BIAS TESTS\n")
      cat("======================\n")
      
      # Egger's test
      tryCatch({
        egger_test <- metafor::regtest(model)
        cat("Egger's Test for Funnel Plot Asymmetry:\n")
        cat("  z-value:", round(egger_test$zval, 3), "\n")
        cat("  p-value:", round(egger_test$pval, 4), "\n")
        cat("  Interpretation:", ifelse(egger_test$pval < 0.05, 
                                        "Possible publication bias", 
                                        "No significant publication bias"), "\n")
      }, error = function(e) {
        cat("Egger's test: Could not be computed\n")
      })
    })
    
    # 使用新的累积meta分析图函数
    output$cumulative_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      create_cumulative_plot(data, meta_model)
    })
    
    # 使用新的影响分析图函数
    output$influence_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      create_influence_plot(data, meta_model)
    })
    
    # 使用新的Baujat图函数
    output$baujat_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      create_baujat_plot(data, meta_model)
    })
    
    # 使用新的径向图函数
    output$radial_plot <- renderPlot({
      req(manual_values$meta_results)
      
      meta_results <- manual_values$meta_results
      data <- meta_results$data
      meta_model <- meta_results$model
      
      create_radial_plot(data, meta_model)
    })
    
    # Download handlers
    output$download_forest <- downloadHandler(
      filename = function() {
        paste("forest_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Regenerate and save forest plot
        req(manual_values$meta_results)
        meta_results <- manual_values$meta_results
        p <- create_forest_plot(meta_results$data, meta_results$model)
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300)
      }
    )
    
    output$download_funnel <- downloadHandler(
      filename = function() {
        paste("funnel_plot_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Regenerate and save funnel plot
        req(manual_values$meta_results)
        meta_results <- manual_values$meta_results
        result <- create_enhanced_funnel_plot(meta_results$data, meta_results$model)
        ggsave(file, plot = result$plot, width = 10, height = 8, dpi = 300)
      }
    )
    
    output$download_meta_data <- downloadHandler(
      filename = function() {
        paste("meta_analysis_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(manual_values$meta_results)
        write.csv(manual_values$meta_results$data, file, row.names = FALSE)
      }
    )
    
    # Return reactive values for use in other modules
    return(list(
      meta_results = reactive(manual_values$meta_results),
      analysis_complete = reactive(manual_values$analysis_complete),
      using_example_data = reactive(manual_values$using_example_data),
      mapped_data = reactive(manual_values$mapped_data)
    ))
  })
}