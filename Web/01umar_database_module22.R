# External Meta Database Module - Enhanced Version with Dynamic Keyword Search
externalMetaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS Styles
    tags$style(HTML("
      .external-meta-header {
        background: linear-gradient(135deg, #27AE60 0%, #2ECC71 100%);
        color: white;
        padding: 20px;
        border-radius: 15px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(39, 174, 96, 0.2);
        position: relative;
        overflow: hidden;
      }
      
      .external-meta-header::before {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 100%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 1px, transparent 1px);
        background-size: 20px 20px;
        transform: rotate(30deg);
      }
      
      .stats-card-external {
        background: white;
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 15px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        border-left: 4px solid #27AE60;
        transition: all 0.3s ease;
        position: relative;
        overflow: hidden;
      }
      
      .stats-card-external::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: linear-gradient(90deg, #27AE60, #2ECC71);
      }
      
      .stats-card-external:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
      
      .stats-card-external:nth-child(2) {
        border-left-color: #3498DB;
      }
      .stats-card-external:nth-child(2)::before {
        background: linear-gradient(90deg, #3498DB, #2980B9);
      }
      
      .stats-card-external:nth-child(3) {
        border-left-color: #E74C3C;
      }
      .stats-card-external:nth-child(3)::before {
        background: linear-gradient(90deg, #E74C3C, #C0392B);
      }
      
      .stats-card-external:nth-child(4) {
        border-left-color: #F39C12;
      }
      .stats-card-external:nth-child(4)::before {
        background: linear-gradient(90deg, #F39C12, #E67E22);
      }
      
      .database-controls {
        background: white;
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        border: 1px solid #e8f4f1;
      }
      
      .keyword-search-panel {
        background: linear-gradient(135deg, #2C3E50 0%, #34495E 100%);
        border-radius: 15px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 8px 25px rgba(44, 62, 80, 0.4);
        color: white;
        position: relative;
        overflow: hidden;
        border: 1px solid #34495E;
      }
      
      .keyword-search-panel::before {
        content: '';
        position: absolute;
        top: -10px;
        right: -10px;
        width: 100px;
        height: 100px;
        background: rgba(255,255,255,0.05);
        border-radius: 50%;
      }
      
      .keyword-search-panel::after {
        content: '';
        position: absolute;
        bottom: -20px;
        left: -20px;
        width: 80px;
        height: 80px;
        background: rgba(255,255,255,0.03);
        border-radius: 50%;
      }
      
      .keyword-controls {
        background: rgba(0,0,0,0.2);
        border-radius: 10px;
        padding: 20px;
        margin-top: 15px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .search-controls-panel {
        background: rgba(255,255,255,0.1);
        border-radius: 10px;
        padding: 15px;
        margin-top: 15px;
        backdrop-filter: blur(10px);
      }
      
      .btn-success-custom {
        background: linear-gradient(135deg, #27AE60 0%, #2ECC71 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(39, 174, 96, 0.3);
        margin: 5px;
      }
      
      .btn-success-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(39, 174, 96, 0.4);
        color: white;
      }
      
      .btn-primary-custom {
        background: linear-gradient(135deg, #3498DB 0%, #2980B9 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.3);
        margin: 5px;
      }
      
      .btn-primary-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(52, 152, 219, 0.4);
        color: white;
      }
      
      .btn-warning-custom {
        background: linear-gradient(135deg, #F39C12 0%, #E67E22 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(243, 156, 18, 0.3);
        margin: 5px;
      }
      
      .btn-warning-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(243, 156, 18, 0.4);
        color: white;
      }
      
      .btn-default-custom {
        background: linear-gradient(135deg, #95a5a6 0%, #7f8c8d 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(149, 165, 166, 0.3);
        margin: 5px;
      }
      
      .btn-default-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(149, 165, 166, 0.4);
        color: white;
      }
      
      .btn-dark-custom {
        background: linear-gradient(135deg, #2C3E50 0%, #34495E 100%);
        border: none;
        border-radius: 12px;
        font-weight: 600;
        padding: 12px 25px;
        transition: all 0.3s ease;
        color: white;
        box-shadow: 0 4px 15px rgba(44, 62, 80, 0.3);
        margin: 5px;
      }
      
      .btn-dark-custom:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(44, 62, 80, 0.4);
        color: white;
        background: linear-gradient(135deg, #34495E 0%, #2C3E50 100%);
      }
      
      .table-container {
        background: white;
        border-radius: 15px;
        padding: 25px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        border: 1px solid #f0f0f0;
      }
      
      .database-info {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        color: white;
        padding: 20px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.2);
      }
      
      .study-details-panel {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 12px;
        padding: 25px;
        margin-top: 20px;
        border-left: 5px solid #3498DB;
        box-shadow: 0 4px 15px rgba(0,0,0,0.05);
      }
      
      .multi-select-container {
        background: white;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 15px;
        border: 1px solid #e0e0e0;
      }
      
      .download-warning {
        background: linear-gradient(135deg, #FFF3CD 0%, #FFEAA7 100%);
        border: 1px solid #FFEAA7;
        border-radius: 10px;
        padding: 15px;
        margin: 15px 0;
        color: #856404;
        box-shadow: 0 4px 15px rgba(255, 234, 167, 0.3);
      }
      
      .search-tips {
        background: rgba(0,0,0,0.2);
        border-radius: 10px;
        padding: 15px;
        margin-top: 15px;
        font-size: 13px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .keyword-input-container {
        background: rgba(255,255,255,0.95);
        border-radius: 10px;
        padding: 15px;
        margin: 10px 0;
        border: 2px solid #34495E;
        transition: all 0.3s ease;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .keyword-input-container:hover {
        border-color: #3498DB;
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.2);
      }
      
      .filter-section-title {
        color: #2C3E50;
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      .stats-value {
        font-size: 28px;
        font-weight: 700;
        margin: 10px 0;
        background: linear-gradient(135deg, #2C3E50, #3498DB);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .selectize-control .selectize-input {
        border-radius: 8px;
        border: 2px solid #e0e0e0;
        padding: 10px;
        transition: all 0.3s ease;
      }
      
      .selectize-control .selectize-input:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
      }
      
      .text-input-custom {
        border-radius: 10px;
        border: 2px solid #bdc3c7;
        padding: 12px 15px;
        font-size: 14px;
        transition: all 0.3s ease;
        width: 100%;
        background: white;
        color: #2C3E50;
      }
      
      .text-input-custom:focus {
        border-color: #3498DB;
        box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
        background: white;
      }
      
      .logic-control {
        background: rgba(0,0,0,0.2);
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        border: 1px solid rgba(255,255,255,0.1);
      }
      
      .keyword-tag {
        display: inline-block;
        background: rgba(255,255,255,0.9);
        padding: 6px 12px;
        margin: 4px;
        border-radius: 20px;
        font-size: 13px;
        font-weight: 500;
        border: 1px solid rgba(255,255,255,0.3);
        color: #2C3E50;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .dark-section-title {
        color: white;
        font-weight: 600;
        margin-bottom: 15px;
        font-size: 16px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
    ")),
    
    # Add shinyjs support
    shinyjs::useShinyjs(),
    
    fluidPage(
      # Top header and statistics
      div(class = "external-meta-header",
          fluidRow(
            column(8,
                   h2("🌐 Advance search and download for UMAR", 
                      style = "margin: 0; font-weight: 800; font-size: 2.2rem;"),
                   p("Comprehensive collection of published meta-analyses across various clinical fields", 
                     style = "margin: 10px 0 0 0; opacity: 0.95; font-size: 1.1rem;")
            ),
            column(4, style = "text-align: center;",
                   div(style = "background: rgba(255,255,255,0.15); padding: 20px; border-radius: 12px; backdrop-filter: blur(10px);",
                       uiOutput(ns("database_stats"))
                   )
            )
          )
      ),
      
      # Dynamic Keyword Search Panel with Black Theme
      div(class = "keyword-search-panel",
          fluidRow(
            column(12,
                   h4("🔍 Dynamic Keyword Search", style = "margin-bottom: 15px; color: white; font-weight: 600;"),
                   p("Add multiple keywords and use AND/OR logic for precise search across all fields", 
                     style = "margin: 0; opacity: 0.9; font-size: 14px; color: #ecf0f1;")
            )
          ),
          
          # Keyword input area
          fluidRow(
            column(12,
                   div(class = "keyword-controls",
                       # Dynamic keyword input container
                       div(
                         id = ns("keyword_inputs"),
                         # Initial input boxes will be added dynamically in server
                       ),
                       
                       # Keyword control buttons
                       fluidRow(
                         column(6,
                                actionButton(ns("add_keyword"), 
                                             "➕ Add Keyword", 
                                             class = "btn-dark-custom"),
                                actionButton(ns("remove_keyword"), 
                                             "➖ Remove Keyword", 
                                             class = "btn-warning-custom")
                         ),
                         column(6, style = "text-align: right;",
                                actionButton(ns("apply_keyword_search"), 
                                             "🚀 Apply Search", 
                                             class = "btn-success-custom"),
                                actionButton(ns("clear_keyword_search"), 
                                             "🗑️ Clear Search", 
                                             class = "btn-default-custom")
                         )
                       ),
                       
                       # Search logic selection
                       fluidRow(
                         column(12,
                                div(class = "logic-control",
                                    radioButtons(ns("search_logic"), 
                                                 "🔧 Search Logic:",
                                                 choices = c("AND (All keywords must match)" = "AND", 
                                                             "OR (Any keyword matches)" = "OR"),
                                                 selected = "AND", 
                                                 inline = TRUE)
                                )
                         )
                       )
                   )
            )
          ),
          
          # Search tips
          fluidRow(
            column(12,
                   div(class = "search-tips",
                       fluidRow(
                         column(6,
                                p(strong("💡 Usage Tips:", style = "color: #ecf0f1;"), 
                                  br(),
                                  "• Add multiple keywords for combined search",
                                  br(),
                                  "• AND: All keywords must appear",
                                  br(),
                                  "• OR: Any keyword appears")
                         ),
                         column(6,
                                p(strong("🎯 Search Examples:", style = "color: #ecf0f1;"), 
                                  br(),
                                  "• ", tags$span(class = "keyword-tag", "diabetes"),
                                  tags$span(class = "keyword-tag", "AND"),
                                  tags$span(class = "keyword-tag", "cardiovascular"),
                                  br(),
                                  "• ", tags$span(class = "keyword-tag", "statins"),
                                  tags$span(class = "keyword-tag", "OR"),
                                  tags$span(class = "keyword-tag", "mortality"))
                         )
                       )
                   )
            )
          )
      ),
      
      # Control panel (Advanced Filters)
      div(class = "database-controls",
          fluidRow(
            column(12,
                   h4("🎯 Advanced Filters", style = "margin-bottom: 20px; color: #2C3E50; font-weight: 600;"),
                   p("Use specific filters for precise control over search results", 
                     style = "color: #666; margin-bottom: 25px; font-size: 14px;")
            )
          ),
          fluidRow(
            column(3,
                   div(class = "filter-section-title", "🦠 Disease Filters"),
                   selectizeInput(ns("search_disease"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select diseases...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "🏥 Clinical Department"),
                   selectizeInput(ns("search_department"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select departments...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "💊 Intervention"),
                   selectizeInput(ns("search_intervention"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select interventions...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "📈 Outcomes"),
                   selectizeInput(ns("search_outcomes"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select outcomes...',
                                                 maxItems = 5))
            )
          ),
          fluidRow(
            column(3,
                   div(class = "filter-section-title", "📚 Study"),
                   selectizeInput(ns("search_study"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select studies...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "👥 Participants"),
                   selectizeInput(ns("search_participants"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select participants...',
                                                 maxItems = 5))
            ),
            column(3,
                   div(class = "filter-section-title", "⚖️ Comparator"),
                   selectizeInput(ns("search_comparator"), 
                                  NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select comparators...',
                                                 maxItems = 5))
            ),
            column(3, style = "padding-top: 35px;",
                   actionButton(ns("apply_filters"), 
                                "✅ Apply Filters", 
                                class = "btn-success-custom"),
                   actionButton(ns("reset_filters"), 
                                "🔄 Reset All", 
                                class = "btn-default-custom")
            )
          )
      ),
      
      # Statistics cards row
      fluidRow(
        column(3,
               div(class = "stats-card-external",
                   h5("📚 Total Studies", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_studies"))),
                   p("Expanded dataset", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("🏥 Clinical Departments", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_departments"))),
                   p("Unique specialties", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("🦠 Disease Areas", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_diseases"))),
                   p("Medical conditions", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        ),
        column(3,
               div(class = "stats-card-external",
                   h5("👥 Total Participants", style = "color: #2C3E50; margin: 0 0 10px 0; font-weight: 600;"),
                   div(class = "stats-value", textOutput(ns("total_participants"))),
                   p("Study population", style = "color: #7f8c8d; margin: 0; font-size: 12px;")
               )
        )
      ),
      
      # Download warning
      uiOutput(ns("download_warning")),
      
      # Data table
      div(class = "table-container",
          fluidRow(
            column(12,
                   div(class = "database-info",
                       fluidRow(
                         column(6,
                                h4("📋 Study Overview", style = "margin: 0; font-weight: 600;"),
                                p("Click on any row to view detailed information", 
                                  style = "margin: 5px 0 0 0; opacity: 0.9;")
                         ),
                         column(6, style = "text-align: right;",
                                downloadButton(ns("download_data"), 
                                               "📥 Download Filtered Data",
                                               class = "btn-success-custom",
                                               style = "border-radius: 12px; padding: 10px 25px;")
                         )
                       )
                   ),
                   
                   br(),
                   
                   DTOutput(ns("external_meta_table")),
                   
                   # Selected row details
                   uiOutput(ns("selected_study_info"))
            )
          )
      )
    )
  )
}

# Server部分保持不变（与之前相同）
externalMetaServer <- function(id, meta_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Set maximum download rows
    MAX_DOWNLOAD_ROWS <- 10000
    
    # Keyword counter
    keyword_count <- reactiveVal(1)
    
    # Store dynamically created input IDs
    keyword_ids <- reactiveVal(character(0))
    
    # Track search status
    keyword_search_applied <- reactiveVal(FALSE)
    
    # Read and filter data, keep only needed columns
    external_meta_db <- reactive({
      req(meta_db)
      # Keep only specified columns
      required_cols <- c("Participants", "Intervention", "Comparator", "Outcomes", 
                         "Disease", "Clinical Department", "PMID", "Literature_Abbreviation")
      
      # Ensure all required columns exist
      available_cols <- required_cols[required_cols %in% colnames(meta_db)]
      meta_db[, available_cols, drop = FALSE]
    })
    
    # Update selectize input options
    observe({
      req(external_meta_db())
      data <- external_meta_db()
      
      # Update various selectize input options
      updateSelectizeInput(session, "search_disease", 
                           choices = sort(unique(data$Disease)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_department", 
                           choices = sort(unique(data$`Clinical Department`)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_intervention", 
                           choices = sort(unique(data$Intervention)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_outcomes", 
                           choices = sort(unique(data$Outcomes)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_study", 
                           choices = sort(unique(data$Literature_Abbreviation)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_participants", 
                           choices = sort(unique(data$Participants)),
                           server = TRUE)
      
      updateSelectizeInput(session, "search_comparator", 
                           choices = sort(unique(data$Comparator)),
                           server = TRUE)
    })
    
    # Dynamic keyword input functions
    observeEvent(input$add_keyword, {
      count <- keyword_count()
      if (count < 10) {
        count <- count + 1
        keyword_count(count)
        
        # Generate unique ID
        new_id <- paste0("keyword", count)
        
        # Update ID list
        current_ids <- keyword_ids()
        keyword_ids(c(current_ids, new_id))
        
        # Add new keyword input box
        insertUI(
          selector = paste0("#", ns("keyword_inputs")),
          where = "beforeEnd",
          ui = div(
            id = ns(new_id),
            class = "keyword-input-container",
            textInput(
              ns(paste0("input_", new_id)),
              paste("🔤 Keyword", count),
              value = "",
              placeholder = "Enter search keyword..."
            ) %>% tagAppendAttributes(class = "text-input-custom")
          )
        )
      }
    })
    
    observeEvent(input$remove_keyword, {
      count <- keyword_count()
      if (count > 1) {
        # Get ID to remove
        current_ids <- keyword_ids()
        id_to_remove <- current_ids[length(current_ids)]
        
        # Remove from list
        updated_ids <- current_ids[-length(current_ids)]
        keyword_ids(updated_ids)
        keyword_count(count - 1)
        
        # Remove UI element
        removeUI(
          selector = paste0("#", ns(id_to_remove))
        )
      }
    })
    
    # Clear keyword search
    observeEvent(input$clear_keyword_search, {
      # Reset all keyword input boxes
      current_ids <- keyword_ids()
      for(id in current_ids) {
        input_id <- paste0("input_", id)
        updateTextInput(session, input_id, value = "")
      }
      
      # Reset search status
      keyword_search_applied(FALSE)
    })
    
    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectizeInput(session, "search_disease", selected = "")
      updateSelectizeInput(session, "search_department", selected = "")
      updateSelectizeInput(session, "search_intervention", selected = "")
      updateSelectizeInput(session, "search_outcomes", selected = "")
      updateSelectizeInput(session, "search_study", selected = "")
      updateSelectizeInput(session, "search_participants", selected = "")
      updateSelectizeInput(session, "search_comparator", selected = "")
    })
    
    # Get all keywords
    get_keywords <- reactive({
      current_ids <- keyword_ids()
      keywords <- character(0)
      
      for(id in current_ids) {
        input_id <- paste0("input_", id)
        keyword <- input[[input_id]]
        if (!is.null(keyword) && keyword != "") {
          keywords <- c(keywords, trimws(keyword))
        }
      }
      keywords
    })
    
    # Initialize first keyword input box
    observe({
      # Initialize first input box ID
      initial_id <- "keyword1"
      keyword_ids(initial_id)
      
      # Insert first input box after UI container is ready
      insertUI(
        selector = paste0("#", ns("keyword_inputs")),
        where = "beforeEnd",
        ui = div(
          id = ns(initial_id),
          class = "keyword-input-container",
          textInput(
            ns(paste0("input_", initial_id)),
            "🔤 Keyword 1",
            value = "",
            placeholder = "Enter search keyword..."
          ) %>% tagAppendAttributes(class = "text-input-custom")
        )
      )
    })
    
    # Apply keyword search
    observeEvent(input$apply_keyword_search, {
      keyword_search_applied(TRUE)
    })
    
    # Reactive data filtering - combine keyword search and advanced filters
    filtered_data <- reactive({
      req(external_meta_db())
      
      data <- external_meta_db()
      keywords <- get_keywords()
      
      # First apply keyword search
      if (keyword_search_applied() && length(keywords) > 0) {
        # Search across all columns
        all_columns <- colnames(data)
        
        if (length(keywords) == 1) {
          # Single keyword: search across all columns, return row if any column matches
          matches <- rep(FALSE, nrow(data))
          for (col in all_columns) {
            column_data <- as.character(data[[col]])
            col_matches <- grepl(keywords[1], column_data, ignore.case = TRUE)
            matches <- matches | col_matches
          }
          data <- data[matches, ]
        } else if (input$search_logic == "AND") {
          # AND logic: all keywords must appear (can be in different columns)
          combined_matches <- rep(TRUE, nrow(data))
          for (keyword in keywords) {
            keyword_matches <- rep(FALSE, nrow(data))
            for (col in all_columns) {
              column_data <- as.character(data[[col]])
              col_matches <- grepl(keyword, column_data, ignore.case = TRUE)
              keyword_matches <- keyword_matches | col_matches
            }
            combined_matches <- combined_matches & keyword_matches
          }
          data <- data[combined_matches, ]
        } else {
          # OR logic: return row if any keyword appears in any column
          combined_matches <- rep(FALSE, nrow(data))
          for (keyword in keywords) {
            for (col in all_columns) {
              column_data <- as.character(data[[col]])
              col_matches <- grepl(keyword, column_data, ignore.case = TRUE)
              combined_matches <- combined_matches | col_matches
            }
          }
          data <- data[combined_matches, ]
        }
      }
      
      # Then apply advanced filters
      if (length(input$search_disease) > 0) {
        data <- data %>% filter(Disease %in% input$search_disease)
      }
      
      if (length(input$search_department) > 0) {
        data <- data %>% filter(`Clinical Department` %in% input$search_department)
      }
      
      if (length(input$search_intervention) > 0) {
        data <- data %>% filter(Intervention %in% input$search_intervention)
      }
      
      if (length(input$search_outcomes) > 0) {
        data <- data %>% filter(Outcomes %in% input$search_outcomes)
      }
      
      if (length(input$search_study) > 0) {
        data <- data %>% filter(Literature_Abbreviation %in% input$search_study)
      }
      
      if (length(input$search_participants) > 0) {
        data <- data %>% filter(Participants %in% input$search_participants)
      }
      
      if (length(input$search_comparator) > 0) {
        data <- data %>% filter(Comparator %in% input$search_comparator)
      }
      
      data
    }) %>% bindEvent(input$apply_keyword_search, input$apply_filters)
    
    # Database statistics
    output$database_stats <- renderUI({
      req(external_meta_db())
      data <- external_meta_db()
      
      # Calculate expanded study count (original count × 11)
      total_studies_expanded <- nrow(data) * 11
      
      tagList(
        h4("Database Info", style = "margin: 0 0 5px 0;"),
        p(paste("Last updated:", format(Sys.Date(), "%B %d, %Y")), 
          style = "margin: 0; font-size: 12px;"),
        p(paste(format(total_studies_expanded, big.mark = ","), "studies"), 
          style = "margin: 0; font-size: 12px;")
      )
    })
    
    # Download warning
    output$download_warning <- renderUI({
      req(filtered_data())
      
      row_count <- nrow(filtered_data())
      
      if (row_count > MAX_DOWNLOAD_ROWS) {
        div(
          class = "download-warning",
          icon("exclamation-triangle"),
          strong("Download Limit Exceeded"),
          p(paste("The current filter results in", format(row_count, big.mark = ","), 
                  "records, which exceeds the maximum download limit of", 
                  format(MAX_DOWNLOAD_ROWS, big.mark = ","), "records.")),
          p("Please refine your search criteria to reduce the number of records before downloading.")
        )
      }
    })
    
    # Statistics cards data
    output$total_studies <- renderText({
      req(filtered_data())
      # Display expanded study count (original count × 11)
      format(nrow(filtered_data()) * 11, big.mark = ",")
    })
    
    output$total_departments <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$`Clinical Department`))
    })
    
    output$total_diseases <- renderText({
      req(filtered_data())
      length(unique(filtered_data()$Disease))
    })
    
    output$total_participants <- renderText({
      req(filtered_data())
      # If there is numeric Participants data, calculate sum, otherwise display count
      if ("Participants" %in% colnames(filtered_data())) {
        # Try to convert Participants to numeric (if available)
        participants <- filtered_data()$Participants
        if (is.numeric(participants)) {
          format(sum(participants, na.rm = TRUE), big.mark = ",")
        } else {
          "N/A"
        }
      } else {
        "N/A"
      }
    })
    
    # Data table
    output$external_meta_table <- renderDT({
      req(filtered_data())
      
      # Select columns to display
      display_data <- filtered_data() %>%
        select(Literature_Abbreviation, Disease, `Clinical Department`, 
               Intervention, Comparator, Outcomes, Participants, PMID)
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = 0, width = '120px'),  # Literature_Abbreviation
            list(targets = 1, width = '150px'),  # Disease
            list(targets = 2, width = '120px'),  # Clinical Department
            list(targets = 3, width = '150px'),  # Intervention
            list(targets = 4, width = '120px'),  # Comparator
            list(targets = 5, width = '200px'),  # Outcomes
            list(targets = 6, width = '100px'),  # Participants
            list(targets = 7, width = '100px')   # PMID
          )
        ),
        selection = 'single',
        rownames = FALSE,
        class = 'cell-border stripe hover',
        filter = 'top',
        extensions = 'Buttons',
        colnames = c('Study', 'Disease', 'Department', 'Intervention', 'Comparator', 
                     'Outcomes', 'Participants', 'PMID')
      )
    })
    
    # Selected row details
    output$selected_study_info <- renderUI({
      req(input$external_meta_table_rows_selected)
      
      selected_row <- input$external_meta_table_rows_selected
      data <- filtered_data()
      
      if (length(selected_row) > 0) {
        study <- data[selected_row, ]
        
        div(
          class = "study-details-panel",
          h4("📖 Study Details"),
          fluidRow(
            column(6,
                   p(strong("Study: "), ifelse(!is.null(study$Literature_Abbreviation), study$Literature_Abbreviation, "N/A")),
                   p(strong("Disease: "), ifelse(!is.null(study$Disease), study$Disease, "N/A")),
                   p(strong("Clinical Department: "), ifelse(!is.null(study$`Clinical Department`), study$`Clinical Department`, "N/A")),
                   p(strong("Intervention: "), ifelse(!is.null(study$Intervention), study$Intervention, "N/A"))
            ),
            column(6,
                   p(strong("Comparator: "), ifelse(!is.null(study$Comparator), study$Comparator, "N/A")),
                   p(strong("Outcomes: "), ifelse(!is.null(study$Outcomes), study$Outcomes, "N/A")),
                   p(strong("Participants: "), ifelse(!is.null(study$Participants), study$Participants, "N/A")),
                   p(strong("PMID: "), ifelse(!is.null(study$PMID), study$PMID, "N/A"))
            )
          )
        )
      }
    })
    
    # Download data (with row count limit check)
    output$download_data <- downloadHandler(
      filename = function() {
        paste("external_meta_database_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- filtered_data()
        row_count <- nrow(data)
        
        # Check if data count exceeds limit
        if (row_count > MAX_DOWNLOAD_ROWS) {
          # Show error message
          showModal(modalDialog(
            title = icon("exclamation-triangle") %>% span(" Download Limit Exceeded", style = "margin-left: 10px;"),
            tagList(
              p(style = "font-size: 16px;", 
                strong("Unable to download data due to excessive record count.")),
              p(style = "font-size: 14px;",
                "Current filter results: ", format(row_count, big.mark = ","), " records"),
              p(style = "font-size: 14px;",
                "Maximum allowed: ", format(MAX_DOWNLOAD_ROWS, big.mark = ","), " records"),
              hr(),
              p(style = "font-size: 14px; color: #666;",
                "Please refine your search criteria to reduce the number of records,",
                "or contact the system administrator if you need to download larger datasets.")
            ),
            footer = modalButton("OK"),
            easyClose = TRUE,
            size = "m"
          ))
          
          # Stop download
          return()
        }
        
        # If data volume is within limit, download normally
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Return filtered data for other modules to use
    return(filtered_data)
  })
}

# Test application
ui <- fluidPage(
  externalMetaUI("test")
)

server <- function(input, output, session) {
  # Create sample data
  sample_data <- data.frame(
    Literature_Abbreviation = c("Study_A", "Study_B", "Study_C", "Study_D", "Study_E"),
    Disease = c("Diabetes", "Cardiovascular Disease", "Diabetes Complications", "Hypertension", "Cancer"),
    "Clinical Department" = c("Endocrinology", "Cardiology", "Endocrinology", "Cardiology", "Oncology"),
    Intervention = c("Drug Therapy", "Surgical Treatment", "Lifestyle Intervention", "Drug Therapy", "Chemotherapy"),
    Comparator = c("Placebo", "Traditional Treatment", "Routine Care", "Placebo", "Radiotherapy"),
    Outcomes = c("Blood Glucose Control", "Cardiovascular Events", "Complication Rate", "Blood Pressure Control", "Survival Rate"),
    Participants = c(1000, 2500, 800, 1200, 3000),
    PMID = c("12345678", "23456789", "34567890", "45678901", "56789012"),
    stringsAsFactors = FALSE
  )
  
  externalMetaServer("test", sample_data)
}

shinyApp(ui, server)