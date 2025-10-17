# app.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(metafor)
library(shinythemes)
library(ggplot2)
library(fresh)
library(dplyr)
library(tidyr)
library(purrr)
library(ggpattern)
library(ggforce)
library(RColorBrewer)
library(scales)

# 加载模块
#setwd("C://BaiduSyncdisk/DT20250220/20250919UMAR/UMAR/")
source("00meta_db_example.R")
source("01umar_database_module22.R")
source("02department_analysis_module3.R")
source("03networkModule.R")
source("04Evidence Map2.R")
source("05UMARforestplot5.R")
source("06agent_Forest10.R")
source("07manuForest10.R")
source("07LLMbenchmark_module2.R")

source("08About_module.R")

# 统一的顶部卡片函数
departmentHeader <- function(title, subtitle, icon = "📊") {
  div(class = "department-header",
      h2(paste(icon, title), style = "margin: 0; font-weight: 700;"),
      p(subtitle, style = "margin: 5px 0 0 0; opacity: 0.9;")
  )
}

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2C3E50",
    aqua = "#3498DB",
    green = "#2ECC71",
    yellow = "#F1C40F",
    red = "#E74C3C"
  ),
  adminlte_sidebar(
    dark_bg = "#2C3E50",
    dark_hover_bg = "#34495E",
    dark_color = "#ECF0F1"
  ),
  adminlte_global(
    content_bg = "#F8F9FA",
    box_bg = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)

# 加载数据
meta_db<- readRDS("newfilter_database250926.rds")


# meta_db <- readRDS("UMARtemp/rawdatabase0930.rds")
# meta_db$`Clinical Department` <- meta_db$Clinical_Department
umar_data_clean <- readRDS("umar_data_clean0927.rds")

# 定义 meta_db_network 数据集
meta_db_network <- local({
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
      CI_Width = CI_Upper - CI_Lower
    )
  
  return(data)
})

# LLM Benchmarking 数据加载和函数
load_llm_benchmark_data <- function() {
  # 这里假设您有一个benchmarkresults0925.csv文件
  # 如果没有，我们创建一个示例数据
  if (file.exists("benchmarkresults0925.csv")) {
    data <- read.csv("benchmarkresults0925.csv")
  } else {
    # 创建示例数据
    set.seed(123)
    models <- c("GPT-4", "Claude-3", "Gemini-Pro", "LLaMA-2", "Mistral", 
                "ChatGLM", "Baichuan", "Qwen", "InternLM", "Yi-34B")
    
    data <- data.frame(
      model_name = models,
      Developer = c("OpenAI", "Anthropic", "Google", "Meta", "Mistral AI",
                    "Zhipu AI", "Baichuan Inc", "Alibaba", "Shanghai AI", "01.ai"),
      Release_Year = sample(2020:2024, 10),
      Context_Length_K_tokens = sample(32:128, 10),
      Total_Parameters_B = round(runif(10, 7, 70), 1),
      avg_response_time = round(runif(10, 1.5, 8.5), 1),
      match_success_rate = round(runif(10, 0.65, 0.95), 3),
      studies_accuracy = round(runif(10, 0.70, 0.98), 3),
      sample_accuracy = round(runif(10, 0.75, 0.96), 3),
      extract_accuracy = round(runif(10, 0.68, 0.94), 3),
      amstar_non_critical_accuracy = round(runif(10, 0.72, 0.97), 3),
      amstar_critical_accuracy = round(runif(10, 0.65, 0.92), 3),
      amstar_accuracy = round(runif(10, 0.70, 0.95), 3),
      overall_score = round(runif(10, 0.72, 0.96), 3)
    )
  }
  
  return(data)
}

# UI
ui <- dashboardPage(
  skin = "blue",
  
  # 移除dashboardHeader
  dashboardHeader(disable = TRUE),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    use_theme(mytheme),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        /* 主内容区域样式 */
        .content-wrapper, .right-side {
          background-color: #F8F9FA;
        }
        
        /* 静态标题区域样式 */
        .main-title-container {
          background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
          color: white;
          padding: 25px 0;
          margin-bottom: 0;
          border-bottom: 1px solid #BDC3C7;
        }
        
        .main-title {
          font-weight: 800;
          font-size: 2.4rem;
          margin: 0;
          text-shadow: 1px 1px 3px rgba(0,0,0,0.3);
        }
        
        .main-subtitle {
          font-size: 1.15rem;
          opacity: 0.95;
          margin: 8px 0 0 0;
          font-weight: 400;
        }
        
        /* 导航标签容器样式 */
        .nav-container {
          background: white;
          border-bottom: 2px solid #2C3E50;
          padding: 0;
        }
        
        /* 导航标签样式 - 无边框，单行显示 */
        .nav-tabs-custom {
          margin-bottom: 0;
          background: white;
          border: none;
          box-shadow: none;
        }
        
        .nav-tabs-custom .nav-tabs {
          border-bottom: none;
          margin: 0;
          display: flex;
          flex-wrap: nowrap;
          overflow-x: auto;
          white-space: nowrap;
        }
        
        .nav-tabs-custom .nav-tabs > li {
          margin-bottom: 0;
          flex-shrink: 0;
        }
        
        .nav-tabs-custom .nav-tabs > li > a {
          color: #2C3E50;
          border: none;
          border-radius: 0;
          margin-right: 0;
          font-weight: 600;
          padding: 15px 20px;
          font-size: 14px;
          background: transparent;
          border-right: none;
          transition: all 0.3s ease;
          position: relative;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover {
          background: transparent;
          color: #3498DB;
        }
        
        .nav-tabs-custom .nav-tabs > li > a:hover::after {
          content: '';
          position: absolute;
          bottom: 0;
          left: 20%;
          width: 60%;
          height: 3px;
          background: #3498DB;
          border-radius: 2px;
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a {
          background: transparent;
          color: #2C3E50;
          border: none;
          font-weight: 700;
        }
        
        .nav-tabs-custom .nav-tabs > li.active > a::after {
          content: '';
          position: absolute;
          bottom: 0;
          left: 15%;
          width: 70%;
          height: 3px;
          background: #2C3E50;
          border-radius: 2px;
        }
        
        /* 标签内容区域样式 */
        .tab-content {
          padding: 0;
          background: white;
          min-height: calc(100vh - 200px);
          border: none;
          box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        }
        
        /* About页面样式 */
        .about-container {
          max-width: 900px;
          margin: 0 auto;
          padding: 25px;
        }
        
        .author-card {
          background: #f8f9fa;
          border-left: 4px solid #3498DB;
          padding: 20px;
          margin: 20px 0;
          border-radius: 0 8px 8px 0;
        }
        
        .author-title {
          color: #2C3E50;
          font-weight: 600;
          margin-bottom: 10px;
          font-size: 1.1rem;
        }
        
        .author-info {
          color: #555;
          line-height: 1.6;
        }
        
        .citation-section {
          background: #fff;
          border: 1px solid #e1e1e1;
          border-radius: 8px;
          padding: 25px;
          margin-top: 30px;
        }
        
        .citation-title {
          color: #2C3E50;
          font-weight: 700;
          margin-bottom: 15px;
          border-bottom: 2px solid #3498DB;
          padding-bottom: 8px;
        }
        
        .citation-text {
          background: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
          font-family: 'Courier New', monospace;
          line-height: 1.5;
          border-left: 3px solid #3498DB;
        }
        
        /* 滚动条样式 */
        .nav-tabs-custom .nav-tabs::-webkit-scrollbar {
          height: 4px;
        }
        
        .nav-tabs-custom .nav-tabs::-webkit-scrollbar-track {
          background: #f1f1f1;
        }
        
        .nav-tabs-custom .nav-tabs::-webkit-scrollbar-thumb {
          background: #c1c1c1;
          border-radius: 2px;
        }
        
        .nav-tabs-custom .nav-tabs::-webkit-scrollbar-thumb:hover {
          background: #a8a8a8;
        }
      "))
    ),
    
    # 静态标题区域
    fluidRow(
      class = "main-title-container",
      column(12,
             div(style = "text-align: center; padding: 60px 20px;",
                 h2("🌐 Agent-based Secondary Meta-Analysis Platform (ASAP)", 
                    style = "font-size: 3rem; font-weight: 800; margin-bottom: 20px;"),
                 p("Large Language Model-Driven Automation in Systematic Review and Meta-Analysis", 
                   style = "font-size: 1.4rem; opacity: 0.9; margin: 0;")
             )
      )
    ),
    # 导航标签区域 - 调整后的顺序
    fluidRow(
      class = "nav-container",
      column(12,
             div(class = "nav-tabs-custom",
                 tabsetPanel(
                   id = "main_tabs",
                   type = "tabs",
                   tabPanel("UMAR Search", 
                            icon = icon("search"),
                            externalMetaUI("external_meta_module")),
                   
                   
                   tabPanel("UMAR Department Analysis", 
                            icon = icon("chart-bar"),
                            departmentAnalysisUI("department_analysis_module")),
                   tabPanel("UMAR Network Analysis", 
                            icon = icon("project-diagram"),
                            networkAnalysisUI("network_analysis_module")),
                   tabPanel("UMAR Evidence Map", 
                            icon = icon("globe"),
                            evidenceMapUI("evidence_map_module")),
                   tabPanel("UMAR Meta-Analysis", 
                            icon = icon("database"),
                            keywordSearchUI("keyword_search_module")),
                   tabPanel("AI Agent Meta-Analysis", 
                            icon = icon("robot"),
                            systematicReviewUI("systematic_review_module")),
                   tabPanel("Manu Meta-Analysis", 
                            icon = icon("hand"),
                            ManualMetaAnalysisUI("ManualMetaAnalysis")),
                   
                   tabPanel("Benchmark", 
                            icon = icon("file-medical"),
                            llmBenchmarkingUI("llm_benchmarking_module")),
                   tabPanel("About", 
                            icon = icon("info-circle"),
                            aboutUI("about_module"))
                 )
             )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  
  # 初始化各个模块
  externalMetaServer("external_meta_module", meta_db = meta_db)#01 展示数据库
  departmentAnalysisServer("department_analysis_module", umar_data = umar_data_clean)#02Department
  networkAnalysisServer("network_analysis_module", meta_db = meta_db_network)#03 Network
  
  evidenceMapServer("evidence_map_module", meta_db = meta_db_example)#04 map
  keywordSearchServer("keyword_search_module", meta_db = umar_data_clean)#05 UMAR forest
  systematicReviewServer("systematic_review_module", meta_db = meta_db)# 06 Agent
  ManualMetaAnalysisServer("ManualMetaAnalysis")# 06 Agent
  
  # 初始化LLM Benchmarking模块
  llmBenchmarkingServer("llm_benchmarking_module")# 07 llm benchmark
  
  # 初始化About模块
  aboutServer("about_module") # 08 author info
}

# 运行应用
shinyApp(ui, server)


#rsconnect::setAccountInfo(name='chatgptmodel', token='C4D0E4CB00DE9399D6D00806DB22BDB8', secret='ml00NRWn648kzZfC5XLN3Bzf9TZrTkMshb4tCmO/')
