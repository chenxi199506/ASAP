# About模块UI
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "about-container",
        h2("About UMAR", style = "color: #2C3E50; margin-bottom: 25px;"),
        
        # 第一作者信息
        div(class = "author-card",
            div(class = "author-title", "First Author"),
            div(class = "author-info",
                "Xi Chen",
                tags$br(),
                "Department of Adult Joint Reconstructive Surgery,",
                tags$br(), 
                "Beijing Jishuitan Hospital, Capital Medical University,",
                tags$br(),
                "31 East Xinjiekou Street, Beijing, 100035, China"
            )
        ),
        
        # 通讯作者信息
        div(class = "author-card",
            div(class = "author-title", "Corresponding Author"),
            div(class = "author-info",
                "Yixin Zhou",
                tags$br(),
                "Department of Adult Joint Reconstructive Surgery,",
                tags$br(), 
                "Beijing Jishuitan Hospital, Capital Medical University,",
                tags$br(),
                "31 East Xinjiekou Street, Beijing, 100035, China"
            )
        ),
        
        # 引用文献部分
        div(class = "citation-section",
            h3(class = "citation-title", "How to Cite"),
            div(class = "citation-text",
                "Chen, X., & Zhou, Y. (2024). UMAR: Unified Meta-Analysis Repository. ",
                tags$br(),
                "Beijing Jishuitan Hospital, Capital Medical University.",
                tags$br(),
                "Available at: [URL will be provided upon publication]"
            )
        ),
        
        # 联系信息
        div(class = "citation-section",
            h3(class = "citation-title", "Contact Information"),
            div(style = "line-height: 1.8;",
                p(strong("Email: "), "For correspondence: zhouyixin@jst-hosp.com.cn"),
                p(strong("Institution: "), "Beijing Jishuitan Hospital, Capital Medical University"),
                p(strong("Address: "), "31 East Xinjiekou Street, Beijing, 100035, China")
            )
        )
    )
  )
}

# About模块Server
aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # About页面不需要特殊的服务器逻辑
  })
}