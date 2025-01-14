# Portfolio Shiny App
library(shiny)
library(bslib)

#source("slick.R")

# Define theme for the app
my_theme <- bs_theme(
  bootswatch = "journal",
  secondary = "#ff7f0e",
  success = "#28a745",
  info = "#17a2b8",
  light = "#f8f9fa",
  dark = "#343a40",
  base_font = "Roboto",
  heading_font = "Lato",
  font_scale = 1
)



projects <- data.frame(
  name = c("Simulating Optimal Roulette Strategies",
           "Sports QA Automation Application",
           "Milwaukee Housing Similarity index",
           "Amazon Review NLP Sentiment Analysis",
           "Australian Rainfall Categorical Prediction Analysis",
           "Employee Retention Rates with Support Vector Classifiers",
           "Boston Marathon Hadoop, HBase, and Phoenix Analysis",
           "Sports e-Commerce Database / Web Application",
           "Music's Impact During the Lockdown"),
  
  category = I(list(
    c("Statistics", "Visualization"), 
    c("Consulting", "Visualization", "Data Engineering", "Database"), 
    c("Machine Learning", "Consulting", "Statistics"), 
    c("Consulting", "Machine Learning"), 
    c("Machine Learning", "Statistics"),
    c("Machine Learning", "Data Engineering","Statistics","Consulting"),
    c("Database", "Visualization","Consulting","Data Engineering"),
    c("Consulting", "Visualization","Data Engineering","Database"),
    c("Statistics"))),
  
  link = c("https://github.com/ajtparas/Monte-Carlo-Simulation",
           "https://github.com/ajtparas/qa-automation",
           "https://github.com/ajtparas/Milwaukee-Property-Recommendations",
           "https://github.com/ajtparas/Sentiment-Analysis-Amazon_Reviews",
           "https://github.com/ajtparas/Stat388_Project",
           "",
           "",
           "https://github.com/ajtparas/COMP353-eCommerce",
           ""     
  ))


# Define UI
ui <-
  # slickROutput("box_scores",width='100%',height='200px'),
  
  navbarPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", 
                href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&family=Lato:wght@400;700&display=swap")
    ),
    
    # Custom CSS to set fonts for body and headers
    tags$style(HTML("
    body {
      font-family: 'Lato', sans-serif;
    }
    h1, h2, h3, h4, h5, h6 {
      font-family: 'Roboto', sans-serif;
    }
  ")),
    
    theme = my_theme,
    
    tags$style(HTML("
    
    .nav-tabs {
      display: flex;
      justify-content: center; /* Center the tabs horizontally */
    }
     .navbar-nav li a {
      font-family: 'Roboto', sans-serif; /* Set font for navbar menu items */
      font-size: 15px; /* Set font size for navbar menu items */
      margin: 5 5px;
    }
    .nav-tabs > li {
      margin: 0 10px; /* Add spacing between tabs */
    }
    .nav-tabs > li > a {
      text-align: center; /* Center-align text within each tab */
      font-weight: bold;  /* Optional: Make text bold */
    }
    .card-container {
      text-align: center;
      margin: 10px;
      display: inline-block;
      width: 20%;
    }
    .card {
      padding: 45px;
      border: 1px solid #ddd;
      border-radius: 5px;
      transition: box-shadow 0.2s ease;
    }
    .card:hover {
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
    }
    .card-title {
      margin-top: 10px;
      font-weight: bold;
      font-size: 13px;
    }
  ")),
    
  title = div(
      img(src = "logo_transparent.png", height = "50px", 
          style = "vertical-align: middle;"),
      
      style = "display: inline-block; margin-left: 10px;"),
  
  # Home Tab
  tabPanel(
    title = "Home",
    sidebarLayout(
    
    sidebarPanel(
      "Top Headlines"
    ),
    
    mainPanel(
    fluidPage(
    fluidRow(
      column(12, h1("Aldrich Paras", style = "text-align:center;")),
      column(12, p("A showcase of my recent work, available to view on Github", 
                   style = "text-align:center;")),
      column(12,
             div(class = "card",
                # Tabset Panel for categories
                 tabsetPanel(id = "category_tabs",
                             tabPanel("All", value = "all"),
                             tabPanel("Consulting", value = "consulting"),
                             tabPanel("Machine Learning", value = "ml"),
                             tabPanel("Database", value = "db"),
                             tabPanel("Visualization", value = "viz"),
                             tabPanel("Statistics", value = "stats"),
                             tabPanel("Data Engineering", value = "de"),
                 ),
                 # Placeholder for dynamic content
                 uiOutput("project_grid"))),
    
      column(12, hr()),
      column(12, h3("Key Skills")),
      column(12,
        tags$ul(
          tags$li("R, Python, SQL"),
          tags$li("Machine Learning & Predictive Modeling"),
          tags$li("Data Visualization & Dashboards"),
          tags$li("Statistical Analysis")
        ))
      )
    )
  ))),
  
  # About Me Tab
  tabPanel(
    title = "About",
    fluidRow()
  ),
  
  tabPanel(
    title = "Services",
  ),
  
  navbarMenu("Extras",
    tabPanel("Hobbies"),
    tabPanel("Resume")
  ),
  # Contact Tab
  tabPanel(
    title = "Contact",
    fluidRow(
      column(12, h1("Get in Touch")),
      column(
        12,
        tags$ul(
          tags$li(tags$a(href = "https://github.com/yourgithub", "GitHub")),
          tags$li(tags$a(href = "https://www.linkedin.com/in/yourlinkedin", "LinkedIn")),
          tags$li(tags$a(href = "mailto:your.email@example.com", "Email"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render the project grid based on selected tab and filters
  output$project_grid <- renderUI({
    # Filter projects based on selected tab and other attributes
    selected_tab <- input$category_tabs
    
    filtered_projects <- switch(selected_tab,
        "all" = projects,
        "consulting" = projects[sapply(projects$category, function(x) "Consulting" %in% x), ],
        "ml" = projects[sapply(projects$category, function(x) "Machine Learning" %in% x), ],
        "viz" = projects[sapply(projects$category, function(x) "Visualization" %in% x), ],
        "db" = projects[sapply(projects$category, function(x) "Database" %in% x), ],
        "stats" = projects[sapply(projects$category, function(x) "Statistics" %in% x), ],
        "de" = projects[sapply(projects$category, function(x) "Data Engineering" %in% x), ]
    )  
  
    # grid of cards
    project_cards <- lapply(1:nrow(filtered_projects), function(i) {
      project <- filtered_projects[i, ]
      div(class = "card-container",
          a(href = project$link, target = "_blank", 
            div(class = "card",
                p(project$description)
            )),
          div(class = "card-title", project$name) 
      )
    })
    
    # Return the grid of cards
    do.call(tagList, project_cards)
  })

  
  # Placeholder for resume download (update with actual file path)
  output$download_resume <- downloadHandler(
    filename = function() {
      "My_Resume.pdf"
    },
    content = function(file) {
      # Replace with the actual path to your resume file
      file.copy("path/to/your/resume.pdf", file)
    }
  )
  
}

# Run the Shiny app
shinyApp(ui, server)


