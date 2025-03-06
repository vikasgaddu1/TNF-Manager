library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(shinymaterial)
library(shinythemes)
library(shinyBS)
library(DT)
library(ggplot2)
library(tidyverse)
library(RSQLite)
library(pool)
library(DBI)
library(rhandsontable)
library(openxlsx)
library(plotly)
library(gtsummary)
library(gt)
library(bslib)
library(jsonlite)
library(reactlog)
library(lubridate)

# Create a pool of connections to the database using RSQLite
dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# Get list of tables in the database
tables <- dbListTables(dbPoolCon)

# Call createTables.R to create the tables and triggers
createTables(dbPoolCon)


ui <- dashboardPage(
  dashboardHeader(title = tags$img(src = "image/small_logo.png", height = "50px", style = "margin: -15px 0;"),
    tags$li(
      class = "dropdown",
      textOutput("headerWelcomeMessage")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Management", tabName = "crud_menu", icon = icon("th"), badgeLabel = "Admin", badgeColor = "red"),
      menuItem(
        "Associate Task to RE",
        tabName = "re_reports",
        icon = icon("link")
      ),
      menuItem(
        "Programming Tracker",
        tabName = "tracker",
        icon = icon("tasks")
      ),
      menuItem(
        "Project Timeline",
        tabName = "milestone",
        icon = icon("calendar-days")
      ),
      menuItem(
        "Decision Log",
        tabName = "decision_log",
        icon = icon("book")
      ),
      menuItem(
        "Search",
        tabName = "search",
        icon = icon("search")
      ),  
      menuItem(
        "FAQ",
        tabName = "faq",
        icon = icon("question")
      )

      # Add more menu items for other tables
    ),
    collapsed = TRUE
  ),
  dashboardBody( 
    shinyFeedback::useShinyFeedback(), 
    tags$head(
      # Add the favicon here
      tags$link(rel = "icon", type = "image/x-icon", href = "image/favicon.ico"),
      # Add custom CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "home",
        landingPageUI("landing")
      ),
      tabItem(
        tabName = "crud_menu",
        fluidRow(
          tabBox(
            id = "crud_tabs",
            width = 12,
            tabPanel("Categories", icon = icon("folder"), genericCRUDUI("categories", "Categories")),
            tabPanel("Sub Categories", icon = icon("folder-tree"), genericCRUDUI("sub_categories", "Sub-Categories")),
            tabPanel("Titles", icon = icon("heading"), genericCRUDUI("titles", "Titles")),
            tabPanel("Footnotes", icon = icon("note-sticky"), genericCRUDUI("footnotes", "Footnotes")),
            tabPanel("Populations", icon = icon("users"), genericCRUDUI("populations", "Populations")),
            tabPanel("Users", icon = icon("user"), genericCRUDUI("users", "Users")),
            tabPanel("Datasets", icon = icon("database"), genericCRUDUI("datasets", "Datasets")),
            tabPanel("Reports", icon = icon("file-lines"), genericCRUDUI("reports", "Reports")),
            tabPanel("Reporting Effort", icon = icon("chart-line"), genericCRUDUI("reporting_effort", "Reporting Effort")),
            tabPanel("Upload", icon = icon("upload"), uploadUI("upload")),
            tabPanel("View DB", icon = icon("table"), tableSelectorUI("tableSelector"))
          )
        )
      ),
      tabItem(
        "re_reports",
        associateTask_RE_UI("re_reports", "Associate Task to Reporting Effort")
      ),
      tabItem("tracker", programmingTrackerUI("tracker")),
      tabItem("milestone", milestoneUI("milestone")),
      tabItem("decision_log", decisionLogUI("decision_log")),
      tabItem("search", searchUI("search")),
      tabItem("faq", FAQModuleUI("faq"))
      # Add more tabItems for other tables
    )
  ),
  skin = "black-light"
)

server <- function(input, output, session) {

  # Function to get current user
  get_current_user <- function(session) {
    if (is.null(session$user)) {
      return("vgaddu")
    }
    return(session$user)
  }

  # Function to get current user's role
  get_current_user_role <- function(session) {
    current_user <- get_current_user(session)
    
    # Get user role from users table
    user_role <- dbGetQuery(
      dbPoolCon,
      "SELECT role FROM users WHERE username = ?",
      params = list(current_user)
    )$role
    
    return(user_role)
  }

  # Reactive expressions for current user and role
  current_user <- reactive({
    get_current_user(session)
  })

  current_user_role <- reactive({
    get_current_user_role(session)
  })

  # Header welcome message
  output$headerWelcomeMessage <- renderText({
    paste0("Welcome, ", current_user(), " (", current_user_role(), ")")
  })

  tracker_data <- reactiveVal()
  tables <- dbListTables(dbPoolCon)
  tables <- tables[tables != "sqlite_sequence"]
  
  # Initialize the reactive tables data
  tables_data <- pollAllTables(dbPoolCon, tables)
  

  
  # Use reactive values in modules
  singleColumnCRUDServer("categories", dbPoolCon, "categories", "category_name", tables_data = tables_data)
  subCategoriesCRUDServer("sub_categories", dbPoolCon, tabs_input = reactive(input$crud_tabs), tables_data = tables_data)
  singleColumnCRUDServer("titles", dbPoolCon, "titles", "title_text", tables_data = tables_data)
  singleColumnCRUDServer("footnotes", dbPoolCon, "footnotes", "footnote_text", tables_data = tables_data)
  singleColumnCRUDServer("populations", dbPoolCon, "populations", "population_text", tables_data = tables_data)
  datasetsCRUDServer("datasets", dbPoolCon, tables_data = tables_data)
  reportCRUDServer("reports", dbPoolCon, tables_data = tables_data)
  reportingEffortsServer("reporting_effort", dbPoolCon, tables_data = tables_data)
  usersServer("users", dbPoolCon, tables_data = tables_data)
  associateTask_RE_Server("re_reports", dbPoolCon, tables_data = tables_data)
  uploadServer("upload", upload_directory = "data/excel_import")
  search_data <- programmingTrackerServer("tracker", dbPoolCon, tables_data = tables_data)
  tracker_data <- tableSelectorServer("tableSelector", tables_data = tables_data, table_names = tables)
    # Landing page
  landingPageServer("landing", dbPoolCon, tracker_data,tables_data, session)
  taskSummaryServer("taskSummary", tracker_data, current_user)
  searchServer("search", search_data[[1]], search_data[[2]], search_data[[3]])
  milestoneServer("milestone", dbPoolCon, tables_data = tables_data)
  decisionLogServer("decision_log", dbPoolCon, tables_data = tables_data)
  
  # Set the initial tab to Home
  observe({
    updateTabItems(session, "tabs", "home")
  })
  
  # Disconnect pool when session ends
  onStop(function() {
    poolClose(dbPoolCon)
  })
}

shinyApp(ui, server)
