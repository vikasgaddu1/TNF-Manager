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

# Call mod_create_tables.r to create the tables and triggers
mod_create_tables(dbPoolCon)


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
        mod_landing_page_ui("landing")
      ),
      tabItem(
        tabName = "crud_menu",
        fluidRow(
          tabBox(
            id = "crud_tabs",
            width = 12,
            tabPanel("Categories", icon = icon("folder"), mod_generic_crud_ui("categories", "Categories")),
            tabPanel("Sub Categories", icon = icon("folder-tree"), mod_generic_crud_ui("sub_categories", "Sub-Categories")),
            tabPanel("Titles", icon = icon("heading"), mod_generic_crud_ui("titles", "Titles")),
            tabPanel("Footnotes", icon = icon("note-sticky"), mod_generic_crud_ui("footnotes", "Footnotes")),
            tabPanel("Populations", icon = icon("users"), mod_generic_crud_ui("populations", "Populations")),
            tabPanel("Users", icon = icon("user"), mod_generic_crud_ui("users", "Users")),
            tabPanel("Datasets", icon = icon("database"), mod_generic_crud_ui("datasets", "Datasets")),
            tabPanel("Reports", icon = icon("file-lines"), mod_generic_crud_ui("reports", "Reports")),
            tabPanel("Reporting Effort", icon = icon("chart-line"), mod_generic_crud_ui("reporting_effort", "Reporting Effort")),
            tabPanel("Upload", icon = icon("upload"), mod_upload_ui("upload")),
            tabPanel("View DB", icon = icon("table"), mod_table_selector_ui("tableSelector"))
          )
        )
      ),
      tabItem(
        "re_reports",
        mod_associate_task_re_ui("re_reports", "Associate Task to Reporting Effort")
      ),
      tabItem(
        tabName = "tracker",
        mod_programming_tracker_ui("tracker")
      ),
      tabItem(
        tabName = "milestone",
        mod_milestone_ui("milestone")
      ),
      tabItem(
        tabName = "decision_log",
        mod_decision_log_ui("decision_log")
      ),
      tabItem(
        tabName = "search",
        mod_search_ui("search")
      ),
      tabItem(
        tabName = "faq",
        mod_faq_ui("faq")
      )
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
  mod_single_column_crud_server("categories", dbPoolCon, "categories", "category_name", tables_data = tables_data)
  mod_sub_categories_crud_server("sub_categories", dbPoolCon, tabs_input = reactive(input$crud_tabs), tables_data = tables_data)
  mod_single_column_crud_server("titles", dbPoolCon, "titles", "title_text", tables_data = tables_data)
  mod_single_column_crud_server("footnotes", dbPoolCon, "footnotes", "footnote_text", tables_data = tables_data)
  mod_single_column_crud_server("populations", dbPoolCon, "populations", "population_text", tables_data = tables_data)
  mod_datasets_crud_server("datasets", dbPoolCon, tables_data = tables_data)
  mod_report_crud_server("reports", dbPoolCon, tables_data = tables_data)
  mod_reporting_efforts_server("reporting_effort", dbPoolCon, tables_data = tables_data)
  mod_users_server("users", dbPoolCon, tables_data = tables_data)
  mod_associate_task_re_server("re_reports", dbPoolCon, tables_data = tables_data)
  mod_upload_server("upload", upload_directory = "data/excel_import")
  search_data <- mod_programming_tracker_server("tracker", dbPoolCon, tables_data = tables_data)
  tracker_data <- mod_table_selector_server("tableSelector", tables_data = tables_data, table_names = tables)
  # Landing page
  mod_landing_page_server("landing", dbPoolCon, tracker_data, tables_data, session)
  mod_task_summary_server("taskSummary", tracker_data, current_user)
  mod_search_server("search", search_data[[1]], search_data[[2]], search_data[[3]])
  mod_milestone_server("milestone", dbPoolCon, tables_data = tables_data)
  mod_decision_log_server("decision_log", dbPoolCon, tables_data = tables_data)
  
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
