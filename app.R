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
  dashboardHeader(title = "PT v0.4",  

        tags$li(
          class = "dropdown",
          taskSummaryUI("taskSummary")
        ),
        tags$li(
          class = "dropdown margin-top-10",
          selectizeInput(
            "user_select",
            label = NULL,
            choices = NULL,
            width = "200px"
          )
        )
      ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("CRUD Menu", tabName = "crud_menu", icon = icon("th"), badgeLabel = "Admin", badgeColor = "red"),
      menuItem(
        "Associate Task to RE",
        tabName = "re_reports",
        icon = icon("code")
      ),
      menuItem(
        "Programming Tracker",
        tabName = "tracker",
        icon = icon("tasks")
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
      tags$link(rel = "icon", type = "image/x-icon", href = "image/favicon.ico")
    ),
    tabItems(
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
            tabPanel("View DB", icon = icon("table"), tableSelectorUI("tableSelector"))
          )
        )
      ),
      tabItem(
        "re_reports",
        associateTask_RE_UI("re_reports", "Associate Task to Reporting Effort")
      ),
      tabItem("tracker", programmingTrackerUI("tracker")),
      tabItem("search", searchUI("search")),
      tabItem("faq", FAQModuleUI("faq"))
      # Add more tabItems for other tables
    )
  ),
  freshTheme = TRUE,
  skin = "black-light"
)

server <- function(input, output, session) {

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
  search_data <- programmingTrackerServer("tracker", dbPoolCon, tables_data = tables_data)
  tracker_data <- tableSelectorServer("tableSelector", tables_data = tables_data, table_names = tables)
  taskSummaryServer("taskSummary", tracker_data, reactive(input$user_select))
  searchServer("search", search_data[[1]], search_data[[2]], search_data[[3]])
  userchoices <- reactive({
    tables_data$users() %>% dplyr::filter(id != 1) %>% dplyr::pull(username) %>% unique()
  })
  observe({
    updateSelectizeInput(
      session,
      "user_select",
      choices = userchoices()
    )
  })
  # Disconnect pool when session ends
  onStop(function() {
    poolClose(dbPoolCon)
  })
}

shinyApp(ui, server)
