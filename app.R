

# Source utility functions
source("R/createTables.R")
source("R/functions.R")

# load the libraries related to shiny
load_libraries(
  c(
    "shiny",
    "shinydashboard",
    "shinyjs",
    "shinyWidgets",
    "shinyalert",
    "shinybusy",
    "shinymaterial",
    "shinythemes",
    "shinyBS",
    "DT"
  )
)

library(rhandsontable)

# load libraries related to database
load_libraries(c("tidyverse", "RSQLite", "pool", "DBI"))

# Create a pool of connections to the database using RSQLite
dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# Get list of tables in the database
tables <- dbListTables(dbPoolCon)

# Call createTables.R to create the tables and triggers
createTables(dbPoolCon)


ui <- dashboardPage(
  dashboardHeader(title = "TNF Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Categories", tabName = "categories", icon = icon("list")),
      menuItem("Sub Categories", tabName = "sub_categories", icon = icon("list-alt")),
      menuItem("Titles", tabName = "titles", icon = icon("heading")),
      menuItem("Footnotes", tabName = "footnotes", icon = icon("asterisk")),
      menuItem("Populations", tabName = "populations", icon = icon("users")),
      menuItem("Reports", tabName = "reports", icon = icon("book")),
      menuItem("Reporting Effort", tabName = "reporting_effort", icon = icon("chart-line")),
      menuItem("Reporting Effort Reports", tabName = "re_reports", icon = icon("code")),
      menuItem("Users", tabName = "users", icon = icon("user")),
      menuItem("Programming Tracker", tabName = "tracker", icon = icon("code"))
      # Add more menu items for other tables
    )
  ),
  dashboardBody(tabItems(
    tabItem("categories", genericCRUDUI("categories", "Categories")),
    tabItem("sub_categories", genericCRUDUI("sub_categories","Sub-Categories")),
    tabItem("titles", genericCRUDUI("titles", "Titles")),
    tabItem("footnotes", genericCRUDUI("footnotes", "Footnotes")),
    tabItem("populations", genericCRUDUI("populations", "Populations")),
    tabItem("reports", genericCRUDUI("reports", "Reports")),
    tabItem("reporting_effort", genericCRUDUI("reporting_effort", "Reporting Effort")),
    tabItem("users", genericCRUDUI("users", "Users")),
    tabItem("re_reports", reportingEffortReportsUI("re_reports", "Reporting Effort Reports")),
    tabItem("tracker", reportProgrammingTrackerUI("tracker"))
    # Add more tabItems for other tables
  )),
  skin = "black"
)

server <- function(input, output, session) {
  #categoriesCRUDServer("categories", dbPoolCon)
  singleColumnCRUDServer("categories", dbPoolCon, "categories", "category_name")
  subCategoriesCRUDServer("sub_categories", dbPoolCon, tabs_input = reactive(input$tabs))
  singleColumnCRUDServer("titles", dbPoolCon, "titles", "title_text")
  singleColumnCRUDServer("footnotes", dbPoolCon, "footnotes", "footnote_text")
  singleColumnCRUDServer("populations", dbPoolCon, "populations", "population_text")
  reportCRUDServer("reports", dbPoolCon, tabs_input = reactive(input$tabs))
  reportingEffortsServer("reporting_effort", dbPoolCon)
  usersServer("users", dbPoolCon)
  reportingEffortReportsServer("re_reports", dbPoolCon,tabs_input = reactive(input$tabs))
  reportProgrammingTrackerServer("tracker", dbPoolCon,tabs_input = reactive(input$tabs))
  
  # Disconnect pool when session ends
  onStop(function() {
    poolClose(dbPoolCon)
  })
  
}

shinyApp(ui, server)
