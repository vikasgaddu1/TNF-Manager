


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
    "DT",
    "ggplot2",
    "tidyverse",
    "RSQLite",
    "pool",
    "DBI",
    "rhandsontable",
    "openxlsx",
    "plotly",
    "gtsummary",
    "gt"
  )
)


# Create a pool of connections to the database using RSQLite
dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# Get list of tables in the database
tables <- dbListTables(dbPoolCon)

# Call createTables.R to create the tables and triggers
createTables(dbPoolCon)


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("CRUD Menu", tabName = "crud_menu", icon = icon("th"), badgeLabel = "Admin", badgeColor = "red"),
      menuItem("Reports", tabName = "reports", icon = icon("book")),
      menuItem(
        "Reporting Effort",
        tabName = "reporting_effort",
        icon = icon("chart-line")
      ),
      menuItem(
        "Reporting Effort Reports",
        tabName = "re_reports",
        icon = icon("code")
      ),
      menuItem(
        "Programming Tracker",
        tabName = "tracker",
        icon = icon("tasks")
      )
      # Add more menu items for other tables
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "crud_menu",
        fluidRow(
          tabBox(
            id = "crud_tabs",
            width = 12,
            tabPanel("Categories", genericCRUDUI("categories", "Categories")),
            tabPanel("Sub Categories", genericCRUDUI("sub_categories", "Sub-Categories")),
            tabPanel("Titles", genericCRUDUI("titles", "Titles")),
            tabPanel("Footnotes", genericCRUDUI("footnotes", "Footnotes")),
            tabPanel("Populations", genericCRUDUI("populations", "Populations")),
            tabPanel("users", genericCRUDUI("users", "Users")),
          )
        )
      ),
      tabItem("reports", genericCRUDUI("reports", "Reports")),
      tabItem(
        "reporting_effort",
        genericCRUDUI("reporting_effort", "Reporting Effort")
      ),
      tabItem(
        "re_reports",
        reportingEffortReportsUI("re_reports", "Reporting Effort Reports")
      ),
      tabItem("tracker", reportProgrammingTrackerUI("tracker"))
      # Add more tabItems for other tables
    )
  ),
  skin = "black"
)

server <- function(input, output, session) {
  # Move server logic for CRUD operations
  singleColumnCRUDServer("categories", dbPoolCon, "categories", "category_name")
  subCategoriesCRUDServer("sub_categories", dbPoolCon, tabs_input = reactive(input$tabs))
  singleColumnCRUDServer("titles", dbPoolCon, "titles", "title_text")
  singleColumnCRUDServer("footnotes", dbPoolCon, "footnotes", "footnote_text")
  singleColumnCRUDServer("populations", dbPoolCon, "populations", "population_text")
  reportCRUDServer("reports", dbPoolCon, tabs_input = reactive(input$tabs))
  reportingEffortsServer("reporting_effort", dbPoolCon)
  usersServer("users", dbPoolCon)
  reportingEffortReportsServer("re_reports", dbPoolCon, tabs_input = reactive(input$tabs))
  reportProgrammingTrackerServer("tracker", dbPoolCon, tabs_input = reactive(input$tabs))
  
  # Disconnect pool when session ends
  onStop(function() {
    poolClose(dbPoolCon)
  })
}

shinyApp(ui, server)
