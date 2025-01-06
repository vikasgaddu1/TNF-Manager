
library(shiny)
library(shinydashboard)
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
library(dashboardthemes)
library(bslib)


# Create a pool of connections to the database using RSQLite
dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# Get list of tables in the database
tables <- dbListTables(dbPoolCon)

# Call createTables.R to create the tables and triggers
createTables(dbPoolCon)


ui <- dashboardPage(
  dashboardHeader(title = "Geron App v0.2"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("CRUD Menu", tabName = "crud_menu", icon = icon("th"), badgeLabel = "Admin", badgeColor = "red"),
      menuItem(
        "Reporting Effort Reports",
        tabName = "re_reports",
        icon = icon("code")
      ),
      menuItem(
        "Programming Tracker",
        tabName = "tracker",
        icon = icon("tasks")
      ),
      menuItem(
        "FAQ",
        tabName = "faq",
        icon = icon("question")
      )
      # Add more menu items for other tables
    )
  ),
  dashboardBody( 
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
            tabPanel("Reporting Effort", icon = icon("chart-line"), genericCRUDUI("reporting_effort", "Reporting Effort"))
          )
        )
      ),
      tabItem(
        "re_reports",
        associateTask_RE_UI("re_reports", "Reporting Effort Reports")
      ),
      tabItem("tracker", programmingTrackerUI("tracker")),
      tabItem("faq", FAQModuleUI("faq"))
      # Add more tabItems for other tables
    )
  ),
  skin = "black"
)

server <- function(input, output, session) {
  # Move server logic for CRUD operations
  singleColumnCRUDServer("categories", dbPoolCon, "categories", "category_name")
  subCategoriesCRUDServer("sub_categories", dbPoolCon, tabs_input = reactive(input$crud_tabs))
  singleColumnCRUDServer("titles", dbPoolCon, "titles", "title_text")
  singleColumnCRUDServer("footnotes", dbPoolCon, "footnotes", "footnote_text")
  singleColumnCRUDServer("populations", dbPoolCon, "populations", "population_text")
  datasetsCRUDServer("datasets", dbPoolCon)
  reportCRUDServer("reports", dbPoolCon, tabs_input = reactive(input$tabs))
  reportingEffortsServer("reporting_effort", dbPoolCon)
  usersServer("users", dbPoolCon)
  associateTask_RE_Server("re_reports", dbPoolCon, tabs_input = reactive(input$tabs))
  programmingTrackerServer("tracker", dbPoolCon, tabs_input = reactive(input$tabs))
  
  # Disconnect pool when session ends
  onStop(function() {
    poolClose(dbPoolCon)
  })
}

shinyApp(ui, server)
