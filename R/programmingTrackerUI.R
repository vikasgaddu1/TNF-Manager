programmingTrackerUI <- function(id, title = "Programming Tracker") {
  ns <- NS(id)
  
  fluidPage(
    tags$style(
      HTML("
        .datepicker, .ui-datepicker { z-index: 99999 !important; }
        .table-spacing { margin-top: 20px; }
      ")
    ),
    h2(title, class = "mb-4"),
    
    fluidRow(
      column(
        width = 4, 
        wellPanel(
          selectInput(
            ns("reporting_effort"),
            label = "Select Reporting Effort:",
            choices = NULL,
            selected = NULL,
            width = "100%"
          )
        )
      )
    ),
    
    tabBox(
      id = ns("programming_tracker_tabs"),
      width = 12,
      tabPanel(
        "TFL Tracker",
        icon = icon("table"),
        trackerUI(ns("tfl_tracker"))
      ),
      tabPanel(
        "SDTM Tracker",
        icon = icon("table"),
        trackerUI(ns("sdtm_tracker"))
      ),
      tabPanel(
        "ADaM Tracker",
        icon = icon("table"),
        trackerUI(ns("adam_tracker"))
      )
    )
  )
}
