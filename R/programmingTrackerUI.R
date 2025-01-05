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
    
    programmingEffortUI(ns("programming_effort")),
    tflTrackerUI(ns("tfl_tracker"))

  )
}