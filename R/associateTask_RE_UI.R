associateTask_RE_UI <- function(id, title) {
  ns <- NS(id)
  
  fluidPage(
    # Page title
    h2(title, class = "mb-4"),
    
    # Filter section with reporting effort selection
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
    
    # Main tabBox containing both TFL Tracker and Dataset Tracker
    fluidRow(
      tabBox(
        id = ns("main_tab_box"),
        width = 12,
        tabPanel(
          "TFL",
          at_tfl_UI(id = ns("at_tfl")),
          icon = icon("table")
        ),
        tabPanel(
          "SDTM",
          at_dataset_UI(id = ns("at_sdtm"),"SDTM"),
          icon = icon("table")
        ),
        tabPanel(
          "ADaM",
          at_dataset_UI(id = ns("at_adam"),"ADaM"),
          icon = icon("table")
        )
      )
    )
  )
}
