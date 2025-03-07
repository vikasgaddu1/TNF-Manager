mod_associate_task_re_ui <- function(id, title) {
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
          mod_at_tfl_ui(id = ns("at_tfl")),
          icon = icon("table")
        ),
        tabPanel(
          "SDTM",
          mod_at_dataset_ui(id = ns("at_sdtm")),
          icon = icon("table")
        ),
        tabPanel(
          "ADaM",
          mod_at_dataset_ui(id = ns("at_adam")),
          icon = icon("table")
        )
      )
    )
  )
}
