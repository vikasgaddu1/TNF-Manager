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
    
    # Main content
    at_tfl_UI(id = ns("at_tfl"))
  )
}
