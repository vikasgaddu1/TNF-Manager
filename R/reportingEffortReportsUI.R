reportingEffortReportsUI <- function(id, title) {
  ns <- NS(id)
  fluidPage(
    h2(title, class = "mb-4"),
    div(
      class = "mb-4",
      selectInput(
        ns("reporting_effort"),
        label = "Select Reporting Effort:",
        choices = NULL, # Will be populated dynamically
        selected = NULL,
        width = "100%"
      ),
      actionButton(
        ns("save_selection"),
        "Save Selection",
        class = "btn btn-success",
        icon = icon("save")
      )
    ),
    br(),
    textInput(
      ns("search"),
      label = "Search:",
      placeholder = "Type to search...",
      width = "400px"
    ),
    rHandsontableOutput(ns("reports_table")) 

  )
}
