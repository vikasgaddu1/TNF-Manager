reportProgrammingTrackerUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Report Programming Tracker"),
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("reporting_effort"),
          label = "Select Reporting Effort:",
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 8,
        fluidRow(
          box(
            title = "Status Distribution",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            plotOutput(ns("status_pie_chart")),
            collapsed = TRUE
          ),
          box(
            title = "Due Dates by Month",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            plotOutput(ns("due_date_bar_chart")),
            collapsed = TRUE
          )
        )
      )
    ),
    hr(),
    DT::dataTableOutput(ns("tracker_table"), width = "100%", height = "auto")
  )
}
