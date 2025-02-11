tflTrackerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, programmingEffortUI(ns("programming_effort")))
    ),
    fluidRow(
      column(
        width = 12, # Full width for smaller screens
        class = "col-md-6", # Half-width for medium or larger screens
        wellPanel(
          class = "p-3",
          div(
            class = "d-flex flex-wrap justify-content-start", # Added flex-wrap for better responsiveness
            assignTaskUI(ns("assign_task")),
            editCustomFootnoteUI(ns("edit_fn_button")),
            editPopulationUI(ns("edit_population")),
            commentsUI(ns("comments")),
            downloadButton(
              ns("download_tracker"),
              label = "Download Tracker Data as Excel",
              class = "btn btn-primary btn-sm"
            )
          )
        )
      ),
      column(
        width = 12,
        class = "col-md-6",
        box(
          title = "Column Visibility",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxGroupInput(
            ns("column_selection"),
            label = "Select Columns to Hide:",
            choices = NULL,
            selected = "id"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          class = "p-3",
          DT::DTOutput(ns("tracker_table"))
        )
      )
    )
  )
}
