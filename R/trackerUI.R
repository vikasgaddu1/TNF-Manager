trackerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, programmingEffortUI(ns("programming_effort")))
    ),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          class = "p-3",
          div(
            class = "d-flex justify-content-start",
            assignTaskUI(ns("assign_task")),
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
        width = 6,
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
