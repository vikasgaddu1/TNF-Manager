mod_tracker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, mod_programming_effort_ui(ns("programming_effort")))
    ),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          class = "p-3",
          div(
            class = "d-flex justify-content-start",
            mod_assign_task_ui(ns("assign_task")),
            mod_comments_ui(ns("comments")),
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
