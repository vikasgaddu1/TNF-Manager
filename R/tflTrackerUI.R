tflTrackerUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabBox(
      id = ns("tracker_tabs"),
      width = 12,
      tabPanel(
        "TFL",
        icon = icon("table"),
        fluidRow(
          column(
            width = 4,
            wellPanel(
              class = "p-3",
              div(
                class = "d-flex justify-content-start",
                actionButton(
                  ns("edit_button"),
                  "Edit Selected Row",
                  class = "btn btn-warning btn-sm",
                  icon = icon("edit")
                ),
               
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
            div(
              class = "table-responsive", # Wrapper for responsive table
              DT::DTOutput(ns("tracker_table"))
            )
          )
        )
      )
    )
  )
}
