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
            width = 3, 
            div(
              class = "action-buttons mb-3",
              actionButton(
                ns("edit_button"),
                "Edit Selected Row",
                class = "btn btn-warning btn-block",
                icon = icon("edit")
              )
            )
          ),
          column(
            width = 3, 
            div(
              class = "action-buttons mb-3",
              downloadButton(
                ns("download_tracker"),
                label = "Download Tracker Data as Excel",
                class = "btn btn-primary btn-block"
              )
            )
          ),
          column(
            width = 6,
            box(
              title = "Hide Columns",
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
              class = "table-responsive table-spacing",
              DT::DTOutput(ns("tracker_table"))
            )
          )
        )
      )
    )
  )
}

