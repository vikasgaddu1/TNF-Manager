mod_at_dataset_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        fluidRow(
          column(
            width = 3,
            wellPanel(
              div(
                style = "text-align: center;",
                span(HTML("Choose Specific Tasks: <br>"), style = "color: black; font-weight: bold; margin-right: 10px;"),
                actionButton(
                  ns("select_highlighted"),
                  "Select Highlighted",
                  class = "btn btn-success btn-sm me-2"
                ),
                actionButton(
                  ns("deselect_highlighted"),
                  "Deselect Highlighted",
                  class = "btn btn-danger btn-sm me-2"
                )
              )
            )
          ),
          column(
            width = 3,
            wellPanel(
              div(
                style = "text-align: center;",
                span(HTML("Choose Filtered Task: <br>"), style = "color: black; font-weight: bold; margin-right: 10px;"),
                actionButton(
                  ns("select_all_visible"),
                  "Select all Visible",
                  class = "btn btn-success btn-sm me-2"
                ),
                actionButton(
                  ns("deselect_all_visible"),
                  "Deselect all Visible",
                  class = "btn btn-danger btn-sm me-2"
                )
              )
            )
          ),
          column(
            width = 3,
            wellPanel(
              div(
                style = "text-align: center;",
                mod_at_copy_ui(ns("copy_reporting_effort"))
              )
            )
          ),
          column(
            width = 3,
            wellPanel(
              downloadButton(
                ns("download_dataset"), 
                label = "Download TOC", 
                class = "btn btn-primary btn-sm me-2"
              )
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          div(class = "table-container", DTOutput(ns("datasets_table")))
        )
      )
    )
  )
}
