at_dataset_UI <- function(id, dataset_type) {
  ns <- NS(id)
  wellPanel(
    fluidRow(
      column(width = 3, uiOutput(ns("category_select")))
    ),
    # Search box
    fluidRow(column(
      width = 3,
      textInput(
        ns("search"),
        label = "Free Text Search",
        placeholder = "Enter keywords to filter...",
        width = "100%"
      )
    )),
    # Save button
    fluidRow(column(
      width = 3,
      div(
        class = "d-flex justify-content-start",
        actionButton(
          ns("save_selection"),
          "Save Selection",
          class = "btn btn-success btn-sm me-2",
          icon = icon("save")
        ),
        downloadButton(ns("download_tnf"), label = "Download Tracker as Excel", class = "btn btn-primary btn-sm me-2")
      )
    )),
    tags$hr(),
    # Table output
    div(class = "table-container", rHandsontableOutput(ns("datasets_table")))
  )
}