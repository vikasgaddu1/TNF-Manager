# UI module
tableSelectorUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(3, 
          selectizeInput(ns("table_dropdown"), "Select a table", choices = NULL), 
          downloadButton(ns("download_button"), "Download",class = "btn btn-primary btn-sm"),
          br(),
          #add spacer
          hr(),
          downloadButton(ns("merge_download_button"), "Download Merged Data", class = "btn btn-primary btn-sm")
          ),
        column(6, tableOutput(ns("table_view")))
      )
    )
  )
}