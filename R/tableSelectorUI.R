# UI module
tableSelectorUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("table_dropdown"), "Select a table", choices = NULL),
    tableOutput(ns("table_view"))
  )
}