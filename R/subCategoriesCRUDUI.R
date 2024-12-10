subCategoriesCRUDUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h2("Manage Sub-Categories", class = "mb-4"),
    div(
      class = "mb-3",
      actionButton(
        ns("add"),
        "Add Record",
        class = "btn btn-primary",
        icon = icon("plus")
      ),
      actionButton(
        ns("edit"),
        "Edit Record",
        class = "btn btn-warning",
        icon = icon("edit")
      ),
      actionButton(
        ns("delete"),
        "Delete Record",
        class = "btn btn-danger",
        icon = icon("trash")
      ),
      actionButton(
        ns("refresh"),
        label = NULL,
        icon = icon("sync"),
        class = "btn btn-outline-secondary",
        title = "Refresh data",
        style = "border-radius: 50%;"  # Makes the button circular
      )
    ),
    # add line break
    br(),
    DT::dataTableOutput(ns("table"), width = "100%", height = "auto")
  )
}