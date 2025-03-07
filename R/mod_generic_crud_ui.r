mod_generic_crud_ui <- function(id, title) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "card shadow-sm",
      div(
        class = "card-header bg-light",
        h3(paste("Manage", title), class = "card-title m-0")
      ),

       
        wellPanel(
          class = "p-3",

            div(
              class = "d-flex justify-content-start",
              actionButton(
                ns("add"),
                "Add",
                class = "btn btn-primary btn-sm me-2", # Use mr-2 instead of me-2
                icon = icon("plus")
              ),
              actionButton(
                ns("edit"),
                "Edit",
                class = "btn btn-warning btn-sm me-2", # Use mr-2 instead of me-2
                icon = icon("edit")
              ),
              actionButton(
                ns("delete"),
                "Delete",
                class = "btn btn-danger btn-sm", # No margin needed for the last button
                icon = icon("trash")
              )
          )
      ),
      div(
        class = "card-body p-3",
        DT::dataTableOutput(ns("table"), width = "100%", height = "auto")
      )
    )
  )
}