at_tfl_UI <- function(id) {
  ns <- NS(id)
  fluidRow(tabBox(
    id = ns("crud_tabs"),
    width = 12,
    tabPanel(
      "TFL Tracker",
      
      # Row for dropdowns and search box
      wellPanel(
        fluidRow(
          column(width = 3, uiOutput(ns(
            "report_type_select"
          ))),
          column(width = 3, uiOutput(ns(
            "category_select"
          ))),
          column(width = 3, uiOutput(ns(
            "subcategory_select"
          )))
        ),
        # Search box in a separate fluidRow
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
            downloadButton(ns("download_tnf"), label = "Download TNF as Excel", class = "btn btn-primary btn-sm me-2")
          )
        ))
        
      ),
      # Table output
      div(class = "table-container", rHandsontableOutput(ns("reports_table")))
    )
  ))
}