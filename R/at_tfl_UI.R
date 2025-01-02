at_tfl_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    tabBox(
      id = ns("crud_tabs"),
      width = 12,
      tabPanel(
        "TFL Tracker",
        
        # Row for dropdowns and search box
        fluidRow(
          column(
            width = 3,
            uiOutput(ns("report_type_select"))
          ),
          column(
            width = 3,
            uiOutput(ns("category_select"))
          ),
          column(
            width = 3,
            uiOutput(ns("subcategory_select"))
          ),
          column(
            width = 3,
            div(
              class = "search-box",
              icon("search", class = "search-icon"),
              textInput(
                ns("search"),
                label = NULL,
                placeholder = "Enter keywords to filter...",
                width = "100%"
              )
            )
          )
        ),
        
        # Save button
        fluidRow(column(
          width = 3,
          div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            actionButton(
              ns("save_selection"),
              "Save Selection",
              class = "btn btn-success btn-block",
              icon = icon("save")
            )
          )
        ),
        #add download button
        column(
          width = 3,
          div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            downloadButton(
              ns("download_tnf"),
              label = "Download TNF as Excel",
              class = "btn btn-primary btn-block"
            )
          )
        )),
  
        
        # Table output
        div(
          class = "table-container",
         rHandsontableOutput(ns("reports_table"))
        )
      )
    )
  )
}