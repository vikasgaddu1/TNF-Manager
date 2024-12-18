reportingEffortReportsUI <- function(id, title) {
  ns <- NS(id)
  
  fluidPage(
    tags$style(
      HTML(
        "
      .filter-section {
        background: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .search-box {
        position: relative;
        margin: 15px 0;
      }
      .search-box .form-control {
        padding-left: 35px;
      }
      .search-icon {
        position: absolute;
        left: 10px;
        top: 38px;
        color: #666;
      }
      .handsontable td.modified {
        background-color: #fff3cd !important;
      }
    "
      )
    ),
    
    h2(title, class = "mb-4"),
    
    div(
      class = "filter-section",
      fluidRow(column(
        width = 8,
        selectInput(
          ns("reporting_effort"),
          label = "Select Reporting Effort:",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      ), column(
        width = 4, div(
          style = "margin-top: 25px",
          actionButton(
            ns("save_selection"),
            "Save Selection",
            class = "btn btn-success btn-block",
            icon = icon("save")
          )
        )
      )),
      
      div(
        class = "search-box",
        icon("search", class = "search-icon"),
        textInput(
          ns("search"),
          label = "Search Reports:",
          placeholder = "Enter keywords to filter...",
          width = "100%"
        )
      )
    ),
    
    div(
      class = "table-container",
      rHandsontableOutput(ns("reports_table"))
    )
  )
}