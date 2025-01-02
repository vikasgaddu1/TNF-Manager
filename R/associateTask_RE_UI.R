associateTask_RE_UI <- function(id, title) {
  ns <- NS(id)
  
  fluidPage(
    # Add custom styles
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
        top: 10px;
        color: #666;
      }
      .handsontable td.modified {
        background-color: #fff3cd !important;
      }
    "
      )
    ),
    
    # Page title
    h2(title, class = "mb-4"),
    
    # Filter section with reporting effort selection
    div(
      class = "filter-section",
      fluidRow(
        column(
          width = 8,
          selectInput(
            ns("reporting_effort"),
            label = "Select Reporting Effort:",
            choices = NULL,
            selected = NULL,
            width = "100%"
          )
        )
      )
    ),
    
    # Main content
    fluidRow(
      tabBox(
        id = "crud_tabs",
        width = 12,
        tabPanel(
          "TFL Tracker",
          
          # Save button
          column(
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
          
          # Search box
          column(
            width = 4,
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
          ),
          
          # Table output
          div(
            class = "table-container",
            rHandsontableOutput(ns("reports_table"))
          )
        )
      )
    )
  )
}
