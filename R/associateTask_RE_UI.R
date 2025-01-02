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
    at_tfl_UI(id = ns("at_tfl"))
  )
}
