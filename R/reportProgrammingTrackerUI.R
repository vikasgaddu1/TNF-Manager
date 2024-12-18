reportProgrammingTrackerUI <- function(id, title = "Programming Tracker") {
  ns <- NS(id)
  
  fluidPage(
    tags$style(
      HTML(
        "
      .datepicker, .ui-datepicker { z-index: 99999 !important; }
      .box { margin-bottom: 20px; }
      .action-buttons { margin-bottom: 15px; }
      .status-badge {
        padding: 5px 10px;
        border-radius: 4px;
        display: inline-block;
      }
      .summary-stats {
        padding: 15px;
        background: #f8f9fa;
        border-radius: 5px;
        margin-bottom: 20px;
      }
    "
      )
    ),
    
    h2(title, class = "mb-4"),
    
    fluidRow(column(
      width = 4, div(
        class = "well",
        selectInput(
          ns("reporting_effort"),
          label = "Select Reporting Effort:",
          choices = NULL,
          selected = NULL,
          width = "100%"
        )
      )
    )),
    
    fluidRow(
      title = "Report Summary",
      box(
        title = "Programming Effort Progress",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 6,
        plotOutput(ns("progressPlot"), height = "300px")
      ),
      box(
        title = "Programming Effort Progress by Report Type",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 6,
        plotOutput(ns("progressPlotType"), height = "300px")
      ),
    box(
      title = "Programming Effort Progress by Prod Programmer",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 6,
      plotOutput(ns("progressPlotProd"), height = "300px")
    ),
    box(
      title = "Programming Effort Progress by QC Programmer",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 6,
      plotOutput(ns("progressPlotQC"), height = "300px")
    ),
    box(
      title = "Programming Effort Progress by Status",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 6,
      plotOutput(ns("progressPie"), height = "300px")
    ),
    box(
      title = "Programming Effort Progress by Status and Type",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 6,
      plotOutput(ns("progressPieType"), height = "300px")
    )
  ),
    fluidRow(column(
      width = 8, div(
        class = "action-buttons",
        actionButton(
          ns("edit_button"),
          "Edit Selected Row",
          class = "btn btn-warning",
          icon = icon("edit")
        )
      )
    )),
    
    fluidRow(column(
      width = 12,
      div(class = "table-responsive", DT::dataTableOutput(ns("tracker_table")))
    ))
  )
}