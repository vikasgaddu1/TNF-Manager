programmingEffortUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = tagList(
        icon("chart-line"), 
        "Programming Effort Progress Details",
        tags$small(
          style = "display: block; font-size: 80%; color: #fff; font-style: italic;",
          "Click +/- sign on right to expand/collapse"
        )
      ),
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      fluidRow(uiOutput(ns("valueBoxes"))),
      tabsetPanel(
        id = ns("effortTabs"),
        type = "tabs",
        tabPanel(
          tagList(icon("clock"), "Due Soon"),
          div(style="margin-top:20px;",DTOutput(ns("dueSoon")))
        ),
        tabPanel(
          tagList(icon("calendar-times"), "Past Due"),
          div(style="margin-top:20px;",DTOutput(ns("pastDue")))
        ),
        tabPanel(
          tagList(icon("users"), "Programmer Workload"),
          fluidRow(
            column(width = 5, div(style="margin-top:20px;",gt_output(ns("summaryTable1")))),
            column(width = 1, div(style="margin-top:20px;",HTML("&nbsp;"))),
            column(width = 5, div(style="margin-top:20px;",gt_output(ns("summaryTable2"))))
          )
        ),
        tabPanel(
          tagList(icon("tasks"), "Task Summary"),
          div(style="margin-top:20px;",plotlyOutput(ns("taskPlot")))
        ),
        tabPanel(
          tagList(icon("chart-pie"), "Status Distribution"),
          div(style="margin-top:20px;",plotlyOutput(ns("statusPieChart")))
        ),
        tabPanel(
          tagList(icon("chart-bar"), "Status by Priority"),
          div(style="margin-top:20px;",plotlyOutput(ns("statusPriorityChart")))
        )
      )
    )
  )
}