programmingEffortUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Programming Effort Progress Details",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      fluidRow(uiOutput(ns("valueBoxes"))),
      tabsetPanel(
        tabPanel("Due Soon",div(style="margin-top:20px;",DTOutput(ns("dueSoon")))),
        tabPanel("Past Due", div(style="margin-top:20px;",DTOutput(ns("pastDue")))) ,
        tabPanel(
          "Programmer Workload Table",
          fluidRow(
            column(width = 5, div(style="margin-top:20px;",gt_output(ns("summaryTable1")))),
            column(width = 1, div(style="margin-top:20px;",HTML("&nbsp;"))),
            column(width = 5, div(style="margin-top:20px;",gt_output(ns("summaryTable2"))))
          )
        ),
        tabPanel("Task Summary Graph", div(style="margin-top:20px;",plotlyOutput(ns("taskPlot")))),
        tabPanel("Status Pie Chart", div(style="margin-top:20px;",plotlyOutput(ns("statusPieChart")))),
        tabPanel("Status Priority Chart", div(style="margin-top:20px;",plotlyOutput(ns("statusPriorityChart"))))
      )
    )
  )
}