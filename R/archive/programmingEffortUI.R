programmingEffortUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Programming Effort Progress Details",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,  # Initially collapsed
      width = 12,  # Full width of the row
      fluidRow(
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
      )
    )
  )
}
