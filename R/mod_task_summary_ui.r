mod_task_summary_ui <- function(id) {
  ns <- NS(id)
  box(
    title = tagList(
      "Outstanding Tasks - ",
      textOutput(ns("userTitle"), inline = TRUE)
    ),
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    htmlOutput(ns("taskSummaryContent"))
  )
}