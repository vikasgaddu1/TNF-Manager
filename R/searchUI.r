searchUI <- function(id, title = "Search") {
  ns <- NS(id)
  
  fluidPage(
    h2(title, class = "mb-4"),
    tabBox(
      id = ns("search_tabs"),
      width = 12,
      tabPanel(
        "TFL Tracker",
        icon = icon("table"),
        DT::DTOutput(ns("tfl_tracker"))
      ),
      tabPanel(
        "SDTM Tracker",
        icon = icon("table"),
        DT::DTOutput(ns("sdtm_tracker"))
      ),
      tabPanel(
        "ADaM Tracker",
        icon = icon("table"),
        DT::DTOutput(ns("adam_tracker"))
      )
    )
  )
}
