# add module with button to add comment to a row in the tracker

commentsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    actionButton(ns("add_comment"), "Add/View Comments", class = "btn btn-sm btn-info", icon = icon("comment"))
  )
}


