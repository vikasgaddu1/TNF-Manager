mod_comments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    actionButton(ns("add_comment"),
                 "Add/View Comments",
                 class = "btn btn-sm btn-info",
                 icon = icon("comment"))
  )
}
