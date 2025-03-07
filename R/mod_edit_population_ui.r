mod_edit_population_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    actionButton(
      ns("edit_pop_button"),
      "Edit Population",
      class = "btn btn-info btn-sm",
      icon = icon("edit")
    )
  )
}