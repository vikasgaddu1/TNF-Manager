mod_assign_task_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    actionButton(
              ns("edit_button"),
              "Assign/Edit Tasks",
              class = "btn btn-warning btn-sm",
              icon = icon("edit")
            )
  )
}