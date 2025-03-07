mod_at_copy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    selectizeInput(
                  ns("copy_reporting_effort"),
                  "Copy Task from Other RE:",
                  choices = NULL,
                  )
  )
}