editCustomFootnoteUI  <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),

    actionButton(
      ns("edit_fn_button"),
      "Edit Footnote",
      class = "btn btn-info btn-sm",
      icon = icon("edit")


    )
  )
}