mod_edit_custom_footnote_server <- function(id, pool, tables_data, selected_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger when the user clicks the edit button
    observeEvent(input$edit_fn_button, {
      # Check that at least one row has been selected.
      if (is.null(selected_id()) || length(selected_id()) == 0 || (length(selected_id()) == 1 && selected_id() == "0")) {
        show_toast(
          title = "Edit Footnotes",
          type = "info",
          text = "Please select at least one row before editing.",
          position = "center"
        )
        return()
      }
      
      # Get the available footnotes choices
      fn_choices <- tables_data$footnotes() %>%
        dplyr::select(id, footnote_text)
      fn_choices <- setNames(fn_choices$id, fn_choices$footnote_text)
      
      # Build the modal.
      # Note: selected is left as NULL and we set a placeholder using options.
      edit_fn_modal <- modalDialog(
        title = "Edit Footnotes",
        easyClose = TRUE,
        footer = tagList(

          actionButton(ns("save_fn_changes"), "Save", class = "btn btn-success"),
          modalButton("Cancel")
        ),
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              ns("footnotes"),
              "Footnotes:",
              choices = fn_choices,
              multiple = TRUE,
              selected = NULL,
              options = list(placeholder = "Select footnotes")
            )
          )
        )
      )
      
      showModal(edit_fn_modal)
    })
    
    # When the user clicks "Save" in the modal
    observeEvent(input$save_fn_changes, {
      footnotes <- input$footnotes
      if (is.null(footnotes) || length(footnotes) == 0) {
        show_toast(
          title = "Edit Footnotes",
          type = "info",
          text = "Please select at least one footnote.",
          position = "center" 
        )
        return()
      }
      

      # Attempt the update within a transaction.
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          # Loop over every selected reporting effort
          for (report_id in selected_id()) {
            # Delete any previous custom footnotes for this report.
            DBI::dbExecute(
              conn,
              "DELETE FROM custom_footnotes WHERE report_programming_tracker_id = ?",
              params = list(report_id)
            )
            
            # Insert each selected footnote with a sequence number.
            for (i in seq_along(footnotes)) {
              footnote_id <- footnotes[i]
              DBI::dbExecute(
                conn,
                "INSERT INTO custom_footnotes (report_programming_tracker_id, sequence, footnote_id) VALUES (?, ?, ?)",
                params = list(report_id, i, footnote_id)
              )
            }
          }
        })
      }, error = function(e) {
        message("Error updating Footnotes: ", e$message)
        show_toast(
          title = "Edit Footnotes",
          type = "error",
          text = paste("Error updating Footnotes:", e$message),
          position = "center"
        )

        return()
      })
      
      removeModal()
    })
  })
}
