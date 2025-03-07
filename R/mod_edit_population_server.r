mod_edit_population_server <- function(id, pool, tables_data, selected_id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      # When the "Edit" button is pressed
      observeEvent(input$edit_pop_button, {

        if (is.null(selected_id()) || length(selected_id()) == 0 || (length(selected_id()) == 1 && selected_id() == "0")) {
          show_toast(
            title = "Edit Population Details",
            type = "info",
            text = "Please select at least one row before editing.",
            position = "center"

          )
          return()
        }

        #req(input$tracker_table_rows_selected)  # make sure at least one row is selected
        # Store the selected rows (these might be more than one)
        # selected_id(input$tracker_table_rows_selected)
        
        # Get the population choices from your populations table
        pop_choices <- tables_data$populations() %>% dplyr::select(id, population_text)
        pop_choices <- setNames(pop_choices$id, pop_choices$population_text)
        
        # For pre-selection in the modal, you might decide to:
        # 1. Use the first row's current population_text, or
        # 2. Compute the common set among all selected rows.
        # Here, we'll simply use the first selected row.
        # first_row <- tracker_data()[input$tracker_table_rows_selected[1], ]
        # selected_ids <- pop_choices[names(pop_choices) %in% strsplit(first_row$population_text, "<br>")[[1]]]
        

        # Create the modal dialog for editing
        edit_pop_modal <- modalDialog(
          title = "Edit Population Details",
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("save_pop_changes"), "Save", class = "btn btn-success"),

            modalButton("Cancel")
          ),
          fluidRow(
            column(width = 6,
                   # Note that multiple=FALSE means one new selection will apply to all selected rows
                   selectizeInput(ns("population"), "Population:",
                                  choices = c("Select Population", pop_choices),
                                  multiple = FALSE,
                                  selected = NULL)
            )
          )

        )
        showModal(edit_pop_modal)
      })
      
      # When the "Save" button in the modal is clicked
      observeEvent(input$save_pop_changes, {
        req(selected_id())  # Ensure we have stored selected rows
        if (input$population == "Select Population") {
          show_toast(
            title = "Edit Population Details",
            type = "error",
            text = "Please select a population",
            position = "center"

          )
          return()
        }
        population_id <- input$population
        
        tryCatch({
          # Use a transaction to update all selected rows
          poolWithTransaction(pool, function(conn) {
            # Loop over each selected row id and update it
            for (row in selected_id()) {
              # Delete existing populations for this report
              dbExecute(conn,
                        "DELETE FROM custom_populations WHERE report_programming_tracker_id = ?",
                        params = list(row))
              # Insert the new population for this report
              dbExecute(conn,
                        "INSERT INTO custom_populations (report_programming_tracker_id, population_id) VALUES (?, ?)",
                        params = list(row, population_id))
            }
          })
        }, error = function(e) {
          # Error handling (you can customize the toast message)
          print(paste0("Error updating populations: ", e$message))
          show_toast(
            title = "Edit Population Details",
            type = "error",
            text = paste("Error updating populations:", e$message),
            position = "center"
          )

          return()
        })
        
        # Remove the modal after saving
        removeModal()
      })
    }
  )
}