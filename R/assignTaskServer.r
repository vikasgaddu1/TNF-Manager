assignTaskServer <- function(id,
                             pool,
                             tables_data,
                             selected_id,    # reactive returning a vector of IDs
                             tracker_data    # reactive returning the tracker data frame
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$edit_button, {
      # Check that at least one row is selected.
      if (is.null(selected_id()) || length(selected_id()) == 0) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "info",
          text = "Please select a row before editing.",
          position = "center"
        )
        return()
      }
      
      # Get tracker data and use the first selected row for default values.
      df <- tracker_data()  # tracker_data is reactive, so call it
      row_data <- df[selected_id()[1], ]
      
      # Retrieve available programmer choices from tables_data.
      # (Assuming that both production and QC programmers come from tables_data$users())
      prod_prog <- tables_data$users() %>% dplyr::select(id, username)
      qc_prog   <- tables_data$users() %>% dplyr::select(id, username)
      
      # Create choice lists. (Note the use of the non-reactive prod_prog and qc_prog objects.)
      prod_choices <- setNames(prod_prog$id, prod_prog$username)
      qc_choices   <- setNames(qc_prog$id, qc_prog$username)
      
      # Determine default programmer IDs from the first selected row.
      current_prod_id <- prod_prog %>% dplyr::filter(username == row_data$production_programmer) %>% dplyr::pull(id)
      current_qc_id   <- qc_prog %>% dplyr::filter(username == row_data$qc_programmer) %>% dplyr::pull(id)
      
      # If multiple rows are selected, display a note in the modal.
      multi_note <- if (length(selected_id()) > 1) {
        tags$p(em(paste("Note:", length(selected_id()),
                        "rows are selected. Changes will be applied to all selected rows.")))
      } else {
        NULL
      }
      
      # Build the modal dialog.
      edit_modal <- modalDialog(
        title = "Edit Report Programming Details",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("save_changes"), "Save", class = "btn btn-success"),
          modalButton("Cancel")
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("production_programmer"),
              "Production Programmer:",
              choices = prod_choices,
              selected = if (length(current_prod_id) == 1) current_prod_id else prod_choices[[1]]
            ),
            dateInput(
              ns("assign_date"),
              "Assign Date:",
              value = if (is.na(row_data$assign_date)) Sys.Date() else as.Date(row_data$assign_date, format = "%Y-%m-%d")
            ),
            selectInput(
              ns("status"),
              "Status:",
              choices = c(
                "Not Started",
                "Production Started",
                "Production Ready",
                "Under QC",
                "QC Failed",
                "QC Pass"
              ),
              selected = if (is.na(row_data$status)) "Not Started" else row_data$status
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("qc_programmer"),
              "QC Programmer:",
              choices = qc_choices,
              selected = if (length(current_qc_id) == 1) current_qc_id else qc_choices[[1]]
            ),
            dateInput(
              ns("due_date"),
              "Due Date:",
              value = if (is.na(row_data$due_date)) Sys.Date() + 7 else as.Date(row_data$due_date, format = "%Y-%m-%d")
            ),
            selectInput(
              ns("qc_level"),
              "Assigned Validation Level:",
              choices = 1:3,
              selected = if (is.na(row_data$qc_level)) 3 else as.integer(row_data$qc_level)
            ),
            selectInput(
              ns("priority"),
              "Priority (1 <- Highest, 5 <- Lowest):",
              choices = 1:5,
              selected = if (is.na(row_data$priority)) 1 else as.integer(row_data$priority)
            )
          )
        ),
        multi_note
      )
      
      showModal(edit_modal)
    })
    
    observeEvent(input$save_changes, {
      # Retrieve input values.
      prod_id     <- input$production_programmer
      qc_id       <- input$qc_programmer
      assign_date <- input$assign_date
      due_date    <- input$due_date
      priority    <- input$priority
      status      <- input$status
      qc_level    <- input$qc_level
      
      # Validate that production and QC programmer are not the same.
      if (prod_id == qc_id) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = "Production and QC programmer cannot be the same person.",
          position = "center"
        )
        return()
      }
      
      # Validate that the due date is after the assign date.
      if (as.Date(due_date) <= as.Date(assign_date)) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = "Due date must be after the assign date.",
          position = "center"
        )
        return()
      }
      
      # Attempt the update for each selected report.
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          for (report_id in selected_id()) {
            DBI::dbExecute(
              conn,
              "UPDATE report_programming_tracker
               SET production_programmer_id = ?,
                   assign_date = ?,
                   qc_programmer_id = ?,
                   due_date = ?,
                   priority = ?,
                   status = ?,
                   qc_level = ?,
                   updated_at = CURRENT_TIMESTAMP
               WHERE id = ? ;",
              params = list(
                prod_id,
                as.character(assign_date),
                qc_id,
                as.character(due_date),
                as.integer(priority),
                status,
                as.integer(qc_level),
                report_id
              )
            )
          }
        })
        
        show_toast(
          title = "Edit Report Programming Details",
          type = "success",
          text = "Record(s) updated successfully.",
          position = "center"
        )
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = paste("Error updating record:", e$message),
          position = "center"
        )
      })
    })
    
  })
}
