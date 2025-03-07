mod_single_column_crud_server <- function(id, pool, table_name, text_column, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    refresh_trigger <- reactiveVal(0)
    # Prepare display names
    display_name <- text_column %>%
      str_extract("^[^_]+") %>%
      str_to_title()
    display_name_plural <- table_name %>%
      str_to_title()
    
    # Create a reactive to access the table data
    data <- reactive({
      refresh_trigger()
      tables_data[[table_name]]()
    })
    
    # Add Record
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), paste("Add", display_name)),
        div(
          class = "form-group",
          textInput(
            ns("text_input"),
            display_name,
            placeholder = paste("Enter", tolower(display_name)),
            width = "100%"
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_add"),
            "Add Record",
            class = "btn btn-primary",
            icon = icon("plus")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$text_input, {
      if (!(is.null(input$text_input) || input$text_input == "")) {
          shinyFeedback::hideFeedback("text_input")
      }
    })
    
    observeEvent(input$confirm_add, {
      # Validate the input field
      if (is.null(input$text_input) || input$text_input == "") {
        shinyFeedback::showFeedbackDanger("text_input", "Input cannot be empty.")
        return()
      }
      
      # Hide feedback if validation passes
      shinyFeedback::hideFeedback("text_input")
      
      tryCatch({
        # Use poolWithTransaction to manage transactions
        # Manually update updated_at column
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("INSERT INTO %s (%s, updated_at) VALUES (?, CURRENT_TIMESTAMP)", table_name, text_column),
            params = list(input$text_input)
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        # Notify the user of success using shinyWidgets::show_toast
        show_toast(
          title = paste(display_name, "Added"),
          type = "success",
          text = paste(display_name, "added successfully!"),
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully and notify using shinyWidgets::show_toast
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Similar transaction logic can be applied to the "Edit Record" and "Delete Record" sections
    
    # Edit Record
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Edit Error",
          type = "warning",
          text = "No row selected for editing.",
          position = "center"
        )
        return()
      }
      record <- data()[selected, ]
      showModal(modalDialog(
        title = div(icon("edit"), paste("Edit", display_name)),
        div(
          class = "form-group",
          textInput(
            ns("text_input"),
            label = display_name,
            value = record[[text_column]],
            width = "100%"
          )
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_edit"), "Save Changes", class = "btn btn-warning")
        )
      ))
    })
    
    observeEvent(input$confirm_edit, {
      # Validate the input field
      if (is.null(input$text_input) || input$text_input == "") {
        shinyFeedback::showFeedbackDanger("text_input", "Input cannot be empty.")
        return()
      }
      
      # Hide feedback if validation passes
      shinyFeedback::hideFeedback(ns("text_input"))
      
      tryCatch({
        selected <- input$table_rows_selected
        if (length(selected) == 0) {
          show_toast(
            title = "Edit Error",
            type = "warning",
            text = "No row selected for editing.",
            position = "top-end"
          )
          return()
        }
        
        # Use poolWithTransaction to manage transactions
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("UPDATE %s SET %s = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?", table_name, text_column),
            params = list(input$text_input, data()[selected, "id"])
          )
        })
        
        refresh_trigger(refresh_trigger() + 1)
        # Notify the user of success using shinyWidgets::show_toast
        show_toast(
          title = paste(display_name, "Updated"),
          type = "success",
          text = paste(display_name, "updated successfully!"),
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully and notify using shinyWidgets::show_toast
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error updating", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Delete Record
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Delete Error",
          type = "warning",
          text = "No row selected for deletion.",
          position = "center"
        )
        return()
      }
      record <- data()[selected, ]
      showModal(modalDialog(
        title = div(icon("trash-alt"), paste("Delete", display_name)),
        div(
          class = "alert alert-danger",
          sprintf("Are you sure you want to delete this %s: '%s'?", tolower(display_name), record[[text_column]])
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete Record", class = "btn btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_delete, {
      selected <- input$table_rows_selected
      
      # Validate that a row is selected
      if (is.null(selected) || length(selected) == 0) {
        show_toast(
          title = "Delete Error",
          type = "warning",
          text = "No row selected for deletion.",
          position = "top-end"
        )
        return()
      }
      
      tryCatch({
        # Use poolWithTransaction to manage transactions
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("DELETE FROM %s WHERE id = ?", table_name),
            params = list(data()[selected, "id"])
          )
        })
        
        refresh_trigger(refresh_trigger() + 1)
        # Notify the user of success using shinyWidgets::show_toast
        show_toast(
          title = paste(display_name, "Deleted"),
          type = "success",
          text = paste(display_name, "deleted successfully!"),
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully and notify using shinyWidgets::show_toast
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error deleting", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Render DataTable
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = setNames(c("id", text_column, "updated_at"), c("ID", display_name_plural, "Last Updated")),
        selection = "single",
        rownames = FALSE,
        options = list(columnDefs = list(list(targets = c(0, 2), visible = FALSE)))
      )
    })
  })
}
