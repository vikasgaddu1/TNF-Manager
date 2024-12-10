tfpCRUDServer <- function(id, pool, table_name, text_column) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Capitalize first letter for display and fetch first word
    
    #display_name <- tools::toTitleCase(gsub("_", " ", text_column))
    display_name <- text_column %>%
      str_extract("^[^_]+") %>%         # Extract everything before first underscore
      str_to_title() 
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()
      dbReadTable(pool, table_name)
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
    
    observeEvent(input$confirm_add, {
      req(input$text_input)
      
      tryCatch({
        new_record <- data.frame(setNames(list(input$text_input), text_column), stringsAsFactors = FALSE)
        
        dbWriteTable(pool,
                     table_name,
                     new_record,
                     append = TRUE,
                     row.names = FALSE)
        
        showNotification(
          paste(display_name, "added successfully!"),
          type = "message",
          duration = 3
        )
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error adding", tolower(display_name), ":", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Edit Record
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          paste("Please select a", tolower(display_name), "to edit"),
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("edit", class = "text-warning me-2"),
          paste("Edit", display_name)
        ),
        div(
          class = "form-group mb-3",
          textInput(
            ns("text_input"),
            label = div(class = "form-label", display_name, span(class = "text-danger", "*")),
            value = record[[text_column]],
            width = "100%"
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_edit"),
            "Save Changes",
            class = "btn btn-warning",
            icon = icon("save")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_edit, {
      req(input$text_input)
      selected <- input$table_rows_selected
      
      tryCatch({
        query <- sprintf("UPDATE %s SET %s = ? WHERE id = ?", table_name, text_column)
        
        dbExecute(pool, query, params = list(input$text_input, data()[selected, "id"]))
        
        showNotification(
          paste(display_name, "updated successfully!"),
          type = "message",
          duration = 3
        )
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(
          paste(
            "Error updating",
            tolower(display_name),
            ":",
            e$message
          ),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Delete Record
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          paste("Please select a", tolower(display_name), "to delete"),
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("trash-alt", class = "text-danger me-2"),
          paste("Delete", display_name)
        ),
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle", class = "me-2"),
          "This action cannot be undone!"
        ),
        div(
          class = "mb-3",
          sprintf(
            "Are you sure you want to delete this %s: '%s'?",
            tolower(display_name),
            record[[text_column]]
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_delete"),
            "Delete Record",
            class = "btn btn-danger",
            icon = icon("trash-alt")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_delete, {
      req(input$table_rows_selected)
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        query <- sprintf("DELETE FROM %s WHERE id = ?", table_name)
        
        dbExecute(pool, query, params = list(record_id))
        
        showNotification(
          paste(display_name, "deleted successfully!"),
          type = "message",
          duration = 3
        )
        
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(
          paste(
            "Error deleting",
            tolower(display_name),
            ":",
            e$message
          ),
          type = "error",
          duration = 5
        )
      })
    })
    
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = setNames(
          c("id", text_column, "updated_at"),
          c("ID", display_name, "Last Updated")
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(columnDefs = list(list(
          targets = c(0, 2), visible = FALSE
        )))
      )
    })
  })
}