reportingEffortsServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()
      dbReadTable(pool, "reporting_efforts")
    })
    
    # Add Record
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add Reporting Effort"),
        fluidPage(
          textInput(ns("study_input"), "Study", placeholder = "Enter study name"),
          textInput(ns("db_release_input"), "Database Release", placeholder = "Enter database release"),
          textInput(ns("effort_input"), "Reporting Effort", placeholder = "Enter reporting effort description")
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_add"), "Add Record", class = "btn btn-primary", icon = icon("plus"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_add, {
      req(input$study_input, input$db_release_input, input$effort_input)
      
      tryCatch({
        new_record <- data.frame(
          study = input$study_input,
          database_release = input$db_release_input,
          reporting_effort = input$effort_input,
          stringsAsFactors = FALSE
        )
        
        dbWriteTable(pool, "reporting_efforts", new_record, append = TRUE, row.names = FALSE)
        
        showNotification("Reporting Effort added successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding record:", e$message), type = "error", duration = 5)
      })
    })
    
    # Edit Record
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        showNotification("Please select a record to edit.", type = "warning")
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("edit"), "Edit Reporting Effort"),
        fluidPage(
          textInput(ns("study_input"), "Study", value = record$study),
          textInput(ns("db_release_input"), "Database Release", value = record$database_release),
          textInput(ns("effort_input"), "Reporting Effort", value = record$reporting_effort)
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_edit"), "Save Changes", class = "btn btn-warning", icon = icon("save"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_edit, {
      req(input$study_input, input$db_release_input, input$effort_input)
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        query <- "UPDATE reporting_efforts SET study = ?, database_release = ?, reporting_effort = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
        dbExecute(pool, query, params = list(input$study_input, input$db_release_input, input$effort_input, record_id))
        
        showNotification("Reporting Effort updated successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error updating record:", e$message), type = "error", duration = 5)
      })
    })
    
    # Delete Record
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        showNotification("Please select a record to delete.", type = "warning")
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("trash-alt"), "Delete Reporting Effort"),
        div(class = "alert alert-danger", "This action cannot be undone!"),
        div(
          sprintf("Are you sure you want to delete this record: '%s'?", record$study)
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete Record", class = "btn btn-danger", icon = icon("trash-alt"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_delete, {
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        query <- "DELETE FROM reporting_efforts WHERE id = ?"
        dbExecute(pool, query, params = list(record_id))
        
        showNotification("Reporting Effort deleted successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error deleting record:", e$message), type = "error", duration = 5)
      })
    })
    
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
    # Render Data Table
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        filter = "top",
        colnames = c("ID","Study", "Database Release", "Reporting Effort", "Updated At"),
        selection = "single",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(targets = c(0,4), visible = FALSE)  # Hide ID column
          )
        )
      )
    })
  })
}
