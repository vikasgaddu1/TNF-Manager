reportingEffortsServer <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()
      tables_data$reporting_efforts()
    })
    
    observeEvent(input$study_input, {
      shinyFeedback::hideFeedback("study_input")
    })
    observeEvent(input$db_release_input, {
      shinyFeedback::hideFeedback("db_release_input")
    })
    observeEvent(input$effort_input, {
      shinyFeedback::hideFeedback("effort_input")
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
      
      if ((is.null(input$study_input) || input$study_input == "") ||
      (is.null(input$db_release_input) || input$db_release_input == "") ||  
      (is.null(input$effort_input) || input$effort_input == ""))  {
        feedbackDanger("study_input", is.null(input$study_input) || input$study_input == "", "Study is required")
        feedbackDanger("db_release_input", is.null(input$db_release_input) || input$db_release_input == "", "Database Release is required")
        feedbackDanger("effort_input", is.null(input$effort_input) || input$effort_input == "", "Reporting Effort is required")
        return()
      }

      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "INSERT INTO reporting_efforts (study, database_release, reporting_effort, updated_at) 
             VALUES (?, ?, ?, CURRENT_TIMESTAMP)",
            params = list(
              input$study_input,
              input$db_release_input,
              input$effort_input
            )
          )
        })
        
        refresh_trigger(refresh_trigger() + 1)    
        show_toast(
          title = "Success",
          type = "success",
          text = "Reporting Effort added successfully!",
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding record:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Edit Record
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Information",
          type = "info",
          text = "Please select a record to edit.",
          position = "center"
        )
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
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      if ((is.null(input$study_input) || input$study_input == "") ||
      (is.null(input$db_release_input) || input$db_release_input == "") ||
      (is.null(input$effort_input) || input$effort_input == ""))  {
        feedbackDanger("study_input",is.null(input$study_input) || input$study_input == "", "Study is required")
        feedbackDanger("db_release_input", is.null(input$db_release_input) || input$db_release_input == "", "Database Release is required") 
        feedbackDanger("effort_input", is.null(input$effort_input) || input$effort_input == "", "Reporting Effort is required")
        return()
      }
  

      tryCatch({
        query <- "UPDATE reporting_efforts SET study = ?, database_release = ?, reporting_effort = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, query, params = list(input$study_input, input$db_release_input, input$effort_input, record_id))
        })
        
        refresh_trigger(refresh_trigger() + 1)    
        show_toast(
          title = "Success",
          type = "success",
          text = "Reporting Effort updated successfully!",
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error updating record:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Delete Record
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Information",
          type = "info",
          text = "Please select a record to delete.",
          position = "center"
        )
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
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, query, params = list(record_id))
        })
        
        refresh_trigger(refresh_trigger() + 1)    
        show_toast(
          title = "Success",
          type = "success",
          text = "Reporting Effort deleted successfully!",
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error deleting record:", e$message),
          position = "top-end"
        )
      })
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
