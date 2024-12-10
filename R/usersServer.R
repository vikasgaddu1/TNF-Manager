usersServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()
      dbReadTable(pool, "users")
    })
    
    # Add User
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add User"),
        fluidPage(
          textInput(ns("username_input"), "Username", placeholder = "Enter username"),
          selectInput(
            ns("role_input"),
            "Role",
            choices = c("admin", "user", "production_programmer", "qc_programmer"),
            selected = "user"
          )
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_add"), "Add User", class = "btn btn-primary", icon = icon("plus"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_add, {
      req(input$username_input, input$role_input)
      
      tryCatch({
        new_record <- data.frame(
          username = input$username_input,
          role = input$role_input,
          stringsAsFactors = FALSE
        )
        
        dbWriteTable(pool, "users", new_record, append = TRUE, row.names = FALSE)
        
        showNotification("User added successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding user:", e$message), type = "error", duration = 5)
      })
    })
    
    # Edit User
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        showNotification("Please select a user to edit.", type = "warning")
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("edit"), "Edit User"),
        fluidPage(
          textInput(ns("username_input"), "Username", value = record$username),
          selectInput(
            ns("role_input"),
            "Role",
            choices = c("admin", "user", "production_programmer", "qc_programmer"),
            selected = record$role
          )
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_edit"), "Save Changes", class = "btn btn-warning", icon = icon("save"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_edit, {
      req(input$username_input, input$role_input)
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        query <- "UPDATE users SET username = ?, role = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
        dbExecute(pool, query, params = list(input$username_input, input$role_input, record_id))
        
        showNotification("User updated successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error updating user:", e$message), type = "error", duration = 5)
      })
    })
    
    # Delete User
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        showNotification("Please select a user to delete.", type = "warning")
        return()
      }
      
      record <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("trash-alt"), "Delete User"),
        div(class = "alert alert-danger", "This action cannot be undone!"),
        div(
          sprintf("Are you sure you want to delete this user: '%s'?", record$username)
        ),
        footer = div(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete User", class = "btn btn-danger", icon = icon("trash-alt"))
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_delete, {
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        query <- "DELETE FROM users WHERE id = ?"
        dbExecute(pool, query, params = list(record_id))
        
        showNotification("User deleted successfully!", type = "message", duration = 3)
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error deleting user:", e$message), type = "error", duration = 5)
      })
    })
    
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
    # Render Data Table
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = c("ID","Username", "Role", "Updated At"),
        selection = "single",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(targets = c(0,3), visible = FALSE)  # Hide ID column
          )
        )
      )
    })
  })
}
