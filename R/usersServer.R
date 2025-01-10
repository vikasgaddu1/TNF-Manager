usersServer <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Use tables_data for automatic updates
    data <- reactive({
      tables_data$users()
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
            choices = c("admin", "user"),
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
      if (is.null(input$username_input) || input$username_input == "" || 
          is.null(input$role_input) || input$role_input == "") {
        shinyFeedback::feedbackDanger("username_input", is.null(input$username_input) || input$username_input == "", "Username is required")
        shinyFeedback::feedbackDanger("role_input", is.null(input$role_input) || input$role_input == "", "Role is required")
        return()
      }
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "INSERT INTO users (username, role) VALUES (?, ?)",
            params = list(input$username_input, input$role_input)
          )
        })
        
        show_toast(
          title = "User Added",
          type = "success",
          text = "User added successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding user:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Edit User
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Edit Error",
          type = "warning",
          text = "Please select a user to edit.",
          position = "center"
        )
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
            choices = c("admin", "user"),
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
      if (is.null(input$username_input) || input$username_input == "" || is.null(input$role_input) || input$role_input == "") {
        shinyFeedback::feedbackDanger("username_input", is.null(input$username_input) || input$username_input == "", "Username is required")
        shinyFeedback::feedbackDanger("role_input", is.null(input$role_input) || input$role_input == "", "Role is required")
        return()
      }
      selected <- input$table_rows_selected
      record_id <- data()[selected, "id"]
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "UPDATE users SET username = ?, role = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
            params = list(input$username_input, input$role_input, record_id)
          )
        })
        
        show_toast(
          title = "User Updated",
          type = "success",
          text = "User updated successfully!",
          position = "top-end"
        )

        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error updating user:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Delete User
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Delete Error",
          type = "warning",
          text = "Please select a user to delete.",
          position = "center"
        )
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
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "DELETE FROM users WHERE id = ?",
            params = list(record_id)
          )
        })
        
        show_toast(
          title = "User Deleted",
          type = "success",
          text = "User deleted successfully!",
          position = "top-end"
        )
   
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error deleting user:", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Render Data Table
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = c("ID", "Username", "Role", "Updated At"),
        selection = "single",
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(targets = c(0, 3), visible = FALSE)  # Hide ID column
          )
        )
      )
    })
  })
}
