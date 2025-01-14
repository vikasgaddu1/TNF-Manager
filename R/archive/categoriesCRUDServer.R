categoriesCRUDServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data polling using reactiveDatabasePolling
    data <- reactiveDatabasePolling(
      pool,
      checkQuery = "SELECT MAX(updated_at) FROM categories",
      dataQuery = "SELECT * FROM categories"
    )
    
    # Add Category
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add Category"),
        div(
          class = "form-group",
          textInput(
            ns("category_name"),
            "Category Name",
            placeholder = "Enter category name",
            width = "100%"
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_add"),
            "Add Category",
            class = "btn btn-primary",
            icon = icon("plus")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_add, {
      if (is.null(input$category_name) || input$category_name == "") {
        shinyFeedback::showFeedbackDanger(
          "category_name", "Category name cannot be empty."
        )
        return()
      }
      
      req(input$category_name)  # Validate input
      
      if (nchar(input$category_name) < 3) {
        shinyFeedback::showFeedbackDanger(
          "category_name", "Category name must be at least 3 characters long."
        )
        return()
      }
      
      shinyFeedback::hideFeedback(ns("category_name"))
      
      tryCatch({
        dbExecute(
          pool,
          "INSERT INTO categories (category_name) VALUES (?)",
          params = list(input$category_name)
        )
        
        shinyFeedback::showToast(
          "success",
          "Category added successfully!"
        )
        
        removeModal()
      }, error = function(e) {
        shinyFeedback::showToast(
          "error",
          paste("Error adding category:", e$message)
        )
      })
    })
    
    # Edit Category
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        shinyFeedback::showToast(
          "warning",
          "Please select a category to edit."
        )
        return()
      }
      
      category <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("edit"), "Edit Category"),
        div(
          class = "form-group",
          textInput(
            ns("category_name"),
            "Category Name",
            value = category$category_name,
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
      req(input$category_name)
      selected <- input$table_rows_selected
      
      if (nchar(input$category_name) < 3) {
        shinyFeedback::showFeedbackDanger(
          "category_name", "Category name must be at least 3 characters long."
        )
        return()
      }
      
      shinyFeedback::hideFeedback(ns("category_name"))
      
      tryCatch({
        dbExecute(
          pool,
          "UPDATE categories SET category_name = ? WHERE id = ?",
          params = list(input$category_name, data()[selected, "id"])
        )
        
        shinyFeedback::showToast(
          "success",
          "Category updated successfully!"
        )
        
        removeModal()
      }, error = function(e) {
        shinyFeedback::showToast(
          "error",
          paste("Error updating category:", e$message)
        )
      })
    })
    
    # Delete Category
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        shinyFeedback::showToast(
          "warning",
          "Please select a category to delete."
        )
        return()
      }
      
      category <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("trash-alt"), "Delete Category"),
        div(
          class = "alert alert-danger",
          "This action cannot be undone!"
        ),
        sprintf("Are you sure you want to delete the category '%s'?", category$category_name),
        footer = div(
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_delete"),
            "Delete Category",
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
      category_id <- data()[selected, "id"]
      
      tryCatch({
        dbExecute(
          pool,
          "DELETE FROM categories WHERE id = ?",
          params = list(category_id)
        )
        
        shinyFeedback::showToast(
          "success",
          "Category deleted successfully!"
        )
        
        removeModal()
      }, error = function(e) {
        shinyFeedback::showToast(
          "error",
          paste("Error deleting category:", e$message)
        )
      })
    })
    
    # Render DataTable
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = c("ID" = "id", "Category Name" = "category_name"),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(
          columnDefs = list(
            list(
              targets = 0,  # Hide ID column
              visible = FALSE
            )
          )
        )
      )
    })
  })
}
