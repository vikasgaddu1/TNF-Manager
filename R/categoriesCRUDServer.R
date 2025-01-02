categoriesCRUDServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()  # Add dependency on refresh trigger
      dbReadTable(pool, "categories")  
    })
    
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
      req(input$category_name)  # Validate input
      
      tryCatch({
        new_category <- data.frame(category_name = input$category_name,
                                   stringsAsFactors = FALSE)
        dbWriteTable(pool,
                     "categories",
                     new_category,
                     append = TRUE,
                     row.names = FALSE)
        
        # Show success message
        showNotification("Category added successfully!",
                         type = "message",
                         duration = 3)
        
        # Refresh data
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error adding category:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    
    # Edit Category
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          "Please select a category to edit",
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      category <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("edit", class = "text-warning me-2"),
          "Edit Category"
        ),
        div(
          class = "form-group mb-3",
          textInput(
            ns("category_name"),
            label = div(class = "form-label", "Category Name", span(class = "text-danger", "*")),
            value = category$category_name,
            width = "100%",
            placeholder = "Enter category name"
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon=icon("times")),
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
    
    # Add the corresponding edit confirmation handler
    observeEvent(input$confirm_edit, {
      req(input$category_name)
      selected <- input$table_rows_selected
      
      tryCatch({
        dbExecute(
          pool,
          "UPDATE categories SET category_name = ? WHERE id = ?",
          params = list(input$category_name, data()[selected, "id"])
        )
        
        # Show success message
        showNotification("Category updated successfully!",
                         type = "message",
                         duration = 3)
        
        # Refresh data
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error updating category:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Delete Category
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          "Please select a category to delete",
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      category <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("trash-alt", class = "text-danger me-2"),
          "Delete Category"
        ),
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle", class = "me-2"),
          "This action cannot be undone!"
        ),
        div(
          class = "mb-3",
          sprintf("Are you sure you want to delete the category '%s'?", 
                  category$category_name)
        ),
        footer = div(
          class = "modal-footer",
          modalButton(
            "Cancel",
            icon=icon("times")
          ),
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
        # Check for dependencies before deletion
        # Add any necessary foreign key checks here
        
        dbExecute(
          pool, 
          "DELETE FROM categories WHERE id = ?", 
          params = list(category_id)
        )
        
        # Show success message
        showNotification(
          "Category deleted successfully!",
          type = "message",
          duration = 3
        )
        
        # Refresh data
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      },
      error = function(e) {
        showNotification(
          paste("Error deleting category:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Reresh data
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
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
              searching = T,
              targets = c(0,2),  # Hide ID (0-based index)
              visible = FALSE
            )
          ))
      )
    })
  })
}