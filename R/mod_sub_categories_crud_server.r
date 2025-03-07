mod_sub_categories_crud_server <- function(id, pool, tabs_input, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    categories <- reactive(tables_data$categories())
    subCategories <- reactive(tables_data$sub_categories())

    data <- reactive({
      refresh_trigger()
      req(categories(), subCategories())
      
      data <- subCategories() %>%
        left_join(categories(), by = c("category_id" = "id")) %>%
        select(id,
               category_id,
               category_name,
               sub_category_name,
               suggested_ich_number) %>%
        arrange(category_name, sub_category_name)
    })
    
    
    # Add Sub Category
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add Sub-Category"),
        div(
          class = "form-group",
          selectInput(
            ns("category_id"),
            "Category",
            choices = setNames(categories()$id, categories()$category_name)
          ),
          textInput(
            ns("sub_category_name"),
            "Sub Category Name",
            placeholder = "Enter subcategory name",
            width = "100%"
          ),
          textInput(
            ns("suggested_ich_number"),
            "Suggested ICH Number",
            placeholder = "16.x.x.x"
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
    
    observeEvent(input$sub_category_name, {
      shinyFeedback::hideFeedback("sub_category_name")
    })
    
    observeEvent(input$suggested_ich_number, {
      shinyFeedback::hideFeedback("suggested_ich_number")
    })
    
    
    observeEvent(input$confirm_add, {
      # Validate required fields
      if (is.null(input$sub_category_name) ||
          input$sub_category_name == "" ||
          is.null(input$suggested_ich_number) ||
          input$suggested_ich_number == "") {
        # Use shinyFeedback to provide feedback for invalid fields
        shinyFeedback::feedbackDanger(
          "sub_category_name",
          is.null(input$sub_category_name) ||
            input$sub_category_name == "",
          "Sub-category name is required."
        )
        shinyFeedback::feedbackDanger(
          "suggested_ich_number",
          is.null(input$suggested_ich_number) ||
            input$suggested_ich_number == "",
          "Suggested ICH number is required."
        )
        return()
      }
      
      # Proceed with adding logic
      tryCatch({
        # Use poolWithTransaction for transaction handling
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "INSERT INTO sub_categories (category_id, sub_category_name, suggested_ich_number, updated_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP)",
            params = list(
              input$category_id,
              input$sub_category_name,
              input$suggested_ich_number
            )
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        # Notify success and close the modal
        show_toast(
          title = "Add Sub-Category",
          type = "success",
          text = "Sub-category added successfully!",
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully
        show_toast(
          title = "Add Sub-Category",
          type = "error",
          text = paste("Error adding sub-category:", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Edit Category
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        show_toast(
          title = "Edit Sub-Category",
          type = "warning",
          text = "Please select a category to edit",
          position = "center"
        )
        return()
      }
      
      selected_data <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("edit", class = "text-warning me-2"),
          "Edit Category"
        ),
        div(
          class = "form-group mb-3",
          selectInput(
            ns("category_id"),
            "Category",
            choices = setNames(categories()$id, categories()$category_name),
            selected = selected_data$category_id
          ),
          textInput(
            ns("sub_category_name"),
            "Sub Category Name",
            value = selected_data$sub_category_name,
            width = "100%"
          ),
          textInput(
            ns("suggested_ich_number"),
            "Suggested ICH Number",
            value = selected_data$suggested_ich_number
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
      # Validate required fields
      if (is.null(input$sub_category_name) ||
          input$sub_category_name == "" ||
          is.null(input$suggested_ich_number) ||
          input$suggested_ich_number == "") {
        # Use shinyFeedback to provide feedback for invalid fields
        shinyFeedback::feedbackDanger(
          "sub_category_name",
          is.null(input$sub_category_name) ||
            input$sub_category_name == "",
          "Sub-category name is required."
        )
        shinyFeedback::feedbackDanger(
          "suggested_ich_number",
          is.null(input$suggested_ich_number) ||
            input$suggested_ich_number == "",
          "Suggested ICH number is required."
        )
        return()
      }
      
      # Proceed with editing logic
      tryCatch({
        selected <- input$table_rows_selected
        if (length(selected) == 0) {
          show_toast(
            title = "Edit Sub-Category",
            type = "warning",
            text = "No row selected for editing.",
            position = "top-end"
          )
          return()
        }
        
        sub_category_id <- data()[selected, "id"]
        updated_sub_category <- data.frame(
          id = sub_category_id,
          category_id = input$category_id,
          sub_category_name = input$sub_category_name,
          suggested_ich_number = input$suggested_ich_number,
          stringsAsFactors = FALSE
        )
        
        # Use poolWithTransaction for transaction handling
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "UPDATE sub_categories SET category_id = ?, sub_category_name = ?, suggested_ich_number = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
            params = list(
              updated_sub_category$category_id,
              updated_sub_category$sub_category_name,
              updated_sub_category$suggested_ich_number,
              updated_sub_category$id
            )
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        # Notify success and refresh UI
        show_toast(
          title = "Edit Sub-Category",
          type = "success",
          text = "Sub-category updated successfully!",
          position = "top-end"
        )
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully
        show_toast(
          title = "Edit Sub-Category",
          type = "error",
          text = paste("Error updating sub-category:", e$message),
          position = "top-end"
        )
      })
    })
    
    
    # Delete Category
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        show_toast(
          title = "Delete Sub-Category",
          type = "warning",
          text = "Please select a sub-category to delete",
          position = "center"
        )
        return()
      }
      
      selected_data <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("trash-alt", class = "text-danger me-2"),
          "Delete Sub-Category"
        ),
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle", class = "me-2"),
          "This action cannot be undone!"
        ),
        div(
          class = "mb-3",
          sprintf(
            "Are you sure you want to delete the sub-category '%s'?",
            selected_data$sub_category_name
          )
        ),
        footer = div(
          class = "modal-footer",
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
      req(input$table_rows_selected) # Ensure a row is selected
      
      tryCatch({
        selected <- input$table_rows_selected
        sub_category_id <- data()[selected, "id"]
        
        # Use poolWithTransaction for transaction handling
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn,
                    "DELETE FROM sub_categories WHERE id = ?",
                    params = list(sub_category_id))
        })
        
        # Notify the user of success
        show_toast(
          title = "Delete Sub-Category",
          type = "success",
          text = "Sub-category deleted successfully!",
          position = "top-end"
        )
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        # Handle errors gracefully
        show_toast(
          title = "Delete Sub-Category",
          type = "error",
          text = paste("Error deleting sub-category:", e$message),
          position = "top-end"
        )
      })
    })
    
    
    
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = c(
          "ID" = "id",
          "Category ID" = "category_id",
          "Category" = "category_name",
          "Sub Category" = "sub_category_name",
          "ICH Number" = "suggested_ich_number"
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(columnDefs = list(list(
          targets = c(0, 1), # Hide ID and Category ID columns (0-based index)
          visible = FALSE
        )))
      )
    })
    
  })
}