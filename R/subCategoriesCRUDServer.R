subCategoriesCRUDServer <- function(id, pool,tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)

    # Auto-refresh categories when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "Sub Categories") {
        # Add your logic here for when "sub_categories" is selected
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing sub-categories", type = "message", duration = 1)
      }
    }, ignoreInit = TRUE)
   
    
    # Using dplyr if you prefer:
    data <- reactive({
      refresh_trigger()
      
      tbl_categories <- dbReadTable(pool, "categories") %>%
        select(id, category_name)
      tbl_sub_categories <- dbReadTable(pool, "sub_categories")
      
      tbl_sub_categories %>%
        left_join(tbl_categories, by = c("category_id" = "id")) %>%
        select(
          id,
          category_id,
          category_name,
          sub_category_name,
          suggested_ich_number,
          updated_at
        ) %>%
        arrange(category_name, sub_category_name) 
      #%>% 
      #  filter(!is.na(category_name))
    })
    
    categories <- reactive({
      refresh_trigger()
      dbReadTable(pool, "categories")
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
    
    
    observeEvent(input$confirm_add, {
      req(input$sub_category_name,input$suggested_ich_number)  # Validate input
      
      tryCatch({
        new_sub_category <- data.frame(
          category_id = input$category_id,
          sub_category_name = input$sub_category_name,
          suggested_ich_number = input$suggested_ich_number,
          stringsAsFactors = FALSE
        )
        dbWriteTable(
          pool,
          "sub_categories",
          new_sub_category,
          append = TRUE,
          row.names = FALSE
        )
        
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
    
    observeEvent(input$confirm_edit, {
      selected <- input$table_rows_selected
      sub_category_id <- data()[selected, "id"]
      updated_sub_category <- data.frame(
        id = sub_category_id,
        category_id = input$category_id,
        sub_category_name = input$sub_category_name,
        suggested_ich_number = input$suggested_ich_number,
        stringsAsFactors = FALSE
      )
      dbExecute(pool, "UPDATE sub_categories SET category_id = ?, sub_category_name = ?, suggested_ich_number = ? WHERE id = ?",
                params = list(updated_sub_category$category_id, updated_sub_category$sub_category_name, updated_sub_category$suggested_ich_number, updated_sub_category$id))
      refresh_trigger(refresh_trigger() + 1)
      removeModal()
    })
    
    # Delete Category
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          "Please select a sub-category to delete",
          type = "warning",
          duration = 3,
          closeButton = TRUE
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
          sprintf("Are you sure you want to delete the sub-category '%s'?", 
                  selected_data$sub_category_name)
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
      sub_category_id <- data()[selected, "id"]
      dbExecute(pool, "DELETE FROM sub_categories WHERE id = ?", params = list(sub_category_id))
      refresh_trigger(refresh_trigger() + 1)
      removeModal()
    })
    
    # Reresh data
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        colnames = c(
          "ID" = "id",
          "Category ID" = "category_id",
          "Category" = "category_name",
          "Sub Category" = "sub_category_name",
          "ICH Number" = "suggested_ich_number",
          "Last Updated" = "updated_at"
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(
          columnDefs = list(
            list(
              targets = c(0, 1,5),  # Hide ID and Category ID columns (0-based index)
              visible = FALSE
            )
          ))
      )
    })
    
  })
}