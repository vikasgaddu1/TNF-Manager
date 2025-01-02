datasetsCRUDServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    data <- reactive({
      refresh_trigger()  # Add dependency on refresh trigger
      dbReadTable(pool, "datasets")  # Read from dataset table
    })
    
    get_categories <- function(dataset_type) {
      if (dataset_type == "SDTM") {
        return(c("Special Purpose", "Findings", "Event", "Intervention", "Trial Domains", "RELREC"))
      } else if (dataset_type == "ADaM") {
        return(c("ADSL", "BDS", "TTE", "OCC"))
      } else {
        return(NULL)
      }
    }
    
    # Add Dataset
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add Dataset"),
        div(
          class = "form-group",
          selectInput(
            ns("dataset_type"),
            "Dataset Type",
            choices = c("SDTM", "ADaM"),
            selected = NULL
          ),
          uiOutput(ns("category_select")),
          textInput(
            ns("dataset_name"),
            "Dataset Name",
            placeholder = "Enter dataset name",
            width = "100%"
          ),
          textInput(
            ns("dataset_label"),
            "Dataset Label",
            placeholder = "Enter dataset label",
            width = "100%"
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_add"),
            "Add Dataset",
            class = "btn btn-primary",
            icon = icon("plus")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    output$category_select <- renderUI({
      req(input$dataset_type)
      selectInput(
        ns("category_name"),
        "Category Name",
        choices = get_categories(input$dataset_type),
        selected = NULL
      )
    })
    
    observeEvent(input$confirm_add, {
      req(input$dataset_type, input$category_name, input$dataset_name, input$dataset_label)
      
      tryCatch({
        new_dataset <- data.frame(
          dataset_type = input$dataset_type,
          category_name = input$category_name,
          dataset_name = input$dataset_name,
          dataset_label = input$dataset_label,
          stringsAsFactors = FALSE
        )
        dbWriteTable(pool,
                     "datasets",
                     new_dataset,
                     append = TRUE,
                     row.names = FALSE)
        
        showNotification("Dataset added successfully!",
                         type = "message",
                         duration = 3)
        
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error adding dataset:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Edit Dataset
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          "Please select a dataset to edit",
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      dataset <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("edit"), "Edit Dataset"),
        div(
          class = "form-group",
          selectInput(
            ns("dataset_type"),
            "Dataset Type",
            choices = c("SDTM", "ADaM"),
            selected = dataset$dataset_type
          ),
          uiOutput(ns("category_select_edit")),
          textInput(
            ns("dataset_name"),
            "Dataset Name",
            value = dataset$dataset_name,
            width = "100%"
          ),
          textInput(
            ns("dataset_label"),
            "Dataset Label",
            value = dataset$dataset_label,
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
    
    output$category_select_edit <- renderUI({
      req(input$dataset_type)
      selectInput(
        ns("category_name"),
        "Category Name",
        choices = get_categories(input$dataset_type),
        selected = data()[input$table_rows_selected, "category_name"]
      )
    })
    
    observeEvent(input$confirm_edit, {
      req(input$dataset_type, input$category_name, input$dataset_name, input$dataset_label)
      selected <- input$table_rows_selected
      
      tryCatch({
        dbExecute(
          pool,
          "UPDATE datasets SET dataset_type = ?, category_name = ?, dataset_name = ?, dataset_label = ? WHERE id = ?",
          params = list(input$dataset_type, input$category_name, input$dataset_name, input$dataset_label, data()[selected, "id"])
        )
        
        showNotification("Dataset updated successfully!",
                         type = "message",
                         duration = 3)
        
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error updating dataset:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Delete Dataset
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        showNotification(
          "Please select a dataset to delete",
          type = "warning",
          duration = 3,
          closeButton = TRUE
        )
        return()
      }
      
      dataset <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("trash"), "Delete Dataset"),
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          "This action cannot be undone!"
        ),
        div(
          class = "mb-3",
          sprintf("Are you sure you want to delete the dataset '%s'?", 
                  dataset$dataset_name)
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_delete"),
            "Delete Dataset",
            class = "btn btn-danger",
            icon = icon("trash")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_delete, {
      req(input$table_rows_selected)
      selected <- input$table_rows_selected
      dataset_id <- data()[selected, "id"]
      
      tryCatch({
        dbExecute(
          pool, 
          "DELETE FROM datasets WHERE id = ?", 
          params = list(dataset_id)
        )
        
        showNotification(
          "Dataset deleted successfully!",
          type = "message",
          duration = 3
        )
        
        refresh_trigger(refresh_trigger() + 1)
        
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Error deleting dataset:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Refresh Data
    observeEvent(input$refresh, {
      refresh_trigger(refresh_trigger() + 1)
    })
    
    output$table <- DT::renderDataTable({
      DT::datatable(
        data(),
        filter = "top",
        colnames = c(
          "ID" = "id", 
          "Dataset Type" = "dataset_type", 
          "Category Name" = "category_name",
          "Dataset Name" = "dataset_name",
          "Dataset Label" = "dataset_label",
          "Last Updated" = "updated_at"
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(
          columnDefs = list(
            list(
              targets = c(0),  # Hide ID column
              visible = FALSE
            )
          )
        )
      )
    })
  })
}