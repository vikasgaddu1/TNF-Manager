datasetsCRUDServer <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # refresh_trigger <- reactiveVal(0)
    # Create reactive for datasets table
    data <- reactive({
      # refresh_trigger()
      req(tables_data$datasets)
      tables_data$datasets()
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
    
    # Clear feedback when input changes
    observeEvent(input$dataset_name, {
      shinyFeedback::hideFeedback("dataset_name")
    })
    
    observeEvent(input$dataset_label, {
      shinyFeedback::hideFeedback("dataset_label")
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
      # Validate required fields
      if (is.null(input$dataset_type) || 
          is.null(input$category_name) || 
          is.null(input$dataset_name) || 
          input$dataset_name == "" ||
          is.null(input$dataset_label) || 
          input$dataset_label == "") {
        
        shinyFeedback::feedbackDanger(
          "dataset_name",
          is.null(input$dataset_name) || input$dataset_name == "",
          "Dataset name is required."
        )
        shinyFeedback::feedbackDanger(
          "dataset_label",
          is.null(input$dataset_label) || input$dataset_label == "",
          "Dataset label is required."
        )
        return()
      }
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "INSERT INTO datasets (dataset_type, category_name, dataset_name, dataset_label, updated_at) 
             VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
            params = list(
              input$dataset_type,
              input$category_name,
              input$dataset_name,
              input$dataset_label
            )
          )
        })
                # track datasets update
        # refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Add Dataset",
          type = "success",
          text = "Dataset added successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Add Dataset",
          type = "error",
          text = paste("Error adding dataset:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Edit Dataset
    observeEvent(input$edit, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        show_toast(
          title = "Edit Dataset",
          type = "warning",
          text = "Please select a dataset to edit",
          position = "center"
        )
        return()
      }
      
      dataset <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("edit", class = "text-warning me-2"),
          "Edit Dataset"
        ),
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
      # Validate required fields
      if (is.null(input$dataset_type) || 
          is.null(input$category_name) || 
          is.null(input$dataset_name) || 
          input$dataset_name == "" ||
          is.null(input$dataset_label) || 
          input$dataset_label == "") {
        
        shinyFeedback::feedbackDanger(
          "dataset_name",
          is.null(input$dataset_name) || input$dataset_name == "",
          "Dataset name is required."
        )
        shinyFeedback::feedbackDanger(
          "dataset_label",
          is.null(input$dataset_label) || input$dataset_label == "",
          "Dataset label is required."
        )
        return()
      }
      
      tryCatch({
        selected <- input$table_rows_selected
        
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "UPDATE datasets 
             SET dataset_type = ?, 
                 category_name = ?, 
                 dataset_name = ?, 
                 dataset_label = ?,
                 updated_at = CURRENT_TIMESTAMP
             WHERE id = ?",
            params = list(
              input$dataset_type,
              input$category_name,
              input$dataset_name,
              input$dataset_label,
              data()[selected, "id"]
            )
          )
        })

        # track datasets updates
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "INSERT INTO datasets_updates (dataset_id) VALUES (?)",
            params = list(data()[selected, "id"])
          )
        })
        # refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Edit Dataset",
          type = "success",
          text = "Dataset updated successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Edit Dataset",
          type = "error",
          text = paste("Error updating dataset:", e$message),
          position = "top-end"
        )
      })
    })
    
    # Delete Dataset
    observeEvent(input$delete, {
      selected <- input$table_rows_selected
      
      if (length(selected) == 0) {
        show_toast(
          title = "Delete Dataset",
          type = "warning",
          text = "Please select a dataset to delete",
          position = "center"
        )
        return()
      }
      
      dataset <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(
          class = "d-flex align-items-center",
          icon("trash-alt", class = "text-danger me-2"),
          "Delete Dataset"
        ),
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle", class = "me-2"),
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
      dataset_id <- data()[selected, "id"]
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "DELETE FROM datasets WHERE id = ?",
            params = list(dataset_id)
          )
        })
        # refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Delete Dataset",
          type = "success",
          text = "Dataset deleted successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Delete Dataset",
          type = "error",
          text = paste("Error deleting dataset:", e$message),
          position = "top-end"
        )
      })
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
          "Dataset Label" = "dataset_label"
    
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(
          columnDefs = list(
            list(
              targets = c(0,5),  # Hide ID column
              visible = FALSE
            )
          )
        )
      )
    })
  })
}