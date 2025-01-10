# Define the datasetTrackerServer module
datasetTrackerServer <- function(id, pool, reporting_effort, ds_type, tables_data,reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive tracker data
    tracker_data <- reactive({
      req(reporting_effort(), tables_data)
      
      data <- tables_data$report_programming_tracker() %>%
        dplyr::filter(reporting_effort_id == reporting_effort(), report_type == ds_type) %>%
        dplyr::inner_join(tables_data$datasets(), join_by(report_id == id, report_type == dataset_type)) %>%
        dplyr::left_join(
          tables_data$users() %>% dplyr::select(id, username ) %>% dplyr::rename(production_programmer = username),
          by = c("production_programmer_id" = "id")) %>%
        dplyr::left_join(
          tables_data$users() %>% dplyr::select(id, username) %>% dplyr::rename(qc_programmer = username),
          by = c("qc_programmer_id" = "id"))  %>%
        mutate(priority = as.integer(priority)) %>%
        dplyr::select(
          id,
          report_type,
          dataset_name,
          dataset_label,
          category_name,
          production_programmer,
          qc_programmer,
          assign_date,
          due_date,
          priority,
          status
        ) %>%
        dplyr::arrange(desc(priority), desc(assign_date))
      data
    })
    # Server for programming efforts
    programmingEffortServer("programming_effort", tracker_data, reactive(input$column_selection))
    
    production_programmers <- reactive({  
      tables_data$users() %>%
        dplyr::select(id, username)
    })
    
    qc_programmers <- reactive({
      tables_data$users() %>%
        dplyr::select(id, username)
    })

    observeEvent(tracker_data(), {
      col_names <- colnames(tracker_data())
      updateCheckboxGroupInput(
        session,
        "column_selection",
        choices = col_names,
        selected = c(
          "id",
          "report_type"
        ) # Default hidden column
      )
    })
    
    # Download handler for tracker data
    output$download_tracker <- downloadHandler(
      filename = function() {
        paste0(ds_type,"_TRACKER_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- tracker_data()
        
        if (is.null(df) || nrow(df) == 0) {
          show_toast(
            title = "Download",
            type = "info",
            text = "No data available to download.",
            position = "center"
          )
          return(NULL)
        }
        
        write.xlsx(df, file)
      }
    )
    
    # Render tracker data table
    output$tracker_table <- renderDT({
      req(tracker_data())
      df <- tracker_data()
      
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data found")))
      }
      
      hidden_cols <- input$column_selection
      col_defs <- lapply(seq_along(colnames(df)), function(i) {
        list(targets = i - 1,
             # JavaScript column index (0-based)
             visible = !(colnames(df)[i] %in% hidden_cols))
      })
      
      DT::datatable(
        df,
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, autoWidth = TRUE, columnDefs = col_defs),
        colnames = c(
          "ID" = "id",
          "Dataset Type" = "report_type",
          "Category" = "category_name",
          "Dataset Name" = "dataset_name",
          "Dataset Label" = "dataset_label",
          "Production Programmer" = "production_programmer",
          "QC Programmer" = "qc_programmer",
          "Assign Date" = "assign_date",
          "Due Date" = "due_date",
          "Priority" = "priority",
          "Status" = "status"
        )
      ) %>%
        DT::formatDate("Assign Date", method = "toLocaleDateString") %>%
        DT::formatDate("Due Date", method = "toLocaleDateString") %>%
        DT::formatStyle("Status",
                        target = "row",
                        backgroundColor = DT::styleEqual(
                          c(
                            "Not Started",
                            "Production Started",
                            "Production Ready",
                            "Under QC",
                            "QC Failed",
                            "QC Pass"
                          ),
                          c(
                            "#E0E0E0",
                            "#BBE1FA",
                            "#C8E6C9",
                            "#FFF9C4",
                            "#FFCDD2",
                            "#A5D6A7"
                          )
                        ))
    })
    
    # Observe and handle row editing
    observeEvent(input$edit_button, {
      selected_row <- input$tracker_table_rows_selected
      
      if (length(selected_row) == 0) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Please select a row before editing.",
          position = "center"
        )
        return()
      }
      
      df <- tracker_data()
      row_data <- df[selected_row, ]
      
      # Validate if we have programmers to select from
      prod_prog <- production_programmers()
      qc_prog <- qc_programmers()
      
      # If no production programmers found, set to empty
      if (is.null(prod_prog) || nrow(prod_prog) == 0) {
        prod_choices <- c("Not Assigned" = "")
      } else {
        prod_choices <- c("Not Assigned" = "",
                          setNames(prod_prog$id, prod_prog$username))
      }
      
      # If no QC programmers found, set to empty
      if (is.null(qc_prog) || nrow(qc_prog) == 0) {
        qc_choices <- c("Not Assigned" = "")
      } else {
        qc_choices <- c("Not Assigned" = "",
                        setNames(qc_prog$id, qc_prog$username))
      }
      
      # Determine the currently selected production programmer's ID
      current_prod_id <- if (!is.na(row_data$production_programmer) &&
                             row_data$production_programmer != "") {
        prod_prog$id[prod_prog$username == row_data$production_programmer]
      } else {
        ""  # Not Assigned
      }
      
      # Determine the currently selected QC programmer's ID
      current_qc_id <- if (!is.na(row_data$qc_programmer) &&
                           row_data$qc_programmer != "") {
        qc_prog$id[qc_prog$username == row_data$qc_programmer]
      } else {
        ""  # Not Assigned
      }
      
      edit_modal <- modalDialog(
        title = "Edit Dataset Programming Details",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("save_changes"), "Save", class = "btn btn-success"),
          modalButton("Cancel")
        ),
        
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("production_programmer"),
              "Production Programmer:",
              choices = prod_choices,
              selected = ifelse(length(current_prod_id) == 1, current_prod_id, "")
            ),
            dateInput(
              ns("assign_date"),
              "Assign Date:",
              value = if(is.na(row_data$assign_date)) {
                Sys.Date()
              } else {
                as.Date(row_data$assign_date, format = "%Y-%m-%d")
              }),
            selectInput(
              ns("status"),
              "Status:",
              choices = c(
                "Not Started",
                "Production Started",
                "Production Ready",
                "Under QC",
                "QC Failed",
                "QC Pass"
              ),
              selected = row_data$status
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("qc_programmer"),
              "QC Programmer:",
              choices = qc_choices,
              selected = ifelse(length(current_qc_id) == 1, current_qc_id, "")
            ),
            dateInput(
              ns("due_date"),
              "Due Date:",
              value = if(is.na(row_data$due_date)) {
                Sys.Date() + 7
              } else {
                as.Date(row_data$due_date, format = "%Y-%m-%d")
              }
            ),
            selectInput(
              ns("priority"),
              "Priority (1 <- Highest 5 <- Lowest):",
              choices = 1:5,
              selected = row_data$priority
            )
          )
        )
      )
      
      showModal(edit_modal)
    })
    
    # Save changes to the database
    observeEvent(input$save_changes, {
      selected_row <- input$tracker_table_rows_selected
      df <- tracker_data()
      row_data <- df[selected_row, ]
      
      prod_id <- input$production_programmer
      qc_id <- input$qc_programmer
      assign_date <- input$assign_date
      due_date <- input$due_date
      priority <- input$priority
      status <- input$status
      
      if (prod_id == qc_id) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Production and QC programmer cannot be the same person.",
          position = "center"
        )
        return()
      }
      
      if (due_date <= assign_date) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Due date must be after the assign date.",
          position = "center"
        )
        return()
      }
      
      tryCatch({
        dbExecute(
          pool,
          "UPDATE report_programming_tracker
           SET production_programmer_id = ?, 
               qc_programmer_id = ?, 
               assign_date = ?, 
               due_date = ?, 
               priority = ?, 
               status = ?
           WHERE id = ?;",
          params = list(prod_id, qc_id, as.character(assign_date), as.character(due_date), as.integer(priority), status, row_data$id)
        )
        
        show_toast(
          title = "Edit",
          type = "success",
          text = "Record updated successfully.",
          position = "center"
        )
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Edit",
          type = "error",
          text = paste("Error updating record:", e$message),
          position = "center"
        )
      })
    })
  })
  }
