# Define the datasetTrackerServer module
datasetTrackerServer <- function(id, pool, reporting_effort, dataset_type, refresh_trigger,reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive tracker data
    tracker_data <- reactive({
      req(reporting_effort())
      refresh_trigger()
      
      query <- sprintf(
        "
    SELECT
      rpt.id,
      ds.dataset_type,
      ds.dataset_name,
      ds.dataset_label,
      ds.category_name,
      prod.username AS production_programmer,
      qc.username AS qc_programmer,
      rpt.assign_date,
      rpt.due_date,
      rpt.priority,
      rpt.status
    FROM report_programming_tracker rpt
    INNER JOIN datasets ds
      ON rpt.report_id = ds.id
      AND rpt.report_type = ds.dataset_type
    LEFT JOIN users prod
      ON rpt.production_programmer_id = prod.id
    LEFT JOIN users qc
      ON rpt.qc_programmer_id = qc.id
    WHERE rpt.report_type = '%s' and rpt.reporting_effort_id = %d
    ORDER BY rpt.priority DESC, rpt.assign_date DESC;",
        dataset_type, as.integer(reporting_effort())
      )
      
      
      data <- tryCatch({
        dbGetQuery(pool, query)
      }, error = function(e) {
        showNotification(paste("Error loading tracker data:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(data)) {
        data$priority <- as.integer(data$priority)  # Convert priority to integer
        data$report_type <- data$dataset_type  # Add report_type column
      }
      
      data
    })
    
    # Server for programming efforts
    programmingEffortServer("programming_effort", tracker_data, reactive(input$column_selection))
    
    production_programmers <- reactive({
      tryCatch({
        refresh_trigger()
        dbGetQuery(pool, "SELECT id, username FROM users;")
      }, error = function(e) {
        showNotification(paste("Error loading production programmers:", e$message),
                         type = "error")
        NULL
      })
    })
    
    qc_programmers <- reactive({
      tryCatch({
        refresh_trigger()
        dbGetQuery(pool, "SELECT id, username FROM users;")
      }, error = function(e) {
        showNotification(paste("Error loading QC programmers:", e$message),
                         type = "error")
        NULL
      })
    })
    
    observeEvent(tracker_data(), {
      col_names <- colnames(tracker_data())
      updateCheckboxGroupInput(
        session,
        "column_selection",
        choices = col_names,
        selected = c(
          "id",
          "report_type",
          "dataset_type"
        ) # Default hidden column
      )
    })
    
    # Download handler for tracker data
    output$download_tracker <- downloadHandler(
      filename = function() {
        paste0(dataset_type,"_TRACKER_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- tracker_data()
        
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No data available to download.", type = "warning")
          return(NULL)
        }
        
        write.xlsx(df, file)
      }
    )
    
    # Render tracker data table
    output$tracker_table <- renderDT({
      req(tracker_data())
      df <- tracker_data()
      
      #print(names(df))
      
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
          "Dataset Type" = "dataset_type",
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
        showNotification("Please select a row before editing.", type = "warning")
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
              value = ifelse(
                is.na(row_data$assign_date),
                Sys.Date(),
                as.Date(row_data$assign_date)
              ),
              format = "yyyy-mm-dd"
            ),
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
              value = ifelse(
                is.na(row_data$due_date),
                Sys.Date() + 7,
                as.Date(row_data$due_date)
              ),
              format = "yyyy-mm-dd"
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
        showNotification("Production and QC programmer cannot be the same person.",
                         type = "warning")
        return()
      }
      
      if (due_date <= assign_date) {
        showNotification("Due date must be after the assign date.", type = "warning")
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
        
        showNotification("Record updated successfully.", type = "message")
        removeModal()
        refresh_trigger(refresh_trigger() + 1)
      }, error = function(e) {
        showNotification(paste("Error updating record:", e$message), type = "error")
      })
    })
  })
  
  return(refresh_trigger)
}
