reportProgrammingTrackerServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Refresh trigger
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh when the tab is activated
    observeEvent(tabs_input(), {
      if (tabs_input() == "tracker") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing tracker data...", type = "message", duration = 1)
      }
    }, ignoreInit = TRUE)
    
    # Load Reporting Efforts
    reporting_efforts <- reactive({
      refresh_trigger() # Refresh when trigger changes
      tryCatch({
        dbGetQuery(pool, "SELECT id, study || '#' || database_release || '#' || reporting_effort AS label FROM reporting_efforts")
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message), type = "error")
        NULL
      })
    })
    
    # Update the reporting effort dropdown
    observeEvent(reporting_efforts(), {
      efforts <- reporting_efforts()
      if (!is.null(efforts)) {
        updateSelectInput(
          session,
          "reporting_effort",
          choices = setNames(efforts$id, efforts$label)
        )
      }
    })
    
    # Load Tracker Data
    tracker_data <- reactive({
      req(input$reporting_effort)
      refresh_trigger() # Refresh when trigger changes
      tryCatch({
        dbGetQuery(
          pool,
          paste(
            "SELECT 
                rpt.id AS 'Tracker ID', 
                rer.report_id AS 'Report ID',
                r.report_key AS 'Report Key', 
                r.report_type AS 'Report Type',
                c.category_name AS 'Category',
                sc.sub_category_name AS 'Subcategory',
                t.title_text AS 'Title',
                GROUP_CONCAT(f.footnote_text, ', ') AS 'Footnotes',
                pp.username AS 'Production Programmer', 
                rpt.assign_date AS 'Assign Date', 
                qp.username AS 'QC Programmer', 
                rpt.due_date AS 'Due Date', 
                rpt.status AS 'Status'
             FROM reporting_effort_reports rer
             LEFT JOIN reports r ON rer.report_id = r.id
             LEFT JOIN categories c ON r.report_category_id = c.id
             LEFT JOIN sub_categories sc ON r.report_sub_category_id = sc.id
             LEFT JOIN report_titles rt ON r.id = rt.report_id
             LEFT JOIN titles t ON rt.title_id = t.id
             LEFT JOIN report_footnotes rf ON r.id = rf.report_id
             LEFT JOIN footnotes f ON rf.footnote_id = f.id
             LEFT JOIN report_programming_tracker rpt 
               ON rer.report_id = rpt.report_id AND rer.reporting_effort_id = rpt.reporting_effort_id
             LEFT JOIN users pp ON rpt.production_programmer_id = pp.id
             LEFT JOIN users qp ON rpt.qc_programmer_id = qp.id
             WHERE rer.reporting_effort_id = ", input$reporting_effort, "
             GROUP BY rer.report_id, rpt.id, r.report_key, r.report_type, c.category_name, sc.sub_category_name, 
                      t.title_text, pp.username, rpt.assign_date, qp.username, rpt.due_date, rpt.status;"
          )
        )
      }, error = function(e) {
        showNotification(paste("Error loading tracker data:", e$message), type = "error")
        NULL
      })
    })
    

    # Render DataTable
    output$tracker_table <- DT::renderDataTable({
      req(tracker_data())
      data <- tracker_data()
      if (nrow(data) == 0) {
        return(NULL)
      }
      print(data)
      # Create unique button IDs
      data$Actions <- sprintf(
        '<button onclick="Shiny.setInputValue(\'%s\', this.id);" id="edit_%s" class="btn btn-primary btn-sm">Edit</button>',
        ns("edit_button"), # Use ns() to namespace the input
        data$`Tracker ID`
      )
      
      datatable(
        data,
        escape = FALSE,
        selection = "none",
        colnames = c(
          "Tracker ID", "Report ID", "Report Key", "Report Type", "Category", "Subcategory",
          "Title", "Footnotes", "Production Programmer", "Assign Date", "QC Programmer", "Due Date", "Status", "Actions"
        ),
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(list(targets = c(0, 1), visible = FALSE)) # Hide Tracker ID and Report ID
        )
      )
    })
    
    
    # Create a reactive value to store the selected row
    selected_row <- reactiveVal(NULL)
    
    # Observe Edit Button Click
    observeEvent(input$edit_button, {
      tracker_id <- as.integer(sub("edit_", "", input$edit_button)) # Extract Tracker ID
      
      # Ensure the tracker_id is valid and matches in tracker_data
      selected <- tracker_data()[tracker_data()$`Tracker ID` == tracker_id, ]
      if (nrow(selected) == 0) {
        showNotification("Unable to find the selected row. Please refresh the data.", type = "error")
        return()
      }
      selected_row(selected) # Set the selected row
      print(tracker_id)
      # Show Modal
      showModal(
        modalDialog(
          title = "Edit Tracker Entry",
          selectInput(
            ns("edit_production_programmer"), "Production Programmer",
            choices = dbGetQuery(pool, "SELECT username FROM users;")$username,
            selected = selected_row()$`Production Programmer`
          ),
          dateInput(
            ns("edit_assign_date"), "Assign Date"),
          selectInput(
            ns("edit_qc_programmer"), "QC Programmer",
            choices = dbGetQuery(pool, "SELECT username FROM users;")$username,
            selected = selected_row()$`QC Programmer`
          ),
          dateInput(
            ns("edit_due_date"), "Due Date"),
          selectInput(
            ns("edit_status"), "Status",
            choices = c('Not Started', 'Production Started', 'Production Ready', 
                        'Under QC', 'QC Failed', 'QC Pass'),
            selected = selected_row()$Status
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_edit"), "Save Changes", class = "btn btn-primary")
          )
        )
      )
    })
    
    
    
    
    # Save Modal Changes
    # Save Modal Changes
    observeEvent(input$save_edit, {
      req(input$edit_production_programmer, input$edit_assign_date, 
          input$edit_qc_programmer, input$edit_due_date, input$edit_status,
          selected_row())
      
      tryCatch({
        query <- sprintf(
          "INSERT INTO report_programming_tracker 
       (reporting_effort_id, report_id, production_programmer_id, assign_date, 
        qc_programmer_id, due_date, status)
       VALUES (%d, %d, 
               (SELECT id FROM users WHERE username = '%s'), '%s', 
               (SELECT id FROM users WHERE username = '%s'), '%s', '%s')
       ON CONFLICT (reporting_effort_id, report_id) DO UPDATE SET
           production_programmer_id = (SELECT id FROM users WHERE username = '%s'),
           assign_date = '%s',
           qc_programmer_id = (SELECT id FROM users WHERE username = '%s'),
           due_date = '%s',
           status = '%s';",
          input$reporting_effort, selected_row()$`Report ID`,
          input$edit_production_programmer, as.character(input$edit_assign_date), # Properly format date
          input$edit_qc_programmer, as.character(input$edit_due_date), input$edit_status, # Properly format date
          input$edit_production_programmer, as.character(input$edit_assign_date),
          input$edit_qc_programmer, as.character(input$edit_due_date), input$edit_status
        )
        
        # Execute the query
        dbExecute(pool, query)
        
        showNotification("Changes saved successfully.", type = "message")
        refresh_trigger(refresh_trigger() + 1) # Refresh data
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error saving changes:", e$message), type = "error")
      })
    })
    
    
    

  })
}
