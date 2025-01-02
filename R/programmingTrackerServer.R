programmingTrackerServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "tracker") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing tracker data",
                         type = "message",
                         duration = 1)
      }
    }, ignoreInit = TRUE)
    
    # Load Reporting Efforts into dropdown
    observeEvent(refresh_trigger(), {
      reporting_efforts <- tryCatch({
        # DO NOT call refresh_trigger() here again
        dbGetQuery(
          pool,
          "SELECT id,
              study || ' | ' || database_release || ' | ' || reporting_effort AS label
       FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message),
                         type = "error")
        NULL
      })
      
      if (!is.null(reporting_efforts) &&
          nrow(reporting_efforts) > 0) {
        current_effort <- isolate(input$reporting_effort) # Remember current selection
        choices <- setNames(reporting_efforts$id, reporting_efforts$label)
        
        updateSelectInput(
          session,
          "reporting_effort",
          choices = choices,
          # If the current effort is still in the new choices, reselect it
          selected = if (!is.null(current_effort) &&
                         current_effort %in% reporting_efforts$id)
            current_effort
          else
            reporting_efforts$id[1]
        )
      }
    })
    
    
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
    
    tracker_data <- reactive({
      req(input$reporting_effort)
      refresh_trigger()
      
      query <- sprintf(
        "
  SELECT
    rpt.id,
    re.study,
    re.database_release,
    re.reporting_effort,
    r.report_key,
    r.report_type,
    r.title_key,
    c.category_name AS category,
    sc.sub_category_name AS subcategory,
    r.report_ich_number AS ich_number,
    p.population_text AS population,
    GROUP_CONCAT(t.title_text, ' | ') AS titles,
    GROUP_CONCAT(f.footnote_text, ', ') AS footnotes,
    prod.username AS production_programmer,
    qc.username AS qc_programmer,
    rpt.assign_date,
    rpt.due_date,
    rpt.priority,
    rpt.status
  FROM report_programming_tracker rpt
  INNER JOIN reporting_efforts re
    ON rpt.reporting_effort_id = re.id
  INNER JOIN reports r
    ON rpt.report_id = r.id
  LEFT JOIN reporting_effort_reports rer
    ON rer.reporting_effort_id = re.id
    AND rer.report_id = r.id
  LEFT JOIN categories c
    ON r.report_category_id = c.id
  LEFT JOIN sub_categories sc
    ON r.report_sub_category_id = sc.id
  LEFT JOIN populations p
    ON r.population_id = p.id
  LEFT JOIN report_titles rt
    ON r.id = rt.report_id
  LEFT JOIN titles t
    ON rt.title_id = t.id
  LEFT JOIN report_footnotes rf
    ON r.id = rf.report_id
  LEFT JOIN footnotes f
    ON rf.footnote_id = f.id
  LEFT JOIN users prod
    ON rpt.production_programmer_id = prod.id
  LEFT JOIN users qc
    ON rpt.qc_programmer_id = qc.id
  WHERE rpt.reporting_effort_id = %d 
  GROUP BY
    rpt.id,
    re.study,
    re.database_release,
    re.reporting_effort,
    r.report_key,
    r.report_type,
    c.category_name,
    sc.sub_category_name,
    r.report_ich_number,
    p.population_text,
    prod.username,
    qc.username,
    rpt.assign_date,
    rpt.due_date,
    rpt.status
  ",
        as.integer(input$reporting_effort)
      )
      
      observeEvent(tracker_data(), {
        col_names <- colnames(tracker_data())
        updateCheckboxGroupInput(
          session,
          "column_selection",
          choices = col_names,
          selected = c("id","study","database_release","reporting_effort", "category","subcategory","ich_number","footnotes") # Default hidden column
        )
      })
      
      
      data <- tryCatch({
        dbGetQuery(pool, query)
      }, error = function(e) {
        showNotification(paste("Error loading tracker data:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(data)) {
        data$priority <- as.integer(data$priority)  # Convert priority to integer
      }
      
      data
    })
    

    # Download handler for tracker data
    output$download_tracker <- downloadHandler(
      filename = function() {
        paste0("tracker_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- tracker_data()
        
        # Check if data is available
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No data available to download.", type = "warning")
          return(NULL)
        }
        
        # Write data to an Excel file
        write.xlsx(df, file)
      }
    )
    
    programmingEffortServer("programming_effort", tracker_data = tracker_data)
    
    
    
    output$tracker_table <- renderDT({
      req(tracker_data())
      df <- tracker_data()
      
      # If df is empty, return a notification or a placeholder
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data found")))
      }
      
      hidden_cols <- input$column_selection
      col_defs <- lapply(seq_along(colnames(df)), function(i) {
        list(
          targets = i - 1, # JavaScript column index (0-based)
          visible = !(colnames(df)[i] %in% hidden_cols)
        )
      })
  
      DT::datatable(
        df,
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = col_defs
        ),
        colnames = c(
          "id" = "id",
          "Study" = "study",
          "Database Release" = "database_release",
          "Reporting Effort" = "reporting_effort",
          "Report Key" = "report_key",
          "Report Type" = "report_type",
          "Title Key" = "title_key",
          "Category" = "category",
          "Subcategory" = "subcategory",
          "ICH Number" = "ich_number",
          "Population" = "population",
          "Titles" = "titles",
          "Footnotes" = "footnotes",
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
    
    # Storing selected_id for update
    selected_id <- reactiveVal(NULL)
    
    observeEvent(input$edit_button, {
      selected_row <- input$tracker_table_rows_selected
      
      if (length(selected_row) == 0) {
        showNotification("Please select a row before editing.", type = "warning")
        return()
      }
      
      # Set `selected_id` to the `id` of the selected row
      if (length(selected_row) > 0) {
        df <- tracker_data()
        row_data <- df[selected_row, ]
        # print(paste("Selected row:", selected_row))
        # print(paste("Row data:", toString(row_data)))
        selected_id(row_data$id)
        # print(paste("Set selected_id to:", selected_id()))
      }
      
      if (is.null(df) || nrow(df) < selected_row) {
        showNotification("Invalid selection. Please refresh and try again.", type = "error")
        return()
      }
      

      
      if (is.null(selected_id())) {
        showNotification("Could not capture the selected ID. Please refresh and try again.", type = "error")
        return()
      }
      
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
        title = "Edit Report Programming Details",
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
              selected = ifelse(is.na(row_data$status), "Not Started", row_data$status)
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
              # Numeric range
              selected = ifelse(
                is.na(row_data$priority),
                1,
                as.integer(row_data$priority)
              )
            )
          )
        )
      )
      
      
      showModal(edit_modal)
    })
    
    observeEvent(input$save_changes, {
      if (is.null(selected_id())) {
        showNotification("No valid ID selected for update. Please try again.", type = "error")
        return()
      }
      
      prod_id <- input$production_programmer
      qc_id <- input$qc_programmer
      assign_date <- input$assign_date
      due_date <- input$due_date
      priority <- input$priority
      status <- input$status
      
      # Validation Checks
      if (!priority %in% 1:5) {
        showNotification("Priority must be between 1 (Highest) and 5 (Lowest).", type = "error")
        return()
      }
      
      if (prod_id == qc_id) {
        showNotification("Production and QC programmer cannot be the same person.", type = "warning")
        return()
      }
      
      if (due_date <= assign_date) {
        showNotification("Due date must be after the assign date.", type = "warning")
        return()
      }
      
      if (!selected_id() %in% tracker_data()$id) {
        showNotification("Invalid selection. Please refresh and try again.", type = "error")
        return()
      }
      
      cat("Selected ID:", selected_id(), "\n")
      tryCatch({
        dbExecute(
          pool,
          "UPDATE report_programming_tracker
       SET production_programmer_id = ?,
           assign_date = ?,
           qc_programmer_id = ?,
           due_date = ?,
           priority = ?,
           status = ?
       WHERE id = ? AND report_type = 'TFL';",
          params = list(
            prod_id,
            as.character(assign_date),
            qc_id,
            as.character(due_date),
            as.integer(priority),
            status,
            selected_id()
          )
        )
        
        showNotification("Record updated successfully.", type = "message")
        removeModal()
        
        # Trigger a refresh of the tracker data
        refresh_trigger(refresh_trigger() + 1)
      }, error = function(e) {
        showNotification(paste("Error updating record:", e$message), type = "error")
      })
    })

  })
}
