reportingEffortReportsServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh reporting efforts when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "re_reports") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing reporting efforts", type = "message", duration = 1)
      }
    }, ignoreInit = TRUE)
    
    # Load Reporting Efforts
    observeEvent(refresh_trigger(), {
      reporting_efforts <- tryCatch({
        dbGetQuery(
          pool,
          "SELECT id, 
                  study || '#' || database_release || '#' || reporting_effort AS reporting_effort_label 
           FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(reporting_efforts)) {
        updateSelectInput(
          session,
          "reporting_effort",
          choices = setNames(
            reporting_efforts$id, # Values (ids)
            reporting_efforts$reporting_effort_label # Labels (concatenated text)
          )
        )
      }
    })
    
    observeEvent(refresh_trigger(), {
      tryCatch({
        # Step 1: Insert Missing Records
        dbExecute(pool, "
      INSERT INTO report_programming_tracker (
          reporting_effort_id, 
          report_id, 
          production_programmer_id, 
          qc_programmer_id, 
          assign_date, 
          due_date, 
          priority, 
          status
      )
      SELECT 
          rer.reporting_effort_id,
          rer.report_id,
          NULL,  -- Default value for production_programmer_id
          NULL,  -- Default value for qc_programmer_id
          NULL,  -- Default assign_date
          NULL,  -- Default due_date
          1,     -- Default priority (lowest)
          'Not Started'  -- Default status
      FROM 
          reporting_effort_reports rer
      LEFT JOIN 
          report_programming_tracker rpt
      ON 
          rer.reporting_effort_id = rpt.reporting_effort_id
          AND rer.report_id = rpt.report_id
      WHERE 
          rpt.report_id IS NULL;
    ")
        
        # Step 2: Delete Orphaned Records
        dbExecute(pool, "
      DELETE FROM report_programming_tracker
WHERE NOT EXISTS (
    SELECT 1
    FROM reporting_effort_reports
    WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
      AND reporting_effort_reports.report_id = report_programming_tracker.report_id
);
    ")
        
        showNotification("Tracker data synced successfully.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error ensuring tracker data consistency:", e$message), type = "error")
      })
    })
    
    # Reactive: Load and Sort Reports
    reports <- reactive({
      req(input$reporting_effort)
      refresh_trigger() # Depend on refresh_trigger for updates
      
      data <- tryCatch({
        dbGetQuery(
          pool,
          paste(
            "SELECT r.id , r.report_key AS 'Report Key', r.report_type AS 'Report Type', 
                    c.category_name AS 'Category', sc.sub_category_name AS 'Subcategory', 
                    r.report_ich_number AS 'ICH Number', p.population_text AS 'Population',
                    t.title_text AS 'Title', 
                    GROUP_CONCAT(f.footnote_text, ', ') AS 'Footnotes',
                    CASE WHEN rer.reporting_effort_id IS NOT NULL THEN 1 ELSE 0 END AS Selected
             FROM reports r
             LEFT JOIN categories c ON r.report_category_id = c.id
             LEFT JOIN sub_categories sc ON r.report_sub_category_id = sc.id
             LEFT JOIN populations p ON r.population_id = p.id
             LEFT JOIN report_titles rt ON r.id = rt.report_id
             LEFT JOIN titles t ON rt.title_id = t.id
             LEFT JOIN report_footnotes rf ON r.id = rf.report_id
             LEFT JOIN footnotes f ON rf.footnote_id = f.id
             LEFT JOIN reporting_effort_reports rer
             ON r.id = rer.report_id AND rer.reporting_effort_id = ",
            input$reporting_effort, "
             GROUP BY r.id;"
          )
        ) %>%
          dplyr::mutate(Selected = as.logical(Selected)) %>%
          dplyr::select(Selected, everything()) %>% # Move Selected column to the front
          dplyr::arrange(`Report Type`, Category, Subcategory, Population, `ICH Number`) # Sort the data
      }, error = function(e) {
        showNotification(paste("Error loading reports:", e$message), type = "error")
        NULL
      })
      
      # Apply filtering based on the search box input
      if (!is.null(input$search) && input$search != "") {
        data <- data %>%
          dplyr::filter_at(vars(`Report Key`, `Report Type`, `Category`, `Subcategory`, `Population`, `Title`, `Footnotes`),
                           any_vars(stringr::str_detect(., stringr::fixed(input$search, ignore_case = TRUE))))
      }
      
      data
    })
    
    # Render rHandsontable
    output$reports_table <- renderRHandsontable({
      req(reports())
      # Remove id from display but keep it for backend operations
      reports_data <- reports() %>%
        select(id,Selected, `Report Key`, `Report Type`, Category, Subcategory, `ICH Number`, Population, Title, Footnotes) %>% 
        arrange(`Report Type`,`Report Key`, Category, Subcategory, Population, `ICH Number`)
      
      rhandsontable(
        reports_data,
        useTypes = TRUE, # Enable type detection
        rowHeaders = FALSE # Disable row headers
      ) %>%
        hot_col("id", readOnly = TRUE, width = 1) %>% 
        hot_col("Selected", type = "checkbox", halign = "center") %>% # Make Selected column a checkbox
        hot_col("Report Key", readOnly = TRUE, halign = "left") %>%
        hot_col("Report Type", readOnly = TRUE, halign = "left") %>%
        hot_col("Category", readOnly = TRUE, halign = "left") %>%
        hot_col("Subcategory", readOnly = TRUE, halign = "left") %>%
        hot_col("ICH Number", readOnly = TRUE, halign = "center") %>%
        hot_col("Population", readOnly = TRUE, halign = "left") %>%
        hot_col("Title", readOnly = TRUE, halign = "left") %>%
        hot_col("Footnotes", readOnly = TRUE, halign = "left") %>%
        hot_table(contextMenu = FALSE)
    })
    
    
    
    # Save Selection
    observeEvent(input$save_selection, {
      req(input$reporting_effort)
      
      tryCatch({
        # Retrieve edited data
        edited_data <- hot_to_r(input$reports_table)
        
        # Log edited data for debugging
        # cat("Edited Data:\n")
        # print(edited_data)
        
        # Delete existing associations for the reporting effort
        dbExecute(pool, paste(
          "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ",
          input$reporting_effort, ";"
        ))
        # dbExecute(pool, paste(
        #   "DELETE FROM report_programming_tracker WHERE reporting_effort_id = ",
        #   input$reporting_effort, ";"
        # ))        
        
        # Insert new associations for selected rows
        selected_reports <- edited_data %>%
          dplyr::filter(Selected) # Filter rows where Selected is TRUE
        
        if (nrow(selected_reports) > 0) {
          query <- paste(
            "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id) VALUES ",
            paste(sprintf("(%s, %s)", input$reporting_effort, selected_reports$id), collapse = ",")
          )
          dbExecute(pool, query)
          # query <- paste(
          #   "INSERT INTO report_programming_tracker (reporting_effort_id, report_id) VALUES ",
          #   paste(sprintf("(%s, %s)", input$reporting_effort, selected_reports$id), collapse = ",")
          # )
          # dbExecute(pool, query)
          
        }
        
        # Log reporting_effort_reports after saving
        # after_save <- dbGetQuery(pool, "SELECT * FROM reporting_effort_reports;")
        # cat("After Save:\n")
        # print(after_save)
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Selection saved successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error during save operation:", e$message), type = "error")
      })
    })
  })
}
