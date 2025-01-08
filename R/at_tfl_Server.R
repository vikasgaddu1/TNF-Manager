at_tfl_Server <- function(id, pool, reporting_effort,reporting_effort_label, refresh_trigger) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Trigger to refresh data
      # refresh_trigger <- reactiveVal(0)
      
      observeEvent(refresh_trigger(), {
        tryCatch({
          # Step 1: Insert Missing Records
          dbExecute(pool, "
      INSERT INTO report_programming_tracker (
          reporting_effort_id, 
          report_id, 
          report_type,
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
          rer.report_type,
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
          AND rer.report_type = rpt.report_type
      WHERE 
          rpt.report_id IS NULL and rer.report_type IN ('Table', 'Listing', 'Figure') ;
    ")
          
          # Step 2: Delete Orphaned Records
          dbExecute(pool, "
            DELETE FROM report_programming_tracker
              WHERE report_type IN ('Table', 'Listing', 'Figure')  and  NOT EXISTS (
                SELECT 1
                 FROM reporting_effort_reports
                  WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
                        AND reporting_effort_reports.report_id = report_programming_tracker.report_id AND
                        reporting_effort_reports.report_type = report_programming_tracker.report_type AND
                        report_type IN ('Table', 'Listing', 'Figure')

          );
    ")
          
          showNotification("Tracker data synced successfully.", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error ensuring tracker data consistency:", e$message), type = "error")
        })
      })
      
      # Reactive to fetch and cache TFL data from the database
      tfl_data <- reactive({
        req(reporting_effort())
        # cat("Reporting Effort ID: ", reporting_effort(), "\n")
        refresh_trigger() # Depend on refresh_trigger for updates
        
        tryCatch({
          dbGetQuery(
            pool,
            paste(
              "SELECT r.id, 
               r.report_key AS 'Report Key', 
               r.title_key as 'Title Key', 
               r.report_type AS 'Report Type', 
               c.category_name AS 'Category', 
               sc.sub_category_name AS 'Subcategory', 
               r.report_ich_number AS 'ICH Number', 
               p.population_text AS 'Population',
               (SELECT GROUP_CONCAT(t2.title_text, '@#')
                FROM report_titles rt2 
                JOIN titles t2 ON rt2.title_id = t2.id 
                WHERE rt2.report_id = r.id) AS 'Title',
               (SELECT GROUP_CONCAT(f2.footnote_text, '@#')
                FROM report_footnotes rf2 
                JOIN footnotes f2 ON rf2.footnote_id = f2.id 
                WHERE rf2.report_id = r.id) AS 'Footnotes',
               CASE WHEN rer.reporting_effort_id IS NOT NULL THEN 1 ELSE 0 END AS Selected
                  FROM reports r
                  LEFT JOIN categories c ON r.report_category_id = c.id
                  LEFT JOIN sub_categories sc ON r.report_sub_category_id = sc.id
                  LEFT JOIN populations p ON r.population_id = p.id
                  LEFT JOIN reporting_effort_reports rer 
                       ON r.id = rer.report_id 
                       AND rer.reporting_effort_id = ", reporting_effort(), 
                       "AND rer.report_type = r.report_type",
                       "WHERE r.report_type IN ('Table', 'Listing', 'Figure')
                  GROUP BY r.id;"
            )
          ) %>%
            dplyr::mutate(Selected = as.logical(Selected)) %>%
            dplyr::select(Selected, everything()) %>%
            dplyr::arrange(`Report Type`, Category, Subcategory, Population, `ICH Number`)

        }, error = function(e) {
          showNotification(paste("Error loading reports:", e$message), type = "error")
          NULL
        })
      })
      
      # Reactive to filter TFL data based on dropdowns and search input
      reports <- reactive({
        req(tfl_data()) # Ensure tfl_data is available
        
        filtered_data <- tfl_data()
        
        # Apply dropdown filters
        if (!is.null(input$report_type) && input$report_type != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(`Report Type` == input$report_type)
        }
        
        if (!is.null(input$category) && input$category != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(Category == input$category)
        }
        
        if (!is.null(input$subcategory) && input$subcategory != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(Subcategory == input$subcategory)
        }
        
        # Apply search filter
        if (!is.null(input$search) && input$search != "") {
          filtered_data <- filtered_data %>%
            dplyr::filter_at(vars(`Report Key`, `Report Type`,`ICH Number`, `Category`, `Subcategory`, `Population`, `Title`, `Footnotes`),
                             any_vars(stringr::str_detect(., stringr::fixed(input$search, ignore_case = TRUE))))
        }
        
        filtered_data
      })
      
      
      # Dynamically update dropdowns
      output$report_type_select <- renderUI({
        req(tfl_data())
        selectInput(
          ns("report_type"),
          label = "Report Type",
          choices = c("All", unique(tfl_data()$`Report Type`)),
          selected = "All"
        )
      })
      
      output$category_select <- renderUI({
        req(tfl_data())
        selectInput(
          ns("category"),
          label = "Category",
          choices = c("All", unique(tfl_data()$Category)),
          selected = "All"
        )
      })
      
      output$subcategory_select <- renderUI({
        req(tfl_data())
        selectInput(
          ns("subcategory"),
          label = "Subcategory",
          choices = c("All", unique(tfl_data()$Subcategory)),
          selected = "All"
        )
      })
      
      
      # Render rHandsontable
      output$reports_table <- renderRHandsontable({
        req(reports())
        # Remove id from display but keep it for backend operations
        reports_data <- reports() %>%
          select(id,Selected,`Report Type`, `Report Key`,`Title Key`,  Category, Subcategory, `ICH Number`, Population, Title, Footnotes) %>%
          arrange(`Report Type`,`Report Key`, `Title Key`,Category, Subcategory, Population, `ICH Number`)

        rhandsontable(
          reports_data,
          useTypes = TRUE, # Enable type detection
          rowHeaders = FALSE # Disable row headers
        ) %>%
          hot_col("id", readOnly = TRUE, width = 1) %>%
          hot_col("Selected", type = "checkbox", halign = "center") %>% # Make Selected column a checkbox
          hot_col("Report Type", readOnly = TRUE, halign = "left") %>%
          hot_col("Report Key", readOnly = TRUE, halign = "left") %>%
          hot_col("Title Key", readOnly = TRUE, halign = "left") %>%
          hot_col("Category", readOnly = TRUE, halign = "left") %>%
          hot_col("Subcategory", readOnly = TRUE, halign = "left") %>%
          hot_col("ICH Number", readOnly = TRUE, halign = "center") %>%
          hot_col("Population", readOnly = TRUE, halign = "left") %>%
          hot_col("Title", readOnly = TRUE, halign = "left") %>%
          hot_col("Footnotes", readOnly = TRUE, halign = "left") %>%
          hot_table(contextMenu = FALSE)
      })
      

      output$download_tnf <- downloadHandler(
        filename = function() {
          req(reporting_effort_label()) # Ensure the reactive value is available
          paste0("TNF_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          df <- tfl_data() %>%
            dplyr::filter(Selected) %>%
            dplyr::select(-c('Selected', 'id')) # Remove Selected and ID column
          
          req(df) # Ensure data is available
          
          # Check if data is available
          if (nrow(df) == 0) {
            showNotification("No data available to download.", type = "warning")
            return(NULL)
          }
          
          # Write data to an Excel file
          openxlsx::write.xlsx(df, file)
        }
      )
      
      

      
      # Save Selection
      observeEvent(input$save_selection, {
        req(reporting_effort())
        
        tryCatch({
          # Retrieve edited data
          edited_data <- hot_to_r(input$reports_table)
          
          #merge with tfl_data and get previously selected values using dplyr join_by
          edited_data <- dplyr::left_join(
            tfl_data(), # All records from tfl_data
            edited_data %>% dplyr::select(id, Selected), # Selected column from edited_data
            by = "id"
          ) %>%
            dplyr::mutate(
              Selected = ifelse(is.na(Selected.y), Selected.x, Selected.y)
            ) %>%
            dplyr::select(-Selected.x, -Selected.y) # Clean up temporary columns
          
          
          
          # Log edited data for debugging
          # cat("Edited Data:\n")
          # print(edited_data)
          
          # Standardize column names (replace space with underscore)
          edited_data <- edited_data %>%
            dplyr::rename(report_type = `Report Type`) # Adjust column name here
          
          # Delete existing associations for the reporting effort
          dbExecute(pool, paste(
            "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ",
            reporting_effort(), " and report_type IN ('Table', 'Listing', 'Figure');"
          ))
          # dbExecute(pool, paste(
          #   "DELETE FROM report_programming_tracker WHERE reporting_effort_id = ",
          #   reporting_effort(), " and report_type IN ('Table', 'Listing', 'Figure');"
          # ))
          
          # Insert new associations for selected rows
          selected_reports <- edited_data %>%
            dplyr::filter(Selected) # Filter rows where Selected is TRUE
          
          if (nrow(selected_reports) > 0) {
            # Prepare query for reporting_effort_reports
            query_reporting_effort <- paste(
              "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type) VALUES ",
              paste(
                sprintf("(%s, %s, '%s')", 
                        reporting_effort(), 
                        selected_reports$id, 
                        selected_reports$report_type),
                collapse = ","
              )
            )
            dbExecute(pool, query_reporting_effort)
            
            # Prepare query for report_programming_tracker
          #   query_tracker <- paste(
          #     "INSERT INTO report_programming_tracker (reporting_effort_id, report_id, report_type) VALUES ",
          #     paste(
          #       sprintf("(%s, %s, '%s')", 
          #               reporting_effort(), 
          #               selected_reports$id, 
          #               selected_reports$report_type),
          #       collapse = ","
          #     )
          #   )
          #   dbExecute(pool, query_tracker)
           }
          
          # cat("After insertion\n")
          # # Log the inserted data for debugging
          # rpt <- dbGetQuery(pool, "SELECT * FROM report_programming_tracker;")
          # print(rpt)
          
          # Refresh trigger
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Selection saved successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error during save operation:", e$message), type = "error")
        })
      })
      
    }
  )
  return(refresh_trigger)
}