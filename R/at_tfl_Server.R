at_tfl_Server <- function(id, pool,tables_data, reporting_effort, reporting_effort_label) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
            refresh_trigger <- reactiveVal(0)
      # Trigger to refresh data
      observeEvent(refresh_trigger(), {
        tryCatch({
          poolWithTransaction(pool, function(conn) {
            # Step 1: Insert Missing Records
            dbExecute(conn, "
              INSERT INTO report_programming_tracker (
                  reporting_effort_id, 
                  report_id, 
                  report_type,
                  production_programmer_id, 
                  qc_programmer_id, 
                  qc_level,
                  assign_date, 
                  due_date, 
                  priority, 
                  status
              )
              SELECT 
                  rer.reporting_effort_id,
                  rer.report_id,
                  rer.report_type,
                  1,     -- Default value for production_programmer_id (assuming 1 for 'Not Assigned')
                  1,     -- Default value for qc_programmer_id (assuming 1 for 'Not Assigned')
                  3,     -- Default qc_level
                  NULL,  -- Default assign_date
                  NULL,  -- Default due_date
                  5,     -- Default priority (lowest)
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
                  rpt.report_id IS NULL AND rer.report_type in ('Table', 'Listing', 'Figure');"
            )

            # Step 2: Delete Orphaned Records
            dbExecute(conn, "
              DELETE FROM report_programming_tracker
                WHERE report_type in ('Table', 'Listing', 'Figure') AND NOT EXISTS (
                  SELECT 1
                  FROM reporting_effort_reports
                  WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
                        AND reporting_effort_reports.report_id = report_programming_tracker.report_id 
                        AND reporting_effort_reports.report_type = report_programming_tracker.report_type
                        AND reporting_effort_reports.report_type in ('Table', 'Listing', 'Figure'));"
            )
          })
          
          showNotification("Tracker data synced successfully.", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error ensuring tracker data consistency:", e$message), type = "error")
        })
      })

      # Reactive to fetch and cache TFL data from the database
      tfl_data <- reactive({
        req(reporting_effort())
        refresh_trigger()
        # Debugging: Print the reporting effort
        # cat("Reporting Effort ID:", reporting_effort(), "\n")

        # Initial reports data
        reports <- tables_data$reports()
        # cat("Initial number of reports:", nrow(reports), "\n")
        
        # Join with categories
        reports <- reports %>%
          dplyr::left_join(tables_data$categories(), join_by(report_category_id == id))
        # cat("Number of reports after joining categories:", nrow(reports), "\n")
        
        # Join with sub_categories
        reports <- reports %>%
          dplyr::left_join(tables_data$sub_categories(), join_by(report_sub_category_id == id))
        # cat("Number of reports after joining sub_categories:", nrow(reports), "\n")
        
        # Join with populations
        reports <- reports %>%
          dplyr::left_join(tables_data$populations(), join_by(population_id == id))
        # cat("Number of reports after joining populations:", nrow(reports), "\n")
        
        # Filter and join with reporting_effort_reports
         reports <- reports %>%
          dplyr::left_join(
            tables_data$reporting_effort_reports() %>%
              dplyr::filter(reporting_effort_id == reporting_effort()), 
            join_by(id == report_id, report_type == report_type)
          )       
           # cat("Number of reports after joining reporting_effort_reports:", nrow(reports), "\n")
        
        # Filter and mutate
        reports <- reports %>%
          dplyr::filter(report_type %in% c("Table", "Listing", "Figure")) %>%
          dplyr::mutate(Selected = !is.na(reporting_effort_id) & reporting_effort_id == reporting_effort())
        
        # cat("Final number of reports after filtering:", nrow(reports), "\n")
        
        # Process Titles
        titles <- tables_data$report_titles() %>%
          dplyr::left_join(tables_data$titles(), join_by(title_id == id)) %>%
          dplyr::group_by(report_id) %>%
          dplyr::summarise(Title = paste(title_text, collapse = "<br>"))
        
        # Debugging: Print the number of titles processed
        # cat("Number of Titles processed:", nrow(titles), "\n")
        
        # Process Footnotes
        footnotes <- tables_data$report_footnotes() %>%
          dplyr::left_join(tables_data$footnotes(), join_by(footnote_id == id)) %>%
          dplyr::group_by(report_id) %>%
          dplyr::summarise(Footnotes = paste(footnote_text, collapse = "<br>"))
        
        # Debugging: Print the number of footnotes processed
        # cat("Number of Footnotes processed:", nrow(footnotes), "\n")
        
        # Combine all data
        reports <- reports %>%
          dplyr::left_join(titles, join_by(id == report_id)) %>%
          dplyr::left_join(footnotes, join_by(id == report_id))  %>% 
          dplyr::select(
            Selected,
            id,
            report_key,
            title_key,
            report_type,
            category_name,
            sub_category_name,
            report_ich_number,
            population_text,
            Title,
            Footnotes
          ) %>%
          dplyr::arrange(-Selected, report_type, category_name, sub_category_name, population_text, report_ich_number)
        
        # Debugging: Print the final number of reports
        # cat("Final number of Reports:", nrow(reports), "\n")
        
        reports
      })
      
      # Reactive to filter TFL data based on dropdowns and search input
      reports <- reactive({
        req(tfl_data()) # Ensure tfl_data is available
        refresh_trigger()
        filtered_data <- tfl_data()
        
        # Apply dropdown filters
        if (!is.null(input$report_type) && input$report_type != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(report_type == input$report_type)
        }
        
        if (!is.null(input$category) && input$category != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(category_name == input$category)
        }
        
        if (!is.null(input$subcategory) && input$subcategory != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(sub_category_name == input$subcategory)
        }
        
        # Apply search filter
        if (!is.null(input$search) && input$search != "") {
          filtered_data <- filtered_data %>%
            dplyr::filter_at(vars(report_key, report_type, report_ich_number, category_name, sub_category_name, population_text, Title, Footnotes),
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
          choices = c("All", unique(tfl_data()$report_type)),
          selected = "All"
        )
      })
      
      output$category_select <- renderUI({
        req(tfl_data())
        selectInput(
          ns("category"),
          label = "Category",
          choices = c("All", unique(tfl_data()$category_name)),
          selected = "All"
        )
      })
      
      output$subcategory_select <- renderUI({
        req(tfl_data())
        selectInput(
          ns("subcategory"),
          label = "Subcategory",
          choices = c("All", unique(tfl_data()$sub_category_name)),
          selected = "All"
        )
      })
      
    observeEvent(input$select_all, {
      req(reports())
      
      # Update the currently displayed data
      current_data <- reports() %>%
        dplyr::mutate(Selected = TRUE)
      
        # Render updated table
        output$reports_table <- renderRHandsontable({
          rhandsontable(
            current_data %>%
              rename(
                "Selection" = Selected,
                "Report Type" = report_type,
                "Report Key" = report_key,
                "Title Key" = title_key,
                "Category" = category_name,
                "Subcategory" = sub_category_name,
                "ICH Number" = report_ich_number,
                "Population" = population_text,
                "Title" = Title,
                "Footnotes" = Footnotes
              ),
            useTypes = TRUE,
            rowHeaders = FALSE
          ) %>%
            hot_col("id", readOnly = TRUE, width = 1) %>%
            hot_col("Selection", type = "checkbox", halign = "center") %>%
            hot_table(contextMenu = FALSE)%>%
            hot_col("Report Type", readOnly = TRUE, halign = "left") %>%
            hot_col("Report Key", readOnly = TRUE, halign = "left") %>%
            hot_col("Title Key", readOnly = TRUE, halign = "left") %>%
            hot_col("Category", readOnly = TRUE, halign = "left") %>%
            hot_col("Subcategory", readOnly = TRUE, halign = "left") %>%
            hot_col("ICH Number", readOnly = TRUE, halign = "center") %>%
            hot_col("Population", readOnly = TRUE, halign = "left") %>%
            hot_col("Title", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
            hot_col("Footnotes", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) 
        })
      })

      observeEvent(input$select_none, {
        req(reports())
        
        # Update the currently displayed data
        current_data <- reports() %>%
          dplyr::mutate(Selected = FALSE)
        
        # Render updated table
        output$reports_table <- renderRHandsontable({
          rhandsontable(
            current_data %>%
              rename(
                "Selection" = Selected,
                "Report Type" = report_type,
                "Report Key" = report_key,
                "Title Key" = title_key,
                "Category" = category_name,
                "Subcategory" = sub_category_name,
                "ICH Number" = report_ich_number,
                "Population" = population_text,
                "Title" = Title,
                "Footnotes" = Footnotes
              ),
            useTypes = TRUE,
            rowHeaders = FALSE
          ) %>%
            hot_col("id", readOnly = TRUE, width = 1) %>%
            hot_col("Selection", type = "checkbox", halign = "center") %>%
            hot_table(contextMenu = FALSE)%>%
            hot_col("Report Type", readOnly = TRUE, halign = "left") %>%
            hot_col("Report Key", readOnly = TRUE, halign = "left") %>%
            hot_col("Title Key", readOnly = TRUE, halign = "left") %>%
            hot_col("Category", readOnly = TRUE, halign = "left") %>%
            hot_col("Subcategory", readOnly = TRUE, halign = "left") %>%
            hot_col("ICH Number", readOnly = TRUE, halign = "center") %>%
            hot_col("Population", readOnly = TRUE, halign = "left") %>%
            hot_col("Title", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
            hot_col("Footnotes", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) 
        })
      })


      # Render rHandsontable
      output$reports_table <- renderRHandsontable({
        req(reports())
        refresh_trigger()
        # Remove id from display but keep it for backend operations
        reports_data <- reports() %>%
          select(id, Selected, report_type, report_key, title_key, category_name, sub_category_name, report_ich_number, population_text, Title, Footnotes) %>%
          arrange(-Selected, report_type, report_key, title_key, category_name, sub_category_name, population_text, report_ich_number)%>%
          rename(
            "Selection" = Selected,
            "Report Type" = report_type,
            "Report Key" = report_key,
            "Title Key" = title_key,
            "Category" = category_name,
            "Subcategory" = sub_category_name,
            "ICH Number" = report_ich_number,
            "Population" = population_text,
            "Title" = Title,
            "Footnotes" = Footnotes
          )
        
        rhandsontable(
          reports_data,
          useTypes = TRUE, # Enable type detection
          rowHeaders = FALSE, # Disable row headers
          allowedTags = "<br>", # Allow HTML tags
        ) %>%
          hot_col("id", readOnly = TRUE, width = 1) %>%
          hot_col("Selection", type = "checkbox", halign = "center") %>% # Make Selected column a checkbox
          hot_col("Report Type", readOnly = TRUE, halign = "left") %>%
          hot_col("Report Key", readOnly = TRUE, halign = "left") %>%
          hot_col("Title Key", readOnly = TRUE, halign = "left") %>%
          hot_col("Category", readOnly = TRUE, halign = "left") %>%
          hot_col("Subcategory", readOnly = TRUE, halign = "left") %>%
          hot_col("ICH Number", readOnly = TRUE, halign = "center") %>%
          hot_col("Population", readOnly = TRUE, halign = "left") %>%
          hot_col("Title", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
          hot_col("Footnotes", readOnly = TRUE, halign = "left", renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
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
            show_toast(
              title = "Warning",
              type = "warning",
              text = "No data available to download.",
              position = "top-end"
            )
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
          edited_data <- hot_to_r(input$reports_table) %>%
            rename(
              Selected = "Selection",
              report_type = "Report Type",
              report_key = "Report Key",
              title_key = "Title Key",
              category_name = "Category",
              sub_category_name = "Subcategory",
              report_ich_number = "ICH Number",
              population_text = "Population",
              Title = "Title",
              Footnotes = "Footnotes"
            )
          
          # Merge with tfl_data and get previously selected values using dplyr join_by
          edited_data <- dplyr::left_join(
            tfl_data(), # All records from tfl_data
            edited_data %>% dplyr::select(id, Selected), # Selected column from edited_data
            by = "id"
          ) %>%
            dplyr::mutate(
              Selected = ifelse(is.na(Selected.y), Selected.x, Selected.y)
            ) %>%
            dplyr::select(-Selected.x, -Selected.y) # Clean up temporary columns
          
          # Delete existing associations for the reporting effort
          poolWithTransaction(pool, function(conn) {
            dbExecute(conn, paste(
              "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ",
              reporting_effort(), " and report_type IN ('Table', 'Listing', 'Figure');"
            ))
          })
          
          # Insert new associations for selected rows
          selected_reports <- edited_data %>%
            dplyr::filter(Selected) # Filter rows where Selected is TRUE
          
          if (nrow(selected_reports) > 0) {
            # Prepare query for reporting_effort_reports
            query_reporting_effort <- paste(
              "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at) VALUES ",
              paste(
                sprintf("(%s, %s, '%s', CURRENT_TIMESTAMP)", 
                        reporting_effort(), 
                        selected_reports$id, 
                        selected_reports$report_type),
                collapse = ","
              )
            )
            tryCatch({
              poolWithTransaction(pool, function(conn) {
                dbExecute(conn, query_reporting_effort)
              })
            }, error = function(e) {
              showNotification(paste("Error inserting new associations:", e$message), type = "error")
            })
          }
          
          # Refresh trigger
          refresh_trigger(refresh_trigger() + 1)  

          show_toast(
            title = "Save",
            type = "success",
            text = "Selection saved successfully!",
            position = "center"
          )
        }, error = function(e) {
          print(e$message)
          showNotification(paste("Error during save operation:", e$message), type = "error")
          print(e)
        })
      })
    }
  )
}