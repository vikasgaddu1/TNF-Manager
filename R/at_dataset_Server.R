at_dataset_Server <- function(id, pool,tables_data, reporting_effort, reporting_effort_label, ds_type) {
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
                  assign_date, 
                  due_date, 
                  priority, 
                  status,
                  updated_at
              )
              SELECT 
                  rer.reporting_effort_id,
                  rer.report_id,
                  rer.report_type,
                  NULL,  -- Default value for production_programmer_id
                  NULL,  -- Default value for qc_programmer_id
                  NULL,  -- Default assign_date
                  NULL,  -- Default due_date
                  5,     -- Default priority (lowest)
                  'Not Started',  -- Default status
                  CURRENT_TIMESTAMP
              FROM 
                  reporting_effort_reports rer
              LEFT JOIN 
                  report_programming_tracker rpt
              ON 
                  rer.reporting_effort_id = rpt.reporting_effort_id
                  AND rer.report_id = rpt.report_id
                  AND rer.report_type = rpt.report_type
              WHERE 
                  rpt.report_id IS NULL AND rer.report_type = ?;",
                      params = list(ds_type)
            )
            
            # Step 2: Delete Orphaned Records
            dbExecute(conn, "
              DELETE FROM report_programming_tracker
                WHERE report_type = ? AND NOT EXISTS (
                  SELECT 1
                  FROM reporting_effort_reports
                  WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
                        AND reporting_effort_reports.report_id = report_programming_tracker.report_id 
                        AND reporting_effort_reports.report_type = report_programming_tracker.report_type
                        AND report_type = ?);",
                      params = list(ds_type, ds_type)
            )
          })
          
          showNotification("Tracker data synced successfully.", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error ensuring tracker data consistency:", e$message), type = "error")
        })
      })
      
      # Reactive to fetch and cache dataset data from the database
      dataset_data <- reactive({
        req(reporting_effort())
        refresh_trigger()
        # Debugging: Print column names of datasets
        # cat("Columns in datasets:", names(tables_data$datasets()), "\n")
        
        # Debugging: Print column names of reporting_effort_reports
        # cat("Columns in reporting_effort_reports:", names(tables_data$reporting_effort_reports()), "\n")
        
        tables_data$datasets() %>%
          dplyr::left_join(
            tables_data$reporting_effort_reports() %>%
              dplyr::filter(reporting_effort_id == reporting_effort()), 
            join_by(id == report_id, dataset_type == report_type)
          ) %>%
          dplyr::filter(dataset_type == ds_type) %>%
          dplyr::mutate(Selected = !is.na(reporting_effort_id) & reporting_effort_id == reporting_effort()) %>%
          dplyr::select(Selected, id, dataset_name, dataset_label, dataset_type, category_name) %>%
          dplyr::rename(
            `Dataset Name` = dataset_name, 
            `Dataset Label` = dataset_label, 
            `Dataset Type` = dataset_type, 
            Category = category_name
          ) %>%
          dplyr::arrange(`Dataset Type`, Category, `Dataset Name`)
      })
      
      # Reactive to filter dataset data based on dropdowns and search input
      datasets <- reactive({
        req(dataset_data())
        filtered_data <- dataset_data()
        
        if (!is.null(input$category) && input$category != "All") {
          filtered_data <- filtered_data %>%
            dplyr::filter(Category == input$category)
        }
        
        if (!is.null(input$search) && input$search != "") {
          filtered_data <- filtered_data %>%
            dplyr::filter_at(vars(`Dataset Name`, `Dataset Type`, Category),
                             any_vars(stringr::str_detect(., stringr::fixed(input$search, ignore_case = TRUE))))
        }
        
        filtered_data
      })
      
      # Dynamically update dropdowns
      output$category_select <- renderUI({
        req(dataset_data())
        selectInput(
          ns("category"),
          label = "Category",
          choices = c("All", unique(dataset_data()$Category)),
          selected = "All"
        )
      })
      
      # Render rHandsontable
      output$datasets_table <- renderRHandsontable({
        req(datasets())
        datasets_data <- datasets() %>%
          select(id, Selected, `Dataset Type`, `Dataset Name`, `Dataset Label`, Category) %>%
          arrange(`Dataset Type`, `Dataset Name`, `Dataset Label`, Category)
        
        rhandsontable(
          datasets_data,
          useTypes = TRUE,
          rowHeaders = FALSE
        ) %>%
          hot_col("id", readOnly = TRUE, width = 1) %>%
          hot_col("Selected", type = "checkbox", halign = "center") %>%
          hot_col("Dataset Type", readOnly = TRUE, halign = "left") %>%
          hot_col("Dataset Name", readOnly = TRUE, halign = "left") %>%
          hot_col("Dataset Label", readOnly = TRUE, halign = "left") %>%
          hot_col("Category", readOnly = TRUE, halign = "left") %>%
          hot_table(contextMenu = FALSE)
      })
      
      output$download_tnf <- downloadHandler(
        filename = function() {
          req(reporting_effort_label()) # Ensure the reactive value is available
          paste0(ds_type,"_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          df <- dataset_data() %>%
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
          edited_data <- hot_to_r(input$datasets_table)
          edited_data <- dplyr::left_join(
            dataset_data(), 
            edited_data %>% dplyr::select(id, Selected), 
            by = "id"
          ) %>%
            dplyr::mutate(
              Selected = ifelse(is.na(Selected.y), Selected.x, Selected.y)
            ) %>%
            dplyr::select(-Selected.x, -Selected.y)
          
          edited_data <- edited_data %>%
            dplyr::rename(report_type = `Dataset Type`)
          
          tryCatch({
            poolWithTransaction(pool, function(conn) {
              dbExecute(conn, paste(
                "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ",
                reporting_effort(), " AND report_type = ?", ";"
              ), params = list(ds_type))
            })
          }, error = function(e) {
            showNotification(paste("Error deleting existing associations:", e$message), type = "error")
          })
          
          selected_datasets <- edited_data %>%
            dplyr::filter(Selected)
          
          if (nrow(selected_datasets) > 0) {
            query_reporting_effort <- paste(
              "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at) VALUES ",
              paste(
                sprintf("(%s, %s, '%s', CURRENT_TIMESTAMP)", 
                        reporting_effort(), 
                        selected_datasets$id, 
                        selected_datasets$report_type),
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
          
          refresh_trigger(refresh_trigger() + 1)
          show_toast(
            title = "Save",
            type = "success",
            text = "Selection saved successfully!",
            position = "center"
          )
        }, error = function(e) {
          showNotification(paste("Error during save operation:", e$message), type = "error")
        })
      })
    }
  )
}
