at_tfl_Server <- function(id, pool, tables_data, reporting_effort, reporting_effort_label) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      refresh_trigger <- reactiveVal(0)
      refresh_trigger <- at_Copy_Server("copy_reporting_effort", tables_data, reporting_effort, reporting_effort_label, "TFL", pool, refresh_trigger)



      observeEvent(refresh_trigger(), {
        tryCatch({
          poolWithTransaction(pool, function(conn) {
            # Sync tracker data by inserting missing records and deleting orphaned ones
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
                  1, -- Default value for production_programmer_id
                  1, -- Default value for qc_programmer_id
                  3, -- Default qc_level
                  NULL, -- Default assign_date
                  NULL, -- Default due_date
                  5, -- Default priority
                  'Not Started' -- Default status
              FROM 
                  reporting_effort_reports rer
              LEFT JOIN 
                  report_programming_tracker rpt
              ON 
                  rer.reporting_effort_id = rpt.reporting_effort_id
                  AND rer.report_id = rpt.report_id
                  AND rer.report_type = rpt.report_type
              WHERE 
                  rpt.report_id IS NULL 
                  AND rer.report_type IN ('Table', 'Listing', 'Figure');
            ")

            dbExecute(conn, "
              DELETE FROM report_programming_tracker
              WHERE report_type IN ('Table', 'Listing', 'Figure')
                AND NOT EXISTS (
                  SELECT 1
                  FROM reporting_effort_reports
                  WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
                    AND reporting_effort_reports.report_id = report_programming_tracker.report_id 
                    AND reporting_effort_reports.report_type = report_programming_tracker.report_type
                );
            ")
          })
          
          showNotification("Tracker data synced successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error syncing tracker data:", e$message), type = "error")
        })
      })
      
      tfl_data <- reactive({
        req(reporting_effort())
        refresh_trigger()
        
        reports <- tables_data$reports() %>%
          dplyr::left_join(tables_data$categories(), by = c("report_category_id" = "id")) %>%
          dplyr::left_join(tables_data$sub_categories(), by = c("report_sub_category_id" = "id")) %>%
          dplyr::left_join(tables_data$populations(), by = c("population_id" = "id")) %>%
          dplyr::left_join(
            tables_data$reporting_effort_reports() %>%
              dplyr::filter(reporting_effort_id == reporting_effort()), 
            by = c("id" = "report_id", "report_type" = "report_type")
          ) %>%
          dplyr::mutate(Selected = !is.na(reporting_effort_id) & reporting_effort_id == reporting_effort()) %>%
          dplyr::arrange(-Selected, report_type, category_name, sub_category_name, population_text, report_ich_number)
        
        titles <- tables_data$report_titles() %>%
          dplyr::left_join(tables_data$titles(), by = c("title_id" = "id")) %>%
          dplyr::group_by(report_id) %>%
          dplyr::summarise(Title = paste(title_text, collapse = "<br>"))
        
        footnotes <- tables_data$report_footnotes() %>%
          dplyr::left_join(tables_data$footnotes(), by = c("footnote_id" = "id")) %>%
          dplyr::group_by(report_id) %>%
          dplyr::summarise(Footnotes = paste(footnote_text, collapse = "<br>"))
        
        reports <- reports %>%
          dplyr::left_join(titles, by = c("id" = "report_id")) %>%
          dplyr::left_join(footnotes, by = c("id" = "report_id")) %>%
          dplyr::select(
            id,
            Selected,
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
        
        reports
      })

      # download handler for selected rows  as xlsx 
      output$download_tnf <- downloadHandler(
        filename = function() {
          paste0("TNF_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          df <- tfl_data() %>% 
            dplyr::filter(Selected) %>%
            dplyr::select(-c(id, Selected,`Report Key`))

          # Check if data is available
          if (is.null(df) || nrow(df) == 0) {
            showModal(modalDialog(
              title = "Warning",
              "No data available to download.",
              easyClose = TRUE,
              footer = NULL
            ))
            return(NULL)
          }
          
          # Write data to an Excel file
          write.xlsx(df, file)
        }
      )
      
      # Render DataTable
      output$reports_table <- renderDT({
        req(tfl_data())
        reports <- tfl_data()
        
        datatable(
          reports,
          escape = FALSE,
          filter = "top",
          options = list(
            paging = FALSE, # Show all rows
            searching = TRUE, # Enable global search
            search = list(
              regex = TRUE,    # Enable regex matching
              smart = FALSE    # Disable smart (substring) filtering
            ),
            autoWidth = TRUE,
            dom = 'frtip', # Enable export buttons
            columnDefs = list(
              list(targets = c(0,1,2), visible = FALSE) # Hide ID column
            )
          ),
          selection = "multiple"
        ) %>%
          formatStyle(
            'Selected', # Column to check
            target = 'row',
            backgroundColor = styleEqual(c(TRUE, FALSE), c('lightblue', 'white'))
          )
      })

      # Select Highlighted Rows
      observeEvent(input$select_highlighted, {
        req(tfl_data())
        current_data <- tfl_data()
        # get the selected rows
        selected_rows <- input$reports_table_rows_selected  
        highlighted_ids <- current_data[selected_rows, ]$id
        to_insert <- current_data %>%
          filter(id %in% highlighted_ids & !Selected)
        if (nrow(to_insert) == 0) {
          showToast("info", "No rows are highlighted.")
        } else {
          tryCatch({
            poolWithTransaction(pool, function(conn) {
              dbExecute(conn, paste(
                "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at) VALUES ",
                paste(sprintf("(%s, %s, '%s', CURRENT_TIMESTAMP)", 
                              reporting_effort(), 
                              to_insert$id, 
                              to_insert$`Report Type`),
                      collapse = ",")
              ))
            })

            refresh_trigger(refresh_trigger() + 1)
            showToast("success", "Highlighted rows saved successfully.")
          }, error = function(e) {
            showToast("error", paste("Error saving highlighted rows:", e$message))
          })
        }
      })



        # Replace showConfirmationDialog with ask_confirmation
        observeEvent(input$deselect_highlighted, {
          req(tfl_data())
          current_data <- tfl_data()
          selected_rows <- input$reports_table_rows_selected  
          highlighted_ids <- current_data[selected_rows, ]$id
          to_delete <- current_data %>%
            filter(id %in% highlighted_ids & Selected)
          
          if (nrow(to_delete) == 0) {
            showToast("info", "No rows are highlighted.")
          } else {
            ask_confirmation(
              inputId = "confirm_deselect",
              title = "Confirm Action",
              text = "Are you sure you want to deselect all highlighted rows?",
              type = "warning",
              btn_labels = c("Cancel", "Confirm"),
              btn_colors = c("#6C757D", "#DC3545")
            )
          }
        })

        # Handle the confirmation response in a separate observeEvent
        observeEvent(input$confirm_deselect, {
          req(input$confirm_deselect)
          current_data <- tfl_data()
          selected_rows <- input$reports_table_rows_selected  
          highlighted_ids <- current_data[selected_rows, ]$id
          to_delete <- current_data %>%
            filter(id %in% highlighted_ids & Selected)
            if (nrow(to_delete) == 0) {
              showToast("info", "No rows to delete.")
            } else {
                tryCatch({
                  poolWithTransaction(pool, function(conn) {
                  dbExecute(conn, paste(
                    "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ", 
                    reporting_effort(),
                    " AND report_id IN (", paste(to_delete$id, collapse = ","), ")"
                  ))
                })
                
                refresh_trigger(refresh_trigger() + 1)
                showToast("success", "Highlighted rows deselected successfully.")
              }, error = function(e) {
                showToast("error", paste("Error deselecting highlighted rows:", e$message))
              })
            }
        })


      # Select All Visible Rows
      observeEvent(input$select_all_visible, {
        req(tfl_data())
        current_data <- tfl_data()
        selected_rows <- input$reports_table_rows_all 
        visible_ids <- current_data[selected_rows, ]$id
        if (length(visible_ids) == 0) {
          showToast("info", "No rows are visible.")
        } else {
          ask_confirmation(
            inputId = "confirm_select_all_visible",
            title = "Confirm Action",
            text = "Are you sure you want to select all visible rows?",
            type = "warning",
            btn_labels = c("Cancel", "Confirm"),
            btn_colors = c("#6C757D", "#DC3545")
          )
        }
      })

      observeEvent(input$confirm_select_all_visible, {
        req(input$confirm_select_all_visible)
        current_data <- tfl_data()
        visible_ids <- current_data[input$reports_table_rows_all, ]$id
        to_insert <- current_data %>%
          filter(id %in% visible_ids & !Selected)
          if (nrow(to_insert) == 0) {
            showToast("info", "No rows to insert.")
          } else {
            tryCatch({
              poolWithTransaction(pool, function(conn) {
                dbExecute(conn, paste(
                "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at) VALUES ",
                paste(sprintf("(%s, %s, '%s', CURRENT_TIMESTAMP)", 
                              reporting_effort(), 
                              to_insert$id, 
                              to_insert$`Report Type`),
                      collapse = ",")
              ))
            })

            refresh_trigger(refresh_trigger() + 1)
            showToast("success", "All visible rows selected.")
            }, error = function(e) {
              showToast("error", paste("Error selecting all visible rows:", e$message))
            })
          }
      })

      # Deselect All Visible Rows
      observeEvent(input$deselect_all_visible, {
        req(tfl_data())
        current_data <- tfl_data()
        visible_ids <- current_data[input$reports_table_rows_all, ]$id
        if (length(visible_ids) == 0) {
          showToast("info", "No rows are visible.")
        } else {
          ask_confirmation(
            inputId = "confirm_deselect_all_visible",
            title = "Confirm Action",
            text = "Are you sure you want to deselect all visible rows?",
            type = "warning",
            btn_labels = c("Cancel", "Confirm"),
            btn_colors = c("#6C757D", "#DC3545")
          )
        }
      })

      observeEvent(input$confirm_deselect_all_visible, {
        req(input$confirm_deselect_all_visible)
        current_data <- tfl_data()
        visible_ids <- current_data[input$reports_table_rows_all, ]$id
        to_delete <- current_data %>%
          filter(id %in% visible_ids & Selected)
          if (nrow(to_delete) == 0) {
            showToast("info", "No rows to delete.")
          } else {
            tryCatch({
              poolWithTransaction(pool, function(conn) {
                dbExecute(conn, paste(
                  "DELETE FROM reporting_effort_reports WHERE reporting_effort_id = ", 
                  reporting_effort(),
                  " AND report_id IN (", paste(to_delete$id, collapse = ","), ")"
                ))
              })

            refresh_trigger(refresh_trigger() + 1)
            showToast("success", "All visible rows deselected.")
            }, error = function(e) {
              showToast("error", paste("Error deselecting all visible rows:", e$message))
            })
          }
      })

    })
}
