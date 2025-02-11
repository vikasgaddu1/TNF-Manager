at_dataset_Server <- function(id, pool, tables_data, reporting_effort, reporting_effort_label, ds_type) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      refresh_trigger <- reactiveVal(0)
      # Get all reporting efforts except the current one
      refresh_trigger <- at_Copy_Server("copy_reporting_effort", tables_data, reporting_effort, reporting_effort_label, ds_type, pool, refresh_trigger)


      # Trigger to refresh data
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
                  rpt.report_id IS NULL AND rer.report_type = ?;",
              params = list(ds_type)
            )

            dbExecute(conn, "
              DELETE FROM report_programming_tracker
              WHERE report_type = ?
                AND NOT EXISTS (
                  SELECT 1
                  FROM reporting_effort_reports
                  WHERE reporting_effort_reports.reporting_effort_id = report_programming_tracker.reporting_effort_id
                    AND reporting_effort_reports.report_id = report_programming_tracker.report_id 
                    AND reporting_effort_reports.report_type = report_programming_tracker.report_type
                );",
              params = list(ds_type)
            )
          })

          showNotification("Tracker data synced successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error syncing tracker data:", e$message), type = "error")
        })
      })

      dataset_data <- reactive({
        req(reporting_effort())
        refresh_trigger()

        tables_data$datasets() %>%
          dplyr::left_join(
            tables_data$reporting_effort_reports() %>%
              dplyr::filter(reporting_effort_id == reporting_effort()),
            by = c("id" = "report_id", "dataset_type" = "report_type")
          ) %>%
          dplyr::filter(dataset_type == ds_type) %>%
          dplyr::mutate(Selected = !is.na(reporting_effort_id) & reporting_effort_id == reporting_effort()) %>%
          dplyr::select(id, Selected, dataset_type,category_name, dataset_name, dataset_label) %>%
          dplyr::rename(
            `Report Type` = dataset_type,
            `Category` = category_name,
            `Dataset Name` = dataset_name,
            `Dataset Label` = dataset_label
          ) %>%
          dplyr::arrange(-Selected, Category, `Dataset Name`)
      })

 # download handler for selected rows  as xlsx 
      output$download_dataset <- downloadHandler(
        filename = function() {
          paste0(ds_type, "_" , reporting_effort_label(), "_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          df <- dataset_data() %>% filter(Selected) %>%
          dplyr::select(-c(id, Selected))
          


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

      output$datasets_table <- renderDT({
        req(dataset_data())

        datatable(
          dataset_data(),
          escape = FALSE,
          filter = "top",
          options = list(
            paging = FALSE,
            searching = TRUE,
            search = list(
              regex = TRUE,    # Enable regex matching
              smart = FALSE    # Disable smart (substring) filtering
            ),
            autoWidth = TRUE,
            dom = 'frtip',
            columnDefs = list(
              list(targets = c(0, 1,2), visible = FALSE) # Hide ID and Selected columns 
            )
          ),
          selection = "multiple"
        ) %>%
          formatStyle(
            'Selected',
            target = 'row',
            backgroundColor = styleEqual(c(TRUE, FALSE), c('lightblue', 'white'))
          )
      })

      observeEvent(input$select_highlighted, {
        req(dataset_data())
        current_data <- dataset_data()
        selected_rows <- input$datasets_table_rows_selected  
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
                              ds_type),
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

      observeEvent(input$deselect_highlighted, {
        req(dataset_data())
        current_data <- dataset_data()
        selected_rows <- input$datasets_table_rows_selected  
        highlighted_ids <- current_data[selected_rows, ]$id
        to_delete <- current_data %>%
          filter(id %in% highlighted_ids & Selected)
        if (nrow(to_delete) == 0) {
          showToast("info", "No rows are highlighted.")
        } else {
          # add ask_confirmation  
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

      observeEvent(input$confirm_deselect, {
        req(input$confirm_deselect)
        current_data <- dataset_data()
        selected_rows <- input$datasets_table_rows_selected  
        highlighted_ids <- current_data[selected_rows, ]$id
        to_delete <- current_data %>%
          filter(id %in% highlighted_ids & Selected)
          if (nrow(to_delete) == 0) {
            showToast("info", "No rows to delete.")
          } else {
            tryCatch({
              poolWithTransaction(pool, function(conn) {
                dbExecute(conn, paste(
                        "DELETE FROM reporting_effort_reports WHERE ",
                        paste(sprintf("(reporting_effort_id = %s AND report_id = %s AND report_type = '%s')", 
                                      reporting_effort(), 
                                      to_delete$id, 
                                      ds_type),
                              collapse = " OR ")
                      ))
                    })

                    refresh_trigger(refresh_trigger() + 1)
                    showToast("success", "Highlighted rows deselected successfully.")
                  }, error = function(e) {
              showToast("error", paste("Error deselecting highlighted rows:", e$message))
            })
          }
        })

      observeEvent(input$select_all_visible, {
        req(dataset_data())
        current_data <- dataset_data()
        selected_rows <- input$datasets_table_rows_all 
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
        current_data <- dataset_data()
        visible_ids <- current_data[input$datasets_table_rows_all, ]$id
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
                            ds_type),
                    collapse = ",")
              ))
            })

            refresh_trigger(refresh_trigger() + 1)
            showToast("success", "Visible rows selected successfully.")
          }, error = function(e) {
            showToast("error", paste("Error selecting visible rows:", e$message))
          })
        }
      })  

# Deselect All Visible Rows
      observeEvent(input$deselect_all_visible, {
        req(dataset_data())
        current_data <- dataset_data()
        visible_ids <- current_data[input$datasets_table_rows_all, ]$id
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
        current_data <- dataset_data()
        visible_ids <- current_data[input$datasets_table_rows_all, ]$id
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
