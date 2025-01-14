tableSelectorServer <- function(id, tables_data, table_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tracker_data <- reactive({
      req(tables_data)
      
      # Use dplyr to merge tables
      data <- tables_data$report_programming_tracker() %>%
              dplyr::left_join(tables_data$reports(), join_by(report_id == id, report_type == report_type)) %>%
              dplyr::left_join(tables_data$datasets(), join_by(report_id == id, report_type == dataset_type))  %>% 
              dplyr::left_join(tables_data$reporting_efforts(), join_by(reporting_effort_id == id))  %>% 
              dplyr::left_join(tables_data$categories(), join_by(report_category_id == id)) %>%
              dplyr::left_join(tables_data$users() %>% dplyr::rename(production_programmer = username), join_by(production_programmer_id == id)) %>%
              dplyr::left_join(tables_data$users() %>% dplyr::rename(qc_programmer = username), join_by(qc_programmer_id == id)) %>%
              dplyr::select(-dplyr::starts_with("updated_at")) %>% 
              # collasce category_name.x and category_name.y
              dplyr::mutate(category_name = dplyr::coalesce(category_name.x, category_name.y)) %>%
              dplyr::select(-dplyr::starts_with("category_name.x"), -dplyr::starts_with("category_name.y")) %>% 
              # collasce report_key and dataset_name
              dplyr::mutate(report_key = dplyr::coalesce(report_key,dataset_name)) %>%
              dplyr::select(-dplyr::starts_with("dataset_name")) %>% 
              # select study	database_release	reporting_effort	report_type	category_name	report_key	production_programmer	assign_date	qc_programmer	due_date	status
              dplyr::select(study, database_release, reporting_effort, report_type, category_name, report_key, priority, production_programmer, assign_date, qc_programmer, due_date, status) %>%
              # if production_programmer is null, set to "Not Assigned"
              dplyr::mutate(production_programmer = dplyr::if_else(is.na(production_programmer), "Not Assigned", production_programmer)) %>%
              dplyr::mutate(qc_programmer = dplyr::if_else(is.na(qc_programmer), "Not Assigned", qc_programmer))
    })

    # add download button to export tracker_data into xlsx
    output$merge_download_button <- downloadHandler(
      filename = function() {
        paste0("report_programming_tracker_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
          req(tracker_data())
          openxlsx::write.xlsx(tracker_data(), file)  
      }
    )

    output$download_button <- downloadHandler(
      filename = function() {
        req(input$table_dropdown)
        paste0(input$table_dropdown, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
          req(tables_data[[input$table_dropdown]]())
          openxlsx::write.xlsx(tables_data[[input$table_dropdown]](), file)  
      }
    )
    # Update the dropdown choices dynamically
    observe({
      updateSelectInput(session, "table_dropdown", choices = table_names)
    })
    
    # Render selected table
    output$table_view <- renderTable({
      req(input$table_dropdown)
      tables_data[[input$table_dropdown]]()
    })
    
    return(tracker_data)
  })
}
