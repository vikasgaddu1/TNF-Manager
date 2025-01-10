at_tfl_Server <- function(id, tables_data, reporting_effort, reporting_effort_label) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Reactive to fetch and cache TFL data from the database
      tfl_data <- reactive({
        req(reporting_effort())
        
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
          dplyr::summarise(Title = paste(title_text, collapse = "@#"))
        
        # Debugging: Print the number of titles processed
        # cat("Number of Titles processed:", nrow(titles), "\n")
        
        # Process Footnotes
        footnotes <- tables_data$report_footnotes() %>%
          dplyr::left_join(tables_data$footnotes(), join_by(footnote_id == id)) %>%
          dplyr::group_by(report_id) %>%
          dplyr::summarise(Footnotes = paste(footnote_text, collapse = "@#"))
        
        # Debugging: Print the number of footnotes processed
        # cat("Number of Footnotes processed:", nrow(footnotes), "\n")
        
        # Combine all data
        reports <- reports %>%
          dplyr::left_join(titles, join_by(id == report_id)) %>%
          dplyr::left_join(footnotes, join_by(id == report_id)) %>%
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
          dplyr::arrange(report_type, category_name, sub_category_name, population_text, report_ich_number)
        
        # Debugging: Print the final number of reports
        # cat("Final number of Reports:", nrow(reports), "\n")
        
        reports
      })
      
      # Reactive to filter TFL data based on dropdowns and search input
      reports <- reactive({
        req(tfl_data()) # Ensure tfl_data is available
        
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
      
      # Render rHandsontable
      output$reports_table <- renderRHandsontable({
        req(reports())
        # Remove id from display but keep it for backend operations
        reports_data <- reports() %>%
          select(id, Selected, report_type, report_key, title_key, category_name, sub_category_name, report_ich_number, population_text, Title, Footnotes) %>%
          arrange(report_type, report_key, title_key, category_name, sub_category_name, population_text, report_ich_number)
        
        rhandsontable(
          reports_data,
          useTypes = TRUE, # Enable type detection
          rowHeaders = FALSE # Disable row headers
        ) %>%
          hot_col("id", readOnly = TRUE, width = 1) %>%
          hot_col("Selected", type = "checkbox", halign = "center") %>% # Make Selected column a checkbox
          hot_col("report_type", readOnly = TRUE, halign = "left") %>%
          hot_col("report_key", readOnly = TRUE, halign = "left") %>%
          hot_col("title_key", readOnly = TRUE, halign = "left") %>%
          hot_col("category_name", readOnly = TRUE, halign = "left") %>%
          hot_col("sub_category_name", readOnly = TRUE, halign = "left") %>%
          hot_col("report_ich_number", readOnly = TRUE, halign = "center") %>%
          hot_col("population_text", readOnly = TRUE, halign = "left") %>%
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
          edited_data <- hot_to_r(input$reports_table)
          
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
          
          # Standardize column names (replace space with underscore)
          edited_data <- edited_data %>%
            dplyr::rename(report_type = `Report Type`) # Adjust column name here
          
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
              "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type) VALUES ",
              paste(
                sprintf("(%s, %s, '%s')", 
                        reporting_effort(), 
                        selected_reports$id, 
                        selected_reports$report_type),
                collapse = ","
              )
            )
            poolWithTransaction(pool, function(conn) {
              dbExecute(conn, query_reporting_effort)
            })
          }
          
          # Refresh trigger
          show_toast(
            title = "Success",
            type = "success",
            text = "Selection saved successfully!",
            position = "top-end"
          )
        }, error = function(e) {
          show_toast(
            title = "Error",
            type = "error",
            text = "Error during save operation",
            position = "top-end"
          )
        })
      })
    }
  )
}