mod_tfl_tracker_server <- function(id, pool, reporting_effort,tables_data,reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # refresh trigger
    refresh_trigger <- reactiveVal(0)
    # Create a named vector mapping statuses to colors
    colorMap <- c(
      "Not Started"        = "#E0E0E0",
      "Production Started" = "#BBE1FA",
      "Production Ready"   = "#C8E6C9",
      "Under QC"           = "#FFF9C4",
      "QC Failed"          = "#FFCDD2",
      "QC Pass"            = "#A5D6A7"
    )

    colnames <- c(
          "ID" = "id",
          "Report Type" = "report_type",
          "Category" = "category_name",
          "Subcategory" = "sub_category_name",
          "Report Key" = "report_key",
          "Title Key" = "title_key",
          "Titles" = "titles",
          "Footnotes" = "footnotes",
          "ICH Number" = "report_ich_number",
          "Population" = "population_text",
          "Production Programmer" = "production_programmer",
          "Assign Date" = "assign_date",
          "Assigned Validation Level" = "qc_level",
          "QC Programmer" = "qc_programmer",
          "Due Date" = "due_date",
          "Priority" = "priority",
          "Status" = "status",
          "Comments" = "comments"
        )
    tfl_tracker_data <- reactive({
      req(reporting_effort(), tables_data)
      refresh_trigger()
      titles <- tables_data$report_titles() %>%
        dplyr::left_join(tables_data$titles(), join_by(title_id == id)) %>%
        dplyr::group_by(report_id) %>%
        dplyr::summarise(titles = paste(title_text, collapse = "<br>"), .groups = "drop")

      footnotes <- tables_data$report_footnotes() %>%
        dplyr::left_join(tables_data$footnotes(), join_by(footnote_id == id)) %>%
        dplyr::group_by(report_id) %>%
        dplyr::summarise(footnotes = paste(footnote_text, collapse = "<br>"), .groups = "drop")
      # Use dplyr to merge tables
        data <- tables_data$report_programming_tracker() %>%
          dplyr::filter(
            report_type %in% c('Table', 'Listing', 'Figure')
          ) %>%
          dplyr::inner_join(tables_data$reporting_efforts(), join_by(reporting_effort_id == id)) %>%
          dplyr::inner_join(tables_data$reports(), join_by(report_id == id, report_type == report_type)) %>%
          dplyr::left_join(tables_data$categories(), join_by(report_category_id == id)) %>%
          dplyr::left_join(tables_data$sub_categories(), join_by(report_sub_category_id == id)) %>%
          dplyr::left_join(tables_data$populations(), join_by(population_id == id)) %>%
          dplyr::left_join(
            tables_data$users() %>% dplyr::rename(production_programmer = username),
            join_by(production_programmer_id == id)
          ) %>%
          dplyr::left_join(
            tables_data$users() %>% dplyr::rename(qc_programmer = username),
            join_by(qc_programmer_id == id)
          ) %>%
          dplyr::left_join(titles, join_by(report_id == report_id)) %>%
          dplyr::left_join(footnotes, join_by(report_id == report_id)) %>%
          dplyr::left_join(
            tables_data$comments() %>%
              dplyr::group_by(report_programming_tracker_id) %>%
              dplyr::arrange(desc(updated_at)) %>%
              dplyr::summarise(
                comments = paste(comment, collapse = "<br>"),
                .groups = "drop"
              ),
            join_by(id == report_programming_tracker_id)
          ) %>%
          dplyr::left_join(
            tables_data$custom_footnotes() %>%
              dplyr::inner_join(tables_data$footnotes(), join_by(footnote_id == id)) %>%
              dplyr::group_by(report_programming_tracker_id) %>%
              dplyr::summarise(
                custom_footnotes = paste(footnote_text, collapse = "<br>"),
                .groups = "drop"
              ),
            join_by(id == report_programming_tracker_id)
          ) %>%
          dplyr::left_join(
            tables_data$custom_populations() %>%
              dplyr::inner_join(tables_data$populations(), join_by(population_id == id)) %>%
              dplyr::rename(custom_populations = population_text) %>%
              dplyr::select(report_programming_tracker_id, custom_populations),
            join_by(id == report_programming_tracker_id)
          )%>%
          dplyr::mutate(
            footnotes = coalesce(custom_footnotes, footnotes),
            population_text = coalesce(custom_populations, population_text)
          ) %>%
          dplyr::arrange(desc(priority), assign_date) %>%
          dplyr::select(
            study,
            database_release,
            reporting_effort,
            report_type,
            category_name,
            sub_category_name,
            report_key,
            title_key,
            titles,
            footnotes,
            report_ich_number,
            population_text,
            production_programmer,
            assign_date,
            qc_programmer,
            due_date,
            priority,
            status,
            qc_level,
            comments,
            id,
            reporting_effort_id, 
            report_id,
            production_programmer_id,
            qc_programmer_id,              
            report_category_id,
            report_sub_category_id,
            population_id,
            category_id,
            suggested_ich_number
          ) %>% 
          #sort by priority, report_type, report_key, title_key
          dplyr::arrange(priority,report_type, report_key, title_key)

          # print(data %>% dplyr::filter(id == 1670))
          return(data)
    })

    tracker_data <- reactive({
      req(tfl_tracker_data())
      tfl_tracker_data() %>% dplyr::filter(reporting_effort_id == reporting_effort())
    })

    

    mod_programming_effort_server("programming_effort", tracker_data, reactive(input$column_selection), colnames)


    
    observeEvent(tracker_data(), {
      col_names <- colnames(tracker_data())
      updateCheckboxGroupInput(
        session,
        "column_selection",
        choices = col_names,
        selected = c(
          "id",
          "reporting_effort_id", 
          "report_id",
          "production_programmer_id",
          "qc_programmer_id",
          "study",
          "database_release",
          "reporting_effort",
          "report_category_id",
          "report_sub_category_id",
          "population_id",
          "category_id",
          "suggested_ich_number",
          "role.x",
          "role.y",
          "report_key",
          "sub_category_name",
          "report_ich_number"

        ) # Default hidden column
      )
    })
    
    # Download handler for tracker data
    output$download_tracker <- downloadHandler(
      filename = function() {
        paste0("TFL_TRACKER_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- tracker_data()
        
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
    
   
output$tracker_table <- renderDT({
      req(tracker_data())
      df <- tracker_data() 
  
      # If df is empty, return a notification or a placeholder
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data found")))
      }
      
      hidden_cols <- input$column_selection
      col_defs <- lapply(seq_along(colnames(df)), function(i) {
        list(targets = i - 1,
             # JavaScript column index (0-based)
             visible = !(colnames(df)[i] %in% hidden_cols))
      })
      
      # allow html in comments column
      DT::datatable(
        df,
        selection = "multiple",
        rownames = FALSE,
        filter = "top",
        escape = FALSE,

        options = list(
          paging = FALSE,
          searching = TRUE,
          search = list(
            regex = TRUE,    # Enable regex matching
            smart = FALSE    # Disable smart (substring) filtering
          ),
          autoWidth = TRUE,
          scrollX = TRUE,
          select = TRUE,  # Enable row selection (required for the buttons to work)
          responsive = TRUE,  
          dom = 'frtip',
          columnDefs = col_defs

        ),
        colnames = colnames) %>%
              DT::formatDate("Assign Date", method = "toLocaleDateString") %>%
              DT::formatDate("Due Date", method = "toLocaleDateString") %>%
              DT::formatStyle("Status",
                              target = "row",
                              backgroundColor = DT::styleEqual(
                                names(colorMap),
                                unname(colorMap)
                              ))
          })
    
    
    # Storing selected_id for update
    selected_id <- reactiveVal(NULL)

      observe({
        selected_row <- input$tracker_table_rows_selected
        if (is.null(selected_row) || length(selected_row) == 0) {
          selected_id(0)
        } else {
          df <- tracker_data()
          row_data <- df[selected_row, ]
          selected_id(row_data$id)
        }
      })
    mod_comments_server("comments", pool,tables_data, selected_id)
    mod_edit_population_server("edit_population", pool, tables_data, selected_id)
    mod_edit_custom_footnote_server("edit_fn_button", pool, tables_data, selected_id)
    mod_assign_task_server("assign_task", pool, tables_data, selected_id, tracker_data)
    
    return(tfl_tracker_data)
  })
}