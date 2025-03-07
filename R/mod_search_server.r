mod_search_server <- function(id, tfl_tracker_data, sdtm_tracker_data, adam_tracker_data) {
  moduleServer(id, function(input, output, session) {
    # Define column name mappings
    colnames_mapping <- c(
      "study" = "Study",
      "database_release" = "Database Release",
      "reporting_effort" = "Reporting Effort",
      "category_name" = "Category",
      "sub_category_name" = "Sub-Category",
      "report_key" = "Report Key",
      "report_type" = "Report Type",
      "report_ich_number" = "ICH Number",
      "title_key" = "Title Key",
      "titles" = "Titles",
      "footnotes" = "Footnotes",
      "population_text" = "Population",
      "production_programmer" = "Production Programmer",
      "qc_programmer" = "QC Programmer",
      "status" = "Status",
      "priority" = "Priority",
      "assign_date" = "Assign Date",
      "due_date" = "Due Date",
      "qc_level" = "QC Level",
      "suggested_ich_number" = "Suggested ICH Number",
      "dataset_name" = "Dataset Name",
      "dataset_label" = "Dataset Label",
      "dataset_type" = "Dataset Type",
      "category" = "Category",
      "comments" = "Comments"
    )
    
    output$tfl_tracker <- DT::renderDT({
      data <- tfl_tracker_data() %>% 
        dplyr::select(-contains("id"))
      
      # Rename columns using the mapping
      colnames(data) <- sapply(colnames(data), function(x) {
        ifelse(x %in% names(colnames_mapping), colnames_mapping[x], x)
      })
      
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
    })

    output$sdtm_tracker <- DT::renderDT({
      data <- sdtm_tracker_data() %>% 
        dplyr::select(-contains("id"))
      
      # Rename columns using the mapping
      colnames(data) <- sapply(colnames(data), function(x) {
        ifelse(x %in% names(colnames_mapping), colnames_mapping[x], x)
      })
      
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
    })

    output$adam_tracker <- DT::renderDT({
      data <- adam_tracker_data() %>% 
        dplyr::select(-contains("id"))
      
      # Rename columns using the mapping
      colnames(data) <- sapply(colnames(data), function(x) {
        ifelse(x %in% names(colnames_mapping), colnames_mapping[x], x)
      })
      
      datatable(
        data,
        rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
    })
  })
}

