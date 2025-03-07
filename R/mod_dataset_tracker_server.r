# Define the datasetTrackerServer module
mod_dataset_tracker_server <- function(id, pool, reporting_effort, ds_type, tables_data, reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_trigger <- reactiveVal(0)
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
          "Dataset Type" = "report_type",
          "Category" = "category_name",
          "Dataset Name" = "dataset_name",
          "Dataset Label" = "dataset_label",
          "Production Programmer" = "production_programmer",
          "QC Programmer" = "qc_programmer",
          "Assign Date" = "assign_date",
          "Due Date" = "due_date",
          "Priority" = "priority",
          "Status" = "status",
          "Assigned Validation Level" = "qc_level",
          "Comments" = "comments"
        )
    # Reactive tracker data
    ds_tracker_data <- reactive({
      req(reporting_effort(), tables_data)

      refresh_trigger()
      data <- tables_data$report_programming_tracker() %>%
        dplyr::filter(report_type == ds_type) %>%
        dplyr::inner_join(tables_data$reporting_efforts(), join_by(reporting_effort_id == id)) %>%
        dplyr::inner_join(tables_data$datasets(), join_by(report_id == id, report_type == dataset_type)) %>%
        dplyr::left_join(
          tables_data$users() %>% dplyr::select(id, username) %>% dplyr::rename(production_programmer = username),
          by = c("production_programmer_id" = "id")
        ) %>%
        dplyr::left_join(
          tables_data$users() %>% dplyr::select(id, username) %>% dplyr::rename(qc_programmer = username),
          by = c("qc_programmer_id" = "id")
        ) %>%
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
        mutate(priority = as.integer(priority)) %>%
        dplyr::select(
          id,
          study,
          database_release,
          reporting_effort,
          reporting_effort_id,
          report_id,
          report_type,
          dataset_name,
          dataset_label,
          category_name,
          production_programmer,
          qc_programmer,
          assign_date,
          due_date,
          priority,
          status,
          qc_level,
          comments
        ) %>%
        dplyr::arrange(report_type, priority, dataset_name)
      data
    })

    tracker_data <- reactive({
      req(ds_tracker_data())
      ds_tracker_data() %>% dplyr::filter(reporting_effort_id == reporting_effort()) %>%
        dplyr::select(-c(reporting_effort_id, reporting_effort, study, database_release))
    })
    # Server for programming efforts
    mod_programming_effort_server("programming_effort", tracker_data, reactive(input$column_selection), colnames)

    production_programmers <- reactive({
      tables_data$users() %>%
        dplyr::select(id, username)
    })

    qc_programmers <- reactive({
      tables_data$users() %>%
        dplyr::select(id, username)
    })

    observeEvent(tracker_data(), {
      col_names <- colnames(tracker_data())
      updateCheckboxGroupInput(
        session,
        "column_selection",
        choices = col_names,
        selected = c(
          "id",
          "report_type",
          "report_id"
        ) # Default hidden column
      )
    })

    # Download handler for tracker data
    output$download_tracker <- downloadHandler(
      filename = function() {
        paste0(ds_type, "_TRACKER_", reporting_effort_label(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- tracker_data()

        if (is.null(df) || nrow(df) == 0) {
          show_toast(
            title = "Download",
            type = "info",
            text = "No data available to download.",
            position = "center"
          )
          return(NULL)
        }

        write.xlsx(df, file)
      }
    )

    # Render tracker data table
    output$tracker_table <- renderDT({
      req(tracker_data())
      df <- tracker_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No data found")))
      }

      hidden_cols <- input$column_selection
      col_defs <- lapply(seq_along(colnames(df)), function(i) {
        list(
          targets = i - 1,
          # JavaScript column index (0-based)
          visible = !(colnames(df)[i] %in% hidden_cols)
        )
      })

      DT::datatable(
        df,
        selection = "multiple",
        rownames = FALSE,
        filter = "top",
        escape = FALSE,
        options = list(paging = FALSE,
                       searching = TRUE,
                       search = list(
                         regex = TRUE,    # Enable regex matching
                         smart = FALSE    # Disable smart (substring) filtering
                       ),
                       autoWidth = TRUE,
                       dom = 'frtip',
                       columnDefs = col_defs),
        colnames = colnames
      ) %>%
        DT::formatDate("Assign Date", method = "toLocaleDateString") %>%
        DT::formatDate("Due Date", method = "toLocaleDateString") %>%
        DT::formatStyle("Status",
          target = "row",
          backgroundColor = DT::styleEqual(
            names(colorMap),
            unname(colorMap)
          )
        )
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
    mod_assign_task_server("assign_task", pool, tables_data, selected_id, tracker_data)

    return(ds_tracker_data)
  })
}
