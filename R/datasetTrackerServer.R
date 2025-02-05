# Define the datasetTrackerServer module
datasetTrackerServer <- function(id, pool, reporting_effort, ds_type, tables_data, reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_trigger <- reactiveVal(0)

    # Reactive tracker data
    tracker_data <- reactive({
      req(reporting_effort(), tables_data)

      refresh_trigger()
      data <- tables_data$report_programming_tracker() %>%
        dplyr::filter(reporting_effort_id == reporting_effort(), report_type == ds_type) %>%
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
    # Server for programming efforts
    programmingEffortServer("programming_effort", tracker_data, reactive(input$column_selection))

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
          "report_type"
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
        selection = "single",
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
                       dom = 'Bfrtip',
                       columnDefs = col_defs),
        colnames = c(
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
          "Assigned Validation Level" = "qc_level"
        )
      ) %>%
        DT::formatDate("Assign Date", method = "toLocaleDateString") %>%
        DT::formatDate("Due Date", method = "toLocaleDateString") %>%
        DT::formatStyle("Status",
          target = "row",
          backgroundColor = DT::styleEqual(
            c(
              "Not Started",
              "Production Started",
              "Production Ready",
              "Under QC",
              "QC Failed",
              "QC Pass"
            ),
            c(
              "#E0E0E0",
              "#BBE1FA",
              "#C8E6C9",
              "#FFF9C4",
              "#FFCDD2",
              "#A5D6A7"
            )
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
    commentsServer("comments", pool,tables_data, selected_id)
    
    # Observe and handle row editing
    observeEvent(input$edit_button, {
      selected_row <- input$tracker_table_rows_selected

      if (length(selected_row) == 0) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Please select a row before editing.",
          position = "center"
        )
        return()
      }

      df <- tracker_data()
      row_data <- df[selected_row, ]

      # Validate if we have programmers to select from
      prod_prog <- production_programmers()
      qc_prog <- qc_programmers()
      prod_choices <- setNames(prod_prog$id, prod_prog$username)
      qc_choices <- setNames(qc_prog$id, qc_prog$username)

      # Determine the currently selected production programmer's ID
      current_prod_id <- prod_prog %>% dplyr::filter(username == row_data$production_programmer) %>% dplyr::pull(id)
      current_qc_id <- qc_prog %>% dplyr::filter(username == row_data$qc_programmer) %>% dplyr::pull(id)
  

      edit_modal <- modalDialog(
        title = "Edit Dataset Programming Details",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("save_changes"), "Save", class = "btn btn-success"),
          modalButton("Cancel")
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("production_programmer"),
              "Production Programmer:",
              choices = prod_choices,
              selected = ifelse(
                length(current_prod_id) == 1,
                current_prod_id,
                1
              )
            ),
            dateInput(
              ns("assign_date"),
              "Assign Date:",
              value = if (is.na(row_data$assign_date)) {
                Sys.Date()
              } else {
                as.Date(row_data$assign_date, format = "%Y-%m-%d")
              }
            ),
            selectInput(
              ns("status"),
              "Status:",
              choices = c(
                "Not Started",
                "Production Started",
                "Production Ready",
                "Under QC",
                "QC Failed",
                "QC Pass"
              ),
              selected = row_data$status
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("qc_programmer"),
              "QC Programmer:",
              choices = qc_choices,
              selected = ifelse(length(current_qc_id) == 1, current_qc_id, 1)
            ),
            dateInput(
              ns("due_date"),
              "Due Date:",
              value = if (is.na(row_data$due_date)) {
                Sys.Date() + 7
              } else {
                as.Date(row_data$due_date, format = "%Y-%m-%d")
              }
            ),  
            selectInput(
              ns("priority"),
              "Priority (1 <- Highest 5 <- Lowest):",
              choices = 1:5,
              selected = row_data$priority
            ),
            selectInput(
              ns("qc_level"),
              "Assigned Validation Level:",
              choices = 1:3,
              selected = row_data$qc_level
            )
          )
        )
      )

      showModal(edit_modal)
    })

    # Save changes to the database
    observeEvent(input$save_changes, {
      selected_row <- input$tracker_table_rows_selected
      df <- tracker_data()
      row_data <- df[selected_row, ]

      prod_id <- input$production_programmer
      qc_id <- input$qc_programmer
      assign_date <- input$assign_date
      due_date <- input$due_date
      priority <- input$priority
      status <- input$status
      qc_level <- input$qc_level
      if (prod_id == qc_id) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Production and QC programmer cannot be the same person.",
          position = "center"
        )
        return()
      }

      if (due_date <= assign_date) {
        show_toast(
          title = "Edit",
          type = "info",
          text = "Due date must be after the assign date.",
          position = "center"
        )
        return()
      }

      tryCatch(
        {
          dbExecute(
            pool,
            "UPDATE report_programming_tracker
           SET production_programmer_id = ?,
               qc_programmer_id = ?,
               assign_date = ?,
               due_date = ?,
               priority = ?,
               status = ?,
               qc_level = ?,
               updated_at = CURRENT_TIMESTAMP
           WHERE id = ?;",
            params = list(prod_id, qc_id, as.character(assign_date), as.character(due_date), as.integer(priority), status, as.integer(qc_level), row_data$id)
          )

          refresh_trigger(refresh_trigger() + 1)
          show_toast(
            title = "Edit",
            type = "success",
            text = "Record updated successfully.",
            position = "center"
          )
          removeModal()
        },
        error = function(e) {
          show_toast(
            title = "Edit",
            type = "error",
            text = paste("Error updating record:", e$message),
            position = "center"
          )
        }
      )
    })
  })
}
