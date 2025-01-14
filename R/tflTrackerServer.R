tflTrackerServer <- function(id, pool, reporting_effort,tables_data,reporting_effort_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # refresh trigger
    refresh_trigger <- reactiveVal(0)
    
    tracker_data <- reactive({
      req(reporting_effort(), tables_data)
      refresh_trigger()
      # Use dplyr to merge tables
        data <- tables_data$report_programming_tracker() %>%
          dplyr::filter(
            reporting_effort_id == reporting_effort() & 
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
          dplyr::mutate(
            titles = purrr::map_chr(id, ~ {
              tables_data$report_titles() %>%
                dplyr::filter(report_id == .x) %>%
                dplyr::inner_join(tables_data$titles(), join_by(title_id == id)) %>%
                dplyr::pull(title_text) %>%
                paste(collapse = "<br>")
            }),
            footnotes = purrr::map_chr(id, ~ {
              tables_data$report_footnotes() %>%
                dplyr::filter(report_id == .x) %>%
                dplyr::inner_join(tables_data$footnotes(), join_by(footnote_id == id)) %>%
                dplyr::pull(footnote_text) %>%
                paste(collapse = "<br>")
            })
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
          dplyr::arrange(desc(priority), assign_date) %>%
          dplyr::select(
            report_type,
            category_name,
            sub_category_name,
            report_key,
            title_key,
            titles,
            report_ich_number,
            population_text,
            production_programmer,
            assign_date,
            qc_programmer,
            due_date,
            priority,
            status,
            comments,
            id,
            reporting_effort_id, 
            report_id,
            production_programmer_id,
            qc_programmer_id,
            study,
            database_release,
            reporting_effort,
            report_category_id,
            report_sub_category_id,
            population_id,
            category_id,
            suggested_ich_number,
            role.x,
            role.y,
            footnotes
          ) %>% 
          #sort by report_type, priority, report_key, title_key
          dplyr::arrange(report_type, priority, report_key, title_key)
    })

    
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
          "footnotes"

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
        selection = "single",
        rownames = FALSE,
        filter = "top",
        escape = FALSE,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = col_defs
        ),
        colnames = c(
          "Report Type" = "report_type",
          "Category" = "category_name",
          "Subcategory" = "sub_category_name",
          "Report Key" = "report_key",
          "Title Key" = "title_key",
          "Titles" = "titles",
          "ICH Number" = "report_ich_number",
          "Population" = "population_text",
          "Production Programmer" = "production_programmer",
          "Assign Date" = "assign_date",
          "QC Programmer" = "qc_programmer",
          "Due Date" = "due_date",
          "Priority" = "priority",
          "Status" = "status",
          "Comments" = "comments"
        )) %>%
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
    commentsServer("comments", pool,tables_data, selected_id)

    observeEvent(input$edit_button, {
      selected_row <- input$tracker_table_rows_selected

      if (is.null(selected_row) || length(selected_row) == 0) {
        show_toast(
          title = "Edit Report Programming Details",
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
      
      # If no production programmers found, set to empty
      if (is.null(prod_prog) || nrow(prod_prog) == 0) {
        prod_choices <- c("Not Assigned" = "")
      } else {
        prod_choices <- c("Not Assigned" = "",
                          setNames(prod_prog$id, prod_prog$username))
      }
      
      # If no QC programmers found, set to empty
      if (is.null(qc_prog) || nrow(qc_prog) == 0) {
        qc_choices <- c("Not Assigned" = "")
      } else {
        qc_choices <- c("Not Assigned" = "",
                        setNames(qc_prog$id, qc_prog$username))
      }
      
      # Determine the currently selected production programmer's ID
      current_prod_id <- if (!is.na(row_data$production_programmer) &&
                             row_data$production_programmer != "") {
        prod_prog$id[prod_prog$username == row_data$production_programmer]
      } else {
        ""  # Not Assigned
      }
      
      # Determine the currently selected QC programmer's ID
      current_qc_id <- if (!is.na(row_data$qc_programmer) &&
                           row_data$qc_programmer != "") {
        qc_prog$id[qc_prog$username == row_data$qc_programmer]
      } else {
        ""  # Not Assigned
      }
      
      edit_modal <- modalDialog(
        title = "Edit Report Programming Details",
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
              selected = ifelse(length(current_prod_id) == 1, current_prod_id, "")
            ),
            dateInput(
              ns("assign_date"),
              "Assign Date:",
              value = if(is.na(row_data$assign_date)) {
                Sys.Date()
              } else {
                as.Date(row_data$assign_date, format = "%Y-%m-%d")
              }),
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
              selected = ifelse(is.na(row_data$status), "Not Started", row_data$status)
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("qc_programmer"),
              "QC Programmer:",
              choices = qc_choices,
              selected = ifelse(length(current_qc_id) == 1, current_qc_id, "")
            ),
            dateInput(
              ns("due_date"),
              "Due Date:",
              value = if(is.na(row_data$due_date)) {
                Sys.Date() + 7
              } else {
                as.Date(row_data$due_date, format = "%Y-%m-%d")
              }
            ),  
            selectInput(
              ns("priority"),
              "Priority (1 <- Highest 5 <- Lowest):",
              choices = 1:5,
              # Numeric range
              selected = ifelse(
                is.na(row_data$priority),
                1,
                as.integer(row_data$priority)
              )
            )
          )
        )
      )
      
      showModal(edit_modal)
    })
    
    observeEvent(input$save_changes, { 
      prod_id <- input$production_programmer
      qc_id <- input$qc_programmer
      assign_date <- input$assign_date
      due_date <- input$due_date
      priority <- input$priority
      status <- input$status
      
          
      if (prod_id == qc_id) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = "Production and QC programmer cannot be the same person.",
          position = "center"
        )
        return()
      }
      
      if (due_date <= assign_date) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = "Due date must be after the assign date.",
          position = "center"
        )
        return()
      }

      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            "UPDATE report_programming_tracker
         SET production_programmer_id = ?,
             assign_date = ?,
             qc_programmer_id = ?,
             due_date = ?,
             priority = ?,
             status = ?,
             updated_at = CURRENT_TIMESTAMP
         WHERE id = ? ;",
            params = list(
              prod_id,
              as.character(assign_date),
              qc_id,
              as.character(due_date),
              as.integer(priority),
              status,
              selected_id()
            )
          )
        })

         refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Edit Report Programming Details",
          type = "success",
          text = "Record updated successfully.",
          position = "center"
        )
        removeModal()}, error = function(e) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = paste("Error updating record:", e$message),
          position = "center"
        )
      })
    })
    
  })
}