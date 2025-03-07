mod_at_copy_server <- function(id, tables_data, reporting_effort, reporting_effort_label, task_type, pool, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Get all reporting efforts except the current one
    copy_reporting_efforts <- reactive({
      req(reporting_effort(), reporting_effort_label)
      tables_data$reporting_efforts() %>%
        dplyr::mutate(label = paste(study, database_release, reporting_effort, sep = "_")) %>%
        dplyr::select(id, label) %>%
        dplyr::filter(id != reporting_effort())
    })

    # Reactive: Create choices for the select input
    copy_choices <- reactive({
      req(copy_reporting_efforts())
      choices <- setNames(copy_reporting_efforts()$id, copy_reporting_efforts()$label)
      choices <- c("Select a RE" = "", choices)
      choices
    })

    # Update selectize input with available reporting efforts
    observe({
      req(copy_choices())
      updateSelectizeInput(
        session,
        "copy_reporting_effort",
        choices = copy_choices(),
        selected = NULL,
        options = list(placeholder = "Select a RE")
      )
    })

    # Show confirmation dialog when a selection is made
    observeEvent(input$copy_reporting_effort, {
      req(input$copy_reporting_effort)
      copy_reporting_effort <- input$copy_reporting_effort
      if (copy_reporting_effort == "") return()

      copy_reporting_effort_label <- copy_reporting_efforts() %>%
        dplyr::filter(id == copy_reporting_effort) %>%
        dplyr::pull(label)

      ask_confirmation(
        inputId = ns("confirm_copy"),
        title = "Confirm Action",
        text = paste0("Are you sure you want to copy ", task_type, " from ", copy_reporting_effort_label, " to ", reporting_effort_label(), "?"),
        type = "warning",
        btn_labels = c("Cancel", "Confirm"),
        btn_colors = c("#6C757D", "#DC3545")

      )
    })

    # Handle confirmation
    observeEvent(input$confirm_copy, {
      if (!input$confirm_copy) {
        updateSelectizeInput(
          session,
          "copy_reporting_effort",
          choices = copy_choices(),
          selected = "",
          options = list(placeholder = "Select a RE")
        )
        return()
      }

      req(input$confirm_copy)
      copy_reporting_effort <- input$copy_reporting_effort

      tryCatch({
        poolWithTransaction(pool, function(conn) {
            if (task_type == 'TFL') {
                query_dest <- "SELECT report_id, report_type FROM reporting_effort_reports WHERE report_type IN ('Table', 'Listing', 'Figure') AND reporting_effort_id = %s"

            } else {
                 query_dest <- paste0("SELECT report_id, report_type FROM reporting_effort_reports WHERE report_type = '", task_type, "' AND reporting_effort_id = %s")
            }
            # Fetch records from reporting_effort_reports
            records_to_copy <- dbGetQuery(conn, sprintf(query_dest, copy_reporting_effort))
            # Fetch existing records in the target reporting_effort
            existing_records <- dbGetQuery(conn, sprintf(query_dest, reporting_effort()))
            # Perform anti-join to exclude already existing records
            records_to_copy <- dplyr::anti_join(records_to_copy, existing_records, by = c("report_id", "report_type"))

          if (nrow(records_to_copy) == 0) {
            show_toast(
              title = "No Records Found",
              type = "info",
              text = paste0("Selected RE records already exist in the target RE or no ", task_type, " to copy."),
              position = "center"

            )
          } else {
            # Insert fetched records into reporting_effort_reports
            dbExecute(conn, paste(
              "INSERT INTO reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at) VALUES ",
              paste(sprintf("(%s, %s, '%s', CURRENT_TIMESTAMP)", 
                            reporting_effort(), 
                            records_to_copy$report_id, 
                            records_to_copy$report_type),
                    collapse = ",")
            ))

            refresh_trigger(refresh_trigger() + 1)

            show_toast(
              title = "Copy Successful",
              type = "success",
              text = "Records have been successfully copied.",
              position = "center"
            )
          }

          updateSelectizeInput(
            session,
            "copy_reporting_effort",
            choices = copy_choices(),
            selected = "",
            options = list(placeholder = "Select a RE")
          )
        })
      }, error = function(e) {
        print(e)
        show_toast(
          title = "Copy Failed",
          type = "error",
          text = "An error occurred while copying records.",
          position = "center"
        )
      })
    })
    return(refresh_trigger)
  })
}
