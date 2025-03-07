# -----------------------------
# Module Server: mod_decision_log_server
# -----------------------------
mod_decision_log_server <- function(id, pool, tables_data, table_name = "decision_logs") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A refresh trigger to re-read the data after modifications
    refresh_trigger <- reactiveVal(0)
    
    # Prepare display names
    display_name <- "Decision Log Entry"
    
    # Load Reporting Efforts into dropdown
    reporting_efforts <- reactive({
      tables_data$reporting_efforts() %>%
        dplyr::mutate(label = paste(study, database_release, reporting_effort, sep = "_")) %>%
        dplyr::select(id, label)
    })

    # Load users for owners dropdown
    users <- reactive({
      tables_data$users() %>%
      select(username) %>%
      unique() %>%
      pull(username)
    })

    # Reactive to access the table data from the database
    data <- reactive({
      refresh_trigger()
      dbGetQuery(pool, sprintf("SELECT dl.*, re.study || '_' || re.database_release || '_' || re.reporting_effort as Reporting_Effort 
                               FROM %s dl 
                               LEFT JOIN reporting_efforts re ON dl.reporting_effort_id = re.id", table_name)) %>%
        mutate(date_resolved = as.Date(date_resolved))
    })

    # Update reporting efforts selectize input
    observe({
      req(reporting_efforts())
      current_effort <- isolate(input$formReportingEffort)
      choices <- setNames(reporting_efforts()$id, reporting_efforts()$label)
      
      updateSelectInput(
        session,
        "formReportingEffort",
        choices = choices,
        selected = if (!is.null(current_effort) && current_effort %in% reporting_efforts()$id)
          current_effort
        else
          reporting_efforts()$id[1]
      )
    })

    # Update users selectize input
    observe({
        updateSelectizeInput(session, "formOwners", choices = users(), selected = users()[1])
    })

    # Update the inline form when a row is selected in the table
    observeEvent(input$decisionLogTable_rows_selected, {
      selected <- input$decisionLogTable_rows_selected
      if (length(selected) > 0) {
        record <- data()[selected, ]
        updateSelectInput(session, "formReportingEffort", selected = record$reporting_effort_id)
        updateSelectInput(session, "formCategory", selected = record$category)
        updateTextAreaInput(session, "formQuestion", value = record$question)
        updateTextAreaInput(session, "formAnswerRationale", value = record$answer_rationale)
        updateSelectInput(session, "formStatus", selected = record$status)
        updateSelectInput(session, "formConfidence", selected = record$confidence)
        updateSelectizeInput(session, "formOwners", 
                           selected = if (!is.null(record$owners)) strsplit(record$owners, ",")[[1]] else NULL)
        updateTextAreaInput(session, "formComments", value = record$comments)
        updateTextAreaInput(session, "formDownstreamImpact", value = record$downstream_impact)
        updateDateInput(session, "formDateResolved", value = record$date_resolved)
        updateTextAreaInput(session, "formLinks", value = record$links)
      }
    })

    # Add Record
    observeEvent(input$addRecord, {
      # if datatable row is selected, don't add
      if (length(input$decisionLogTable_rows_selected) > 0) {
        show_toast(
          title = "Add Error",
          type = "warning",
          text = "You have a record selected, do you want to update instead or deselect the record and then try to add a new one?",
          position = "top-end"
        )
        return()
      }

      # Validate required fields
      if (is.null(input$formReportingEffort) || input$formReportingEffort == "") {
        shinyFeedback::showFeedbackDanger("formReportingEffort", "Reporting Effort is required.")
        return()
      }
      if (is.null(input$formQuestion) || input$formQuestion == "") {
        shinyFeedback::showFeedbackDanger("formQuestion", "Question is required.")
        return()
      }
      if (is.null(input$formAnswerRationale) || input$formAnswerRationale == "") {
        shinyFeedback::showFeedbackDanger("formAnswerRationale", "Answer/Rationale is required.")
        return()
      }
      
      shinyFeedback::hideFeedback("formReportingEffort")
      shinyFeedback::hideFeedback("formQuestion")
      shinyFeedback::hideFeedback("formAnswerRationale")

      tryCatch({
        poolWithTransaction(pool, function(conn) {
          owners_string <- paste(input$formOwners, collapse = ",")
          dbExecute(
            conn,
            sprintf("INSERT INTO %s (reporting_effort_id, category, question, answer_rationale, status, confidence, owners, comments, downstream_impact, date_resolved, links, updated_at) 
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)", table_name),
            params = list(
              input$formReportingEffort,
              input$formCategory,
              input$formQuestion,
              input$formAnswerRationale,
              input$formStatus,
              input$formConfidence,
              owners_string,
              input$formComments,
              input$formDownstreamImpact,
              as.character(input$formDateResolved),
              input$formLinks
            )
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = paste(display_name, "Added"),
          type = "success",
          text = paste(display_name, "added successfully!"),
          position = "top-end"
        )
        # Clear form after adding
        updateSelectInput(session, "formReportingEffort", selected = reporting_efforts()$id[1])
        updateSelectInput(session, "formCategory", selected = "Analysis")
        updateTextAreaInput(session, "formQuestion", value = "")
        updateTextAreaInput(session, "formAnswerRationale", value = "")
        updateSelectInput(session, "formStatus", selected = "Open")
        updateSelectInput(session, "formConfidence", selected = "Medium")
        updateSelectizeInput(session, "formOwners", selected = NULL)
        updateTextAreaInput(session, "formComments", value = "")
        updateTextAreaInput(session, "formDownstreamImpact", value = "")
        updateDateInput(session, "formDateResolved", value = NULL)
        updateTextAreaInput(session, "formLinks", value = "")
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })

    # Update Record
    observeEvent(input$updateRecord, {
      selected <- input$decisionLogTable_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Update Error",
          type = "warning",
          text = "No row selected for update.",
          position = "top-end"
        )
        return()
      }

      # Validate required fields
      if (is.null(input$formReportingEffort) || input$formReportingEffort == "") {
        shinyFeedback::showFeedbackDanger("formReportingEffort", "Reporting Effort is required.")
        return()
      }
      if (is.null(input$formQuestion) || input$formQuestion == "") {
        shinyFeedback::showFeedbackDanger("formQuestion", "Question is required.")
        return()
      }
      if (is.null(input$formAnswerRationale) || input$formAnswerRationale == "") {
        shinyFeedback::showFeedbackDanger("formAnswerRationale", "Answer/Rationale is required.")
        return()
      }

      shinyFeedback::hideFeedback("formReportingEffort")
      shinyFeedback::hideFeedback("formQuestion")
      shinyFeedback::hideFeedback("formAnswerRationale")

      tryCatch({
        poolWithTransaction(pool, function(conn) {
          owners_string <- paste(input$formOwners, collapse = ",")
          dbExecute(
            conn,
            sprintf("UPDATE %s SET reporting_effort_id = ?, category = ?, question = ?, answer_rationale = ?, status = ?, confidence = ?, owners = ?, comments = ?, downstream_impact = ?, date_resolved = ?, links = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?", table_name),
            params = list(
              input$formReportingEffort,
              input$formCategory,
              input$formQuestion,
              input$formAnswerRationale,
              input$formStatus,
              input$formConfidence,
              owners_string,
              input$formComments,
              input$formDownstreamImpact,
              as.character(input$formDateResolved),
              input$formLinks,
              data()[selected, "id"]
            )
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = paste(display_name, "Updated"),
          type = "success",
          text = paste(display_name, "updated successfully!"),
          position = "top-end"
        )
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error updating", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })

    # Delete Record
    observeEvent(input$deleteRecord, {
      selected <- input$decisionLogTable_rows_selected
      if (length(selected) == 0) {
        show_toast(
          title = "Delete Error",
          type = "warning",
          text = "No row selected for deletion.",
          position = "top-end"
        )
        return()
      }

      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("DELETE FROM %s WHERE id = ?", table_name),
            params = list(data()[selected, "id"])
          )
        })
        refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = paste(display_name, "Deleted"),
          type = "success",
          text = paste(display_name, "deleted successfully!"),
          position = "top-end"
        )
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error deleting", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })

    # Clear Form
    observeEvent(input$clearForm, {
      updateSelectInput(session, "formReportingEffort", selected = reporting_efforts()$id[1])
      updateSelectInput(session, "formCategory", selected = "Analysis")
      updateTextAreaInput(session, "formQuestion", value = "")
      updateTextAreaInput(session, "formAnswerRationale", value = "")
      updateSelectInput(session, "formStatus", selected = "Open")
      updateSelectInput(session, "formConfidence", selected = "Medium")
      updateSelectizeInput(session, "formOwners", selected = NULL)
      updateTextAreaInput(session, "formComments", value = "")
      updateTextAreaInput(session, "formDownstreamImpact", value = "")
      updateDateInput(session, "formDateResolved", value = NULL)
      updateTextAreaInput(session, "formLinks", value = "")
    })

    # Render DataTable
    output$decisionLogTable <- DT::renderDataTable({
      DT::datatable(
        data() %>% select(
          id,
          Reporting_Effort,
          category,
          question,
          answer_rationale,
          status,
          confidence,
          owners,
          comments,
          downstream_impact,
          date_resolved,
          links,
          updated_at
        ),
        colnames = c(
          "ID",
          "Reporting Effort",
          "Category",
          "Question",
          "Answer/Rationale",
          "Status",
          "Confidence",
          "Owner(s)",
          "Comments",
          "Downstream Impact",
          "Date Resolved",
          "Links",
          "Last Updated"
        ),
        selection = "single",
        filter = "top",
        rownames = FALSE,
        options = list(
          server = FALSE,
          columnDefs = list(list(
            targets = c(0, 12),
            visible = FALSE
          ))
        )
      )
    })
  })
} 