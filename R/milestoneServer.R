# -----------------------------
# Module Server: milestoneServer
# -----------------------------
milestoneServer <- function(id, pool, tables_data, table_name = "milestones") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A refresh trigger to re-read the data after modifications
    refresh_trigger <- reactiveVal(0)
    
    # Prepare display names
    display_name <- "Milestone"
    
    # Load Reporting Efforts into dropdown
    reporting_efforts <- reactive({
      tables_data$reporting_efforts() %>%
        dplyr::mutate(label = paste(study, database_release, reporting_effort, sep = "_")) %>%
        dplyr::select(id, label)
    })

    # Reactive to access the table data from the database.
    data <- reactive({
      refresh_trigger()
      tables_data$milestones() %>%
        left_join(reporting_efforts(), by = c("reporting_effort_id" = "id")) %>%
        mutate(Date = as.Date(Date, origin = "1970-01-01"),
               Reporting_Effort = label)
    })
        
    users <- reactive({
      tables_data$users() %>%
      select(username) %>%
      unique() %>%
      pull(username)
    })

    #update reporting efforts selectize input
    # Load Reporting Efforts into dropdown
    observe({
      req(reporting_efforts())
      current_effort <- isolate(input$formReportingEffort)
      choices <- setNames(reporting_efforts()$id, reporting_efforts()$label)
      
      updateSelectInput(
        session,
        "formReportingEffort",
        choices = choices,
        selected = if (!is.null(current_effort) &&
                       current_effort %in% reporting_efforts()$id)
          current_effort
        else
          reporting_efforts()$id[1]
      )
    })

    #update users selectize input
    observe({
      updateSelectizeInput(session, "formAssignedTo", choices = users(), selected = users()[1])
    })

    # Update the inline form when a row is selected in the table.
    observeEvent(input$milestoneTable_rows_selected, {
      selected <- input$milestoneTable_rows_selected
      if (length(selected) > 0) {
        record <- data()[selected, ]
        updateDateInput(session, "formDate", value = record$Date)
        reporting_effort_id <- reporting_efforts() %>%
          filter(label == record$Reporting_Effort) %>%
          pull(id)
        updateSelectizeInput(session, "formReportingEffort", selected = reporting_effort_id)
        updateTextInput(session, "formMilestone", value = record$Milestone)
        updateSelectizeInput(session, "formAssignedTo", selected = record$Assigned_To)
        updateNumericInput(session, "formPosition", value = record$Position)
        updateSelectizeInput(session, "formStatus", selected = record$Status)
      }
    })
    
    # -----------------------------
    # Add Record (Inline)
    # -----------------------------
    observeEvent(input$addRecord, {
    # if datatable row is selected, don't add
    if (length(input$milestoneTable_rows_selected) > 0) {
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
      if (is.null(input$formMilestone) || input$formMilestone == "") {
        shinyFeedback::showFeedbackDanger("formMilestone", "Milestone is required.")
        return()
      }
      if (is.null(input$formStatus) || input$formStatus == "") {
        shinyFeedback::showFeedbackDanger("formStatus", "Status is required.")
        return()
      }
      shinyFeedback::hideFeedback("formReportingEffort")
      shinyFeedback::hideFeedback("formMilestone")
      shinyFeedback::hideFeedback("formStatus")
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("INSERT INTO %s (Date, reporting_effort_id, Milestone, Assigned_To, Position, Status, updated_at) VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)", table_name),
            params = list(
              as.character(input$formDate),
              input$formReportingEffort,
              input$formMilestone,
              input$formAssignedTo,
              input$formPosition,
              input$formStatus
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
        updateDateInput(session, "formDate", value = Sys.Date())
        updateSelectizeInput(session, "formReportingEffort", selected = reporting_efforts()[1])
        updateTextInput(session, "formMilestone", value = "")
        updateSelectizeInput(session, "formAssignedTo", selected = users()[1])
        updateNumericInput(session, "formPosition", value = 0)
        updateSelectizeInput(session, "formStatus", selected = "On Target")
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding", tolower(display_name), ":", e$message),
          position = "top-end"
        )
      })
    })
    
    # -----------------------------
    # Update Record (Inline)
    # -----------------------------
    observeEvent(input$updateRecord, {
      selected <- input$milestoneTable_rows_selected
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
      if (is.null(input$formMilestone) || input$formMilestone == "") {
        shinyFeedback::showFeedbackDanger("formMilestone", "Milestone is required.")
        return()
      }
      if (is.null(input$formStatus) || input$formStatus == "") {
        shinyFeedback::showFeedbackDanger("formStatus", "Status is required.")
        return()
      }
      shinyFeedback::hideFeedback("formReportingEffort")
      shinyFeedback::hideFeedback("formMilestone")
      shinyFeedback::hideFeedback("formStatus")
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(
            conn,
            sprintf("UPDATE %s SET Date = ?, reporting_effort_id = ?, Milestone = ?, Assigned_To = ?, Position = ?, Status = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?", table_name),
            params = list(
              as.character(input$formDate),
              input$formReportingEffort,
              input$formMilestone,
              input$formAssignedTo,
              input$formPosition,
              input$formStatus,
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
    
    # -----------------------------
    # Delete Record (Inline)
    # -----------------------------
    observeEvent(input$deleteRecord, {
      selected <- input$milestoneTable_rows_selected
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
          dbExecute(conn,
                    sprintf("DELETE FROM %s WHERE id = ?", table_name),
                    params = list(data()[selected, "id"]))
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
    
    # -----------------------------
    # Clear Form
    # -----------------------------
    observeEvent(input$clearForm, {
      updateDateInput(session, "formDate", value = Sys.Date())
      updateSelectizeInput(session, "formReportingEffort", selected = reporting_efforts()[1])
      updateTextInput(session, "formMilestone", value = "")
      updateSelectizeInput(session, "formAssignedTo", selected = users()[1])
      updateNumericInput(session, "formPosition", value = 0)
      updateSelectizeInput(session, "formStatus", selected = "On Target")
    })
    
    # -----------------------------
    # Render DataTable (with client-side filtering)
    # -----------------------------
    output$milestoneTable <- DT::renderDataTable({
      DT::datatable(
        data() %>% select(
          id,
          Date,
          Reporting_Effort,
          Milestone,
          Assigned_To,
          Position,
          Status,
          updated_at
        ),
        colnames = c(
          "ID",
          "Date",
          "Reporting Effort",
          "Milestone",
          "Assigned To",
          "Position",
          "Status",
          "Last Updated"
        ),
        selection = "single",
        filter = "top",
        rownames = FALSE,
                options = list(server = FALSE,  # Enable client-side filtering so we can access filtered rows
columnDefs = list(list(
          targets = c(0, 7), visible = FALSE
        )))
      )
    })
    
    # -----------------------------
    # Plotly Timeline Output (using filtered data)
    # -----------------------------
    getColor <- function(status) {
      if (status == "Complete")
        return("gray")
      else if (status == "On Target")
        return("orange")
      else if (status == "At Risk")
        return("red")
      else if (status == "Critical")
        return("black")
      else
        return("blue")
    }
    
    output$timelinePlot <- renderPlotly({
      # Start with the full data and subset using the filtered rows from the DT
      df <- data()
      req(input$milestoneTable_rows_all)
      if (!is.null(input$milestoneTable_rows_all)) {
        df <- df[input$milestoneTable_rows_all, ]
      }
      df <- df %>% arrange(Date)
      req(nrow(df) > 0)
      df$Color <- sapply(df$Status, getColor)
      
      p <- plot_ly()
      for (s in unique(df$Status)) {
        df_sub <- df %>% filter(Status == s)
        p <- p %>% add_segments(
          data = df_sub,
          x = ~ Date,
          xend = ~ Date,
          y = 0,
          yend = ~ Position,
          line = list(color = getColor(s), width = 2),
          hoverinfo = "none"
        )
      }
      for (s in unique(df$Status)) {
        df_sub <- df %>% filter(Status == s)
        p <- p %>% add_markers(
          data = df_sub,
          x = ~ Date,
          y = ~ Position,
          marker = list(color = getColor(s), size = 8),
          text = ~ paste(
            "Date:", format(Date, "%Y-%m-%d"),
            "<br>Reporting Effort:", Reporting_Effort,
            "<br>Milestone:", Milestone,
            "<br>Assigned To:", Assigned_To,
            "<br>Status:", Status
          ),
          hoverinfo = "text"
        )
      }
      p <- p %>% layout(
        showlegend = FALSE,
        shapes = list(
          list(
            type = "line",
            xref = "x",
            yref = "y",
            x0 = min(df$Date),
            x1 = max(df$Date),
            y0 = 0,
            y1 = 0,
            line = list(
              dash = "dash",
              color = "orange",
              width = 2
            )
          )
        ),
        xaxis = list(
          type = "date",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        hovermode = "closest",
        annotations = {
          ann_list <- list()
          for (i in seq_len(nrow(df))) {
            ann_list[[length(ann_list) + 1]] <- list(
              x = df$Date[i],
              y = 0,
              text = format(df$Date[i], "%b\n%y"),
              showarrow = FALSE,
              xanchor = "center",
              yanchor = "top",
              font = list(color = "black"),
              bgcolor = "white",
              bordercolor = "white"
            )
            yanchor_val <- if (df$Position[i] >= 0) "bottom" else "top"
            ann_list[[length(ann_list) + 1]] <- list(
              x = df$Date[i],
              y = df$Position[i],
              text = df$Milestone[i],
              showarrow = FALSE,
              xanchor = "center",
              yanchor = yanchor_val,
              font = list(color = "white", size = 12),
              bgcolor = getColor(df$Status[i]),
              bordercolor = getColor(df$Status[i])
            )
          }
          ann_list
        }
      )
      p
    })
  })
}


