programmingTrackerServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "tracker") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing tracker data",
                         type = "message",
                         duration = 1)
      }
    }, ignoreInit = TRUE)
    
    # Load Reporting Efforts into dropdown
    observeEvent(refresh_trigger(), {
      reporting_efforts <- tryCatch({
        # DO NOT call refresh_trigger() here again
        dbGetQuery(
          pool,
          "SELECT id,
              study || ' | ' || database_release || ' | ' || reporting_effort AS label
       FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message),
                         type = "error")
        NULL
      })
      
      if (!is.null(reporting_efforts) &&
          nrow(reporting_efforts) > 0) {
        current_effort <- isolate(input$reporting_effort) # Remember current selection
        choices <- setNames(reporting_efforts$id, reporting_efforts$label)
        
        updateSelectInput(
          session,
          "reporting_effort",
          choices = choices,
          # If the current effort is still in the new choices, reselect it
          selected = if (!is.null(current_effort) &&
                         current_effort %in% reporting_efforts$id)
            current_effort
          else
            reporting_efforts$id[1]
        )
      }
    })
    
    tracker_data <- reactive({
      req(input$reporting_effort)
      refresh_trigger()
      
      query <- sprintf(
        "
          SELECT
            rpt.id,
            re.study,
            re.database_release,
            re.reporting_effort,
            r.report_key,
            r.report_type,
            r.title_key,
            c.category_name AS category,
            sc.sub_category_name AS subcategory,
            r.report_ich_number AS ich_number,
            p.population_text AS population,
            (SELECT GROUP_CONCAT(t2.title_text, '@#')
             FROM report_titles rt2 
             JOIN titles t2 ON rt2.title_id = t2.id 
             WHERE rt2.report_id = r.id) AS titles,
            (SELECT GROUP_CONCAT(f2.footnote_text, '@#')
             FROM report_footnotes rf2 
             JOIN footnotes f2 ON rf2.footnote_id = f2.id 
             WHERE rf2.report_id = r.id) AS footnotes,
            prod.username AS production_programmer,
            qc.username AS qc_programmer,
            rpt.assign_date,
            rpt.due_date,
            rpt.priority,
            rpt.status
          FROM report_programming_tracker rpt
          INNER JOIN reporting_efforts re
            ON rpt.reporting_effort_id = re.id
          INNER JOIN reports r
            ON rpt.report_id = r.id
          LEFT JOIN reporting_effort_reports rer
            ON rer.reporting_effort_id = re.id
            AND rer.report_id = r.id
          LEFT JOIN categories c
            ON r.report_category_id = c.id
          LEFT JOIN sub_categories sc
            ON r.report_sub_category_id = sc.id
          LEFT JOIN populations p
            ON r.population_id = p.id
          LEFT JOIN users prod
            ON rpt.production_programmer_id = prod.id
          LEFT JOIN users qc
            ON rpt.qc_programmer_id = qc.id
          WHERE rpt.reporting_effort_id = %d 
          AND rpt.report_type in ('Table','Listing','Figure')
          GROUP BY
            rpt.id,
            re.study,
            re.database_release,
            re.reporting_effort,
            r.report_key,
            r.report_type,
            r.title_key,
            c.category_name,
            sc.sub_category_name,
            r.report_ich_number,
            p.population_text,
            prod.username,
            qc.username,
            rpt.assign_date,
            rpt.due_date,
            rpt.priority,
            rpt.status
ORDER BY rpt.priority DESC, rpt.assign_date DESC;  ",
        as.integer(input$reporting_effort)
      )
      
      
      data <- tryCatch({
        dbGetQuery(pool, query)
      }, error = function(e) {
        showNotification(paste("Error loading tracker data:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(data)) {
        data$priority <- as.integer(data$priority)  # Convert priority to integer
      }
      
      data
    })
    
    programmingEffortServer("programming_effort", tracker_data)
    refresh_trigger <- tflTrackerServer("tfl_tracker", tracker_data, pool, refresh_trigger)
    
  })
}
