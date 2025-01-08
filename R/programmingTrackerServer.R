programmingTrackerServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Load Reporting Efforts into dropdown
    reporting_efforts <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(
          pool,
          "SELECT id,
              study || '_' || database_release || '_' || reporting_effort AS label
           FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message),
                         type = "error")
        NULL
      })
    })
    
    
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
      req(reporting_efforts())
      current_effort <- isolate(input$reporting_effort)
      choices <- setNames(reporting_efforts()$id, reporting_efforts()$label)
      
      updateSelectInput(
        session,
        "reporting_effort",
        choices = choices,
        selected = if (!is.null(current_effort) &&
                       current_effort %in% reporting_efforts()$id)
          current_effort
        else
          reporting_efforts()$id[1]
      )
    })
    
    # Reactive to get the selected reporting effort label
    selected_reporting_effort_label <- reactive({
      req(reporting_efforts(), input$reporting_effort)
      reporting_efforts() %>%
        dplyr::filter(id == input$reporting_effort) %>%
        dplyr::pull(label)
    })
    
    refresh_trigger <- tflTrackerServer("tfl_tracker", pool, reactive(input$reporting_effort),refresh_trigger, selected_reporting_effort_label)
    refresh_trigger <- datasetTrackerServer("sdtm_tracker", pool, reactive(input$reporting_effort),"SDTM",refresh_trigger, selected_reporting_effort_label)
    refresh_trigger <- datasetTrackerServer("adam_tracker", pool, reactive(input$reporting_effort),"ADaM",refresh_trigger, selected_reporting_effort_label)
    
  })
}
