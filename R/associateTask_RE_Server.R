associateTask_RE_Server <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Define reporting_efforts as a reactive expression
    reporting_efforts <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(
          pool,
          "SELECT id,
              study || ' | ' || database_release || ' | ' || reporting_effort AS label,
              reporting_effort AS reporting_effort_label
           FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message),
                         type = "error")
        NULL
      })
    })
    
    # Auto-refresh reporting efforts when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "re_reports") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing reporting efforts", type = "message", duration = 1)
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
        dplyr::pull(reporting_effort_label)
    })
    
    # Pass reporting effort id and label to at_tfl_Server
    at_tfl_Server(
      "at_tfl",
      pool,
      reactive(input$reporting_effort),
      selected_reporting_effort_label
    )
  })
}
