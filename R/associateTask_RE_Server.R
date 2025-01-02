associateTask_RE_Server <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh reporting efforts when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "re_reports") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing reporting efforts", type = "message", duration = 1)
      }
    }, ignoreInit = TRUE)
    
    # Load Reporting Efforts
    observeEvent(refresh_trigger(), {
      reporting_efforts <- tryCatch({
        dbGetQuery(
          pool,
          "SELECT id, 
                  study || '#' || database_release || '#' || reporting_effort AS reporting_effort_label 
           FROM reporting_efforts;"
        )
      }, error = function(e) {
        showNotification(paste("Error loading reporting efforts:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(reporting_efforts)) {
        updateSelectInput(
          session,
          "reporting_effort",
          choices = setNames(
            reporting_efforts$id, # Values (ids)
            reporting_efforts$reporting_effort_label # Labels (concatenated text)
          )
        )
      }
    })
    
    # Reactive to get the selected reporting effort label
    # selected_reporting_effort_label <- reactive({
    #   req(reporting_efforts(), input$reporting_effort)
    #   reporting_efforts() %>%
    #     dplyr::filter(id == input$reporting_effort) %>%
    #     dplyr::pull(reporting_effort_label)
    # })
    
    # Pass reporting effort id and label to at_tfl_Server
    at_tfl_Server(
      "at_tfl",
      pool,
      reactive(input$reporting_effort)
    )
    
  })
}
