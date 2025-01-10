associateTask_RE_Server <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reporting_efforts as a reactive expression
    reporting_efforts <- reactive({
      tables_data$reporting_efforts() %>%
        dplyr::mutate(label = paste(study, database_release, reporting_effort, sep = "_")) %>%
        dplyr::select(id, label)
    })
      
    # Load Reporting Efforts into dropdown
    observe({
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
    
    # Pass reporting effort id and label to child modules
    at_tfl_Server(
      "at_tfl",
      tables_data,
      reactive(input$reporting_effort),
      selected_reporting_effort_label
    )
    
    at_dataset_Server(
      "at_sdtm",
      tables_data,
      reactive(input$reporting_effort),
      selected_reporting_effort_label,
      "SDTM"
    )
    
    at_dataset_Server(
      "at_adam",
      tables_data,
      reactive(input$reporting_effort),
      selected_reporting_effort_label,
      "ADaM"
    )
  })
}
