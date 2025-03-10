mod_programming_tracker_server <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load Reporting Efforts into dropdown
    reporting_efforts <- reactive({
      tables_data$reporting_efforts() %>%
        dplyr::mutate(label = paste(study, database_release, reporting_effort, sep = "_"))
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
    
      tfl_tracker_data <- mod_tfl_tracker_server("tfl_tracker", pool, reactive(input$reporting_effort),tables_data, selected_reporting_effort_label)
      sdtm_tracker_data <- mod_dataset_tracker_server("sdtm_tracker", pool, reactive(input$reporting_effort),"SDTM",tables_data, selected_reporting_effort_label)
      adam_tracker_data <- mod_dataset_tracker_server("adam_tracker", pool, reactive(input$reporting_effort),"ADaM",tables_data, selected_reporting_effort_label)
    
      return(list(tfl_tracker_data, sdtm_tracker_data, adam_tracker_data))
  })
}
