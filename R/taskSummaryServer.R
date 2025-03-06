taskSummaryServer <- function(id, tracker_data, user_select) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add title with current user
    output$userTitle <- renderText({
      user_select()
    })
    
    # Reactive expression for programmer data
    programmer_data <- reactive({
      req(tracker_data(), user_select())
      pdata <- tracker_data() %>%
        pivot_longer(cols = c(production_programmer, qc_programmer), names_to = "role", values_to = "programmer") %>%
        filter(programmer == user_select(), status != "QC Pass")
      return(pdata)
    })
    
    # Create a reactive for the nested summary to use in both renderUI and table outputs
    nested_summary <- reactive({
      programmer_data() %>%
        group_by(study, database_release, reporting_effort, report_type, priority) %>%
        summarise(
          task_count = n(),
          .groups = "drop"
        ) %>%
        group_by(study, database_release, reporting_effort) %>%
        summarise(
          details = list(data.frame(
            report_type = report_type,
            priority    = priority,
            task_count  = task_count
          )),
          .groups = "drop"
        )
    })
    
    # Create table outputs first, outside of renderUI
    observe({
      summary_data <- nested_summary()
      
      for (i in seq_len(nrow(summary_data))) {
        local({
          local_i <- i
          details <- summary_data$details[[local_i]]
          
          output_id <- paste0("detailsTable", local_i)
          
          output[[output_id]] <- renderTable({
            details %>%
              arrange(priority, report_type) %>%
              rename(
                "Report Type" = report_type,
                "Priority" = priority,
                "Task Count" = task_count
              )
          }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%", align = 'l')
        })
      }
    })
    
    # Render the task summary content
    output$taskSummaryContent <- renderUI({
      summary_data <- nested_summary()
      
      # Split the studies into groups of 3 for the rows
      n_studies <- nrow(summary_data)
      row_groups <- split(seq_len(n_studies), ceiling(seq_len(n_studies)/3))
      
      # Create rows with up to 3 study boxes each
      study_rows <- lapply(row_groups, function(group_indices) {
        fluidRow(
          lapply(group_indices, function(i) {
            row <- summary_data[i, ]
            
            column(
              width = 4,  # Set width to 4 to fit 3 boxes per row (12/3 = 4)
              div(
                style = "margin-bottom: 15px;",
                div(
                  class = "info-box",
                  style = "margin-bottom: 0;",
                  div(
                    class = "info-box-content",
                    style = "padding: 0;",
                    div(
                      style = "background-color: #00c0ef; color: white; padding: 10px; font-weight: bold;",
                      paste(row$study, "-", row$database_release, "-", row$reporting_effort)
                    ),
                    div(
                      style = "padding: 10px;",
                      tableOutput(ns(paste0("detailsTable", i)))
                    )
                  )
                )
              )
            )
          })
        )
      })
      
      # Return the list of study rows
      tagList(study_rows)
    })
  })
}
