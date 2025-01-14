taskSummaryServer <- function(id, tracker_data, user_select) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression for programmer data
    programmer_data <- reactive({
      tracker_data() %>%
        pivot_longer(cols = c(production_programmer, qc_programmer), names_to = "role", values_to = "programmer") %>%
        filter(programmer == user_select(), status != "QC Pass")
    })
    
    # Render the message in the dropdown menu
    output$messageMenu <- renderMenu({
        nested_summary <- programmer_data() %>%
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

  
    # Build an HTML table
    html_message <- paste0(
      "<div style='width:100%; border-collapse:collapse;'>",
      paste0(
        # Loop over each row in nested_summary
        purrr::map_chr(seq_len(nrow(nested_summary)), function(i) {
          
          row <- nested_summary[i, ]
          # row$details is a list of length 1 containing the data frame
          details <- row$details[[1]]
          
          # Build HTML for the group header table
          group_header <- paste0(
            "<table style='margin-bottom:10px; width:100%;'>",
            "<tr><td style='padding-right:10px;'><b>Study:</b></td><td>", row$study, "</td></tr>",
            "<tr><td style='padding-right:10px;'><b>DB:</b></td><td>", row$database_release, "</td></tr>",
            "<tr><td style='padding-right:10px;'><b>RE:</b></td><td>", row$reporting_effort, "</td></tr>",
            "</table>"
          )
          
          # Build HTML for the details table
          detail_rows <- paste0(
            "<table style='margin-bottom:20px; width:100%;'>",
            "<tr><th style='padding-right:10px;'>Type</th><th style='padding-right:10px;'>Priority</th><th>Count</th></tr>",
            paste0(
              apply(details, 1, function(x) {
                paste0(
                  "<tr>",
                  "<td style='padding-right:10px;'>", x["report_type"], "</td>",
                  "<td style='padding-right:10px;'>", x["priority"], "</td>",
                  "<td>", x["task_count"], "</td>",
                  "</tr>"
                )
              }),
              collapse = ""
            ),
            "</table>"
          )
          
          paste0(group_header, detail_rows)
        }),
        collapse = ""
      ),
      "</div>"
    )
  
  dropdownMenu(type = "messages",
    messageItem(
      from = HTML("<b>Task Summary</b><hr>"),
      message = HTML(html_message)
    )
  )
})
})
}