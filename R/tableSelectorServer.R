# Server module
tableSelectorServer <- function(id, tables_data, table_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update the dropdown choices dynamically
    observe({
      updateSelectInput(session, "table_dropdown", choices = table_names)
    })
    
    # Render selected table
    output$table_view <- renderTable({
      req(input$table_dropdown)
      tables_data[[input$table_dropdown]]()
    })
  })
}