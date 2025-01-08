# Server module
tableSelectorServer <- function(id, db_connection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive to fetch table names
    table_names <- reactive({
      dbListTables(db_connection)
    })
    
    # Update the dropdown choices dynamically
    observe({
      updateSelectInput(session, "table_dropdown", choices = table_names())
    })
    
    # Render selected table
    output$table_view <- renderTable({
      req(input$table_dropdown)
      dbReadTable(db_connection, input$table_dropdown)
    })
  })
}