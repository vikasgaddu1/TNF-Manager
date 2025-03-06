# -----------------------------
# Module Server: landingPageServer
# -----------------------------
landingPageServer <- function(id, pool, tracker_data, tables_data, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    # Get current user and role as module-level reactives
    current_user <- reactive({
      get_current_user(parentSession)
    })
    
    current_role <- reactive({
      get_current_user_role(pool,parentSession)
    })
    
    # Task Summary
    taskSummaryServer("taskSummary", tracker_data, current_user)
    
    # Calculate statistics for the dashboard
    output$taskCount <- renderText({
      nrow(tables_data$report_programming_tracker())
    })
    
    output$reportCount <- renderText({
      nrow(tables_data$reports())
    })
    
    output$milestoneCount <- renderText({
      nrow(tables_data$milestones())
    })
    
    output$decisionCount <- renderText({
      tryCatch({
        decision_logs <- dbGetQuery(pool, "SELECT COUNT(*) as count FROM decision_logs")
        decision_logs$count
      }, error = function(e) {
        0
      })
    })
    
    # Navigation handlers
    observeEvent(input$goToTracker, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "tracker")
    })
    
    observeEvent(input$goToMilestone, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "milestone")
    })
    
    observeEvent(input$goToDecisionLog, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "decision_log")
    })
    
    observeEvent(input$goToReReports, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "re_reports")
    })
    
    observeEvent(input$goToCrudMenu, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "crud_menu")
    })
    
    observeEvent(input$goToSearch, {
      req(parentSession)  # Ensure parent session exists
      updateTabItems(parentSession, "tabs", "search")
    })
  })
} 