# -----------------------------
# Module UI: mod_landing_page_ui
# -----------------------------
mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # App Title and Introduction
    div(
      class = "jumbotron",
      style = "background-color: #f8f9fa; padding: 2rem; margin-bottom: 2rem; border-radius: 0.3rem;",
      fluidRow(
        column(
          width = 12,
          h2(HTML("<b><i>PEARL</i></b> - <b>P</b>rogramming <b>E</b>fforts, <b>A</b>nalysis, <b>R</b>eporting & <b>L</b>ifecycle"), class = "display-4"),
          p("A comprehensive tool for managing reporting efforts, milestones, decisions and programming tasks.", class = "lead"),
          mod_task_summary_ui(ns("taskSummary"))
        )
      )
    ),
    
    # Features Section
    h2("Key Features", style = "margin-top: 2rem; margin-bottom: 1.5rem;"),
    
    fluidRow(
      # Quick Navigation Cards
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Programming Tracker", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Track the status of programming tasks, assign programmers, and set priorities."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("tasks", style = "font-size: 3rem; color: #007bff;")),
            actionButton(
              ns("goToTracker"),
              "Go to Tracker",
              class = "btn btn-primary btn-block",
              icon = icon("tasks")
            )
          )
        )
      ),
      
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Project Timeline", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Visualize project milestones and track progress with an interactive timeline."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("calendar-days", style = "font-size: 3rem; color: #fd7e14;")),
            actionButton(
              ns("goToMilestone"),
              "Go to Timeline",
              class = "btn btn-warning btn-block",
              icon = icon("calendar-days")
            )
          )
        )
      ),
      
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Decision Log", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Document important decisions, their rationale, and track their implementation."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("book", style = "font-size: 3rem; color: #17a2b8;")),
            actionButton(
              ns("goToDecisionLog"),
              "Go to Decision Log",
              class = "btn btn-info btn-block",
              icon = icon("book")
            )
          )
        )
      )
    ),
    
    # Second row of feature cards
    fluidRow(
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Task Association", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Associate tasks with specific reporting efforts and track their progress."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("link", style = "font-size: 3rem; color: #28a745;")),
            actionButton(
              ns("goToReReports"),
              "Associate Tasks",
              class = "btn btn-success btn-block",
              icon = icon("link")
            )
          )
        )
      ),
      
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Data Management", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Manage reference data, categories, and user information."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("th", style = "font-size: 3rem; color: #dc3545;")),
            actionButton(
              ns("goToCrudMenu"),
              "Go to Data Management",
              class = "btn btn-danger btn-block",
              icon = icon("th")
            )
          )
        )
      ),
      
      div(
        class = "col-md-4",
        div(
          class = "panel panel-default",
          style = "border: 1px solid #ddd; border-radius: 4px; margin-bottom: 20px;",
          div(
            class = "panel-heading",
            style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #ddd;",
            h3("Search", class = "panel-title")
          ),
          div(
            class = "panel-body",
            style = "padding: 15px;",
            p("Search across all data to quickly find relevant information."),
            div(style = "text-align: center; margin-bottom: 10px;", 
                icon("search", style = "font-size: 3rem; color: #6c757d;")),
            actionButton(
              ns("goToSearch"),
              "Go to Search",
              class = "btn btn-secondary btn-block",
              icon = icon("search")
            )
          )
        )
      )
    ),
        # Statistics Row
    fluidRow(
      id = ns("statsRow"),
      class = "text-center",
      
      # Tasks Stats Box
      valueBox(
        value = textOutput(ns("taskCount")),
        subtitle = "Active Tasks",
        icon = icon("tasks"),
        color = "green",
        width = 3
      ),
      
      # Reports Stats Box
      valueBox(
        value = textOutput(ns("reportCount")),
        subtitle = "Reports",
        icon = icon("file-alt"),
        color = "blue",
        width = 3
      ),
      
      # Milestones Stats Box
      valueBox(
        value = textOutput(ns("milestoneCount")),
        subtitle = "Milestones",
        icon = icon("calendar-days"),
        color = "orange",
        width = 3
      ),
      
      # Decisions Stats Box
      valueBox(
        value = textOutput(ns("decisionCount")),
        subtitle = "Decisions",
        icon = icon("book"),
        color = "purple", 
        width = 3
      )
    ),
    # Footer
    div(
      class = "footer text-center",
      style = "margin-top: 3rem; padding-top: 1.5rem; border-top: 1px solid #eee;",
      p("PEARL v0.1 • © Geron 2025")
    )
  )
} 