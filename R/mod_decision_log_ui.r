# -----------------------------
# Module UI: mod_decision_log_ui
# -----------------------------
mod_decision_log_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyFeedback(),
    tags$h1("DECISION LOG", style = "font-weight: bold; text-transform: uppercase;"),
    
    # Form Panel
    wellPanel(
      h3("Add / Edit Decision Log Entry"),
      # Form inputs in two columns
      fluidRow(
        # Left column
        column(
          6,
          selectizeInput(ns("formReportingEffort"), "Reporting Effort", choices = NULL),
          selectizeInput(ns("formCategory"), "Category", 
                        choices = c("Analysis", "Programming", "Documentation", "Process", "Other"),
                        options = list(create = TRUE)),
          textAreaInput(ns("formQuestion"), "Question", rows = 3),
          textAreaInput(ns("formAnswerRationale"), "Answer/Rationale", rows = 3),
          selectizeInput(ns("formStatus"), "Status",
                        choices = c("Open", "In Progress", "Resolved", "Deferred"),
                        selected = "Open")
        ),
        # Right column
        column(
          6,
          selectizeInput(ns("formConfidence"), "Confidence Level",
                        choices = c("High", "Medium", "Low"),
                        selected = "Medium"),
          selectizeInput(ns("formOwners"), "Owner(s)", choices = NULL, multiple = TRUE),
          textAreaInput(ns("formComments"), "Comments", rows = 2),
          textAreaInput(ns("formDownstreamImpact"), "Downstream Impact", rows = 2),
          fluidRow(
            column(6, dateInput(ns("formDateResolved"), "Date Resolved", value = NULL)),
            column(6, textAreaInput(ns("formLinks"), "Links", placeholder = "Comma-separated list of relevant links"))
          )
        )
      ),
      
      # Action buttons centered
      div(
        style = "text-align: center; margin-top: 20px;",
        actionButton(ns("addRecord"), "Add", class = "btn btn-primary"),
        actionButton(ns("updateRecord"), "Update", class = "btn btn-warning"),
        actionButton(ns("deleteRecord"), "Delete", class = "btn btn-danger"),
        actionButton(ns("clearForm"), "Clear", class = "btn btn-info")
      )
    ),
    
    # DataTable below
    div(
      style = "margin-top: 20px;",
      DTOutput(ns("decisionLogTable"))
    )
  )
} 