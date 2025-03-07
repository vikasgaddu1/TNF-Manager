# -----------------------------
# Module UI: mod_milestone_ui
# -----------------------------
mod_milestone_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyFeedback(),
    tags$h1("PROJECT TIMELINE", style = "font-weight: bold; text-transform: uppercase;"),
    plotlyOutput(ns("timelinePlot"), height = "400px"),
    fluidRow(
      # Left column: Milestone table
      column(
        8,
        h3("Project Milestones"),
        DTOutput(ns("milestoneTable"))
      ),
      # Right column: Inline add/edit form split into two columns
      column(
        4,
        h3("Add / Edit Milestone"),
        wellPanel(
          # Action buttons row (with a Clear button)
          fluidRow(
            column(3, actionButton(ns("addRecord"), "Add", class = "btn btn-primary")),
            column(3, actionButton(ns("updateRecord"), "Update", class = "btn btn-warning")),
            column(3, actionButton(ns("deleteRecord"), "Delete", class = "btn btn-danger")),
            column(3, actionButton(ns("clearForm"), "Clear", class = "btn btn-info"))
          ),
          tags$hr(),
          # Two-column form layout: 3 inputs per column
          fluidRow(
            column(6,
                    selectizeInput(ns("formReportingEffort"), "Reporting Effort", width = "100%", choices = NULL),
                    dateInput(ns("formDate"), "Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                    textInput(ns("formMilestone"), "Milestone", placeholder = "Enter Milestone", width = "100%")
            ),
            column(6,
                   selectizeInput(ns("formAssignedTo"), "Assigned To", width = "100%", choices = NULL),
                   numericInput(ns("formPosition"), "Position", value = 0),
                   selectizeInput(
                     ns("formStatus"),
                     "Status",
                     choices = c("Complete", "On Target", "At Risk", "Critical"),
                     selected = "On Target",
                     options = list(create = TRUE)
                   )
            )
          )
        )
      )
    ),
    tags$style(HTML("
      .shiny-modal { z-index: 99999 !important; }
      .datepicker { z-index: 99999 !important; }
      .ui-datepicker { z-index: 99999 !important; }
    "))
  )
}