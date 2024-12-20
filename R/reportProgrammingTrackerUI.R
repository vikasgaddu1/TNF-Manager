reportProgrammingTrackerUI <- function(id, title = "Programming Tracker") {
  ns <- NS(id)
  
  fluidPage(
    tags$style(
      HTML("
        .datepicker, .ui-datepicker { z-index: 99999 !important; }
        .table-spacing { margin-top: 20px; } /* Add spacing above the tracker_table */
      ")
    ),
    h2(title, class = "mb-4"),
    
    fluidRow(
      column(
        width = 4, 
        div(
          class = "well",
          selectInput(
            ns("reporting_effort"),
            label = "Select Reporting Effort:",
            choices = NULL,
            selected = NULL,
            width = "100%"
          )
        )
      ),
      column(
        width = 6,
        box(
          title = "Column Visibility Settings",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxGroupInput(
            ns("column_selection"),
            label = "Select Columns to Hide:",
            choices = NULL,
            # Will be updated dynamically
            selected = "id"
          )
        )
      )
    ),
    
    programmingEffortUI(ns("programming_effort")),
    
    # Buttons in a new fluidRow
    fluidRow(
      column(
        width = 3, 
        div(
          class = "action-buttons mb-3",
          actionButton(
            ns("edit_button"),
            "Edit Selected Row",
            class = "btn btn-warning btn-block", # Add btn-block for full-width
            icon = icon("edit")
          )
        )
      ),
      column(
        width = 3, 
        div(
          class = "action-buttons mb-3",
          downloadButton(
            ns("download_tracker"),
            label = "Download Tracker Data as Excel",
            class = "btn btn-primary btn-block" # Add btn-block for full-width
          )
        )
      )
    ),
    
    # Tracker table with spacing
    fluidRow(
      column(
        width = 12, 
        div(
          class = "table-responsive table-spacing", # Add class for top spacing
          DT::DTOutput(ns("tracker_table"))
        )
      )
    )
  )
}
