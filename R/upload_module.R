# upload_module.R

library(shiny)
library(shinyalert)

# UI module for uploading an Excel file
uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Upload Excel File",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fileInput(ns("file_upload"), "Choose Excel File",
                accept = c(".xlsx", ".xls")),
      helpText("Uploaded file will be stored in data/excel_import")
    )
  )
}

# Server module for processing the uploaded file
uploadServer <- function(id, upload_directory) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$file_upload, {
        req(input$file_upload)
        
        # Ensure the upload_directory exists
        if (!dir.exists(upload_directory)) {
          dir.create(upload_directory, recursive = TRUE)
        }
        
        # Construct the destination file path
        dest_file <- file.path(upload_directory, input$file_upload$name)
        
        # Copy the uploaded file to the destination
        success <- file.copy(from = input$file_upload$datapath, to = dest_file, overwrite = TRUE)
        
        if (success) {
          shinyalert::shinyalert(title = "Success", text = "Excel file uploaded successfully.", type = "success")
        } else {
          shinyalert::shinyalert(title = "Error", text = "Failed to upload the Excel file.", type = "error")
        }
      })
    }
  )
} 