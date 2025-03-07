mod_assign_task_server <- function(id,
                             pool,
                             tables_data,
                             selected_id,    # reactive returning a vector of IDs
                             tracker_data    # reactive returning the tracker data frame
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # When the Edit button is clicked, build the modal with a rhandsontable.
    observeEvent(input$edit_button, {
      # Check that at least one row is selected.
      if (is.null(selected_id()) || length(selected_id()) == 0 || (length(selected_id()) == 1 && selected_id() == "0")) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "info",
          text = "Please select at least one row before editing.",
          position = "center"

        )
        return()
      }
      
      # Retrieve the tracker data and subset it to only the selected rows.
      df <- tracker_data()
      edit_df <- df %>% dplyr::filter(id %in% selected_id())
      
      if (nrow(edit_df) == 0) {
        show_toast(
          title = "Edit Report Programming Details",
          type = "error",
          text = "No data found for the selected row(s).",
          position = "center"
        )
        return()
      }
      
      # Build the modal dialog with a styled container around the table.
      modalContent <- modalDialog(
        title = tagList(
          tags$div(
            tags$i(class = "fa fa-edit", style = "color:#337ab7; margin-right:5px;"),
            tags$span("Edit Programming Details", style = "color:#337ab7;")
          )
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("save_changes"), "Save", class = "btn btn-success"),
          modalButton("Cancel")
        ),
        # Wrap the table output in a styled div.
        div(class = "rhandsontable-container", rHandsontableOutput(ns("update_table")))
      )
      
      showModal(modalContent)
    })
    
    # Render the editable table.
    output$update_table <- renderRHandsontable({
      req(selected_id())
      
      # Retrieve and subset data.
      df <- tracker_data()
      if ("dataset_name" %in% colnames(df)) {
        df <- df %>% dplyr::rename(report_key = dataset_name)
      }
      edit_df <- df %>% 
        dplyr::filter(id %in% selected_id()) %>% 
        dplyr::select(id, report_key,
                      production_programmer, assign_date,
                      qc_programmer, due_date, qc_level,
                      priority, status) %>%
        dplyr::mutate(
          status = ifelse(is.na(status), "Not Started", status),
          priority = ifelse(is.na(priority), 1, priority),
          qc_level = ifelse(is.na(qc_level), 3, qc_level),
          assign_date = ifelse(is.na(assign_date), as.character(Sys.Date()), assign_date),
          due_date = ifelse(is.na(due_date), as.character(as.Date(assign_date) + 7), due_date)

        ) %>%
        dplyr::rename(
          "Production Programmer" = production_programmer,
          "Assign Date" = assign_date,
          "QC Programmer" = qc_programmer,
          "Due Date" = due_date,
          "QC Level" = qc_level,
          "Priority" = priority,  
          "Status" = status,
          "Report Key" = report_key
        )
      
      # Create the table with specified dimensions.
      rhot <- rhandsontable::rhandsontable(edit_df, 
                                           rowHeaders = FALSE, 
                                           stretchH = "all", 
                                           width = "100%", 
                                           height = 400,
                                           overflow = "visible")
      
      # Retrieve options for dropdowns.
      prod_prog <- tables_data$users() %>% dplyr::select(id, username)
      qc_prog   <- tables_data$users() %>% dplyr::select(id, username)
      prod_options <- prod_prog$username  
      qc_options   <- qc_prog$username
      status_options <- c("Not Started", "Production Started", "Production Ready", "Under QC", "QC Failed", "QC Pass")
      qc_level_options <- as.character(1:3)
      priority_options <- as.character(1:5)
      
      # Set up dropdowns and date editors.
      rhot <- rhandsontable::hot_col(rhot, "Production Programmer", type = "dropdown", source = prod_options, strict = TRUE, allowInvalid = FALSE)
      rhot <- rhandsontable::hot_col(rhot, "QC Programmer", type = "dropdown", source = qc_options, strict = TRUE, allowInvalid = FALSE)
      rhot <- rhandsontable::hot_col(rhot, "Status", type = "dropdown", source = status_options, strict = TRUE, allowInvalid = FALSE)
      rhot <- rhandsontable::hot_col(rhot, "QC Level", type = "dropdown", source = qc_level_options, strict = TRUE, allowInvalid = FALSE)
      rhot <- rhandsontable::hot_col(rhot, "Priority", type = "dropdown", source = priority_options, strict = TRUE, allowInvalid = FALSE)
      rhot <- rhandsontable::hot_col(rhot, "Assign Date", type = "date", dateFormat = "YYYY-MM-DD")
      rhot <- rhandsontable::hot_col(rhot, "Due Date", type = "date", dateFormat = "YYYY-MM-DD")
      
      # Set read-only columns.
      rhot <- rhandsontable::hot_col(rhot, "Report Key", readOnly = TRUE)
      rhot <- rhandsontable::hot_col(rhot, "id", colWidth = 0, readOnly = TRUE)
      
      return(rhot)
    })
    
    # Process the edited table when Save is clicked.
    observeEvent(input$save_changes, {
      if (!is.null(input$update_table)) {
        # Convert the edited table into a data frame.
        edited_df <- rhandsontable::hot_to_r(input$update_table) %>%
          dplyr::rename(
            "production_programmer" = `Production Programmer`,
            "qc_programmer" = `QC Programmer`,
            "status" = `Status`,
            "qc_level" = `QC Level`,
            "priority" = `Priority`,
            "assign_date" = `Assign Date`,
            "due_date" = `Due Date`,
            "report_key" = `Report Key`
          )
        
        # Retrieve programmer info to convert names to IDs.
        prod_users <- tables_data$users() %>% dplyr::select(id, username)
        qc_users <- tables_data$users() %>% dplyr::select(id, username)
        
        # Validate each row.
        for (i in seq_len(nrow(edited_df))) {
          row <- edited_df[i, ]
          if (!is.na(row$production_programmer) && !is.na(row$qc_programmer) && row$production_programmer == row$qc_programmer) {
            show_toast(
              title = "Edit Report Programming Details",
              type = "error",
              text = paste("Row", i, ": Production and QC programmer cannot be the same person."),
              position = "center"
            )
            return()
          }
          assign_date_val <- as.Date(row$assign_date)
          due_date_val <- as.Date(row$due_date)
          if (!is.na(assign_date_val) && !is.na(due_date_val) && due_date_val <= assign_date_val) {
            show_toast(
              title = "Edit Report Programming Details",
              type = "error",
              text = paste("Row", i, ": Due date must be after the assign date."),
              position = "center"
            )
            return()
          }
        }
        
        # Attempt the update.
        tryCatch({
          poolWithTransaction(pool, function(conn) {
            for (i in seq_len(nrow(edited_df))) {
              row <- edited_df[i, ]
              prod_id <- prod_users %>% dplyr::filter(username == row$production_programmer) %>% dplyr::pull(id)
              qc_id   <- qc_users %>% dplyr::filter(username == row$qc_programmer) %>% dplyr::pull(id)
              
              if (length(prod_id) == 0 || length(qc_id) == 0) {
                stop("Invalid programmer selection in row ", i)
              }
              
              DBI::dbExecute(
                conn,
                "UPDATE report_programming_tracker
                 SET production_programmer_id = ?,
                     assign_date = ?,
                     qc_programmer_id = ?,
                     due_date = ?,
                     priority = ?,
                     status = ?,
                     qc_level = ?,
                     updated_at = CURRENT_TIMESTAMP
                 WHERE id = ? ;",
                params = list(
                  prod_id,
                  as.character(row$assign_date),
                  qc_id,
                  as.character(row$due_date),
                  as.integer(row$priority),
                  row$status,
                  as.integer(row$qc_level),
                  row$id
                )
              )
            }
          })
          
          show_toast(
            title = "Edit Report Programming Details",
            type = "success",
            text = "Record(s) updated successfully.",
            position = "center"
          )
          removeModal()
        }, error = function(e) {
          show_toast(
            title = "Edit Report Programming Details",
            type = "error",
            text = paste("Error updating record:", e$message),
            position = "center"
          )
        })
      }
    })
  })
}
