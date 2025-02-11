
# Server module with added “addressed” checkbox functionality.
commentsServer <- function(id, pool, tables_data, selected_id) { 
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # When the add/view comments button is clicked…
    observeEvent(input$add_comment, {
      if (is.null(selected_id()) ||
          length(selected_id()) == 0 ||
          (length(selected_id()) == 1 && selected_id() == "0")) {
        show_toast(
          title = "Add/View Comment",
          type = "info",
          text = "Please select at least one row before adding/viewing a comment.",
          position = "center"
        )
        return()
      }
      
      ## Prepare the data
      
      # It’s best to rename the primary key from the comments table so that we can keep
      # both the tracker id and the comment id.
      comments_tbl <- tables_data$comments() %>% 
        dplyr::rename(
          comment_id = id,
          comment_updated_at = updated_at
        )
      
      # Build a reactive that joins tracker data, comments, and user info.
      # We now also select the “addressed” field.
      comment_data <- reactive({
        tables_data$report_programming_tracker() %>%
          dplyr::left_join(
            comments_tbl,
            by = c("id" = "report_programming_tracker_id")
          ) %>%
          dplyr::left_join(
            tables_data$users() %>% dplyr::rename(comment_username = username),
            by = c("comment_user_id" = "id")
          ) %>%
          dplyr::filter(!is.na(comment)) %>%
          dplyr::select(
            tracker_id = id,
            comment_id,
            comment_username,
            comment_updated_at,
            comment,
            addressed
          )
      })
      
      # Fetch users for the dropdown menu.
      users <- reactive({
        tables_data$users() %>% dplyr::select(id, username)
      })
      user_choices <- reactive({
        setNames(users()$id, users()$username)
      })
      
      # If more than one row is selected, show a note in the modal.
      multi_message <- if (length(selected_id()) > 1) {
        paste0("<p><em>Note: ", length(selected_id()),
               " reporting efforts are selected. This new comment will be applied to all selected rows.</em></p>")
      } else {
        ""
      }
      
      # Build and show the modal dialog.
      showModal(
        modalDialog(
          title = "Add/View Comment",
          tagList(
            # The existing comments are rendered via a UI output.
            uiOutput(ns("existing_comments")),
            HTML(multi_message),
            # Section for adding a new comment.
            textAreaInput(
              ns("new_comment"),
              HTML("New Comment: <a href='https://www.markdownguide.org/cheat-sheet/' target='_blank'>Markdown Supported</a>"),
              width = "100%",
              height = "100px",
              resize = "vertical"
            ),
            selectInput(
              ns("user"),
              "User",
              choices = user_choices(),
              selected = NULL
            )
          ),
          easyClose = TRUE,
          footer = tagList(
            # Two buttons: one to update addressed statuses, one to add a new comment.
            actionButton(ns("update_addressed"), "Update Addressed", class = "btn btn-sm btn-primary"),
            actionButton(ns("save_new_comment"), "Add Comment", class = "btn btn-sm btn-success"),
            modalButton("Close")
          )
        )
      )
      
      #### Render the Existing Comments with Checkboxes
      
      output$existing_comments <- renderUI({
        req(selected_id())
        # Filter comments for the currently selected reporting efforts.
        comms <- comment_data() %>%
          dplyr::filter(tracker_id %in% selected_id()) %>%
          dplyr::arrange(desc(comment_updated_at))
        
        if (nrow(comms) == 0) {
          return(HTML("<p>No comments yet.</p>"))
        }
        
        # For each comment, create a fluidRow with the comment text and a checkbox.
        tagList(
          lapply(seq_len(nrow(comms)), function(i) {
            row <- comms[i, ]
            fluidRow(
              column(10,
                     HTML(sprintf(
                       "<strong>%s</strong> (%s):<br>%s",
                       row$comment_username,
                       row$comment_updated_at,
                       markdown::markdownToHTML(text = row$comment, fragment.only = TRUE)
                     ))
              ),
              column(2,
                     # Each checkbox’s id is constructed from the comment_id.
                     checkboxInput(ns(paste0("addressed_", row$comment_id)),
                                   "Addressed",
                                   value = as.logical(row$addressed))
              )
            )
          })
        )
      })
    })  # end observeEvent(input$add_comment)
    
    
    #### Observer for Inserting a New Comment
    observeEvent(input$save_new_comment, {
      # Only add a comment if one was entered.
      if (is.null(input$new_comment) || input$new_comment == "") {
        shinyFeedback::showFeedbackDanger("new_comment", "Comment cannot be empty.")
        return()
      }
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          for (report_id in selected_id()) {
            dbExecute(
              conn,
              "INSERT INTO comments (report_programming_tracker_id, comment_user_id, comment, updated_at)
               VALUES (?, ?, ?, CURRENT_TIMESTAMP)",
              params = list(report_id, input$user, input$new_comment)
            )
          }
        })
        show_toast(
          title = "Success",
          type = "success",
          text = "Comment added successfully.",
          position = "center"
        )
        removeModal()
      }, error = function(e) {
        message("Error occurred while saving comment: ", e$message)
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Failed to add comment:", e$message),
          position = "center"
        )
      })
    })
    
    
    #### Observer for Updating the “Addressed” Status
    observeEvent(input$update_addressed, {
      req(selected_id())
      
      # Recreate the comment_data (it was defined in the add_comment observer) so that
      # we get the subset of comments for the selected rows.
      comments_tbl <- tables_data$comments() %>% 
        dplyr::rename(
          comment_id = id,
          comment_updated_at = updated_at
        )
      comment_data <- tables_data$report_programming_tracker() %>%
        dplyr::left_join(
          comments_tbl,
          by = c("id" = "report_programming_tracker_id")
        ) %>%
        dplyr::left_join(
          tables_data$users() %>% dplyr::rename(comment_username = username),
          by = c("comment_user_id" = "id")
        ) %>%
        dplyr::filter(!is.na(comment)) %>%
        dplyr::select(
          tracker_id = id,
          comment_id,
          comment_username,
          comment_updated_at,
          comment,
          addressed
        ) %>%
        dplyr::filter(tracker_id %in% selected_id())
      
      if(nrow(comment_data) == 0) {
        show_toast(
          title = "Info",
          type = "info",
          text = "No comments to update.",
          position = "center"
        )
        return()
      }
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          # Loop over each comment and check its corresponding checkbox.
          for (i in seq_len(nrow(comment_data))) {
            row <- comment_data[i, ]
            checkbox_id <- paste0("addressed_", row$comment_id)
            new_value <- input[[checkbox_id]]
            if (is.null(new_value)) new_value <- FALSE
            # Compare with the current value from the database.
            current_value <- as.logical(row$addressed)
            if (new_value != current_value) {
              dbExecute(
                conn,
                "UPDATE comments SET addressed = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
                params = list(as.integer(new_value), row$comment_id)
              )
            }
          }
        })
        show_toast(
          title = "Success",
          type = "success",
          text = "Addressed statuses updated successfully.",
          position = "center"
        )
        removeModal()
      }, error = function(e) {
        message("Error updating addressed status: ", e$message)
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Failed to update addressed status:", e$message),
          position = "center"
        )
      })
    })
    
    # Optionally, you can hide feedback when the new comment is modified.
    observeEvent(input$new_comment, {
      if (!is.null(input$new_comment) && input$new_comment != "") {
        shinyFeedback::hideFeedback("new_comment")
      }
    })
    
  })
}
