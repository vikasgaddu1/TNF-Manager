commentsServer <- function(id, pool, tables_data, selected_id) {
  moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # req(selected_id)
        # add observe event for add_comment button
        observeEvent(input$add_comment, {

            # Tracker data with joined comments and user information
            comment_data <- reactive({
                tables_data$report_programming_tracker() %>%
                dplyr::left_join(
                    tables_data$comments() %>% dplyr::rename(comment_updated_at = updated_at),
                    join_by(id == report_programming_tracker_id)
                )  %>%
                dplyr::left_join(
                    tables_data$users() %>% dplyr::rename(comment_username = username),
                    join_by(comment_user_id == id)
                ) %>%
                dplyr::select(id, comment_username, comment_updated_at, comment)
            })
            # Fetch users for dropdown
            users <- reactive({
                tables_data$users() %>% dplyr::select(id, username)
            })
            
            user_choices <- reactive({
                setNames(users()$id, users()$username)
            })

            # Show toast if no row is selected
            if (is.null(selected_id()) || length(selected_id()) == 0 || selected_id() == 0) {
                show_toast(
                title = "Add/View Comment",
                type = "info",
                text = "**Please select a row before adding/viewing a comment.",
                position = "center"
                )
                return()
            }

            # Fetch existing comments with user info
            existing_comments <-  comment_data() %>%
                dplyr::filter(id == selected_id()) %>%
                dplyr::select(comment, comment_username, comment_updated_at) %>%
                dplyr::arrange(desc(comment_updated_at))

            # Convert markdown to HTML for display
            existing_comments_html <- if (nrow(existing_comments) > 0) {
                paste0(
                "<ul>",
                paste(
                    apply(existing_comments, 1, function(row) {
                    sprintf(
                        "<li><strong>%s</strong> (%s):<br>%s</li>",
                        row["comment_username"],
                        row["comment_updated_at"],
                        markdown::markdownToHTML(text = row["comment"], fragment.only = TRUE)
                    )
                    }),
                    collapse = ""
                ),
                "</ul>"
                )
            } else {
                "<p>No comments yet.</p>"
            }

            # Add modal to show previous comments and to add a new comment
            showModal(
                modalDialog(
                title = "Add/View Comment",
                tagList(
                    div(id=ns("existing_comments"), class = "existing-comments", HTML(existing_comments_html)),
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
                    actionButton(ns("save_changes"), "Save", class = "btn btn-sm btn-success"),
                    modalButton("Cancel")
                )
                )
            )
        })

        observeEvent(input$new_comment, {
            if (!(is.null(input$new_comment) || input$new_comment == "")) {
                shinyFeedback::hideFeedback("new_comment")
            }
        })

        observeEvent(input$save_changes, {
            # add feedback to user if comment is empty using shinyFeedback
            if (input$new_comment == "" || is.null(input$new_comment)) {
                shinyFeedback::showFeedbackDanger("new_comment", "Comment cannot be empty.")
                return()
            }
   
            # Save the new comment to the database
            tryCatch({
                    poolWithTransaction(pool, function(conn) {
                    dbExecute(
                        conn,
                        "INSERT INTO comments (report_programming_tracker_id, comment_user_id, comment, updated_at)
                        VALUES (?, ?, ?, CURRENT_TIMESTAMP)",
                        params = list(selected_id(), input$user, input$new_comment)
                    )
                    })

                    # Success toast
                    show_toast(
                    title = "Success",
                    type = "success",
                    text = "Comment added successfully.",
                    position = "center"
                    )

                    # Close the modal
                    removeModal()
                }, error = function(e) {
                    # Log and display error
                    message("Error occurred while saving comment: ", e$message)
                    show_toast(
                    title = "Error",
                    type = "error",
                    text = paste("Failed to add comment:", e$message),
                    position = "center"
                    )
                })
        })
  })
}
