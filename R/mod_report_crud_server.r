mod_report_crud_server <- function(id, pool, tables_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh_trigger <- reactiveVal(0)
    # Reactive Values and Data ----------------------------------------
    data <- reactive({
      refresh_trigger()
      # Get data from reactive sources
      reports_data <- tables_data$reports()
      categories_data <- tables_data$categories()
      sub_categories_data <- tables_data$sub_categories()
      populations_data <- tables_data$populations()
      titles_data <- tables_data$titles()
      footnotes_data <- tables_data$footnotes()
      report_titles_data <- tables_data$report_titles()
      report_footnotes_data <- tables_data$report_footnotes()
      
      # Main data join
      main_data <- reports_data %>%
        left_join(categories_data, by = c("report_category_id" = "id")) %>%
        left_join(
          sub_categories_data,
          by = c(
            "report_sub_category_id" = "id",
            "report_category_id" = "category_id"
          )
        ) %>%
        left_join(populations_data, by = c("population_id" = "id")) %>%
        select(
          id, report_key, title_key, report_type,
          category_name, sub_category_name, suggested_ich_number,
          population_text, report_ich_number
        )
      
      # Process titles
      titles_processed <- report_titles_data %>%
        inner_join(titles_data, by = c("title_id" = "id")) %>%
        group_by(report_id) %>%
        summarise(titles = paste(title_text, collapse = "<br>"))
      
      # Process footnotes
      footnotes_processed <- report_footnotes_data %>%
        inner_join(footnotes_data, by = c("footnote_id" = "id")) %>%
        group_by(report_id) %>%
        summarise(footnotes = paste(footnote_text, collapse = "<br>"))
      
      # Combine all data
      main_data %>%
        left_join(titles_processed, by = c("id" = "report_id")) %>%
        left_join(footnotes_processed, by = c("id" = "report_id")) %>%
        arrange(category_name, sub_category_name, report_type, report_key) %>%
        select(
          id, report_type, category_name, sub_category_name,
          report_key, title_key, population_text,
          report_ich_number, titles, footnotes
        )
    })
    
    cat_subcat <- reactive({
      tables_data$categories() %>%
        left_join(
          tables_data$sub_categories(),
          by = c("id" = "category_id")
        ) %>%
        select(
          category_name,
          sub_category_name,
          suggested_ich_number
        )
    })
    
    # Input Validation Handlers ---------------------------------------
    observeEvent(input$report_type, { shinyFeedback::hideFeedback("report_type") })
    observeEvent(input$category_name, { shinyFeedback::hideFeedback("category_name") })
    observeEvent(input$sub_category_name, { shinyFeedback::hideFeedback("sub_category_name") })
    observeEvent(input$report_key, { shinyFeedback::hideFeedback("report_key") })
    observeEvent(input$title_key, { shinyFeedback::hideFeedback("title_key") })
    observeEvent(input$report_ich_number, { shinyFeedback::hideFeedback("report_ich_number") })
    observeEvent(input$titles, { shinyFeedback::hideFeedback("titles") })
    observeEvent(input$populations, { shinyFeedback::hideFeedback("populations") })
    
    # UI Outputs ---------------------------------------------------
    output$table <- DT::renderDT({
      DT::datatable(
        data(),
        filter = "top",
        escape = FALSE,
        colnames = c(
          "ID" = "id",
          "Report Key" = "report_key",
          "Title Key" = "title_key",
          "Report Type" = "report_type",
          "Category" = "category_name",
          "Sub-Category" = "sub_category_name",
          "Population" = "population_text",
          "ICH Number" = "report_ich_number",
          "Titles" = "titles",
          "Footnotes" = "footnotes"
        ),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(columnDefs = list(list(
          targets = c(0), visible = FALSE
        )))
      )
    })
    
    output$category_dropdown <- renderUI({
      selectInput(
        ns("category_name"),
        "Category",
        choices = tables_data$categories() %>% pull(category_name),
        selected = NULL
      )
    })
    
    output$sub_category_dropdown <- renderUI({
      req(input$category_name)
      
      sub_cats <- cat_subcat() %>%
        filter(category_name == input$category_name) %>%
        pull(sub_category_name)
      
      selectInput(
        ns("sub_category_name"),
        "Sub-Category",
        choices = sub_cats,
        selected = NULL
      )
    })
    
    output$suggested_ich_number <- renderUI({
      tagList(
        tags$label("Suggested ICH Number based on Category and Sub-Category"),
        textOutput(ns("suggested_ich_number_text"))
      )
    })
    
    output$suggested_ich_number_text <- renderText({
      req(input$category_name, input$sub_category_name)
      cat_subcat() %>%
        filter(
          category_name == input$category_name,
          sub_category_name == input$sub_category_name
        ) %>%
        pull(suggested_ich_number)
    })
    
    # Event Handlers -----------------------------------------------
    # Add Report
    observeEvent(input$add, {
      showModal(modalDialog(
        title = div(icon("plus-circle"), "Add Report"),
        div(
          class = "form-group",
          selectizeInput(
            ns("report_type"),
            "Select Report Type",
            choices = c("Table", "Listing", "Figure"),
            selected = NULL
          ),
          uiOutput(ns("category_dropdown")),
          uiOutput(ns("sub_category_dropdown")),
          textInput(
            ns("report_key"),
            "Report Key",
            placeholder = "Enter unique report name (eg tdmsaf)",
            width = "100%"
          ),
          textInput(
            ns("title_key"),
            "Title Key",
            placeholder = "Enter titlekey (eg tdm)",
            width = "100%"
          ),
          uiOutput(ns("suggested_ich_number")),
          textInput(
            ns("report_ich_number"),
            "ICH Number",
            placeholder = "Enter ICH number",
            width = "100%"
          ),
          selectizeInput(
            ns("titles"),
            "Select Titles",
            choices = tables_data$titles()$title_text,
            multiple = TRUE
          ),
          selectizeInput(
            ns("footnotes"),
            "Select Footnotes",
            choices = tables_data$footnotes()$footnote_text,
            multiple = TRUE
          ),
          selectizeInput(
            ns("populations"),
            "Select Population",
            choices = tables_data$populations()$population_text
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_add"),
            "Add Report",
            class = "btn btn-primary",
            icon = icon("plus")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })

 # Database Operations ------------------------------------------
    # Add Report Handler
    observeEvent(input$confirm_add, {
      has_error <- FALSE
      
      # Validate required inputs
      if (is.null(input$report_type) || input$report_type == "") {
        shinyFeedback::feedbackDanger("report_type", TRUE, "Report type is required")
        has_error <- TRUE
      }
      
      if (is.null(input$category_name) || input$category_name == "") {
        shinyFeedback::feedbackDanger("category_name", TRUE, "Category is required")
        has_error <- TRUE
      }
      
      if (is.null(input$sub_category_name) || input$sub_category_name == "") {
        shinyFeedback::feedbackDanger("sub_category_name", TRUE, "Sub-category is required")
        has_error <- TRUE
      }
      
      if (is.null(input$report_key) || input$report_key == "") {
        shinyFeedback::feedbackDanger("report_key", TRUE, "Report key is required")
        has_error <- TRUE
      }
      
      if (is.null(input$title_key) || input$title_key == "") {
        shinyFeedback::feedbackDanger("title_key", TRUE, "Title key is required")
        has_error <- TRUE
      }
      
      if (is.null(input$report_ich_number) || input$report_ich_number == "") {
        shinyFeedback::feedbackDanger("report_ich_number", TRUE, "ICH number is required")
        has_error <- TRUE
      }
      
      if (is.null(input$titles) || length(input$titles) == 0) {
        shinyFeedback::feedbackDanger("titles", TRUE, "At least one title is required")
        has_error <- TRUE
      }
      
      if (is.null(input$populations) || input$populations == "") {
        shinyFeedback::feedbackDanger("populations", TRUE, "Population is required")
        has_error <- TRUE
      }
      
      # Validate keys format
      if (!validate_keys(input$report_type, input$report_key, input$title_key)) {
        prefix <- switch(input$report_type,
                      "Table" = "t",
                      "Listing" = "l",
                      "Figure" = "f")
        shinyFeedback::feedbackDanger("report_key", TRUE, sprintf("Must start with '%s'", prefix))
        shinyFeedback::feedbackDanger("title_key", TRUE, sprintf("Must start with '%s'", prefix))
        has_error <- TRUE
      }
      
      # Validate ICH number format
      if (!validate_ich_number(input$report_ich_number)) {
        prefix <- switch(input$report_type, "Table" = "14", "Listing" = "15", "Figure" = "16")
        shinyFeedback::feedbackDanger(
          "report_ich_number", 
          TRUE, 
          sprintf("Must start with %s for %s", prefix, tolower(input$report_type))
        )
        has_error <- TRUE
      }
      
      if (has_error) {
        show_toast(
          title = "Validation Error",
          type = "error",
          text = "Please correct the highlighted fields",
          position = "top-end"
        )
        return()
      }

      tryCatch({
        # Get IDs from reactive tables
        category_id <- tables_data$categories() %>%
          filter(category_name == input$category_name) %>%
          pull(id)
        
        sub_category_id <- tables_data$sub_categories() %>%
          filter(
            sub_category_name == input$sub_category_name,
            category_id == category_id
          ) %>%
          pull(id)
        
        population_id <- tables_data$populations() %>%
          filter(population_text == input$populations) %>%
          pull(id)
        
        # For titles and footnotes
        if (!is.null(input$titles)) {
          titles_data <- tables_data$titles() %>%
            filter(title_text %in% input$titles)
        }
        
        if (!is.null(input$footnotes)) {
          footnotes_data <- tables_data$footnotes() %>%
            filter(footnote_text %in% input$footnotes)
        }
        
        poolWithTransaction(pool, function(conn) {
          # Insert main report
          dbExecute(
            conn,
            "INSERT INTO reports (
              report_key, title_key, report_type, report_category_id,
              report_sub_category_id, report_ich_number, population_id,
              updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)",
            params = list(
              input$report_key,
              input$title_key,
              input$report_type,
              category_id,
              sub_category_id,
              input$report_ich_number,
              population_id
            )
          )
          
          report_id <- dbGetQuery(conn, "SELECT last_insert_rowid()")
          
          # Insert report titles
          if (!is.null(input$titles)) {
            for (title_text in input$titles) {
              sequence <- match(title_text, input$titles)
              title_id <- titles_data$id[titles_data$title_text == title_text]
              dbExecute(
                conn,
                "INSERT INTO report_titles (report_id, title_id, sequence) VALUES (?, ?, ?)",
                params = list(report_id[[1, 1]], title_id, sequence)
              )
            }
          }
          
          # Insert footnotes
          if (!is.null(input$footnotes)) {
            for (footnote_text in input$footnotes) {
              sequence <- match(footnote_text, input$footnotes)
              footnote_id <- footnotes_data$id[footnotes_data$footnote_text == footnote_text]
              dbExecute(
                conn,
                "INSERT INTO report_footnotes (report_id, footnote_id, sequence) VALUES (?, ?, ?)",
                params = list(report_id[[1, 1]], footnote_id, sequence)
              )
            }
          }
        })
        refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Success",
          type = "success",
          text = "Report added successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error adding report:", e$message),
          position = "top-end"
        )
      })
    })    

     # Edit Button Handler
    observeEvent(input$edit, {
       if (is.null(input$table_rows_selected)) {
        show_toast(
          title = "Information",
          type = "info",
          text = "Please select a record to edit.",
          position = "center"
        )
        return()
      }
      selected <- input$table_rows_selected
      report <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("edit"), "Edit Report"),
        div(
          class = "form-group",
          selectizeInput(
            ns("report_type"),
            "Select Report Type",
            choices = c("Table", "Listing", "Figure"),
            selected = report$report_type
          ),
          uiOutput(ns("category_dropdown")),
          uiOutput(ns("sub_category_dropdown")),
          textInput(
            ns("report_key"),
            "Report Key",
            value = report$report_key,
            width = "100%"
          ),
          textInput(
            ns("title_key"),
            "Title Key",
            value = report$title_key,
            width = "100%"
          ),
          uiOutput(ns("suggested_ich_number")),
          textInput(
            ns("report_ich_number"),
            "ICH Number",
            value = report$report_ich_number,
            width = "100%"
          ),
          selectizeInput(
            ns("titles"),
            "Select Titles",
            choices = tables_data$titles()$title_text,
            selected = strsplit(report$titles, "<br>")[[1]],
            multiple = TRUE
          ),
          selectizeInput(
            ns("footnotes"),
            "Select Footnotes",
            choices = tables_data$footnotes()$footnote_text,
            selected = if (!is.na(report$footnotes)) strsplit(report$footnotes, "<br>")[[1]] else NULL,
            multiple = TRUE
          ),
          selectizeInput(
            ns("populations"),
            "Select Population",
            choices = tables_data$populations()$population_text,
            selected = report$population_text
          )
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_edit"),
            "Save Changes",
            class = "btn btn-primary",
            icon = icon("save")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })
 # Edit Report Handler
    observeEvent(input$confirm_edit, {
      has_error <- FALSE
      
      # Validate required inputs
      if (is.null(input$report_type) || input$report_type == "") {
        shinyFeedback::feedbackDanger("report_type", TRUE, "Report type is required")
        has_error <- TRUE
      }
      
      if (is.null(input$category_name) || input$category_name == "") {
        shinyFeedback::feedbackDanger("category_name", TRUE, "Category is required")
        has_error <- TRUE
      }
      
      if (is.null(input$sub_category_name) || input$sub_category_name == "") {
        shinyFeedback::feedbackDanger("sub_category_name", TRUE, "Sub-category is required")
        has_error <- TRUE
      }
      
      if (is.null(input$report_key) || input$report_key == "") {
        shinyFeedback::feedbackDanger("report_key", TRUE, "Report key is required")
        has_error <- TRUE
      }
      
      if (is.null(input$title_key) || input$title_key == "") {
        shinyFeedback::feedbackDanger("title_key", TRUE, "Title key is required")
        has_error <- TRUE
      }
      
      if (is.null(input$report_ich_number) || input$report_ich_number == "") {
        shinyFeedback::feedbackDanger("report_ich_number", TRUE, "ICH number is required")
        has_error <- TRUE
      }
      
      if (is.null(input$titles) || length(input$titles) == 0) {
        shinyFeedback::feedbackDanger("titles", TRUE, "At least one title is required")
        has_error <- TRUE
      }
      
      if (is.null(input$populations) || input$populations == "") {
        shinyFeedback::feedbackDanger("populations", TRUE, "Population is required")
        has_error <- TRUE
      }
      
      # Validate keys format
      if (!validate_keys(input$report_type, input$report_key, input$title_key)) {
        prefix <- switch(input$report_type,
                      "Table" = "t",
                      "Listing" = "l",
                      "Figure" = "f")
        shinyFeedback::feedbackDanger("report_key", TRUE, sprintf("Must start with '%s'", prefix))
        shinyFeedback::feedbackDanger("title_key", TRUE, sprintf("Must start with '%s'", prefix))
        has_error <- TRUE
      }
      
      # Validate ICH number format
      # if (!validate_ich_number(input$report_ich_number)) {
      #   prefix <- switch(input$report_type, "Table" = "14", "Listing" = "15", "Figure" = "16")
      #   shinyFeedback::feedbackDanger(
      #     "report_ich_number", 
      #     TRUE, 
      #     sprintf("Must start with %s for %s", prefix, tolower(input$report_type))
      #   )
      #   has_error <- TRUE
      # }
      
      if (has_error) {
        show_toast(
          title = "Validation Error",
          type = "error",
          text = "Please correct the highlighted fields",
          position = "top-end"
        )
        return()
      }

      tryCatch({
        selected_row <- input$table_rows_selected
        report_id <- data()[selected_row, "id"]
        
        # Get IDs from reactive tables
        category_id <- tables_data$categories() %>%
          filter(category_name == input$category_name) %>%
          pull(id)
        
        sub_category_id <- tables_data$sub_categories() %>%
          filter(sub_category_name == input$sub_category_name) %>%
          inner_join(tables_data$categories() %>% filter(category_name == input$category_name) , by = c("category_id" = "id")) %>%
          pull(id)
        
        population_id <- tables_data$populations() %>%
          filter(population_text == input$populations) %>%
          pull(id)
        
        # For titles and footnotes
        if (!is.null(input$titles)) {
          titles_data <- tables_data$titles() %>%
            filter(title_text %in% input$titles)
        }
        
        if (!is.null(input$footnotes)) {
          footnotes_data <- tables_data$footnotes() %>%
            filter(footnote_text %in% input$footnotes)
        }
      

        poolWithTransaction(pool, function(conn) {
          # Update main report
          dbExecute(
            conn,
            "UPDATE reports SET 
              report_key = ?, title_key = ?, report_type = ?,
              report_category_id = ?, report_sub_category_id = ?,
              report_ich_number = ?, population_id = ?,
              updated_at = CURRENT_TIMESTAMP
             WHERE id = ?",
            params = list(
              input$report_key, input$title_key, input$report_type,
              category_id, sub_category_id,
              input$report_ich_number, population_id,
              report_id
            )
          )
          
          # Update titles
          dbExecute(conn, "DELETE FROM report_titles WHERE report_id = ?", 
                   params = list(report_id))
          
          if (!is.null(input$titles)) {
            for (title_text in input$titles) {
              sequence <- match(title_text, input$titles)
              title_id <- titles_data$id[titles_data$title_text == title_text]
              dbExecute(
                conn,
                "INSERT INTO report_titles (report_id, title_id, sequence) VALUES (?, ?, ?)",
                params = list(report_id, title_id, sequence)
              )
            }
          }
          
          # Update footnotes
          dbExecute(conn, "DELETE FROM report_footnotes WHERE report_id = ?", 
                   params = list(report_id))
          
          if (!is.null(input$footnotes)) {
            for (footnote_text in input$footnotes) {
              sequence <- match(footnote_text, input$footnotes)
              footnote_id <- footnotes_data$id[footnotes_data$footnote_text == footnote_text]
              dbExecute(
                conn,
                "INSERT INTO report_footnotes (report_id, footnote_id, sequence) VALUES (?, ?, ?)",
                params = list(report_id, footnote_id, sequence)
              )
            }
          }
        })
        refresh_trigger(refresh_trigger() + 1)
        show_toast(
          title = "Success",
          type = "success",
          text = "Report updated successfully!",
          position = "top-end"
        )
        
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error updating report:", e$message),
          position = "top-end"
        )
      })
    })

    # Delete Button Handler
    observeEvent(input$delete, {
      if (is.null(input$table_rows_selected)) {
        show_toast(
          title = "Information",
          type = "info",
          text = "Please select a record to delete.",
          position = "center"
        )
        return()
      }
      selected <- input$table_rows_selected
      report <- data()[selected, ]
      
      showModal(modalDialog(
        title = div(icon("trash"), "Delete Report"),
        div(
          "Are you sure you want to delete this report?",
          tags$br(), tags$br(),
          tags$strong("Report Details:"),
          tags$br(),
          sprintf("Type: %s", report$report_type),
          tags$br(),
          sprintf("Key: %s", report$report_key),
          tags$br(),
          sprintf("Category: %s", report$category_name),
          tags$br(),
          sprintf("Sub-Category: %s", report$sub_category_name)
        ),
        footer = div(
          class = "modal-footer",
          modalButton("Cancel", icon = icon("times")),
          actionButton(
            ns("confirm_delete"),
            "Delete Report",
            class = "btn btn-danger",
            icon = icon("trash")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })

    # Delete Report Handler
    observeEvent(input$confirm_delete, {
      tryCatch({
        selected_row <- input$table_rows_selected
        report_id <- data()[selected_row, "id"]
        
        poolWithTransaction(pool, function(conn) {
          # Delete related records first
          dbExecute(conn, "DELETE FROM report_titles WHERE report_id = ?", 
                   params = list(report_id))
          dbExecute(conn, "DELETE FROM report_footnotes WHERE report_id = ?", 
                   params = list(report_id))
          
          # Delete main report
          dbExecute(conn, "DELETE FROM reports WHERE id = ?", 
                   params = list(report_id))
        })
        
        show_toast(
          title = "Success",
          type = "success",
          text = "Report deleted successfully!",
          position = "top-end"
        )
        refresh_trigger(refresh_trigger() + 1)
        removeModal()
      }, error = function(e) {
        show_toast(
          title = "Error",
          type = "error",
          text = paste("Error deleting report:", e$message),
          position = "top-end"
        )
      })
    })



  })
}    