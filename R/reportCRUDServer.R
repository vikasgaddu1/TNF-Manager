reportCRUDServer <- function(id, pool, tabs_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger to refresh data
    refresh_trigger <- reactiveVal(0)
    
    # Auto-refresh categories when tab is selected
    observeEvent(tabs_input(), {
      if (tabs_input() == "reports") {
        refresh_trigger(refresh_trigger() + 1)
        showNotification("Refreshing reports",
                         type = "message",
                         duration = 1)
      }
    }, ignoreInit = TRUE)
    
    
    # Load and join data only once, when the app initializes
    cat_subcat <- reactive({
      refresh_trigger()
      tbl_categories <- dbReadTable(pool, "categories") %>%
        select(id, category_name)
      cat_subcat <- dbReadTable(pool, "sub_categories")
      
      cat_subcat %>%
        left_join(tbl_categories, by = c("category_id" = "id")) %>%
        select(
          id,
          category_id,
          category_name,
          sub_category_name,
          suggested_ich_number,
          updated_at
        ) %>%
        arrange(category_name, sub_category_name) %>%
        filter(!is.na(category_name))
      
    })
    
    titles <- reactive({
      refresh_trigger()
      dbReadTable(pool, "titles")
    })
    
    footnotes <- reactive({
      refresh_trigger()
      dbReadTable(pool, "footnotes")
    })
    
    populations <- reactive({
      refresh_trigger()
      dbReadTable(pool, "populations")
    })
    
    # Category choices for the dropdown
    output$category_dropdown <- renderUI({
      selectInput(
        ns("category_name"),
        "Category",
        choices = unique(cat_subcat()$category_name),
        selected = NULL
      )
    })
    
    data <- reactive({
      refresh_trigger()
      
      # Get main report data
      main_query <- "
    SELECT
      r.report_key,
      r.title_key,
      r.report_type,
      c.category_name,
      s.sub_category_name,
      s.suggested_ich_number,
      p.population_text,
      r.report_ich_number,
      r.id
    FROM reports r
    LEFT JOIN categories c ON r.report_category_id = c.id
    LEFT JOIN sub_categories s ON r.report_sub_category_id = s.id and r.report_category_id = s.category_id
    LEFT JOIN populations p ON r.population_id = p.id
  "
      main_data <- dbGetQuery(pool, main_query)
      
      # Get titles
      titles_query <- "
    SELECT rt.report_id,
           GROUP_CONCAT(t.title_text, '@# ') as titles
    FROM report_titles rt
    JOIN titles t ON rt.title_id = t.id
    GROUP BY rt.report_id
  "
      titles_data <- dbGetQuery(pool, titles_query)
      
      # Get footnotes
      footnotes_query <- "
    SELECT rf.report_id,
           GROUP_CONCAT(f.footnote_text, '@# ') as footnotes
    FROM report_footnotes rf
    JOIN footnotes f ON rf.footnote_id = f.id
    GROUP BY rf.report_id
  "
      footnotes_data <- dbGetQuery(pool, footnotes_query)
      
      # Combine data
      result <- main_data %>%
        left_join(titles_data, join_by(id == report_id)) %>% 
        left_join(footnotes_data, join_by(id == report_id)) %>% 
        arrange(category_name, sub_category_name, report_type, report_key) %>% 
        select(
          id,
          report_type,
          category_name,
          sub_category_name,
          report_key,
          title_key,
          population_text,
          report_ich_number,
          titles,
          footnotes
        )
      
    })
    
    output$table <- DT::renderDT({
      data <- data() # Ensure the reactive data is fetched
      DT::datatable(
        data,
        filter = "top",
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
          "Footnotes" = "footnotes"),
        selection = "single",
        rownames = FALSE,
        class = 'table table-striped table-bordered',
        options = list(columnDefs = list(list(
          targets = c(0), visible = FALSE
        )))
      )
    })
    
    
    # Sub-category drop down that updates based on selected category
    output$sub_category_dropdown <- renderUI({
      req(input$category_name)
      sub_cats <- cat_subcat() %>%
        filter(category_name == input$category_name) %>%
        pull(sub_category_name) %>%
        unique()
      
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
        filter(category_name == input$category_name,
               sub_category_name == input$sub_category_name) %>%
        pull(suggested_ich_number)
    })
    
    # Display modal with cascading dropdowns
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
            choices = titles()$title_text,
            multiple = TRUE
          ),
          selectizeInput(
            ns("footnotes"),
            "Select Footnotes",
            choices = footnotes()$footnote_text,
            multiple = TRUE
          ),
          selectizeInput(
            ns("populations"),
            "Select Population",
            choices = populations()$population_text
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
    
    # Add logic for confirm add button (e.g., add to database or refresh data)
    observeEvent(input$confirm_add, {
      # print("Confirm Add button clicked")
      # print(paste("Category:", input$category_name)) # Check input values
      # print(paste("Sub-category:", input$sub_category_name))
      # print(paste("Report Key:", input$report_key))
      # print(paste("Title Key:", input$title_key))
      # print(paste("Suggested ICH Number:", input$report_ich_number))
      # print(paste("Report Type:", input$report_type))
      # print(paste("Titles:", toString(input$titles)))
      # print(paste("Footnotes:", toString(input$footnotes)))
      # print(paste("Population:", input$populations))
      
      # Validate required inputs
      req(
        input$category_name,
        input$sub_category_name,
        input$report_key,
        input$title_key,
        input$report_ich_number,
        input$report_type,
        input$titles,
        input$populations
      )
      
      # Add validation rules for report_key and title_key prefixes
      validate_keys <- function(report_type, report_key, title_key) {
        expected_prefix <- switch(report_type,
                                  "Table" = "t",
                                  "Listing" = "l",
                                  "Figure" = "f",
                                  NULL)
        
        if (is.null(expected_prefix)) {
          return(FALSE)
        }
        
        report_key_valid <- startsWith(tolower(report_key), expected_prefix)
        title_key_valid <- startsWith(tolower(title_key), expected_prefix)
        
        return(report_key_valid && title_key_valid)
      }
      
      validate_ich_number <- function(ich_number) {
        # Regex pattern: ^(1[4-6])\.(\d+\.)+(\d+|x)$
        # Examples of valid patterns:
        # 14.1.1
        # 14.2.1.1
        # 15.1.1.x
        # 16.2.1.1
        if (input$report_type == "Table"){
          pattern <- "^(1[4])\\.((\\d+|x)\\.)+(\\d+|x)$"
        }else if (input$report_type == "Listing"){
          pattern <- "^(1[5])\\.((\\d+|x)\\.)+(\\d+|x)$"
        }else if (input$report_type == "Figure"){
          pattern <- "^(1[6])\\.((\\d+|x)\\.)+(\\d+|x)$"
        }
        
        return(grepl(pattern, ich_number))
      }
      
      
      # Validate keys before proceeding
      if (!validate_keys(input$report_type, input$report_key, input$title_key)) {
        showNotification(
          sprintf("For %s, both report_key and title_key must start with '%s'",
                  input$report_type,
                  switch(input$report_type,
                         "Table" = "t",
                         "Listing" = "l",
                         "Figure" = "f")),
          type = "error"
        )
        return()
      }
      
      # Validate ICH number
      if (!validate_ich_number(input$report_ich_number)) {
        showNotification(
          paste("Invalid ICH number format. Must start with",
                "14 for table, 15 for listing, or 16 for figure,",
                "followed by number sections separated by dots,",
                "and end with a number or 'x'.",
                "\nExample: 14.1.1 or 15.2.1.x"),
          type = "error"
        )
        return()
      }
      
      # Insert new report
      tryCatch({
        # Start transaction
        dbExecute(pool, "BEGIN")
        
        # Insert main report record
        category_id <- dbGetQuery(pool, "SELECT id FROM categories WHERE category_name = ?", params = list(input$category_name))
        sub_category_id <- dbGetQuery(
          pool,
          "
          SELECT id 
          FROM sub_categories 
          WHERE sub_category_name = ? 
            AND category_id = (SELECT id FROM categories WHERE category_name = ?)
          ",
          params = list(input$sub_category_name, input$category_name)
        )
        
        population_id <- dbGetQuery(pool, "SELECT id FROM populations WHERE population_text = ?", params = list(input$populations))
        
        # print(paste("Category ID:", category_id)) # Debugging
        # print(paste("Sub-category ID:", sub_category_id))
        # print(paste("Population ID:", population_id))
        # 
        # print(typeof(category_id$id))
        # print(typeof(sub_category_id$id))
        # print(typeof(population_id$id))
        
        
        # Now pass these IDs to the query
        dbExecute(
          pool,
          "
          INSERT INTO reports (
            report_key,
            title_key,
            report_type,
            report_category_id,
            report_sub_category_id,
            report_ich_number,
            population_id
          )
          VALUES (?, ?, ?, ?, ?, ?, ?)
          
          ",
          params = list(
            input$report_key,
            input$title_key,
            input$report_type,
            category_id$id,
            sub_category_id$id,
            input$report_ich_number,
            population_id$id
          )
        )
        
        report_id <- dbGetQuery(pool, "SELECT last_insert_rowid()")
        # print(typeof(report_id[[1, 1]]))
        # 
        # print(paste("Inserted report ID:", report_id[[1, 1]])) # Debugging
        
        # Insert report titles
        if (!is.null(input$titles)) {
          for (title_text in input$titles) {
            sequence <- match(title_text, input$titles) # Get the first match
            
            # Ensure all parameters have valid values
            # print(list(report_id = report_id[[1, 1]], sequence = sequence, title_text = title_text)) # Debugging
            
            dbExecute(
              pool,
              "INSERT INTO report_titles (report_id, title_id, sequence)
       SELECT ?, id, ? FROM titles WHERE title_text = ?",
              params = list(
                report_id[[1, 1]],
                sequence,
                title_text
              )
            )
          }
        }
        
        
        # Insert footnotes if selected
        if (!is.null(input$footnotes)) {
          for (footnote_text in input$footnotes) {
            sequence <- match(footnote_text, input$footnotes) # Get the first match
            print(which(input$footnotes == footnote_text))
            dbExecute(
              pool,
              "INSERT INTO report_footnotes (report_id, footnote_id, sequence)
           SELECT ?, id, ? FROM footnotes WHERE footnote_text = ?",
              params = list(
                report_id[[1, 1]],
                which(input$footnotes == footnote_text),
                footnote_text
              )
            )
          }
        }
        
        # Commit transaction
        dbExecute(pool, "COMMIT")
        
        # Refresh the data
        refresh_trigger(refresh_trigger() + 1)
        
        # Show success notification
        showNotification("Report added successfully", type = "message")
        
      }, error = function(e) {
        # Rollback on error
        print(paste("Error:", e$message))
        showNotification(paste("Error:", e$message), type = "error")
        dbExecute(pool, "ROLLBACK")
        
      })
      
      # Close the modal
      removeModal()
    })
    
    # Edit Report
    # Add these inside the moduleServer function, after the existing code
    
    # Edit button observer
    observeEvent(input$edit, {
      selected_row <- input$table_rows_selected
      if (length(selected_row) > 0) {
        report_data <- data()[selected_row, ]
        #print(report_data$footnotes)
        
        selected_footnotes <- if (!is.null(report_data$footnotes) && is.character(report_data$footnotes)) {
          trimws(unlist(strsplit(report_data$footnotes, "@#")))
        } else {
          NULL
        }
        
        selected_titles <- if (!is.null(report_data$titles) && is.character(report_data$titles)) {
          trimws(unlist(strsplit(report_data$titles, "@#")))
        } else {
          NULL
        }
        
        
        showModal(modalDialog(
          title = div(icon("edit"), "Edit Report"),
          div(
            class = "form-group",
            selectizeInput(
              ns("report_type"), 
              "Select Report Type",
              choices = c("Table", "Listing", "Figure"),
              selected = report_data$report_type
            ),
            uiOutput(ns("category_dropdown")),
            uiOutput(ns("sub_category_dropdown")),
            textInput(
              ns("report_key"),
              "Report Key",
              value = report_data$report_key
            ),
            textInput(
              ns("title_key"),
              "Title Key",
              value = report_data$title_key
            ),
            textInput(
              ns("report_ich_number"),
              "ICH Number",
              value = report_data$report_ich_number
            ),
            selectizeInput(
              ns("titles"),
              "Select Titles",
              choices = titles()$title_text,
              selected = selected_titles,
              multiple = TRUE
            ),
            selectizeInput(
              ns("footnotes"),
              "Select Footnotes",
              choices = footnotes()$footnote_text,
              selected = selected_footnotes,
              multiple = TRUE
            ),
            selectizeInput(
              ns("populations"),
              "Select Population",
              choices = populations()$population_text,
              selected = report_data$population_text
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
          size = "m"
        ))
      }else{
        showNotification("Please select a row to edit", type = "warning", duration = 3)
      }
    })
    
    # Delete button observer
    observeEvent(input$delete, {
      selected_row <- input$table_rows_selected
      if (length(selected_row) > 0) {
        report_id <- data()[selected_row, "id"]
        report_key <- data()[selected_row, "report_key"]
        
        showModal(modalDialog(
          title = div(
            class = "d-flex align-items-center",
            icon("trash-alt", class = "text-danger me-2"),
            "Delete Sub-Category"
          ),
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle", class = "me-2"),
            "This action cannot be undone!"
          ),
          div(
            class = "mb-3",
            sprintf("Are you sure you want to delete the report '%s'?", 
                    report_key)
          ),
          footer = div(
            class = "modal-footer",
            modalButton(
              "Cancel",
              icon=icon("times")
            ),
            actionButton(
              ns("confirm_delete"),
              "Delete Category",
              class = "btn btn-danger",
              icon = icon("trash-alt")
            )
          ),
          size = "m",
          easyClose = TRUE
        ))
      }
    })
    
    # Confirm delete observer
    observeEvent(input$confirm_delete, {
      selected_row <- input$table_rows_selected
      if (length(selected_row) > 0) {
        report_id <- data()[selected_row, "id"]
        
        tryCatch({
          dbExecute(pool, "BEGIN")
          
          # Delete related records first
          dbExecute(pool, "DELETE FROM report_titles WHERE report_id = ?", 
                    params = list(report_id))
          dbExecute(pool, "DELETE FROM report_footnotes WHERE report_id = ?", 
                    params = list(report_id))
          
          # Delete the report
          dbExecute(pool, "DELETE FROM reports WHERE id = ?", 
                    params = list(report_id))
          
          dbExecute(pool, "COMMIT")
          
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Report deleted successfully", type = "message")
        }, error = function(e) {
          dbExecute(pool, "ROLLBACK")
          showNotification(paste("Error:", e$message), type = "error")
        })
        
        removeModal()
      }
    })
    
    # Confirm edit observer
    observeEvent(input$confirm_edit, {
      selected_row <- input$table_rows_selected
      # Validate required inputs
      req(
        input$category_name,
        input$sub_category_name,
        input$report_key,
        input$title_key,
        input$report_ich_number,
        input$report_type,
        input$titles,
        input$populations
      )
      
      # Add validation rules for report_key and title_key prefixes
      validate_keys <- function(report_type, report_key, title_key) {
        expected_prefix <- switch(report_type,
                                  "Table" = "t",
                                  "Listing" = "l",
                                  "Figure" = "f",
                                  NULL)
        
        if (is.null(expected_prefix)) {
          return(FALSE)
        }
        
        report_key_valid <- startsWith(tolower(report_key), expected_prefix)
        title_key_valid <- startsWith(tolower(title_key), expected_prefix)
        
        return(report_key_valid && title_key_valid)
      }
      
      validate_ich_number <- function(ich_number) {
        # Regex pattern: ^(1[4-6])\.(\d+\.)+(\d+|x)$
        # Examples of valid patterns:
        # 14.1.1
        # 14.2.1.1
        # 15.1.1.x
        # 16.2.1.1
        if (input$report_type == "Table"){
          pattern <- "^(1[4])\\.(\\d+\\.)+(\\d+|x)$"
        }else if (input$report_type == "Listing"){
          pattern <- "^(1[5])\\.(\\d+\\.)+(\\d+|x)$"
        }else if (input$report_type == "Figure"){
          pattern <- "^(1[6])\\.(\\d+\\.)+(\\d+|x)$"
        }
        
        return(grepl(pattern, ich_number))
      }
      
      
      # Validate keys before proceeding
      if (!validate_keys(input$report_type, input$report_key, input$title_key)) {
        showNotification(
          sprintf("For %s, both report_key and title_key must start with '%s'",
                  input$report_type,
                  switch(input$report_type,
                         "Table" = "t",
                         "Listing" = "l",
                         "Figure" = "f")),
          type = "error"
        )
        return()
      }
      
      # Validate ICH number
      if (!validate_ich_number(input$report_ich_number)) {
        showNotification(
          paste("Invalid ICH number format. Must start with",
                "14 for table, 15 for listing, or 16 for figure,",
                "followed by number sections separated by dots,",
                "and end with a number or 'x'.",
                "\nExample: 14.1.1 or 15.2.1.x"),
          type = "error"
        )
        return()
      }
      if (length(selected_row) > 0) {
        report_id <- data()[selected_row, "id"]
        
        tryCatch({
          dbExecute(pool, "BEGIN")
          
          # Update main report record
          dbExecute(
            pool,
            "UPDATE reports SET 
         report_key = ?,
         title_key = ?,
         report_type = ?,
         report_ich_number = ?,
         report_category_id = (SELECT id FROM categories WHERE category_name = ?),
         report_sub_category_id = (SELECT id FROM sub_categories WHERE sub_category_name = ?),
         population_id = (SELECT id FROM populations WHERE population_text = ?)
         WHERE id = ?",
            params = list(
              input$report_key,
              input$title_key,
              input$report_type,
              input$report_ich_number,
              input$category_name,
              input$sub_category_name,
              input$populations,
              report_id
            )
          )
          
          # Update titles
          dbExecute(pool, "DELETE FROM report_titles WHERE report_id = ?", 
                    params = list(report_id))
          for (title_text in input$titles) {
            sequence <- match(title_text, input$titles)
            dbExecute(
              pool,
              "INSERT INTO report_titles (report_id, title_id, sequence) 
           SELECT ?, id, ? FROM titles WHERE title_text = ?",
              params = list(report_id, sequence, title_text)
            )
          }
          
          # Update footnotes
          dbExecute(pool, "DELETE FROM report_footnotes WHERE report_id = ?", 
                    params = list(report_id))
          for (footnote_text in input$footnotes) {
            sequence <- match(footnote_text, input$footnotes)
            dbExecute(
              pool,
              "INSERT INTO report_footnotes (report_id, footnote_id, sequence) 
           SELECT ?, id, ? FROM footnotes WHERE footnote_text = ?",
              params = list(report_id, sequence, footnote_text)
            )
          }
          
          dbExecute(pool, "COMMIT")
          refresh_trigger(refresh_trigger() + 1)
          showNotification("Report updated successfully", type = "message")
        }, error = function(e) {
          dbExecute(pool, "ROLLBACK")
          showNotification(paste("Error:", e$message), type = "error")
        })
        
        removeModal()
      }
    })
    
  })
}
