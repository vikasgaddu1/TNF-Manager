searchServer <- function(id, tfl_tracker_data, sdtm_tracker_data, adam_tracker_data) {
  moduleServer(id, function(input, output, session) {
    output$tfl_tracker <- DT::renderDT({
      datatable(
        tfl_tracker_data() %>% dplyr::select(-contains("id")),
        rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
      })

    output$sdtm_tracker <- DT::renderDT({
      datatable(
        sdtm_tracker_data() %>% dplyr::select(-contains("id")),
         rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
      })

    output$adam_tracker <- DT::renderDT({
      datatable(
        adam_tracker_data() %>% dplyr::select(-contains("id")),
         rownames = FALSE,
        selection = "none",
        filter = "top",   # Enables column-wise filtering
        class = "display compact",
        escape = FALSE,
        extensions = c("Buttons", "Scroller"),  # Added Scroller for scrollbars
        options = list(
          pageLength = 50,    # Show 50 rows at a time
          lengthMenu = c(10, 25, 50, 100),  # Allow user to change rows per page
          lengthChange = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,   # Enable horizontal scrolling
          scrollY = "600px", # Enable vertical scrolling with a fixed height
          dom = "lBfrtip",
          buttons = c("copy", "csv", "excel"),
          search = list(regex = TRUE)  # Enable regex search
        )
      )
    })
  })
}

