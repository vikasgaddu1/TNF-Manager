FAQModuleUI <- function(id) {
  ns <- NS(id)
  
  # Create FAQ tibble using tribble
  faq_data <- tibble::tribble(
    ~faq,
    "**Q: What is the difference between Report Key and Title Key?**",
    "A: Report Key can be thought like a program name, and one program can produce multiple outputs. Each output has a different Title Key. Another way to think of it: for the same Title Key, we may have different layouts like Default, Conditional, and Optional.",
    "**Q: Another Common Question?**",
    "A: Answer to the second question goes here.",
    "**Q: Yet Another Question?**",
    "A: Answer to the third question goes here."
  )
  
  # Convert Markdown to HTML for display
  faq_data <- faq_data %>%
    dplyr::mutate(faq = purrr::map_chr(faq, ~ markdown::markdownToHTML(text = ., fragment.only = TRUE)))
  
  tagList(
    h2("Frequently Asked Questions (FAQ)", style = "margin: 20px 0;"),
    fluidRow(
      column(
        width = 12,
        DT::datatable(
          faq_data,
          options = list(
            pageLength = 100,
            dom = 'ft',  # Only show the table and footer
            scrollX = TRUE,
            searching = TRUE,
            searchHighlight = TRUE,
            ordering = FALSE  # Disable column ordering
          ),
          colnames = NULL,
          class = 'stripe hover',  # Add styling
          rownames = FALSE,  # Hide row names
          selection = 'none',  # Disable row selection
          escape = FALSE  # Allow HTML rendering for bold text
        )
      )
    )
  )
}
