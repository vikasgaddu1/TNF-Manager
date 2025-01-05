FAQModuleUI <- function(id) {
  ns <- NS(id)
  
  # Create FAQ tibble
  faq_data <- tibble::tibble(
    Question = c(
      "What is the difference between Report Key and Title Key?",
      "Another Common Question?",
      "Yet Another Question?"
    ),
    Answer = c(
      "Report Key can be thought like a program name, and one program can produce multiple outputs. Each output has a different Title Key. Another way to think of it: for the same Title Key, we may have different layouts like Default, Conditional, and Optional.",
      "Answer to the second question goes here.",
      "Answer to the third question goes here."
    )
  )
  
  tagList(
    h2("Frequently Asked Questions (FAQ)", style = "margin: 20px 0;"),
    fluidRow(
      column(
        width = 12,
        DT::datatable(
          faq_data,
          options = list(
            pageLength = 100,
            dom = 'ft',
            scrollX = TRUE,
            searching = TRUE,
            searchHighlight = TRUE,
            ordering = FALSE
          ),
          class = 'stripe hover',
          rownames = FALSE,
          selection = 'none'  # Disable selection
        )
      )
    )
  )
}
