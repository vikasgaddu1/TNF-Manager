FAQModuleUI <- function(id) {
  ns <- NS(id)
  
  # Create FAQ tibble using tribble
  faq_data <- tibble::tribble(
    ~faq,
    "**Q: What is the difference between Report Key and Title Key?**",
    "A: Report Key can be thought like a program name, and one program can produce multiple outputs. Each output has a different Title Key. Another way to think of it: for the same Title Key, we may have different layouts like Default, Conditional, and Optional.",
    
    "**Q: How do I add a new report?**",
    "A: Click on the 'Add Report' button, fill in the required fields such as Report Type, Category, Sub-Category, Report Key, Title Key, and ICH Number, and then click 'Add Report'. Ensure all fields are correctly filled to avoid validation errors.",
    
    "**Q: What are the validation rules for Report Key and Title Key?**",
    "A: The Report Key and Title Key must start with specific prefixes based on the Report Type: 't' for Table, 'l' for Listing, and 'f' for Figure. This ensures consistency and helps in categorizing reports.",
    
    "**Q: How is the ICH Number validated?**",
    "A: The ICH Number must follow a specific format: it starts with '14' for Table, '15' for Listing, or '16' for Figure, followed by sections separated by dots, and ends with a number or 'x'. For example, '14.1.1' or '15.2.1.x'.",
    
    "**Q: Can I edit or delete a report after adding it?**",
    "A: Yes, you can edit or delete a report. To edit, select a report from the table and click 'Edit'. To delete, select a report and click 'Delete'. Confirm your actions in the modal dialogs that appear.",
    
    "**Q: What happens if I encounter an error while adding or editing a report?**",
    "A: If an error occurs, a notification will appear with details about the error. Ensure all fields are correctly filled and follow the validation rules. If the issue persists, contact support.",
    
    "**Q: How are titles and footnotes managed in reports?**",
    "A: Titles and footnotes can be selected from predefined lists when adding or editing a report. They are stored separately and linked to reports, allowing for flexible management and reuse across different reports."
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