# This function will take a vector with names of libraries and check if 
# they are installed. If not, it will install them and load them
load_libraries <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# Helper function for database reads
fetch_table <- function(pool, table_name, cols = NULL) {
  if (is.null(cols)) {
    dbReadTable(pool, table_name)
  } else {
    dbReadTable(pool, table_name) %>% select(all_of(cols))
  }
}

# Helper function for joins
combine_data <- function(main_data, titles_data, footnotes_data) {
  main_data %>%
    left_join(titles_data, join_by(id == report_id)) %>%
    left_join(footnotes_data, join_by(id == report_id)) %>%
    arrange(category_name, sub_category_name, report_type, report_key)
}

# Helper function for validation
validate_report_inputs <- function(report_type, report_key, title_key, ich_number) {
  expected_prefix <- switch(report_type, "Table" = "t", "Listing" = "l", "Figure" = "f", NULL)
  validate(
    need(!is.null(expected_prefix), "Invalid report type"),
    need(startsWith(tolower(report_key), expected_prefix), paste("Report key must start with", expected_prefix)),
    need(startsWith(tolower(title_key), expected_prefix), paste("Title key must start with", expected_prefix))
  )
  
  pattern <- switch(
    report_type,
    "Table" = "^(14)\\.(\\d+\\.)+(\\d+|x)$",
    "Listing" = "^(15)\\.(\\d+\\.)+(\\d+|x)$",
    "Figure" = "^(16)\\.(\\d+\\.)+(\\d+|x)$"
  )
  validate(need(grepl(pattern, ich_number), "Invalid ICH number format"))
}