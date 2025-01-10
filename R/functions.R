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

pollAllTables <- function(pool, tableNames) {
  rv <- reactiveValues()
  
  # Create a list of reactive pollers, one for each table
  for (table in tableNames) {
    local({
      tbl <- table  # Create local copy of table name
      tableCheckQuery <- sprintf("SELECT MAX(updated_at) as last_update, COUNT(*) as row_count FROM %s", tbl)
      dataQuery <- sprintf("SELECT * FROM %s", tbl)
      
      # Create reactive polling for this table
      rv[[tbl]] <- reactiveDatabasePolling(
        pool = pool,
        checkQuery = tableCheckQuery,
        dataQuery = dataQuery,
        poll_interval = 1000  # 1 second interval, adjust as needed
      )
    })
  }
  
  return(rv)
}

reactiveDatabasePolling <- function(pool, checkQuery, dataQuery, poll_interval = 1000) {
  reactivePoll(
    intervalMillis = poll_interval,
    session = NULL,
    checkFunc = function() {
      # Check for changes using the checkQuery
      result <- dbGetQuery(pool, checkQuery)
      # Create a unique hash or string from the result to detect changes
      paste(result, collapse = "|")
    },
    valueFunc = function() {
      # Fetch data using the dataQuery
      dbGetQuery(pool, dataQuery)
    }
  )
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

  # Helper Functions -----------------------------------------------
  validate_keys <- function(report_type, report_key, title_key) {
    prefix <- switch(report_type,
                    "Table" = "t",
                    "Listing" = "l",
                    "Figure" = "f")
    
    startsWith(report_key, prefix) && startsWith(title_key, prefix)
  }
  
  validate_ich_number <- function(ich_number) {
    if (is.null(ich_number) || ich_number == "") return(FALSE)
    
    # Check if it starts with valid prefix
    valid_prefixes <- c("14", "15", "16")
    any(sapply(valid_prefixes, function(prefix) startsWith(ich_number, prefix)))
  }