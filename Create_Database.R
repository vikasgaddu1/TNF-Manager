library(readxl)
library(dplyr)
library(pool)
library(DBI)

source("R/mod_create_tables.r")

# Function to process categories
db_insert_categories <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS categories")
    mod_create_tables(pool)
    unique_categories <- unique(df$category)
    categories_df <- data.frame(
      category_name = unique_categories
    )
    dbWriteTable(pool, "categories", categories_df, append = TRUE)
  }
  categories <- dbGetQuery(pool, "SELECT id AS category_id,category_name FROM categories")
  return(categories)
}

# Function to process subcategories
db_insert_subcategories <- function(pool, categories, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS sub_categories")
    mod_create_tables(pool)
    subcategories <- data.frame(
      category_id = categories$category_id,
      sub_category_name = "General",
      suggested_ich_number = "x.x.x.x"
    ) %>% arrange(category_id)
    dbWriteTable(pool, "sub_categories", subcategories, append = TRUE)
  }
  subcategories <- dbGetQuery(pool, "SELECT id AS sub_category_id, category_id, sub_category_name, suggested_ich_number FROM sub_categories")
  return(subcategories)
}

# Function to process populations
db_insert_populations <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS populations")
    mod_create_tables(pool)
    unique_populations <- unique(df$population)
    populations_df <- data.frame(
      population_text = unique_populations
    )
    dbWriteTable(pool, "populations", populations_df, append = TRUE)
  }

  populations <- dbGetQuery(pool, "SELECT id AS population_id, population_text FROM populations")
  return(populations)
}

# Function to process titles
db_insert_titles <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS titles")
    mod_create_tables(pool)
    unique_titles <- unique(df$title)
    titles_df <- data.frame(
      title_text = unique_titles
    )
    dbWriteTable(pool, "titles", titles_df, append = TRUE)
  }

  titles <- dbGetQuery(pool, "SELECT id AS title_id, title_text FROM titles")
  return(titles)
}

db_insert_footnotes <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS footnotes")
    mod_create_tables(pool)
    unique_footnotes <- unique(df$footnote)
    footnotes_df <- data.frame(
      footnote_text = unique_footnotes
    )
    dbWriteTable(pool, "footnotes", footnotes_df, append = TRUE)
  }

  footnotes <- dbGetQuery(pool, "SELECT id AS footnote_id, footnote_text FROM footnotes")
  return(footnotes)
}

# Function to process reports
db_insert_reports <- function(pool, df_with_ids, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS reports")
    mod_create_tables(pool)
    reports_data <- df_with_ids %>%
      transmute(
        report_type = type,
        report_key = delivery_name,
        title_key = delivery_name,
        report_category_id = category_id,
        report_sub_category_id = sub_category_id,
        report_ich_number = suggested_ich_number,
        population_id = population_id
      ) %>% 
      filter(! is.na(report_key))
    dbWriteTable(pool, "reports", reports_data, append = TRUE)
  }
  
  reports <- dbGetQuery(pool, "SELECT id as report_id, report_type, report_key, report_ich_number FROM reports")
  return(reports)
}

db_insert_reports_titles <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS report_titles")
    mod_create_tables(pool)
    reports_titles <- df %>%
      transmute(
        report_id = report_id,
        title_id = title_id,
        sequence = row_number()
      )
    dbWriteTable(pool, "report_titles", reports_titles, append = TRUE)
  }
  
  ret_reports_titles <- dbGetQuery(pool, "SELECT id as report_title_id FROM report_titles")
  return(ret_reports_titles)
}

db_insert_reports_footnotes <- function(pool, df, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS report_footnotes")
    mod_create_tables(pool)
    reports_footnotes <- df %>%
      transmute(
        report_id = report_id,
        footnote_id = footnote_id,
        sequence = row_number()
      )
    dbWriteTable(pool, "report_footnotes", reports_footnotes, append = TRUE)
    
  }
  
  ret_reports_footnotes <- dbGetQuery(pool, "SELECT id as report_footnote_id FROM report_footnotes")
  return(ret_reports_footnotes)
}

# Function to read and process Excel data
import_excel_data <- function(file_path, toc) {
  df <- read_excel(file_path, sheet = toc)
  colnames(df) <- c("dsmt", "hec", "dmc_safety", "dmc", "type", "category", 
                    "title", "population", "delivery_name", "notes", "unnamed1", "unamed2")
  df <- df %>% filter(!if_all(everything(), is.na))
  df$population <- gsub("[()]", "", df$population)
  return(df)
}



db_insert_reporting_efforts <- function(pool, reporting_efforts, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS reporting_efforts")
    mod_create_tables(pool)
    dbWriteTable(pool, "reporting_efforts", reporting_efforts, append = TRUE)
  }
  
  reporting_efforts <- dbGetQuery(pool, "SELECT id as reporting_effort_id, study, database_release, reporting_effort FROM reporting_efforts")
  return(reporting_efforts)

}

db_insert_reporting_effort_reports <- function(pool, reporting_effort_reports, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS reporting_effort_reports")
    re <- reporting_effort_reports %>%
      transmute(reporting_effort_id = reporting_effort_id,
                report_id = report_id, 
                report_type = report_type)
    
    mod_create_tables(pool)
    dbWriteTable(pool, "reporting_effort_reports", re, append = TRUE)
    
  }
  
  reporting_effort_reports <- dbGetQuery(pool, "SELECT * FROM reporting_effort_reports")
  return(reporting_effort_reports)
}

db_insert_report_programming_tracker <- function(pool, reporting_effort_reports, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS report_programming_tracker")
    re <- reporting_effort_reports %>%
      transmute(reporting_effort_id = reporting_effort_id,
                report_id = report_id, 
                report_type = report_type)
    mod_create_tables(pool)
    dbWriteTable(pool, "report_programming_tracker", re, append = TRUE)
  }
  
  report_programming_tracker <- dbGetQuery(pool, "SELECT * FROM report_programming_tracker")
  return(report_programming_tracker)
}

import_sdtm_data <- function(file_path, toc) {
  df <- read_excel(file_path, sheet = toc) %>% 
    transmute(
      dataset_type = "SDTM",
      category_name = Class,
      dataset_name = `Dataset Name`,
      dataset_label = `Dataset Label`
    )
  
  df <- df %>% filter(!if_all(everything(), is.na))
  return(df)
}


db_insert_sdtm_data <- function(pool, sdtm, drop = FALSE) {
  if (drop) {
    dbExecute(pool, "DROP TABLE IF EXISTS datasets")
    mod_create_tables(pool)
    dbWriteTable(pool, "datasets", sdtm, append = TRUE)
  }
  
  sdtm_data <- dbGetQuery(pool, "SELECT id as report_id, dataset_type, category_name, dataset_name, dataset_label  FROM datasets")
  return(sdtm_data)
}
# Main function
main <- function() {
  pool <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
  sdtm <- import_sdtm_data("data/SDTMIG_v3.4.xlsx", "Datasets")
 
  sdtm_id <- db_insert_sdtm_data(pool, sdtm, drop = TRUE)
  # Get list of all tables in pool
  tables <- dbListTables(pool)
  
  # Read and clean data and ensure all columns have name 
  
  df <- import_excel_data("data/MYF3001.xlsx", "TOC of MYF3001") %>% 
    select(-c(unnamed1, unamed2))
  
  # Ensure tables exist
  mod_create_tables(pool)
  
  # Process and insert data into respective tables
  categories <- db_insert_categories(pool, df, drop = TRUE)
  subcategories <- db_insert_subcategories(pool, categories, drop = TRUE)
  populations <- db_insert_populations(pool, df, drop = TRUE)
  titles <- db_insert_titles(pool, df, drop = TRUE)
  #footnotes <- db_insert_footnotes(pool, df, drop = TRUE)
  
  # Add IDs to the main dataframe
  df_with_ids <- df %>%
    left_join(categories, by = c("category" = "category_name")) %>%
    left_join(subcategories, by = "category_id") %>%
    left_join(populations, by = c("population" = "population_text"))  %>% 
    left_join(titles, by = c("title" = "title_text"))
  
  # Insert data into reports
  reports <- db_insert_reports(pool, df_with_ids, drop = TRUE)
  
  report_tnf_data <- df_with_ids %>%
    inner_join(reports, by = c("delivery_name" = "report_key"))
  
    db_insert_reports_titles(pool, report_tnf_data, drop = TRUE)
    
   # db_insert_reports_footnotes(pool, report_tnf_data, drop = TRUE)


reporting_efforts <- data.frame(
    study = c("MYF3001", "MYF3001", "MYF3001"),
    database_release = c("JAN2025", "FEB2025", "MAR2025"),
    reporting_effort = c("DMC", "DMC", "DMC")
  )
  reporting_efforts <- db_insert_reporting_efforts(pool, reporting_efforts, drop = TRUE) 
  
  # cartesian product of reporting_efforts and report_tnf_data
  reporting_effort_reports <- merge(reporting_efforts, report_tnf_data, by = NULL) 
  
  names(reporting_effort_reports)
   # For each reporting_efforts dataframe , insert the reports into the reporting_effort_reports table
  db_insert_reporting_effort_reports(pool, reporting_effort_reports, drop = TRUE)
  db_insert_report_programming_tracker(pool, reporting_effort_reports, drop = TRUE)
  # Close the database pool
  poolClose(pool)
}

# Call main function
main()



