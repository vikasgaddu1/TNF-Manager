source("R/createTables.r")
# export database to xlsx
export_database <- function(pool) {
  library(openxlsx)
  # drop updated_at column from xlsx
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM categories") %>% select(-updated_at),
    "data/excel_export/categories.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM sub_categories") %>% select(-updated_at),
    "data/excel_export/sub_categories.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM populations") %>% select(-updated_at),
    "data/excel_export/populations.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM titles") %>% select(-updated_at),
    "data/excel_export/titles.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM reports") %>% select(-updated_at),
    "data/excel_export/reports.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM users") %>% select(-updated_at),
    "data/excel_export/users"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM datasets") %>% select(-updated_at),
    "data/excel_export/datasets.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM report_titles") %>% select(-updated_at),
    "data/excel_export/report_titles.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM report_footnotes") %>% select(-updated_at),
    "data/excel_export/report_footnotes.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM reporting_efforts") %>% select(-updated_at),
    "data/excel_export/reporting_efforts.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM reporting_effort_reports") %>% select(-updated_at),
    "data/excel_export/reporting_effort_reports.xlsx"
  )
  write.xlsx(
    dbGetQuery(pool, "SELECT * FROM report_programming_tracker") %>% select(-updated_at),
    "data/excel_export/report_programming_tracker.xlsx"
  )
  
  # delete database
  delete_database(pool)
}

# import database from xlsx
import_database <- function(pool) {
  createTables(pool)
  library(readxl)
  df <- read_excel("data/excel_export/categories.xlsx")
  dbWriteTable(pool, "categories", df, append = TRUE)
  
  df <- read_excel("data/excel_export/sub_categories.xlsx")
  dbWriteTable(pool, "sub_categories", df, append = TRUE)
  
  df <- read_excel("data/excel_export/populations.xlsx")
  dbWriteTable(pool, "populations", df, append = TRUE)
  
  df <- read_excel("data/excel_export/titles.xlsx")
  dbWriteTable(pool, "titles", df, append = TRUE)
  
  df <- read_excel("data/excel_export/reports.xlsx")
  dbWriteTable(pool, "reports", df, append = TRUE)
  
  df <- read_excel("data/excel_export/users.xlsx")
  dbWriteTable(pool, "users", df, append = TRUE)
  
  df <- read_excel("data/excel_export/datasets.xlsx")
  dbWriteTable(pool, "datasets", df, append = TRUE)
  
  df <- read_excel("data/excel_export/report_titles.xlsx")
  dbWriteTable(pool, "report_titles", df, append = TRUE)
  
  df <- read_excel("data/excel_export/report_footnotes.xlsx")
  dbWriteTable(pool, "report_footnotes", df, append = TRUE)
  
  df <- read_excel("data/excel_export/reporting_efforts.xlsx")
  dbWriteTable(pool, "reporting_efforts", df, append = TRUE)
  
  df <- read_excel("data/excel_export/reporting_effort_reports.xlsx")
  dbWriteTable(pool, "reporting_effort_reports", df, append = TRUE)
  
  df <- read_excel("data/excel_export/report_programming_tracker.xlsx")
  dbWriteTable(pool, "report_programming_tracker", df, append = TRUE)
}

check_tables <- function(pool) {
  tables <- dbListTables(pool)
  for(table in tables) {
    print(paste0("Tables: ",table))
    # get column names
    print("Columns")
    print(dbListFields(pool, table))
    print("")
  }
}
library(DBI)
library(RSQLite)
library(pool)
pool <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
check_tables(pool)
import_database(pool)
#export_database(pool)
