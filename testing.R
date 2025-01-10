library(tidyverse)
library(janitor)
# import xlsx from data folder and read it
data <- readxl::read_xlsx("data/tracker_data_2024-12-18.xlsx")

library(tidyverse)
library(janitor)
library(gtsummary)

result <- data %>%
  select(report_type, status) %>%
  mutate(status = case_when(
    status == "QC Pass" ~ "Done",
    TRUE ~ "In Progress"
  )) %>%
  group_by(report_type) %>%
  summarise(
    total = n(),
    done = sum(status == "Done"),
    percentage_done = (done / total) * 100
  )

library(tidyverse)
library(gtsummary)

data %>%
  select(production_programmer, qc_programmer) %>%
  mutate(
    production_programmer = ifelse(
      is.na(production_programmer),
      "Unassigned",
      production_programmer
    ),
    qc_programmer = ifelse(is.na(qc_programmer), "Unassigned", qc_programmer)
  ) %>%
  pivot_longer(
    cols = c(production_programmer, qc_programmer),
    names_to = "role",
    values_to = "Programmer"
  ) %>%
  mutate(
    role = case_when(
      role == "production_programmer" ~ "Production",
      role == "qc_programmer" ~ "QC"
    ),
    Programmer = factor(Programmer, 
                        levels = c(setdiff(unique(Programmer), "Unassigned"), "Unassigned"))
  ) %>%
  select(role, Programmer) %>%
  gtsummary::tbl_summary(
    by = role,                           # Grouping by role
    statistic = all_categorical() ~ "{n} ({p}%)",  # Count and percentages
    missing = "no"                       # Exclude missing values
  ) %>%
  add_overall() %>%                      # Adds a Total Column
  modify_header(label = "") %>%
  modify_caption("**Summary Table of Role and Programmer with Totals**") %>%
  bold_labels()

library(tidyverse)
library(janitor)
library(scales)  # For percentage formatting

# Data transformation
table_data <- data %>%
  select(production_programmer, qc_programmer) %>%
  mutate(
    production_programmer = ifelse(is.na(production_programmer), "Unassigned", production_programmer),
    qc_programmer = ifelse(is.na(qc_programmer), "Unassigned", qc_programmer)
  ) %>%
  pivot_longer(
    cols = c(production_programmer, qc_programmer),
    names_to = "role",
    values_to = "Programmer"
  ) %>%
  mutate(
    role = case_when(
      Programmer == "Unassigned" ~ "Unassigned",
      role == "production_programmer" ~ "Production",
      role == "qc_programmer" ~ "QC"
    )
  ) %>%
  group_by(Programmer, role) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = role,
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  adorn_totals("row") %>%
  adorn_totals("col")

# Store denominators
production_total <- table_data$Production[nrow(table_data)]
qc_total <- table_data$QC[nrow(table_data)]
overall_total <- table_data$Total[nrow(table_data)]

# Calculate percentages and format as n (%)
table_data <- table_data %>%
  mutate(
    Production = ifelse(Programmer == "Total", Production,
                        paste0(Production, " (", percent(Production / production_total, accuracy = 0.1), ")")),
    QC = ifelse(Programmer == "Total", QC,
                paste0(QC, " (", percent(QC / qc_total, accuracy = 0.1), ")")),
    Total = ifelse(Programmer == "Total", Total,
                   paste0(Total, " (", percent(Total / overall_total, accuracy = 0.1), ")"))
  )

# Print the table
table_data

# Function to modify datasets table structure
modify_datasets_table <- function(pool) {
  tryCatch({
    poolWithTransaction(pool, function(conn) {
      # Create temporary table
      dbExecute(conn, "CREATE TEMP TABLE datasets_temp AS SELECT * FROM datasets;")
      
      # Drop original table
      dbExecute(conn, "DROP TABLE datasets;")
      
      # Recreate table without UNIQUE constraint
      dbExecute(
        conn,
        "CREATE TABLE datasets (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          dataset_type TEXT NOT NULL,
          category_name TEXT NOT NULL,
          dataset_name TEXT NOT NULL,
          dataset_label TEXT NOT NULL,
          updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );"
      )
      
      # Copy data back
      dbExecute(conn, "INSERT INTO datasets (id, dataset_type, category_name, dataset_name, dataset_label, updated_at) 
                      SELECT id, dataset_type, category_name, dataset_name, dataset_label, updated_at 
                      FROM datasets_temp;")
      
      # Drop temporary table
      dbExecute(conn, "DROP TABLE datasets_temp;")
      
      # Create trigger for updated_at
      dbExecute(
        conn,
        "CREATE TRIGGER IF NOT EXISTS update_datasets_updated_at 
         AFTER UPDATE ON datasets
         FOR EACH ROW
         BEGIN
           UPDATE datasets 
           SET updated_at = DATETIME('now', 'localtime')
           WHERE id = NEW.id;
         END;"
      )
    })
    
    message("Successfully modified datasets table structure")
  }, error = function(e) {
    message("Error modifying datasets table: ", e$message)
  })
}
pool <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# modify_datasets_table(pool)

