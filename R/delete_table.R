  # dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
  # dbExecute(dbPoolCon, "DROP TABLE IF EXISTS milestones")
  # dbExecute(
  #   dbPoolCon,
  #   "CREATE TABLE IF NOT EXISTS comments (
  #   id INTEGER PRIMARY KEY AUTOINCREMENT,
  #   report_programming_tracker_id INTEGER NOT NULL,
  #   comment_user_id INTEGER NOT NULL,
  #   comment TEXT NOT NULL,
  #   addressed INTEGER DEFAULT 0,
  #   updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  #   FOREIGN KEY (report_programming_tracker_id) REFERENCES report_programming_tracker (id) ON DELETE CASCADE,
  #   FOREIGN KEY (comment_user_id) REFERENCES users (id) ON DELETE CASCADE
  # );"
  # )
  
# # Function to insert "Not Assigned" and restructure the table
#  insertNotAssignedTop <- function(dbPoolCon) {
#    # Copy existing data into a temporary dataset
#    temp_data <- dbGetQuery(dbPoolCon, "SELECT * FROM users;")
#    
#    # Ensure that the 'updated_at' column is character in temp_data
#    if ("updated_at" %in% names(temp_data)) {
#      temp_data$updated_at <- as.character(temp_data$updated_at)
#    }
#    
#    # Create the new row with a formatted datetime string
#    new_row <- data.frame(
#      id = 1,
#      username = "Not Assigned",
#      role = "user",
#      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#      stringsAsFactors = FALSE
#    )
#    
#    # Only add "Not Assigned" if it doesn't already exist
#    if (!("Not Assigned" %in% temp_data$username)) {
#      temp_data <- rbind(new_row, temp_data)
#    }
#    
#    # Reassign sequential IDs to avoid conflicts
#    temp_data$id <- seq_len(nrow(temp_data))
#    
#    # Drop the original table
#    dbExecute(dbPoolCon, "DROP TABLE IF EXISTS users;")
#    
#    # Recreate the `users` table
#    dbExecute(
#      dbPoolCon,
#      "CREATE TABLE users (
#       id INTEGER PRIMARY KEY AUTOINCREMENT,
#       username TEXT NOT NULL UNIQUE,
#       role TEXT CHECK (role IN ('admin', 'user')),
#       updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
#     );"
#    )
#    
#    # Insert data from the temporary dataset into the new table.
#    # Note: The default behavior of dbWriteTable is to infer column types.
#    dbWriteTable(dbPoolCon, "users", temp_data, append = TRUE, row.names = FALSE)
#    
#    message("Inserted 'Not Assigned' at the top (if not present) and restructured the table.")
#  }
#  
#  # Call the function
#  insertNotAssignedTop(dbPoolCon)
 
# Call the function
#insertNotAssignedTop(dbPoolCon)
#insert into user not assigned at id 0

#delete comments table
# dbExecute(dbPoolCon, "DROP TABLE categories;")
# dbExecute(dbPoolCon, "DROP TABLE sub_categories;")
# dbExecute(dbPoolCon, "DROP TABLE populations;")
#dbExecute(dbPoolCon, "DROP TABLE comments;")
#dbExecute(dbPoolCon, "DROP TABLE reporting_effort_reports;")
# # Step 1: Create a temporary table with the updated unique constraint
# dbExecute(
#   dbPoolCon,
#   "CREATE TABLE IF NOT EXISTS temp_reporting_effort_reports (
#     id INTEGER PRIMARY KEY AUTOINCREMENT,
#     reporting_effort_id INTEGER NOT NULL,
#     report_id INTEGER NOT NULL,
#     report_type TEXT NOT NULL,
#     updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
#     FOREIGN KEY (reporting_effort_id) REFERENCES reporting_efforts (id) ON DELETE CASCADE,
#     FOREIGN KEY (report_id) REFERENCES reports (id) ON DELETE CASCADE,
#     UNIQUE (reporting_effort_id, report_id, report_type)
#   );"
# )
# 
# # Step 2: Copy data from the old table to the new temporary table
# dbExecute(
#   dbPoolCon,
#   "INSERT INTO temp_reporting_effort_reports (reporting_effort_id, report_id, report_type, updated_at)
#    SELECT reporting_effort_id, report_id, report_type, updated_at FROM reporting_effort_reports;"
# )
# 
# # Step 3: Drop the old table
# dbExecute(dbPoolCon, "DROP TABLE reporting_effort_reports;")
# 
# # Step 4: Rename the temporary table to the original name
# dbExecute(dbPoolCon, "ALTER TABLE temp_reporting_effort_reports RENAME TO reporting_effort_reports;")

 # ds <- dbGetQuery(dbPoolCon, paste(
 #   "SELECT d.id, 
 #               d.dataset_name AS 'Dataset Name', 
 #               d.dataset_label AS 'Dataset Label', 
 #               d.dataset_type AS 'Dataset Type', 
 #               d.category_name AS 'Category', 
 #               CASE WHEN rer.reporting_effort_id IS NOT NULL THEN 1 ELSE 0 END AS Selected
 #               FROM datasets d
 #               LEFT JOIN reporting_effort_reports rer 
 #                   ON d.id = rer.report_id 
 #                   AND rer.reporting_effort_id = '1' 
 #   WHERE d.dataset_type = 'SDTM'
 #   GROUP BY d.id;"
 # ));
 # ds
# dbExecute(dbPoolCon, "
# DROP TABLE reporting_effort_reports;
# ")
# dbExecute(dbPoolCon, "
# DROP TABLE reporting_effort_datasets;
# ")

#  dbExecute(
#    dbPoolCon,
# "ALTER TABLE report_programming_tracker RENAME TO report_programming_tracker_old;")
# tables
# dbExecute(
#   dbPoolCon,
#   "CREATE TABLE report_programming_tracker (
#     id INTEGER PRIMARY KEY,
#     reporting_effort_id INTEGER NOT NULL,
#     report_type TEXT NOT NULL,
#     report_id INTEGER NOT NULL,
#     priority INTEGER,
#     production_programmer_id INTEGER,
#     assign_date DATE,
#     qc_programmer_id INTEGER,
#     due_date DATE,
#     status TEXT,
#     updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
#     FOREIGN KEY (reporting_effort_id) REFERENCES reporting_efforts (id) ON DELETE CASCADE,
#     FOREIGN KEY (production_programmer_id) REFERENCES users (id),
#     FOREIGN KEY (qc_programmer_id) REFERENCES users (id),
#     UNIQUE (reporting_effort_id, report_id, report_type)
# );")
# 
# dbExecute(
#   dbPoolCon,
#   "INSERT INTO report_programming_tracker
# SELECT * FROM report_programming_tracker_old;")
# 
# dbExecute(
#   dbPoolCon,
#   "DROP TABLE report_programming_tracker_old;")
# 
# 
#  dbExecute(
#    dbPoolCon,
#    "ALTER TABLE report_programming_tracker
# ADD CONSTRAINT unique_reporting_effort_report
# UNIQUE (reporting_effort_id, report_id, report_type);")
 
 # # Step 1: Add the new column with a default value
 # dbExecute(
 #   dbPoolCon,
 #   "ALTER TABLE report_programming_tracker
 #   ADD COLUMN report_type TEXT DEFAULT 'TFL';"
 # )
 # 
 # # Step 2: Update any existing rows (optional, as the default applies for new rows)
 # dbExecute(
 #   dbPoolCon,
 #   "UPDATE report_programming_tracker
 #   SET report_type = 'TFL'
 #   WHERE report_type IS NULL;"
 # )
 # dbExecute(dbPoolCon, "DROP TABLE dataset;")
# dbExecute(dbPoolCon, "DROP TABLE reporting_effort_reports;")
# data <- dbGetQuery(dbPoolCon, "select * from report_programming_tracker;")
# print(data)
# # Get list of tables in the database
# tables <- dbListTables(dbPoolCon)
# 
# if ("report_programming_tracker" %in% tables) {
#   dbExecute(dbPoolCon, "DROP TABLE report_programming_tracker;")
#   message("Table 'report_programming_tracker' deleted successfully.")
# } else {
#   message("Table 'report_programming_tracker' does not exist.")
# }

# Check if the table exists before attempting to delete it
# if ("report_programming_tracker" %in% tables) {
#   data <- dbGetQuery(dbPoolCon, "PRAGMA table_info(report_programming_tracker);")
#   print(data)
#   message("printed")
# } else {
#   message("Table 'report_programming_tracker' does not exist.")
# }

# dbExecute(dbPoolCon, "
#   CREATE TABLE IF NOT EXISTS reports_new  (
#     id INTEGER PRIMARY KEY AUTOINCREMENT,
#     report_type TEXT NOT NULL,
#     report_key TEXT UNIQUE NOT NULL,
#     title_key TEXT NOT NULL,
#     report_category_id INTEGER,
#     report_sub_category_id INTEGER,
#     report_ich_number TEXT,
#     population_id INTEGER,
#     updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
#     FOREIGN KEY (population_id) REFERENCES populations (id) ON DELETE CASCADE,
#     FOREIGN KEY (report_category_id) REFERENCES categories (id) ON DELETE CASCADE,
#     FOREIGN KEY (report_sub_category_id) REFERENCES sub_categories (id) ON DELETE CASCADE,
#     UNIQUE (report_key, report_type)
#   );
# ")
# 
# dbExecute(dbPoolCon, "
#   INSERT INTO reports_new (
#     id,
#     report_type,
#     report_key,
#     title_key,
#     report_category_id,
#     report_sub_category_id,
#     report_ich_number,
#     population_id,
#     updated_at
#   )
#   SELECT
#     id,
#     report_type,
#     report_key,
#     title_key,
#     report_category_id,
#     report_sub_category_id,
#     report_ich_number,
#     population_id,
#     updated_at
#   FROM reports;
# ")
# 
# dbExecute(dbPoolCon, "
# DROP TABLE reports;
# ")
# dbExecute(dbPoolCon, "
#   ALTER TABLE reports_new RENAME TO reports;
# ")
