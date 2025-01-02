#  dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
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
