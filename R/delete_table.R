# dbPoolCon <- dbPool(RSQLite::SQLite(), dbname = "data/database.sqlite", create = TRUE)
# dbExecute(dbPoolCon, "DROP TABLE report_programming_tracker;")
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
