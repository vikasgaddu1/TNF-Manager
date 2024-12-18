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

