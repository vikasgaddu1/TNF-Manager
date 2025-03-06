createTables <- function(pool) {
  
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS datasets (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      dataset_type TEXT NOT NULL,
      category_name TEXT NOT NULL,
      dataset_name TEXT NOT NULL,
      dataset_label TEXT NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      UNIQUE (dataset_type, dataset_name)
    );"
  )
  
 
  # Categories table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS categories (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      category_name TEXT UNIQUE NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP    
    );"
  )
  

  # Sub_Categories table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS sub_categories (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      category_id INTEGER NOT NULL,
      sub_category_name TEXT NOT NULL,
      suggested_ich_number TEXT NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE CASCADE,
      UNIQUE (category_id, sub_category_name)
    );"
  )
  
  # Titles table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS titles (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      title_text TEXT UNIQUE NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP    
    );"
  )
  
  # Footnotes table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS footnotes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      footnote_text TEXT UNIQUE NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP    
    );"
  )
  
  # Populations table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS populations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      population_text TEXT UNIQUE NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP    
    );"
  )
  
  # Reports table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_type TEXT NOT NULL,
      report_key TEXT UNIQUE NOT NULL,
      title_key TEXT NOT NULL,
      report_category_id INTEGER ,
      report_sub_category_id INTEGER ,
      report_ich_number TEXT ,
      population_id INTEGER ,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,  
      FOREIGN KEY (population_id) REFERENCES populations (id) ON DELETE CASCADE,
      FOREIGN KEY (report_category_id) REFERENCES categories (id) ON DELETE CASCADE,
      FOREIGN KEY (report_sub_category_id) REFERENCES sub_categories (id) ON DELETE CASCADE,
      UNIQUE (report_key, report_type)
    );"
    )

  # Report_Titles association table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS report_titles (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_id INTEGER NOT NULL,
      title_id INTEGER NOT NULL,
      sequence INTEGER NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,  
      FOREIGN KEY (report_id) REFERENCES reports (id) ON DELETE CASCADE,
      FOREIGN KEY (title_id) REFERENCES titles (id) ON DELETE CASCADE,
      UNIQUE (report_id, title_id)
    );"
  )
  
  # Report_Footnotes association table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS report_footnotes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_id INTEGER NOT NULL,
      footnote_id INTEGER NOT NULL,
      sequence INTEGER NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,  
      FOREIGN KEY (report_id) REFERENCES reports (id) ON DELETE CASCADE,
      FOREIGN KEY (footnote_id) REFERENCES footnotes (id) ON DELETE CASCADE,
      UNIQUE (report_id, footnote_id)
    );"
  )
  
  # Reporting_Efforts table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS reporting_efforts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      study TEXT NOT NULL,
      database_release TEXT NOT NULL,
      reporting_effort TEXT NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP   
    );"
  )

  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS reporting_effort_reports (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      reporting_effort_id INTEGER NOT NULL,
      report_id INTEGER NOT NULL,
      report_type TEXT NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,  
      FOREIGN KEY (reporting_effort_id) REFERENCES reporting_efforts (id) ON DELETE CASCADE,
      FOREIGN KEY (report_id) REFERENCES reports (id) ON DELETE CASCADE,
      UNIQUE (reporting_effort_id, report_id,report_type)
  );"
  )

  # Users table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT NOT NULL UNIQUE,
      role TEXT CHECK (role IN ('Admin', 'Editor')),
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP   
    );"
  )
  
  # Report_Programming_Tracker table
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS report_programming_tracker (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      reporting_effort_id INTEGER NOT NULL,
      report_type TEXT NOT NULL,
      report_id INTEGER NOT NULL,
      priority INTEGER,
      production_programmer_id INTEGER,
      assign_date DATE,
      qc_level INTEGER,
      qc_programmer_id INTEGER,
      due_date DATE,
      status TEXT,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (reporting_effort_id) REFERENCES reporting_efforts (id) ON DELETE CASCADE,
      FOREIGN KEY (production_programmer_id) REFERENCES users (id),
      FOREIGN KEY (qc_programmer_id) REFERENCES users (id),
      UNIQUE (reporting_effort_id, report_id, report_type)
    );"
  )

  # create table comments
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    report_programming_tracker_id INTEGER NOT NULL,
    comment_user_id INTEGER NOT NULL,
    comment TEXT NOT NULL,
    addressed INTEGER DEFAULT 0,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (report_programming_tracker_id) REFERENCES report_programming_tracker (id) ON DELETE CASCADE,
    FOREIGN KEY (comment_user_id) REFERENCES users (id) ON DELETE CASCADE
  );"
  )
  

    # create table custom_footnotes
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS custom_footnotes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_programming_tracker_id INTEGER NOT NULL,
      footnote_id  INTEGER NOT NULL,
      sequence INTEGER NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (report_programming_tracker_id) REFERENCES report_programming_tracker (id) ON DELETE CASCADE,
      FOREIGN KEY (footnote_id) REFERENCES footnotes (id) ON DELETE CASCADE
    );"
  )

      # create table custom_populations
  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS custom_populations (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      report_programming_tracker_id INTEGER NOT NULL,
      population_id INTEGER NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (report_programming_tracker_id) REFERENCES report_programming_tracker (id) ON DELETE CASCADE,
      FOREIGN KEY (population_id) REFERENCES populations (id) ON DELETE CASCADE
    );"
  )

  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS milestones (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      Date TEXT,
      Milestone TEXT,
      Assigned_To TEXT,
      reporting_effort_id INTEGER NOT NULL,
      Position INTEGER,
      Status TEXT,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
  )"
  )

  dbExecute(
    pool,
    "CREATE TABLE IF NOT EXISTS decision_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      reporting_effort_id INTEGER NOT NULL,
      category TEXT NOT NULL,
      question TEXT NOT NULL,
      answer_rationale TEXT NOT NULL,
      status TEXT NOT NULL,
      confidence TEXT NOT NULL,
      owners TEXT NOT NULL,
      comments TEXT,
      downstream_impact TEXT,
      date_resolved DATE,
      links TEXT,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (reporting_effort_id) REFERENCES reporting_efforts (id) ON DELETE CASCADE
    )"
  )

  # Create indexes
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_report_category_id ON reports (report_category_id);")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_report_sub_category_id ON reports (report_sub_category_id);")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_reports_population_id ON reports (population_id);")
  dbExecute(pool, "CREATE INDEX IF NOT EXISTS idx_report_programming_tracker_report_id ON report_programming_tracker (report_id);")
  
#   # Create triggers for updated_at
#   tables <- dbListTables(pool)
#   #drop sqlite_sequence from tables
#   tables <- tables[tables != "sqlite_sequence"]
  
#   for (table in tables) {
#     dbExecute(
#       pool,
#       sprintf(
#         "CREATE TRIGGER IF NOT EXISTS update_%s_updated_at 
#          AFTER UPDATE ON %s
#          FOR EACH ROW
#          BEGIN
#            UPDATE %s 
#            SET updated_at = DATETIME('now', 'localtime')
#            WHERE id = NEW.id;
#          END;",
#         table, table, table
#       )
#}
}