# Function to Create Tables If They Don't Exist
createTables <- function(pool) {
  # Ensure the table exists and populate it if necessary
  if (!dbExistsTable(pool, "titcat")) {
    dbExecute(pool, "
    CREATE TABLE titcat (
    ID INTEGER PRIMARY KEY AUTOINCREMENT,
    Category TEXT NOT NULL,
    Subcategory TEXT,
    tfltype TEXT NOT NULL,
    tflnum TEXT NOT NULL,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
    
    # Insert initial data
    dbExecute(pool, "
    INSERT INTO titcat (Category, Subcategory, tfltype, tflnum) VALUES
    ('DISPOSITION', '', 'Listing', '16.2.1.1.x'),
    ('DISPOSITION', '', 'Table', '15.2.1.1.x'),
    ('DISPOSITION', '', 'Figure', '14.2.1.1.x'),
  ")
  }
  
  # Ensure the titles table exists
  if (!dbExistsTable(pool, "titles")) {
    dbExecute(pool, "
    CREATE TABLE titles (
      ID INTEGER PRIMARY KEY AUTOINCREMENT,
      titcat_id INTEGER NOT NULL,
      Text TEXT UNIQUE NOT NULL,
      tlfnum_user_input TEXT NOT NULL,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(titcat_id) REFERENCES titcat(ID)
    )
  ")
  }
  
  # Populations table
  if (!dbExistsTable(pool, "populations")) {
    dbExecute(
      pool,
      "CREATE TABLE populations (
        ID INTEGER PRIMARY KEY AUTOINCREMENT, 
        Text TEXT UNIQUE NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )"
    )
  }
  
  # Footnotes table
  if (!dbExistsTable(pool, "footnotes")) {
    dbExecute(
      pool,
      "CREATE TABLE footnotes (
        ID INTEGER PRIMARY KEY AUTOINCREMENT, 
        Text TEXT UNIQUE NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )"
    )
  }
  
  # Reports table
  if (!dbExistsTable(pool, "reports")) {
    dbExecute(
      pool,
      "CREATE TABLE reports (
        ID INTEGER PRIMARY KEY AUTOINCREMENT,
        titlekey TEXT UNIQUE NOT NULL,
        tfl_type TEXT NOT NULL,
        tfl_number TEXT NOT NULL,
        population_id INTEGER NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (population_id) REFERENCES populations(ID)
      )"
    )
  }
  
  # Reports_Titles association table
  if (!dbExistsTable(pool, "reports_titles")) {
    dbExecute(
      pool,
      "CREATE TABLE reports_titles (
        report_id INTEGER NOT NULL,
        title_id INTEGER NOT NULL,
        sequence INTEGER NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (report_id) REFERENCES reports(ID),
        FOREIGN KEY (title_id) REFERENCES titles(ID),
        PRIMARY KEY (report_id, title_id)
      )"
    )
  }
  
  # Reports_Footnotes association table
  if (!dbExistsTable(pool, "reports_footnotes")) {
    dbExecute(
      pool,
      "CREATE TABLE reports_footnotes (
        report_id INTEGER NOT NULL,
        footnote_id INTEGER NOT NULL,
        sequence INTEGER NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (report_id) REFERENCES reports(ID),
        FOREIGN KEY (footnote_id) REFERENCES footnotes(ID),
        PRIMARY KEY (report_id, footnote_id)
      )"
    )
  }
  
  # Reporting Efforts table
  if (!dbExistsTable(pool, "reporting_efforts")) {
    dbExecute(
      pool,
      "CREATE TABLE reporting_efforts (
        ID INTEGER PRIMARY KEY AUTOINCREMENT,
        Study TEXT NOT NULL,
        DatabaseRelease TEXT NOT NULL,
        ReportingEffort TEXT NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )"
    )
  }
  
  # Association table between reporting efforts and reports
  if (!dbExistsTable(pool, "reporting_efforts_reports")) {
    dbExecute(
      pool,
      "CREATE TABLE reporting_efforts_reports (
        ReportingEffortID INTEGER NOT NULL,
        ReportID INTEGER NOT NULL,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (ReportingEffortID, ReportID),
        FOREIGN KEY (ReportingEffortID) REFERENCES reporting_efforts(ID),
        FOREIGN KEY (ReportID) REFERENCES reports(ID)
      )"
    )
  }
  
  # Refresh the triggers list
  triggers <- dbGetQuery(pool, "SELECT name FROM sqlite_master WHERE type = 'trigger'")
  
  # Create triggers for tables without updated_at triggers
  
  # Titles Categories trigger
  if (!"update_titcat_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_titcat_updated_at
      AFTER UPDATE ON titcat
      FOR EACH ROW
      BEGIN
        UPDATE titcat SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  
  # Titles trigger
  if (!"update_titles_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_titles_updated_at
      AFTER UPDATE ON titles
      FOR EACH ROW
      BEGIN
        UPDATE titles SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  # Populations trigger
  if (!"update_populations_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_populations_updated_at
      AFTER UPDATE ON populations
      FOR EACH ROW
      BEGIN
        UPDATE populations SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  # Footnotes trigger
  if (!"update_footnotes_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_footnotes_updated_at
      AFTER UPDATE ON footnotes
      FOR EACH ROW
      BEGIN
        UPDATE footnotes SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  # Reports trigger
  if (!"update_reports_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_reports_updated_at
      AFTER UPDATE ON reports
      FOR EACH ROW
      BEGIN
        UPDATE reports SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  # Reports_Titles trigger
  if (!"update_reports_titles_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_reports_titles_updated_at
      AFTER UPDATE ON reports_titles
      FOR EACH ROW
      BEGIN
        UPDATE reports_titles SET updated_at = CURRENT_TIMESTAMP WHERE report_id = NEW.report_id AND title_id = NEW.title_id;
      END;")
  }
  
  # Reports_Footnotes trigger
  if (!"update_reports_footnotes_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_reports_footnotes_updated_at
      AFTER UPDATE ON reports_footnotes
      FOR EACH ROW
      BEGIN
        UPDATE reports_footnotes SET updated_at = CURRENT_TIMESTAMP WHERE report_id = NEW.report_id AND footnote_id = NEW.footnote_id;
      END;")
  }
  
  # Reporting Efforts trigger
  if (!"update_reporting_efforts_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_reporting_efforts_updated_at
      AFTER UPDATE ON reporting_efforts
      FOR EACH ROW
      BEGIN
        UPDATE reporting_efforts SET updated_at = CURRENT_TIMESTAMP WHERE ID = NEW.ID;
      END;")
  }
  
  # Reporting Efforts Reports trigger
  if (!"update_reporting_efforts_reports_updated_at" %in% triggers$name) {
    dbExecute(
      pool, "CREATE TRIGGER update_reporting_efforts_reports_updated_at
      AFTER UPDATE ON reporting_efforts_reports
      FOR EACH ROW
      BEGIN
        UPDATE reporting_efforts_reports SET updated_at = CURRENT_TIMESTAMP WHERE ReportingEffortID = NEW.ReportingEffortID AND ReportID = NEW.ReportID;
      END;")
  }
}