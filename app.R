
# Source utility functions
source("R/createTables.R")
source("R/functions.R")

# load the libraries related to shiny
load_libraries(c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", 
                 "shinyalert", "shinybusy", "shinymaterial", "shinythemes",
                 "shinyBS"))

# load libraries related to database
load_libraries(c("RSQLite", "pool","DBI"))



# Create a pool of connections to the database using RSQLite
dbPoolCon <- dbPool(RSQLite::SQLite(), 
               dbname = "data/database.sqlite",
               create = TRUE)

# Call createTables.R to create the tables and triggers
createTables(dbPoolCon)

# Get list of tables in the database
tables <- dbListTables(dbPoolCon)

# Create a reactiveValues to store each table
rv <- reactiveValues()
for (table in tables) {
  rv[[table]] <- dbGetQuery(dbPoolCon, paste("SELECT * FROM", table))
}

# print values of table titcat stored in reactiveValues rv

