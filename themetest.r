   library(shiny)
   library(shinydashboard)
   library(bslib)

   ui <- dashboardPage(
     dashboardHeader(title = "Test Dashboard"),
     dashboardSidebar(),
     dashboardBody(
       theme = bs_theme(
         version = 5,
         bootswatch = "cerulean"
       ),
       fluidRow(
         box(title = "Test Box", "Content")
       )
     )
   )

   server <- function(input, output, session) {}

   shinyApp(ui, server)