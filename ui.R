library(shiny)
#library(dplyr)
library(shinydashboard)

header <- dashboardHeader(
  title = "MCYJ"
)

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Overview", tabName = "dashboard", icon = icon("chart-simple", lib = "font-awesome")),
  menuItem("Data", tabName = "data", icon = icon("database", lib = "font-awesome")),
  menuItem("Map", tabName = "map", icon = icon("map", lib = "font-awesome")),
  menuItem("About", tabName = "about", icon = icon("question", lib = "font-awesome"))
))

body <- dashboardBody(
  tabItems(
    # Overview dashboard tab content
    tabItem(tabName = "dashboard",
            h2("Overview Dashboard"),
            
            fluidRow(
              box(plotlyOutput("reportsByYear", height = 400)),
              box(plotlyOutput("violationsByYear", height = 400))
            ),
            fluidRow(
              box(plotlyOutput("reportsByFacility", height = 500, width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("violationsByFacility", height = 500, width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("proportionAllegations", height = 300)),
              box(plotlyOutput("numAllegationsSIR", height = 300)),
              box(plotlyOutput("SIRSwithOneViolation", height = 300))
            )
            
    ),
    
    # Data tab content
    tabItem(tabName = "data",
            h2("Data")
    ),
    
    # Map tab content
    tabItem(tabName = "map",
            h2("Map")
    ),
    
    # About tab content
    tabItem(tabName = "about",
            h2("About Us")
    )
    
  ) #closing tabItems
) #closing dashboardBody


dashboardPage(header, 
              sidebar, 
              body)

