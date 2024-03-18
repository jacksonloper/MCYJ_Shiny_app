# library(shiny)
# library(shinydashboard)
# library(dplyr)
# library(plotly)
# library(DT)
# library(readxl)
library(pacman)

# p_load(tigris, leaflet, shinydashboard, shiny, plotly, DT, lubridate, bslib,
#        
#        shinyWidgets, readxl, openxlsx, tidyverse, update = F)

p_load(tigris, leaflet, flexdashboard, shinydashboard, shiny, plotly, DT, lubridate, bslib,
       
       shinyauthr, shinyWidgets, readxl, openxlsx, tidyverse, update = F)


########################
# read in data from excel sheet
########################
data_path = "DATA.xlsx"
info <- read_excel(data_path, "INFORMATION") |>
  
  mutate(
    
    `Program Type` = case_when(                                                 
      
      grepl("CHILD CARING INSTITUTION, GOVERNMENT", `Program Type`) ~ 
        
        "CHILD CARING INSTITUTION, GOVERNMENT - NON-FIA",
      
      grepl("CHILD CARING INSTITUTION, FIA", `Program Type`) ~ 
        
        "CHILD CARING INSTITUTION, FIA",
      
      grepl("CHILD CARING INSTITUTION, PRIVATE", `Program Type`) ~ 
        
        "CHILD CARING INSTITUTION, PRIVATE",
      
      grepl("CHILD PLACING AGENCY, FIA", `Program Type`) ~ 
        
        "CHILD PLACING AGENCY, FIA",
      
      grepl("CHILD PLACING AGENCY, PRIVATE", `Program Type`) ~ 
        
        "CHILD PLACING AGENCY, PRIVATE",
      
      grepl("CHILD PLACING AGENCY", `Program Type`) ~ 
        
        "CHILD PLACING AGENCY",
      
      grepl("CHILD THERAPEUTIC GROUP HOME", `Program Type`) ~ 
        
        "CHILD THERAPEUTIC GROUP HOME",
      
      grepl("COURT OPERATED RESIDENTIAL CARE, FACILITY", `Program Type`) ~ 
        
        "COURT OPERATED RESIDENTIAL CARE FACILITY",
      
      grepl("Court operated residential care facility", `Program Type`) ~ 
        
        "COURT OPERATED RESIDENTIAL CARE FACILITY",
      
      T ~ "OTHER"))    

violations <- read_excel(data_path, "ALLEGATIONS")                 

rules <- read_excel(data_path, "RULES")

zips <- tigris::zctas(starts_with = c('48','49'), cb = T, year = 2020)

dates <- range(info$`Final Report Date`)

#facs <- sort(unique(info$`Facility Name`))

#types <- sort(unique(info$`Program Type`))

########################
# read in helper functions containing code to make plots
########################
source("helpers.R")


########################
# UI
########################
# see ui.R


########################
# server logic
########################
server <- function(input, output) {
  
  ### Value box: Special Investigation Reports
  output$numReports <- renderValueBox({
    numReports(info)
  })
  
  ### Value box: Recent Reports from last 6 months
  output$recentReports <- renderValueBox({
    recentReports(info)
  })
  
  ### Value box: Allegations
  output$numAllegations <- renderValueBox({
    numAllegations(violations)
  })
  
  ### Value box: Violations Established
  output$numViolations <- renderValueBox({
    numViolations(violations)
  })

  ### Reports by Year
  output$reportsByYear <- renderPlotly({
    reportsByYear(info)
  })
  
  ### Violations by Year
  output$violationsByYear <- renderPlotly({
    violationsByYear(violations)
  })
  
  ### Reports by Facility
  output$reportsByFacility <- renderPlotly({
    reportsByFacility(info)
  })
  
  ### Violations by Facility
  output$violationsByFacility <- renderPlotly({
    violationsByFacility(violations)
  })
  
  ### Proportion of Allegations with Violation Established
  output$proportionAllegations <- renderPlotly({
    proportionAllegations(violations)
  })
  
  ### Number of Allegations per SIR
  output$numAllegationsSIR <- renderPlotly({
    numAllegationsSIR(violations)
  })
  
  ### SIRs with at least One Violation Established
  output$SIRSwithOneViolation <- renderPlotly({
    SIRSwithOneViolation(violations)
  })
  
  ### Explorable Data Table: Report Information
  output$reportInformationTable <- renderDataTable(reportInformationTable(info, input))
  
  ### Explorable Data Table: Alleged Violations
  output$allegedViolationsTable <- renderDataTable(allegedViolationsTable(violations, input))
  
  ### Explorable Data Table: Applicable Rules
  output$applicableRulesTable <- renderDataTable(applicableRulesTable(info, rules, input))
  
  # ### Download data button
  # output$downloadUI <- renderUI({
  #   downloadButton("downBtn", "Download the Data", style = "width:100%;")
  # })
  
  ### Download data
  output$downBtn <- downloadHandler(
    
    filename = function() { "DATA.xlsx" },
    
    content  = function(file) { 
      
      l <- list("INFORMATION" = info, "VIOLATIONS" = violations, "RULES" = rules)
      
      write.xlsx(l, file) }
  )
  

  ### Map: Number of Reports
  ouput$mapReports <- renderLeaflet(
    mapReports(violations, info, zips)
  )
  
  ### Map: Number of Allegations
  ouput$mapAllegations <- renderLeaflet(
    mapAllegations(violations, info, zips)
  )
  
  ### Map: Number of Violations
  output$mapViolations <- renderLeaflet(
    mapViolations(violations, info, zips)
  )

}


