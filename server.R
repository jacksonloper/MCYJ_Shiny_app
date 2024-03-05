library(shiny)
#library(dplyr)
library(shinydashboard)
library(plotly)


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

#currently not working for me, but eventually will be needed for map plots
#potentially might be resolved when data is moved off of google drive?
# zips <- zctas(starts_with = c('48','49'), cb = T, year = 2020)

dates <- range(info$`Final Report Date`)


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
  
  
}


