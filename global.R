#load in libraries
library(pacman)

p_load(tigris, leaflet, flexdashboard, shinydashboard, shiny, plotly, DT, lubridate, bslib,
       
       shinyauthr, shinyWidgets, readxl, openxlsx, tidyverse, dplyr, update = F)


########################
# read in data from excel sheet
########################
data_path = "DATA.xlsx"

readData <- function(data_path) {
  data <- list(info = read_excel(data_path, "INFORMATION") |>
                 
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
                     
                     T ~ "OTHER")) ,
               violations = read_excel(data_path, "ALLEGATIONS") ,
               
               rules = read_excel(data_path, "RULES")
  )
  return(data)
}

data <- readData(data_path)

zips <- tigris::zctas(starts_with = c('48','49'), cb = T, year = 2020)

dates<- range(data$info$`Final Report Date`)

#facs <- sort(unique(info$`Facility Name`))

#types <- sort(unique(info$`Program Type`))

########################
# read in helper functions containing code to make plots
########################
source("helpers.R")
