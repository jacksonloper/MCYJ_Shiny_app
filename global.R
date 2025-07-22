#load in libraries
#library(pacman)

#p_load(tigris, leaflet, flexdashboard, shinydashboard, shiny, plotly, DT, lubridate, bslib,
#       shinyauthr, shinyWidgets, readxl, openxlsx, tidyverse, dplyr, sf, zipcodeR,
#       viridis, update = F)
library(tigris)
library(leaflet)
library(flexdashboard)
library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(lubridate)
library(bslib)
library(shinyauthr)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(sf)
#library(zipcodeR)
library(viridis)


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

#zips_2020 <- st_read("cb_2020_us_zcta520_500k/cb_2020_us_zcta520_500k.shp")
#zips <- subset(zips_2020, grepl("^48|^49", ZCTA5CE20))
zips <- tigris::zctas(starts_with = c('48','49'), cb = T, year = 2020)
zip_centroids <- zips %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lng = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2],
    zip = ZCTA5CE20
  ) %>%
  select(zip, lat, lng)

dates <- Sys.Date()

#facs <- sort(unique(info$`Facility Name`))

#types <- sort(unique(info$`Program Type`))

########################
# read in helper functions containing code to make plots
########################
source("helpers.R")
