#helper functions for creating plots in the dashboard
library(dplyr)
library(plotly)
library(DT)


### Value box: Special Investigation Reports
numReports <- function(info) {

  nReports <- reactive({
    
    nrow(info)
  })
  
    
  count <- formatC(nReports(), format = "f", big.mark = ",", digits = 0)
    
  return(valueBox(
    value = count,
    icon  = icon("fas fa-file"),
    subtitle = "Special Investigation Reports",
    color = "blue"
  ))

}

### Value box: Recent Reports from last 6 months
recentReports <- function(info) {
  n6Months <- reactive({
    
    info |> 
      
      filter(
        
        difftime(dates[2], `Investigation Initiation Date`) / dmonths(1) <= 6) |>
      
      count() |> pull(n)
  })
  

  count <- formatC(n6Months(), format = "f", big.mark = ",", digits = 0)
  
  return(valueBox(
    
    value = count,
    icon  = icon("fas fa-clock"),
    subtitle = "Recent Reports < 6 months old",
    color = "purple"
  ))
}

### Value box: Allegations
numAllegations <- function(violations) {
  nAllegations <- reactive({
    
    violations |>
      
      count() |>
      
      pull(n)
  })
  
  count <- formatC(nAllegations(), format = "f", big.mark = ",", digits = 0)
  
  return(valueBox(
    
    value = count,
    icon  = icon("fas fa-users"),
    subtitle = "Allegations",
    color = "red"
  ))
}


### Value box: Violations Established
numViolations <- function(violations) {
  nViolations <- reactive({
    
    violations |>
      
      filter(`Violation Established` == "Yes") |>
      
      count() |>
      
      pull(n)
  })
  
  count <- formatC(nViolations(), format = "f", big.mark = ",", digits = 0)
  
  return(valueBox(
    
    value = count,
    icon  = icon("fas fa-users"),
    subtitle = "Violations Established",
    color = "green"
  ))
}


### Reports by Year
reportsByYear <- function(info) {
  df <- info |>                                                                  
    
    mutate(year = year(`Final Report Date`)) |> 
    
    select(year)
    
  p <- ggplot(df, aes(year)) +
    
    geom_bar(fill = '#489CC9') +
    
    labs(title = '\nNumber of Special Investigation Reports by Year',
         x = 'Year',
         y = '')+
    
    theme_minimal() +
    
    coord_flip()
  
  return(ggplotly(p))
}


### Violations by Year
violationsByYear <- function(violations, info) {
  df <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
      
      "File Name") |>
    
    select(`Facility Name`, `Program Type`, `Final Report Date`, everything()) |>
    
    mutate(year = year(`Final Report Date`)) |>
    
    filter(`Violation Established` %in% c("Yes", "No")) |>
    
    group_by(year, `Violation Established`) |>
    
    count()

  p <- ggplot(df, aes(year, n, fill = `Violation Established`)) +
    
    geom_bar(position="stack", stat="identity") + 
    
    theme_minimal() +
    
    theme(legend.position = 'bottom') +
    
    scale_fill_manual(values = c('No' = '#469d6d', 'Yes' = '#fc7530')) +
    
    labs(title = '\nNumber of Alleged Violations by Year',
         x = 'Year',
         y = '') +
  
    coord_flip()
  
  
     return(ggplotly(p) %>% layout(legend = list(orientation = "h")))
}
  

### Reports by Facility
reportsByFacility <- function(info) {
  df <- info |>
    
    group_by(`Facility Name`) |>
    
    count() |> 
    
    arrange(desc(n)) |>
    
    head(10)
  
  p <- ggplot(df, aes(x = reorder(`Facility Name`, n), y = n)) +
    
    geom_bar(stat = 'identity', fill = '#5c3464') +
    
    theme_minimal() + 
    
    theme() + 
    
    labs(title = '\nTop 10 Facilities with Most Special Investigation Reports',
         x = '',
         y = 'Number of Special Investigation Reports') +
    
    coord_flip()
    
  return(ggplotly(p))
}


### Violations by Facility
violationsByFacility <- function(violations, info) {
  df <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`,
             
             `Capacity`),
      
      "File Name") |>
    
    select(`Facility Name`, `Violation Established`, `Capacity`) |>
    
    filter(`Violation Established` %in% c("Yes", "No")) |>
    
    group_by(`Facility Name`, `Violation Established`) |>
    
    summarize(rate = n() / mean(Capacity, na.rm = T)) |>
    
    ungroup() |>
    
    filter(rate != 'NaN') |>
    
    group_by(`Facility Name`) |>
    
    mutate(rate = round(rate, 2), 
           rate_t = sum(rate)) |>
    
    arrange(desc(rate_t)) 
  
  p <- ggplot(df |> filter(`Facility Name` %in% unique(df$`Facility Name`)[1:10]), 
              aes(x = reorder(`Facility Name`, rate_t), y = rate, 
                  fill = `Violation Established`, 
                  text = paste0(`Facility Name`,'\n',
                               ifelse(`Violation Established` == 'Yes', 
                                      'Established Violation Rate:', 
                                      'Not Established Violation Rate:'), rate, sep = ''))) + 
    
    geom_bar(position="stack", stat="identity") + 
    
    theme_minimal() +
    
    theme(legend.position = 'bottom') +
    
    labs(title = '\nTop 10 Facilities with Highest Rates of Alleged Violations',
         x = '',
         y = '') +
    
    scale_fill_manual(values = c('No' = '#469d6d', 'Yes' = '#fc7530')) +
    
    coord_flip()
  
  
return(ggplotly(p, tooltip = 'text') %>% 
         layout(legend = list(orientation = "h"), 
                margin = list(l = 50, r = 50, b = 100, t = 50),
                annotations = list(x = 1, y = -0.2, text = "Rate = Number of Alleged Violations / Facilitiy Capacity",
                                   xref='paper', yref='paper', showarrow = F, 
                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                   font = list(size = 14))))
}


### Proportion of Allegations with Violation Established
proportionAllegations <- function(violations, info) {
  df <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
      
      "File Name") |>
    
    select(`Facility Name`, `Program Type`, `Final Report Date`, everything()) |>
    
    mutate(
      
      `Violation Established` = if_else(
        
        `Violation Established` == "Yes", "Yes", "No", "No"), 
      
      year = year(`Final Report Date`)) |>
    
    group_by(`Violation Established`, year) |> count() |>
    
    group_by(year) |> mutate(proportion = round(n/sum(n), 2))
  
  p <- ggplot(df, aes(x = year, y = proportion, fill = `Violation Established`)) +
    
    geom_area() +
    
    geom_point(aes(text = paste(proportion*100, ifelse(`Violation Established` == 'Yes',
                                                       '% of allegations ended up with establised violation in ',
                                                       '% of allegations ended up without establised violation in '), year, sep = '')), 
               position="stack") +
    
    theme_minimal() + 
    
    theme(legend.position = 'bottom') +
    
    scale_fill_manual(values = c('No' = '#469d6d', 'Yes' = '#fc7530')) +
    
    labs(title = "\nProportion of Allegations with Violation Established by Year",
         x = "",
         y = "")
  
  return(ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h")))
}


### Number of Allegations per SIR
numAllegationsSIR <- function(violations) {
  df <- violations |>
    
    group_by(`File Name`) |> count() |>
    
    rename(n_allegations = n) |>
    
    group_by(n_allegations) |> count() 
  
  p <- ggplot(df, aes(x = n_allegations, y = n)) +
    
    geom_bar(stat = 'identity', fill = '#93b4cd',
             aes(text = paste('There are ',n, ' SIRs yielded in ',n_allegations, ' allegations per SIR', sep = ''))) +
    
    theme_minimal() + 
    
    theme() + 
    
    labs(title = '\nNumber of Allegations per Special Investigation Reports (SIR)',
         x = 'Number of Allegations per SIR',
         y = 'Number of SIR') #+
    
    #coord_flip()
  
  ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h"))
  
  return(ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h")))
}

### SIRs with at least One Violation Established
SIRSwithOneViolation <- function(violations, info) {
  df <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
      
      "File Name") |>
    
    select(`Facility Name`, `Program Type`, `Final Report Date`, everything()) |>
    
    mutate(
      
      `Violation Established` = if_else(
        
        `Violation Established` == "Yes", T, F, F),
      
      year = year(`Final Report Date`)) |>
    
    group_by(`File Name`, year) |>
    
    summarise(Violation = any(`Violation Established`)) |>
    
    ungroup()|>
    
    group_by(Violation, year) |> count() |>
    
    mutate(Violation = if_else(
      
      Violation, "At Least 1 Violation", "No Violations")) |>
    
    group_by(year) |> mutate(proportion = round(n/sum(n), 2))
  
  
  p <- ggplot(df %>% left_join(data.frame(year = rep(2017:max(df$year), each =2), 
                                          Violation = rep(unique(df$Violation), max(df$year)-2016),
                                          n = rep(0, (max(df$year)-2016)*2), 
                                          proportion = rep(0, (max(df$year)-2016)*2)), .,
                               by = c('year', 'Violation')) %>% 
                mutate(n.y =ifelse(is.na(n.y), n.x, n.y),
                       proportion.y =ifelse(is.na(proportion.y), proportion.x, proportion.y), 
                       Violation = factor(Violation, levels = c('No Violations', 'At Least 1 Violation'))), 
              aes(x = year, y = proportion.y, fill = Violation)) +
    
    geom_area() +
    
    geom_point(aes(text = paste(proportion.y*100, ifelse(Violation == 'No Violations',
                                                         '% of SIRs ended up with no violations in ',
                                                         '% of SIRs ended up with at least 1 violations in '), year, sep = '')), 
               position="stack") +
    
    theme_minimal() + 
    
    theme(legend.position = 'bottom') +
    
    scale_fill_manual(values = c('At Least 1 Violation' = '#a885a8', 'No Violations' = '#93b4cd')) +
    
    labs(title = "\nProportion of SIRs with at least One Violation Established",
         x = "",
         y = "")
  
  
  return(ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h")))
}

### Create Explorable Data Table
### Report Information
reportInformationTable <- function(info, input) {
  data <- info |> 
    
    select(`Facility Name`, `Program Type`, `Final Report Date`, 
           
           everything(), -`File Name`, -Investigation, -Recommendation) |>
    
    filter(
      
      `Facility Name` %in% input$Facility,
      
      `Program Type` %in% input$Program,
      
      `Final Report Date` %within%
        
        interval(ymd(input$Dates[1]), ymd(input$Dates[2]))) |>
    
    rename('License Expiration Date' = 'Expiration Date',                        # Move this to data cleaning code
           'License Effective Date' = 'Effective Date')
  
  return(datatable(data, filter = 'top', 
            
            options = list(pageLength = 5, autoWidth = T, paging = F), fillContainer = T))
  
}

### Alleged Violations Data Table
allegedViolationsTable <- function(violations, info, input) {
  data <- violations |>
      
      left_join(
        
        select(info, 
               
               `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
        
        "File Name") |>
      
      select(`Facility Name`, `Program Type`, `Final Report Date`, 
             
             everything()) |>
      
      filter(
        
        `Facility Name` %in% input$Facility2,
        
        `Program Type` %in% input$Program2,
        
        `Final Report Date` %within%
          
          interval(ymd(input$Dates2[1]), ymd(input$Dates2[2])))
    
    return(datatable(data, filter = 'top', 
              
              options = list(pageLength = 5, autoWidth = T, bPaginate = F), fillContainer = T))
    
}

### Applicable Rules
applicableRulesTable <- function(info, rules, input) {
    
    data <- rules  |>
      
      left_join(
        
        select(info, 
               
               `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
        
        "File Name") |>
      
      select(`Facility Name`, `Final Report Date`, `Program Type`, 
             
             everything()) |>
      
      filter(
        
        `Facility Name` %in% input$Facility3,
        
        `Program Type` %in% input$Program3,
        
        `Final Report Date` %within%
          
          interval(ymd(input$Dates3[1]), ymd(input$Dates3[2])))
    
    return(datatable(data, filter = 'top', 
              
              options = list(pageLength = 5, autoWidth = T, bPaginate = F), fillContainer = T))
}


### Download Data Table



### Map Number of Reports
mapReports <- function(violations, info, zips) {
  dat <- violations |>
    
    left_join(info, "File Name") |>
    
    mutate(
      
      zip = str_sub(`Facility Address`, -5, -1)) |>
    
    filter(zip > 0)
  
  plt_dat <- dat |>
    
    group_by(`File Name`) |>
    
    slice_head(n = 1) |>
    
    group_by(zip) |> count() |> ungroup() |>
    
    mutate(
      
      n = n |> factor()) |>
    
    right_join(zips, c("zip" = "ZCTA5CE20"))
  
  dat_sum <- dat |>
    
    group_by(`File Name`) |>
    
    slice_head(n = 1) |>
    
    group_by(zip) |> count() |> ungroup() 
  
  plt_dat2 <- geo_join(zips, dat_sum, "GEOID20", "zip") 
  
  pal <- colorNumeric("Blues", domain=plt_dat2$n)
  
  popup_dat <- paste0("Number of Reports: \n", plt_dat2$n)
  
  return( leaflet() |>
    
    addProviderTiles("CartoDB.Positron") |>
    
    setView(-84.506836, 44.182205, zoom = 6) |>
    
    addPolygons(data = plt_dat2 , 
                fillColor = ~pal(plt_dat2$n), 
                fillOpacity = if_else(is.na(plt_dat2$n), 0, 1), 
                weight = 0.9, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="white",
                popup = ~popup_dat) |>
    
    addLegend(pal = pal, 
              values = plt_dat2$n, 
              position = "bottomright", 
              title = "Number of Reports")
  )
}


### Map Number of Allegations
mapAllegations <- function(violations, info, zips) {
  dat <- violations |>
    
    left_join(info, "File Name") |>
    
    mutate(
      
      zip = str_sub(`Facility Address`, -5, -1)) |>
    
    filter(zip > 0)
  
  plt_dat <- dat |>
    
    group_by(zip) |> count() |> ungroup() |>
    
    mutate(
      
      n = n |> factor()) |>
    
    right_join(zips, c("zip" = "ZCTA5CE20"))
  
  dat_sum <- dat |>
    
    group_by(zip) |> count() |> ungroup() 
  
  plt_dat2 <- geo_join(zips, dat_sum, "GEOID20", "zip") 
  
  pal <- colorNumeric("Oranges", domain=plt_dat2$n)
  
  popup_dat <- paste0("Number of Allegations: \n", plt_dat2$n)
  
  return(leaflet() |>
    
    addProviderTiles("CartoDB.Positron") |>
    
    setView(-84.506836, 44.182205, zoom = 6) |>
    
    addPolygons(data = plt_dat2 , 
                fillColor = ~pal(plt_dat2$n), 
                fillOpacity = if_else(is.na(plt_dat2$n), 0, 1), 
                weight = 0.9, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="white",
                popup = ~popup_dat) |>
    
    addLegend(pal = pal, 
              values = plt_dat2$n, 
              position = "bottomright", 
              title = "Number of Allegations")
  )
}

### Map Number of Violations
mapViolations <- function(violations, info, zips) {
  dat <- violations |>
    
    left_join(info, "File Name") |>
    
    mutate(
      
      zip = str_sub(`Facility Address`, -5, -1)) |>
    
    filter(zip > 0)
  
  plt_dat <- dat |>
    
    filter(`Violation Established` == "Yes") |>
    
    group_by(zip) |> count() |> ungroup() |>
    
    mutate(
      
      n = n |> factor()) |>
    
    right_join(zips, c("zip" = "ZCTA5CE20"))
  
  dat_sum <- dat |>
    
    filter(`Violation Established` == "Yes") |>
    
    group_by(zip) |> count() |> ungroup() 
  
  plt_dat2 <- geo_join(zips, dat_sum, "GEOID20", "zip") 
  
  pal <- colorNumeric("Greens", domain=plt_dat2$n)
  
  popup_dat <- paste0("Number of Violations: \n", plt_dat2$n)
  
  return(leaflet() |>
    
    addProviderTiles("CartoDB.Positron") |>
    
    setView(-84.506836, 44.182205, zoom = 6) |>
    
    addPolygons(data = plt_dat2 , 
                fillColor = ~pal(plt_dat2$n), 
                fillOpacity = if_else(is.na(plt_dat2$n), 0, 1), 
                weight = 0.9, 
                smoothFactor = 0.2, 
                stroke=TRUE,
                color="white",
                popup = ~popup_dat) |>
    
    addLegend(pal = pal, 
              values = plt_dat2$n, 
              position = "bottomright", 
              title = "Number of Violations")
  )
}


### Update Data table
updateData <- function(data_path) {
  
  #call code to run PDF scraping
  print("Gathering new PDFs")
  
  #call code to run PDF to excel sheet formatting
  print(paste0("Updating information in ", data_path))
  
  #re-read in data
  #updated_data = readData(data_path)

  
  # return(updated_data)
  
}
