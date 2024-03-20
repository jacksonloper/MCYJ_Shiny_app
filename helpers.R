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
  p <- info |>                                                                  
    
    mutate(year = year(`Final Report Date`)) |>
    
    group_by(year) |>
    
    count() |>
    
    plot_ly(x = ~n, y = ~year, type = "bar", orientation = "h", 
            
            textposition = "auto", color = I("#489CC9")) |>
    
    layout(
      
      margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20),
      
      title = list(
        
        text = 'Number of Special Investigation Reports by Year', 
        
        xanchor = 'Left'),
      
      #plot_bgcolor = "#e5ecf6", 
      
      xaxis = list(
        
        title = "",
        
        tickfont = list(size = 20)), 
      
      yaxis = list(
        
        title = "",
        
        tickfont = list(size = 20),
        
        categoryorder = "total ascending"),
      
      annotations = list(
        
        x = 1, y = 0.1, text = "Note: Data were available for facilities open\n and operating as of August 2022. Lower\n report freqencies in 2017-2020 are not\n necessarily indicative of the true number\n of violations across all facilities.", 
        
        showarrow = F, xref='paper', yref='paper', 
        
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        
        font=list(size=15, color="#489CC9")))
  
  return(p)
}


### Violations by Year
violationsByYear <- function(violations) {
  p <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`),
      
      "File Name") |>
    
    select(`Facility Name`, `Program Type`, `Final Report Date`, everything()) |>
    
    mutate(year = year(`Final Report Date`)) |>
    
    group_by(year, `Violation Established`) |>
    
    count() |>
    
    filter(`Violation Established` %in% c("Yes", "No")) |>
    
    pivot_wider(id_cols = year, 
                
                names_from = `Violation Established`, values_from = n) |>
    
    plot_ly(x = ~Yes, y = ~year, type = "bar", orientation = "h", 
            
            name = "Established", textposition = "auto", color = I("#dc720a")) |>
    
    add_trace(x = ~No, name = 'Not Established', orientation = "h", 
              
              color = I("#489CC9")) |>
    
    layout(
      
      barmode = 'stack',
      
      margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20),
      
      title = list(
        
        text = 'Number of Alleged Violations by Year', 
        
        xanchor = 'Left'),
      
      #plot_bgcolor = "#e5ecf6", 
      
      xaxis = list(
        
        title = "",
        
        tickfont = list(size = 20)), 
      
      yaxis = list(
        
        title = "",
        
        tickfont = list(size = 20),
        
        categoryorder = "total ascending"),
      
      annotations = list(
        
        x = 1, y = 0.1, text = "Note: Data were available for facilities open\n and operating as of August 2022. Lower\n report freqencies in 2017-2020 are not\n necessarily indicative of the true number\n of violations across all facilities.", 
        
        showarrow = F, xref='paper', yref='paper', 
        
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        
        font=list(size=15, color="#489CC9")))
    
  return(p)
}
  

### Reports by Facility
reportsByFacility <- function(info) {
  p <- info |>
    
    group_by(`Facility Name`) |>
    
    count() |>
    
    plot_ly(x = ~n, y = ~`Facility Name`, type = "bar", orientation = "h", 
            
            textposition = "auto", color = I("#dc720a")) |>
    
    layout(
      
      margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20),
      
      title = list(
        
        text = 'Number of Special Investigation Reports by Facility', 
        
        xanchor = 'Left'),
      
      #plot_bgcolor = "#e5ecf6", 
      
      xaxis = list(
        
        title = "",
        
        tickfont = list(size = 20)), 
      
      yaxis = list(
        
        title = "",
        
        tickfont = list(size = 20),
        
        categoryorder = "total ascending"),
      
      annotations = list(
        
        x = 1, y = 0.1, text = "Note: Data were available for facilities open\n and operating as of August 2022. Lower\n report freqencies in 2017-2020 are not\n necessarily indicative of the true number\n of violations across all facilities.", 
        
        showarrow = F, xref='paper', yref='paper', 
        
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        
        font=list(size=15, color="#dc720a")))
  return(p)
}


### Violations by Facility
violationsByFacility <- function(violations) {
  p <- violations |>
    
    left_join(
      
      select(info, 
             
             `File Name`, `Facility Name`, `Program Type`, `Final Report Date`,
             
             `Capacity`),
      
      "File Name") |>
    
    select(`Facility Name`, `Violation Established`, `Capacity`) |>
    
    group_by(`Facility Name`, `Violation Established`) |>
    
    summarize(rate = n() / mean(Capacity, na.rm = T)) |>
    
    filter(`Violation Established` %in% c("Yes", "No")) |>
    
    pivot_wider(id_cols = `Facility Name`, 
                
                names_from = `Violation Established`, values_from = rate) |>
    
    plot_ly(x = ~Yes, y = ~`Facility Name`, type = "bar", orientation = "h", 
            
            name = "Established", textposition = "auto", color = I("#dc720a")) |>
    
    add_trace(x = ~No, name = 'Not Established', orientation = "h", 
              
              color = I("#489CC9")) |>
    
    layout(
      
      barmode = 'stack',
      
      margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20),
      
      title = list(
        
        text = 'Rates of Alleged Violations by Facility\nfor Facilities with Known Capacity', 
        
        xanchor = 'Left'),
      
      #plot_bgcolor = "#e5ecf6", 
      
      xaxis = list(
        
        title = "",
        
        tickfont = list(size = 20)), 
      
      yaxis = list(
        
        title = "",
        
        tickfont = list(size = 20),
        
        categoryorder = "total ascending"),
      
      annotations = list(
        
        x = 1, y = 0.1, text = "Note: Data were available for facilities open\n and operating as of August 2022. Lower\n report freqencies in 2017-2020 are not\n necessarily indicative of the true number\n of violations across all facilities.", 
        
        showarrow = F, xref='paper', yref='paper', 
        
        xanchor='right', yanchor='auto', xshift=0, yshift=0,
        
        font=list(size=15, color="#489CC9")))
  
  return(p)
}


### Proportion of Allegations with Violation Established
proportionAllegations <- function(violations) {
  p <- violations |>
    
    mutate(
      
      `Violation Established` = if_else(
        
        `Violation Established` == "Yes", "Yes", "No", "No")) |>
    
    group_by(`Violation Established`) |> count() |>
    
    plot_ly(labels = ~`Violation Established`, values = ~n) |>
    
    add_pie(hole = 0.6, ) |>
    
    layout(
      
      title = list(text = "Proportion of Allegations with Violation Established", font = list(size = 16)), 
      
      showlegend = F, 
      
      font = list(size = 20),
      
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
  
  return(p)
}


### Number of Allegations per SIR
numAllegationsSIR <- function(violations) {
  p <- violations |>
    
    group_by(`File Name`) |> count() |>
    
    rename(n_allegations = n) |>
    
    group_by(n_allegations) |> count() |>
    
    plot_ly(x = ~n, y = ~`n_allegations`, type = "bar", orientation = "h", 
            
            textposition = "auto", color = I("#dc720a")) |>
    
    layout(
      
      margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20),
      
      xaxis = list(
        
        title = "\nNumber of Reports",
        
        titlefont = list(size = 24),
        
        tickfont = list(size = 20)), 
      
      yaxis = list(
        
        title = list(text = "Number of Allegations per SIR", standoff = 40L),
        
        titlefont = list(size = 24),
        
        tickfont = list(size = 20),
        
        categoryorder = "total descending"))
  
  return(p)
}


### SIRs with at least One Violation Established
SIRSwithOneViolation <- function(violations) {
  p <- violations |>
    
    mutate(
      
      `Violation Established` = if_else(
        
        `Violation Established` == "Yes", T, F, F)) |>
    
    group_by(`File Name`) |>
    
    summarise(Violation = any(`Violation Established`)) |>
    
    group_by(Violation) |> count() |>
    
    mutate(Violation = if_else(
      
      Violation, "At Least 1 Violation", "No Violations")) |>
    
    plot_ly(labels = ~Violation, values = ~n) |>
    
    add_pie(hole = 0.6) |>
    
    layout(
      
      title = list(text = "SIRs with at least One Violation Established", font = list(size = 16)), 
      
      showlegend = F, 
      
      font = list(size = 20),
      
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
  
  return(p)
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
        
        `Facility Name` %in% input$Facility,
        
        `Program Type` %in% input$Program,
        
        `Final Report Date` %within%
          
          interval(ymd(input$Dates[1]), ymd(input$Dates[2])))
    
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
        
        `Facility Name` %in% input$Facility,
        
        `Program Type` %in% input$Program,
        
        `Final Report Date` %within%
          
          interval(ymd(input$Dates[1]), ymd(input$Dates[2])))
    
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




