#helper functions for creating plots in the dashboard
library(plotly)


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
    
    add_pie(hole = 0.6) |>
    
    layout(
      
      title = "Proportion of Allegations with Violation Established", showlegend = F, font = list(size = 20),
      
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
      
      title = "SIRs with at least One Violation Established", showlegend = F, font = list(size = 20),
      
      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
      
      yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
  
  return(p)
}


### Create Explorable Data Table



### Download Data Table



### Map Number of Allegations



### Map Number of Violations



### 


