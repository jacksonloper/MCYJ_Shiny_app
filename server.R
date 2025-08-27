########################
# server logic
########################

server <- function(input, output) {
  
  # dates <- reactive({
  #   range(data$info$`Final Report Date`)
  # })
  
  
  ### Value box: Special Investigation Reports
  output$numReports <- renderValueBox({
    numReports(data$info)
  })
  
  ### Value box: Recent Reports from last 6 months
  output$recentReports <- renderValueBox({
    recentReports(data$info)
  })
  
  ### Value box: Allegations
  output$numAllegations <- renderValueBox({
    numAllegations(data$violations)
  })
  
  ### Value box: Violations Established
  output$numViolations <- renderValueBox({
    numViolations(data$violations)
  })

  ### Reports by Year
  output$reportsByYear <- renderPlotly({
    reportsByYear(data$info)
  })
  
  ### Violations by Year - Overall
  output$violationsByYearOverall <- renderPlotly({
    violationsByYearOverall(data$violations, data$info)
  })

  ### Violations by Year - Established
  output$violationsByYearEst <- renderPlotly({
    violationsByYearEst(data$violations, data$info)
  })

  ### Violations by Year - Not-Established
  output$violationsByYearNEst <- renderPlotly({
    violationsByYearNEst(data$violations, data$info)
  })
  
  ### Reports by Facility
  output$reportsByFacility <- renderPlotly({
    data$info <- data$info[year(data$info$`Final Report Date`)>=input$rangeSIR[1] & year(data$info$`Final Report Date`)<=input$rangeSIR[2],]
    reportsByFacility(data$info)
  })
  
  ### Violations by Facility - Overall
  output$violationsByFacilityOverall <- renderPlotly({
    data$info <- data$info[year(data$info$`Final Report Date`)>=input$rangeViolation[1] & year(data$info$`Final Report Date`)<=input$rangeViolation[2],]
    violationsByFacilityOverall(data$violations, data$info)
  })

  ### Violations by Facility - Established
  output$violationsByFacilityEst <- renderPlotly({
    data$info <- data$info[year(data$info$`Final Report Date`)>=input$rangeViolation[1] & year(data$info$`Final Report Date`)<=input$rangeViolation[2],]
    violationsByFacilityEst(data$violations, data$info)
  })
  
  ### Violations by Facility - Not Established
  output$violationsByFacilityNEst <- renderPlotly({
    data$info <- data$info[year(data$info$`Final Report Date`)>=input$rangeViolation[1] & year(data$info$`Final Report Date`)<=input$rangeViolation[2],]
    violationsByFacilityNEst(data$violations, data$info)
  })
  
  
  ### Proportion of Allegations with Violation Established
  output$proportionAllegations <- renderPlotly({
    proportionAllegations(data$violations, data$info)
  })
  
  ### Number of Allegations per SIR
  output$numAllegationsSIR <- renderPlotly({
    numAllegationsSIR(data$violations)
  })
  
  ### SIRs with at least One Violation Established
  output$SIRSwithOneViolation <- renderPlotly({
    SIRSwithOneViolation(data$violations, data$info)
  })
  
  
  ### Histogram by rulecode 
  output$reportsByRulecode <- renderPlotly({
    data$info <- data$info[year(data$info$`Final Report Date`)>=input$rangeRuleCode[1] & year(data$info$`Final Report Date`)<=input$rangeRuleCode[2],]
    reportsByRulecode(data$rules, data$info)
  })
  
  ### Explorable Data Table: Report Information
  output$reportInformationTable <- renderDataTable(reportInformationTable(data$info, input))
  
  ### Explorable Data Table: Alleged Violations
  output$allegedViolationsTable <- renderDataTable({
    dt_object <- allegedViolationsTable(data$violations, data$info, input)
    
    # Extract the data and apply formatting
    if("data" %in% names(dt_object$x)) {
      table_data <- dt_object$x$data
    } else {
      # If it's already a datatable, just apply the formatting
      dt_object %>%
        formatStyle(columns = 1:ncol(dt_object$x$data), 
                    `vertical-align` = 'top')
    }
  }, options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().container()).find('td').css({'vertical-align': 'top'});",
      "}")
  ))  
  ### Explorable Data Table: Applicable Rules
  output$applicableRulesTable <- renderDataTable(applicableRulesTable(data$info, data$rules, input))
  
  ### Download data
  #output$downBtn <- output$downBtn2 <- output$downBtn3 <- downloadHandler(
    
  #  filename = function() { "DATA.xlsx" },
    
  #  content  = function(file) { 
      
  #    l <- list("INFORMATION" = info, "VIOLATIONS" = violations, "RULES" = rules)
      
  #    write.xlsx(l, file) }
  #)
  
  ### Update data

  ### When the update button is selected, run the PDF scraping and reformatting code
  ### TODO: then update the data (and all reactive plots/values) by reading in the new/updated excel file
  
  # data<- eventReactive(input$update, {
  #   print("Updating Data")
  #   
  #   updateData(data_path)
  # 
  # })
  
  observeEvent(input$update, {
    print("Updating Data")
    updateData(data_path)
  })
  
  observeEvent(input$update2, {
    print("Updating Data")
    updateData(data_path)
  })
  
  observeEvent(input$update3, {
    print("Updating Data")
    updateData(data_path)
  })
  
  
  ### Map: Number of Reports
  output$mapReports <- renderLeaflet(
    mapReports(data$violations, data$info, zips)
  )
  
  ### Map: Number of Allegations
  output$mapAllegations <- renderLeaflet(
    mapAllegations(data$violations, data$info, zips)
  )
  
  ### Map: Number of Violations
  output$mapViolations <- renderLeaflet(
    mapViolations(data$violations, data$info, zips)
  )
  
  ### Map: Program Type
  output$mapProgramType <- renderLeaflet({
    # Filter the data based on the slider's year range
    filtered_data <- data$info %>%
      filter(year(`Effective Date`) <= input$rangeYear[2] &
               year(`Expiration Date`) >= input$rangeYear[1])
    mapProgramType(filtered_data)
  })
  
  ### Legend: Program Type
  output$externalLegend <- renderUI({
    # Use the full dataset (or you can use filtered_data if desired)
    progTypes <- sort(unique(data$info$`Program Type`))
    program_colors <- colorFactor(viridis(8), domain = progTypes)
    
    # Create a simple legend: colored box + label for each Program Type
    legendItems <- lapply(progTypes, function(pt) {
      div(
        style = "display: flex; align-items: center; margin-bottom: 4px;",
        div(
          style = sprintf("width: 20px; height: 20px; background-color: %s; margin-right: 5px;",
                          program_colors(pt))
        ),
        span(pt)
      )
    })
    
    tagList(
      h4("Program Type"),
      do.call(tagList, legendItems)
    )
    
    
  })

}


