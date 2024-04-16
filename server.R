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
  
  ### Violations by Year
  output$violationsByYear <- renderPlotly({
    violationsByYear(data$violations, data$info)
  })
  
  ### Reports by Facility
  output$reportsByFacility <- renderPlotly({
    reportsByFacility(data$info)
  })
  
  ### Violations by Facility
  output$violationsByFacility <- renderPlotly({
    violationsByFacility(data$violations, data$info)
  })
  
  ### Proportion of Allegations with Violation Established
  output$proportionAllegations <- renderPlotly({
    proportionAllegations(data$violations)
  })
  
  ### Number of Allegations per SIR
  output$numAllegationsSIR <- renderPlotly({
    numAllegationsSIR(data$violations)
  })
  
  ### SIRs with at least One Violation Established
  output$SIRSwithOneViolation <- renderPlotly({
    SIRSwithOneViolation(data$violations)
  })
  
  ### Explorable Data Table: Report Information
  output$reportInformationTable <- renderDataTable(reportInformationTable(data$info, input))
  
  ### Explorable Data Table: Alleged Violations
  output$allegedViolationsTable <- renderDataTable(allegedViolationsTable(data$violations, data$info, input))
  
  ### Explorable Data Table: Applicable Rules
  output$applicableRulesTable <- renderDataTable(applicableRulesTable(data$info, data$rules, input))
  
  ### Download data
  output$downBtn <- output$downBtn2 <- output$downBtn3 <- downloadHandler(
    
    filename = function() { "DATA.xlsx" },
    
    content  = function(file) { 
      
      l <- list("INFORMATION" = info, "VIOLATIONS" = violations, "RULES" = rules)
      
      write.xlsx(l, file) }
  )
  
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

}


