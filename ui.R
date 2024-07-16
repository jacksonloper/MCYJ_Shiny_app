########################
# UI
########################
### Dashboard Header
header <- dashboardHeader(

)
# Adding MCYJ logo with clickable link as header title
header$children[[2]]$children <-  tags$a(href='https://www.miyouthjustice.org',
                                           tags$img(src='MCYJ-LOGO.png', height='40', width='130'))

### Sidebar
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Overview", tabName = "dashboard", icon = icon("chart-simple", lib = "font-awesome")),
  menuItem("Data", tabName = "data", icon = icon("database", lib = "font-awesome"),
           
           menuSubItem("Report Information",
                       tabName = "reportInfo", icon = icon("table", lib = "font-awesome")),
           
           menuSubItem("Alleged Violations",
                       tabName = "allegedViolations", icon = icon("table", lib = "font-awesome")),
           
           menuSubItem("Applicable Rules",
                       tabName = "applicableRules", icon = icon("table", lib = "font-awesome"))
          ),

  menuItem("Map", tabName = "map", icon = icon("map", lib = "font-awesome")),
  menuItem("About", tabName = "about", icon = icon("question", lib = "font-awesome"))
))


### Dashboard Body content
body <- dashboardBody(
  # Customize the color of the value boxes
  tags$style(".small-box.bg-blue { background-color: #489CC9 !important; color: #FFFFFF !important; }"),
  tags$style(".small-box.bg-purple { background-color: #5C3566 !important; color: #FFFFFF !important; }"),
  tags$style(".small-box.bg-red { background-color: #fc7530 !important; color: #FFFFFF !important; }"),
  tags$style(".small-box.bg-green { background-color: #469D6D !important; color: #FFFFFF !important; }"),
  tabItems(
    # Overview dashboard tab content
    tabItem(tabName = "dashboard",
            h2("Overview Dashboard"),
            
            fluidRow(
              valueBoxOutput("numReports", width = 3),
              valueBoxOutput("recentReports", width = 3),
              valueBoxOutput("numAllegations", width = 3),
              valueBoxOutput("numViolations", width = 3)
            ),
            fluidRow(
              box(width = 12, solidHeader = TRUE, status = "primary",
                  "Note: Data were available for facilities open and operating as of August 2022. Lower report freqencies in 2017-2020 are not necessarily indicative of the true number of violations across all facilities.")
            ),
            fluidRow(
              box(title = 'Number of Special Investigation Reports by Year',
                  plotlyOutput("reportsByYear"), width = 12)
            ),
            fluidRow(
              box(width = 12, title = 'Number of Alleged Violations by Year',
                  tabsetPanel(
                    tabPanel('Overall', plotlyOutput("violationsByYearOverall")),
                    tabPanel('Violation Established', plotlyOutput("violationsByYearEst")),
                    tabPanel('Violaton Not Established', plotlyOutput("violationsByYearNEst"))
                  ))
            ),
            fluidRow(
              box(width = 12, title = 'Top 10 Facilities with Most Special Investigation Reports (SIRs)',
                  plotlyOutput("reportsByFacility"),
                  sliderInput("rangeSIR", "Select the year range to count the number of SIRs",
                              min(year(data$info$`Final Report Date`)), 
                              max(year(data$info$`Final Report Date`)), 
                              value = c(2017, max(year(data$info$`Final Report Date`))),sep = " ")
              )
            ),
            fluidRow(
              box(width = 12, title = 'Top 10 Facilities with Highest Rates of Alleged Violations',
                  tabsetPanel(
                    tabPanel('Overall', plotlyOutput("violationsByFacilityOverall")),
                    tabPanel('Violation Established', plotlyOutput("violationsByFacilityEst")),
                    tabPanel('Violaton Not Established', plotlyOutput("violationsByFacilityNEst"))
                  ),
                  'Rate = Number of Alleged Violations / Facilitiy Capacity',
                  sliderInput("rangeViolation", "Select the year range to calculate the rate",
                              min(year(data$info$`Final Report Date`)), 
                              max(year(data$info$`Final Report Date`)), 
                              value = c(2017, max(year(data$info$`Final Report Date`))),sep = " "))
            ),
            fluidRow(
              box(width = 6, title = 'Proportion of Allegations with Violation Established by Year',
                  plotlyOutput("proportionAllegations")),
              box(width = 6, title = 'Proportion of SIRs with at least One Violation Established by Year',
                  plotlyOutput("SIRSwithOneViolation"))
            ),
            fluidRow(
              box(width = 12, title = 'Number of Allegations per Special Investigation Report (SIR)',
                  plotlyOutput("numAllegationsSIR"))
            )
            
    ),
    
    # facs = sort(unique(data$info$`Facility Name`)),
    # type = sort(unique(data$info$`Program Type`)),
    ### Data tab content
    ### Report Information Table
    tabItem(tabName = "reportInfo",
            h2("Report Information Data"),
            
            fluidPage( 
              
              # User input for data table filtering
              fluidRow(

                column(width = 4, pickerInput("Facility", label = strong("Facility:"),
                                              
                                              choices = sort(unique(data$info$`Facility Name`)), 
                                              #choices = facs(),
                                              selected = sort(unique(data$info$`Facility Name`)), 
                                              #selected = facs(), 
                                              multiple = T,
                                              
                                              options = list(`actions-box` = T))),
                
                
                column(width = 4, pickerInput("Program", label = strong("Program Type:"),
                                              
                                              choices = ({sort(unique(data$info$`Program Type`))}), 
                                              #choices = types(), 
                                              selected = ({sort(unique(data$info$`Program Type`))}), 
                                              #selected = types(),
                                              multiple = T,
                                              
                                              options = list(`actions-box` = T))),
                
                
                column(width = 4, dateRangeInput("Dates", strong("Report Date:"), 
                                                 
                                                 start = dates[1], end = dates[2], min = dates[1], max = dates[2])),
              ),
              
              # Download and update data buttons
              fluidRow(
                column(width = 4),
                
                column(width = 2, downloadButton("downBtn", "Download the Data", style = "width:100%;")),

                column(width = 2, actionButton("update", "Update the Data", icon = icon("refresh"), 
                                               style = "width:100%;")),
                
                column(width = 4)
              ),
              
              # Data Table readout
              #TODO: adjust height to scale with change in browser viewing window
              fluidRow(dataTableOutput("reportInformationTable", width = "100%", height = 800), width = 12)
              
            )
    ),
    
    ### Alleged Violations Table
    tabItem(tabName = "allegedViolations",
            h2("Alleged Violations Data"),
            
            fluidPage( 
              
              # User input for data table filtering
              fluidRow(
                # facs <- sort(unique(info$`Facility Name`)),
                column(width = 4, pickerInput("Facility2", label = strong("Facility:"),

                                              choices = sort(unique(data$info$`Facility Name`)),

                                              selected = sort(unique(data$info$`Facility Name`)), 
                                              
                                              multiple = T,

                                              options = list(`actions-box` = T))),


                # types <- sort(unique(info$`Program Type`)),

                column(width = 4, pickerInput("Program2", label = strong("Program Type:"),

                                              choices = sort(unique(data$info$`Program Type`)),

                                              selected = sort(unique(data$info$`Program Type`)),
                                              
                                              multiple = T,

                                              options = list(`actions-box` = T))),


                column(width = 4, dateRangeInput("Dates2", strong("Report Date:"),

                                                 start = dates[1], end = dates[2], min = dates[1], max = dates[2])),
              ),

              # Download and update data buttons
              fluidRow(
                column(width = 4),

                column(width = 2, downloadButton("downBtn2", "Download the Data", style = "width:100%;")),

                column(width = 2, actionButton("update2", "Update the Data", icon = icon("refresh"), style = "width:100%;")),

                column(width = 4)
              ),
              
              # Data Table readout
              #TODO: adjust height to scale with change in browser viewing window
              fluidRow(dataTableOutput("allegedViolationsTable", width = "100%", height = 800), width = 12)
              
            )
    ),
    
    ### Applicable Rules Table
    tabItem(tabName = "applicableRules",
            h2("Applicable Rules Data"),
            
            fluidPage( 
              
              # User input for data table filtering
              fluidRow(
                # facs <- sort(unique(info$`Facility Name`)),
                column(width = 4, pickerInput("Facility3", label = strong("Facility:"),

                                              choices = sort(unique(data$info$`Facility Name`)),
                                              
                                              selected = sort(unique(data$info$`Facility Name`)), 
                                              
                                              multiple = T,

                                              options = list(`actions-box` = T))),


                # types <- sort(unique(info$`Program Type`)),

                column(width = 4, pickerInput("Program3", label = strong("Program Type:"),

                                              choices = sort(unique(data$info$`Program Type`)),
                                              
                                              selected = sort(unique(data$info$`Program Type`)), 
                                              
                                              multiple = T,

                                              options = list(`actions-box` = T))),


                column(width = 4, dateRangeInput("Dates3", strong("Report Date:"),

                                                 start = dates[1], end = dates[2], min = dates[1], max = dates[2])),
              ),

              # Download and update data buttons
              fluidRow(
                column(width = 4),

                column(width = 2, downloadButton("downBtn3", "Download the Data", style = "width:100%;")),

                column(width = 2, actionButton("update3", "Update the Data", icon = icon("refresh"), style = "width:100%;")),

                column(width = 4)
              ),
              
              # Data Table readout
              #TODO: adjust height to scale with change in browser viewing window
              fluidRow(dataTableOutput("applicableRulesTable", width = "100%", height = 800), width = 12)
              
            )
    ),

    
    # Map tab content
    tabItem(tabName = "map",
            h2("Map"),
            
            fluidRow(
              box(leafletOutput("mapReports"), width = 6),
              box(leafletOutput("mapAllegations"), width = 6),
            ),
            
            fluidRow(
              box(leafletOutput("mapViolations"), width = 12)
            )
    ),
    
    # About tab content
    tabItem(tabName = "about",
            h2("About This Dashboard"),
            
            ### About this Dashboard
            navset_card_tab(full_screen = T, height = 250,
                            
              nav_panel("About",
                                      
                card_body(markdown("
                  ### About 
                  
                  This dashboard was built using [R Shiny](https://shiny.rstudio.com/) in collaboration with the [Michigan Center for Youth Justice](https://www.miyouthjustice.org/), The Edward Ginsberg Center's Community Technical Assistance Collaborative, and [Statistics in the Community](https://sph.umich.edu/biostat/statcom/) at the University of Michigan."))
                  ),
                            
              nav_panel("Key Terms",
                                      
                  card_body(markdown("
                    ### Key Terms and Definitions
                    
                    -   **Restraint**: The use of physical force without the use of a device, for the purpose of restraining the free movement of the child's body
                    
                    -   **Mechanical Restraint**: A device attached or adjacent to the child's body that the child cannot easily remove and restricts freedom of movement or normal access of the child's body
                    
                    -   **Seclusion**: The temporary placement of a child in a room, alone, where they are prevented from leaving. Per MDHHS, may only be used if essential to prevent the child from physically harming others.
                    
                    -   **Child Caring Institution (CCI)**: A child care facility which is organized for the purpose of receiving minor children for care, maintenance, and supervision, usually on a 24-hour basis, in buildings maintained by the institution for that purpose, and operates throughout the year
                    
                    -   **Family Independence Agency (FIA)**: MDHHS agency that directs public assistance and service programs. They also deliver juvenile justice services and aim to the support necessary to strengthen families and individuals
                    
                    -   **Juvenile Justice**: A collection of state and local court-based systems whose purpose is to respond to young people who come into contact with law enforcement and are accused of breaking the law
                    
                    -   **JJ Facility**: Facilities with programs serving youth in the Juvenile Justice system"))
                    ),
                            
            nav_panel("Data",
                                      
              card_body(markdown("
                ### Licensed Capacity as a Proxy for Facility Size
                
                **Why did we use licensed capacity?**
                
                There is no data available for how many youth were in each facility or under the care of MDHHS at any given time. To address this, we used licensed capacity as a proxy for the size of the facility. Using licensed capacity as a proxy for facility size may not be representative of the number of youth in a facility at any given time. 
                
                **How did we use licensed capacity in our analysis?**  
                
                Because facilities are varying sizes (different capacities), raw numbers can be deceiving. To address this, we normalized (divided) the variables by the licensed capacity for each facility to obtain a rate of incident by capacity. 
                
                **Limitations of Using Licensed Capacity (Case Example):** 
                
                In March 2023, public officials declared a public health state of emergency at Wayne County Detention Facility due to overcrowding alongside a lack of staff. County officials reported that the facility can comfortably house 80 youth. The average number of youth at the facility in 2021 was 68; in March 2023 there were 137 youth residing there. Surprisingly, the licensed capacity of the facility is reported as 166 in the data we received.
                
                Data for this case example was reported by Detroit Free Press on 3/21/23.
                
                [https://www.freep.com/story/news/local/michigan/wayne/2023/03/21/public-health-emergency-troubled-juvenile-jail/70032928007/](https://www.freep.com/story/news/local/michigan/wayne/2023/03/21/public-health-emergency-troubled-juvenile-jail/70032928007/) 
                "))
              ),
                            
            nav_panel(
              shiny::icon("circle-info"),
              markdown("Learn more about [Shiny](https://shiny.posit.co/)")
            )
          ),
          
          ### About statcom
          layout_column_wrap(width = 1/3, 
                             
            card(full_screen = T, height = 650,
                                  
            card_image(file = "./images/mcyj_about.jpeg", href = "https://www.miyouthjustice.org/our-mission-values", height = 350),
                                  
            card_body(markdown("The Michigan Center for Youth Justice (MCYJ) is a non-profit organization dedicated to advancing policies and practices that reduce confinement and support trauma-informed, racially equitable, socio-economically and culturally responsive, community-based solutions for Michigan's justice-involved children, youth and young adults. 
    
              **Our Vision**
              
              Our Vision is a fair and effective justice system for Michigan's children, youth, and young adults.
              
              **Our Mission**
              
              We work to advance equitable youth justice policies and practices that protect young people and help them achieve their full potential.
              
              **Our Core Values**
              
              At MCYJ, we value:
              
              -   **Community and Connection**: Youth deserve to remain connected to their families and communities, even if they become justice system-involved. They should be recognized for their strengths and value, not just for their mistakes.
              
              -   **Commitment**: We can be counted on to persist in our work for fair, equitable justice reform for Michigan's children, youth, and young adults. We will never stop re-imagining compassionate solutions.
              
              -   **Equity**: All justice-involved youth are treated in a fair and just manner that considers the systematic practices and biases related to identity, background, and personal experience.
              
              -   **Inclusion**: We believe that diverse perspectives create better solutions. Families who are impacted by the youth justice system should be at the center of our work.
              
              -   **Restoration**: The youth justice system should be restorative and rehabilitative. Kids who get in trouble are still kids.")), 
                                                
            card_footer(markdown("[Michigan Center for Youth Justice](https://www.miyouthjustice.org/)"))),
                                           
              card(full_screen = T, height = 650,
                                                
              card_image(file = "./images/ginsberg_about.png", href = "https://ginsberg.umich.edu/", height = 350),
                                                
              card_body(markdown("The Edward Ginsberg Center is a community and civic engagement center with a mission to cultivate and steward equitable partnerships between communities and the University of Michigan in order to advance social change for the public good. Based upon this mission, our vision is for inclusive democracy; thriving, diverse communities; and equity and social justice. Centered in our principles, our work is focused on:
              
                -   **Partnerships**: We cultivate an expansive network of community partners (nonprofits, local governments, and K-12 schools), surface community-identified priorities, and connect them with students, faculty, and staff who are invested in positive social change.
                
                -   **Preparation**: We prepare and support students and faculty and staff to create socially just community engagement experiences through advising, consulting, trainings and grants
                
                -   **Pathways**: We offer multiple pathways to civic engagement & community change to encourage the development of lifelong habits of civic learning.
                
                [The Edward Ginsberg Center's](https://ginsberg.umich.edu/) [Community Technical Assistance Collaborative](https://ginsberg.umich.edu/ctac) is a community-university partnership convened to serve a universal need identified by community partners around data and evaluation.
                
                Our work is grounded in meaningful community-university partnerships that foster a culture of learning and improvement so that together we have the capability to build thriving, equitable, and just communities.
                
                CTAC partnerships focus on supporting nonprofits, schools, and governmental organizations to build capacity, while meeting the educational goals of students. We are invested in sustainable solutions that advance the impact of our partners' work toward key quality of life outcomes, and bolster their capacity to tell their stories, and secure funding.")),
                                                  
              card_footer(markdown("[The Edward Ginsberg Center](https://ginsberg.umich.edu/)"))),
                             
                card(full_screen = T, height = 650,
                                  
                card_image(file = "./images/statcom_about.jpeg", href = "https://sph.umich.edu/biostat/statcom/", height = 350),
                                  
                card_body(markdown("Statistics in the Community (STATCOM) at the University of Michigan is a community outreach program provided by graduate students in the Departments of Biostatistics, Statistics, and the Program for Survey Methodology at University of Michigan. The program offers the expertise of statistics graduate students, free of charge, to non-profit governmental and community organizations in the areas of data organization, analysis, and interpretation.

                  **Services Provided by STATCOM** 
                  
                  Advice and assistance are offered on a wide variety of statistical issues including:
                  
                  -   Use of data to improve decision making processes 
                  
                  -   Survey/sample design and analysis 
                  
                  -   Design and analysis of studies and experiments 
                  
                  -   Graphical methods of summarizing and gaining meaning from data 
                  
                  -   Use of data to detect trends and make predictions and projections. 
                  
                  STATCOM is able to assist with study design, data analysis, and the interpretation of results, but does not have the resources to collect or enter data.")),
                                                    
                card_footer(markdown("[Statistics in the Community](https://sph.umich.edu/biostat/statcom/)"))),
          )
            
    )
    
  ), #closing tabItems
  
  ### Customizing Dashboard Colors
  tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #5C3566;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #5C3566;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #5C3566;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #b19bb6;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #fc8930;
                              border-left: 3px solid #fc8930;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #b19bb6;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #fc8930;
                              border-left: 3px solid #fc8930;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #fc8930;
                              }
                              '
  )))
  
) #closing dashboardBody


dashboardPage(header, 
              sidebar,
              body)

