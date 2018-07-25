

navbarPage('NYC bicycle accidents', id = "nav", theme = shinytheme("superhero"),
 
           ## Background tab ## 
           
           tabPanel("Background",
                    wellPanel(fluidRow(h4("Bicycle collisions in NYC: what are the numbers?"), align = "center")),
                    wellPanel(fluidRow(h4("An average of 4400 bicycle were reported each year in NYC between 2013 - 2017"), align = "center"),
                    br(),
                    fluidRow(column(1),
                             column(5, plotOutput("total_line")),
                             column(5, plotOutput("total")),
                             column(1, checkboxGroupInput(inputId='seasoncheckbox',
                                                       label = h4("Select Season"),
                                                       choices = unique(bike_total$season),
                                                       selected = unique(bike_total$season)))),
                    
                    fluidRow(a('One-Way Anova, p-value = 0.421'), align = "right")),
                    br(),
                    br(),
                    wellPanel(fluidRow(h4("The number of accidents peaks during the after work rush our"), align = "center"),
                    br(),
                    fluidRow(column(2),
                            column(2, sliderInput(inputId='hourslider',
                                                   label = h4("Time of the day"),
                                                   min = 1, max = 23, value = 1,
                                                   step = 2,
                                                   post = "h",
                                                   animate = TRUE)),
                              column(6, plotOutput("hour"), align = "center"),
                              column(2))),
                    br(),
                    br()),
                    # wellPanel(fluidRow(h4("Most accidents happen in summer and fall resulting in injuries.", align = "center"))),
                    # br(),
                    # fluidRow(column(2),
                    #          column(3, selectizeInput(inputId='outcomecheckbox',
                    #                                   label = "Select outcome category",
                    #                                   choices = unique(bike_outcome$outcome),
                    #                                   selected = "injured")),
                    #          column(7,plotOutput("outcome")))),
           
           ## Explore Causes tab ###
           
           tabPanel("Explore Causes",
                    wellPanel(fluidRow(h4("What are the major causes?"), align = "center")),
                    wellPanel(fluidRow(h4("Inattention and Failure to Yield Right-of-way are the most common causes among all types of vehicle drivers"), align = "center"),
                              br(),
                    fluidRow(htmlOutput("diagram"), align = "center"),
                    fluidRow(a('Dataset was filtered for frequencies >= 1%.'))),
                    br(),
                    br(),
                    wellPanel(fluidRow(h4("Inattention and Failure to Yield Right-of-way are also the major causes for increased accidents during the rush hour"), align = "center"),
                              br(),
                    fluidRow(align = "center", plotOutput("heatmap", width = "70%", height = "500"))),
                    br(),
                    br()),
           
           ## Case Study tab ##
           
           tabPanel("Case Study: bike lanes",
                    wellPanel(fluidRow(h4("Can we increase saftey with more bike lanes?"), align = "center")),
                    wellPanel(fluidRow(h4("New York's streets have either no bike lanes, unprotected bike lanes, or protected bike lanes"), align = "center"),
                    fluidRow(column(4,img(src = "no_lanes.png", height = "300", width = "100%")),
                             column(4, img(src = "unprotected_lanes.png", height = "300", width = "100%")),
                             column(4, img(src = "protected_lanes.png", height = "300", width = "100%")), align = "center"),
                    fluidRow(a("From left to right: no bike lanes (Manhattan), unprotected bike lanes (Manhattan), protected bike lanes (Bronx)"))),
                    br(),
                    br(),
                    wellPanel(fluidRow(h4("Case study: More than 70 % of locations with a high frequency of accidents have unprotected bike lanes"), align = "center"),
                    br(),
                    fluidRow(plotOutput("lanes", width = "50%"), align = "center"),
                    br(),
                    fluidRow(tags$small('Dataset was divided into two sets with locations of high accident and low accident frequencies in downtown Manhattan.')),
                    fluidRow(tags$small('High-freq locations and randomly sampled low-freq locations were analzyed for the presence of bike lanes by using Google StreetView.'))),
                    br(),
                    br(),
                    br(),

                    wellPanel(fluidRow(h4("Can more protected bike lanes decrease the number of bicycle collisions?"), align = "center"),
                              fluidRow(img(src = "NYCgov.png", height = "400", width = "40%"), align = "center"),
                              fluidRow(a("source: NYCgov"), align = "center")),
                    br(),
                    br()),
           
           ## Explore Map tab ##
           
           tabPanel("Explore Map",
                    wellPanel(fluidRow(h4("Explore map displaying locations of all recorded accidents"), align = "center"),
                    fluidRow(leafletOutput("mymap", width = "70%", height = "500"), align = "center")),
                    br(),
                    br(),
                    wellPanel(fluidRow(h4("'Hot Spots' of bicycle accidents (> 10)"), align = "center"),
                    fluidRow(leafletOutput("hot_spots", width = "70%", height = "500"), align = "center")),
                    br()),
           
           ## Data Tables tab ##
           
           tabPanel("Data Tables",
                    wellPanel(fluidRow(h4("High frequency and low frequency data tables"), align = "center"),
                              br(),
                    fluidRow(column(6, dataTableOutput("table_high")),
                             column(6, dataTableOutput("table_low"))),
                    br()
                    ))

)
      
                    
