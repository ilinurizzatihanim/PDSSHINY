#
# 
#

# Packages
library(shiny)
library(ggplot2)
library(reshape)
library(leaflet)
library(shinythemes)
library(DT)
library(ggplot2)
library(tidyverse)

#Loading data
df <- read.csv("https://github.com/DineshRamachandran14/PDSSHINY/blob/main/crimedataset.xlsx%20-%20Sheet1.csv")
#data.frame':	51 obs. of  9 variables:
#$ Region           : chr  "Asia" "Asia" "Asia" "Asia" ...
#$ Sub.region       : chr  "Central Asia" "Central Asia" "Central Asia" "Central Asia" ...
#$ Country          : chr  "Kazakhstan" "Kyrgyzstan" "Tajikistan" "Turkmenistan" ...
#$ Population..2020.: chr  "18776707" "6524195" "9537645" "6,031,200\t" ...
#$ Homicide         : num  606 138 126 203 363 ...
#$ Kidnapping       : int  74 53 172 127 23 NA 228 1 1 3 ...
#$ Rape             : int  1298 314 36 27 NA 33579 1289 112 NA 342 ...
#$ Serious.Assault  : int  2050 283 3735 83 1397 79662 24365 6623 1743 398 ...
#$ Robbery          : int  9469 969 288 NA 831 NA 2332 500 108 1233 ...



# Define UI for application
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/banner.png?token=GHSAT0AAAAAABQZ22SG55IEMKYLSGWOGEZAYPK2HLA", height = 180, width = 890)
    ),
    
    fluidRow(
      tags$br()
    ),

    #Tab Details
    tabsetPanel(
      navi_panel <-tabPanel("User Guide", icon = icon("mouse-pointer"),
                            
                            fluidRow(
                              column(12, align = "center",
                                     h2("A Quick User Guide")),
                              
                              column(12, align = "center",
                                     tags$p(tags$i("This app is a page for analysis of violent crime in Asian region.")), 
                                    tags$i("It will touch upon the basic relationship of the violent crime presently known in Asian region."))),
                              
                              fluidRow(
                                tags$br(),
                              
                              column(10, align = "center",
                                     tags$p(tags$h4("Before you start exploring this app, please read the following guideline")), offset = 1),
                            ),
                            
                            fluidRow(
                              tags$br()
                            ),
                        
                              
                              #Introduction & About Row
                            
                            fluidRow(
                              column(5,
                                img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/intro.png?token=GHSAT0AAAAAABQZ22SH43IE6D44Z75OHI6MYPK2JFQ"), offset = 1),
                              
                              column(3,
                                img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/about.png?token=GHSAT0AAAAAABQZ22SH3ZWXEQ34RS53PRAQYPK2JUQ")),
                              
                            ),
                            
                            fluidRow(
                              tags$br()
                            ),
                            
                            fluidRow(
                              column(4, align = "center",
                                     tags$p("Introduction of the dataset used in the app."), 
                                     tags$p("The type of analysis conducted on the data."), 
                                     tags$p("Explanation on the data science process done to the dataset."), offset = 1),
                              
                              column(4, align = "center",
                                     tags$p("Team, motivation and objective of this app."), offset = 1),
                            ),
                            
                            
                            
                            #Analysis & World Map Row
                            
                            fluidRow(
                              tags$br()
                            ),
                            
                            fluidRow(
                              column(5,
                                     img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/analysis.png?token=GHSAT0AAAAAABQZ22SHL2PGQ4VWYXCWO7QSYPK2KHQ"), offset = 1),
                              
                              column(3,
                                     img(src = "https://github.com/DineshRamachandran14/PDSSHINY/blob/main/CrimeApp/www/worldmap.png")),
                              
                            ),
                            
                            fluidRow(
                              tags$br()
                            ),
                            
                            fluidRow(
                              column(4, align = "center",
                                     tags$p("Plotted graph to view relationship between the type of violent crime and the region."), 
                                     tags$p("User able to choose type of violent crime."), offset = 1),
                              
                              column(4, align = "center",
                                     tags$p("User able to view worldwide map of where the violent crime took place."), offset = 1),
                            ),
                            
                        
      ),
      
                         
                                  
      
      intro_panel <-tabPanel("Introduction",
                             
                             tags$hr(style="border-color: white;"),

                             sidebarLayout(
                               sidebarPanel(strong("Dataset")),
                               mainPanel(
                                 p("The datasets are from United Nation | Office on Drugs and Crime"),
                                 br(),
                                 p("The violent crime data is a combination of kidnapping, homicide, sexual assult, robbery, human trafficking, and rape."),
                                 br(),
                                 p("The year of observation is from 2003 - 2017."),
                                 br())),
                             
                             tags$hr(style="border-color: white;"),
                             
                             sidebarLayout(
                               sidebarPanel(strong("Analysis")),
                               mainPanel(
                                 p("Each of the dataset go through data cleaning and",
                                   span ("exploratory data analysis (EDA)", style = "color:yellow"),
                                   "before plotting of the graph."),
                                 br(),
                                 p("Our aim is to do descriptive analysis which are describing, showing, and summarizing the dataset."),
                                br(),
                               p("The result of analysis can be viewed on the graph under the 'analysis' tab."))),
                             
                             
                             tags$hr(style="border-color: white;"),
                             
                             sidebarLayout(
                               sidebarPanel(strong("Data Science Process")),
                               mainPanel(
                                 img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/datasc.png?token=GHSAT0AAAAAABQZ22SGK2KJBMVUYI2HGYJ4YPK2LUQ", height = 120, width = 550),
                                   p("The data science process ..."),
                                 br())),
                             
                             
               ),
               
      about_panel <-tabPanel("About", icon = icon("info-circle"),
                             
                             fluidRow(
                               column(12, align = "center",
                                 img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/our%20mission.png?token=GHSAT0AAAAAABQZ22SHHSCE6A5WCQRYAOWEYPK2MBA", height = 180, width = 890))
                             ),
                             
                             fluidRow(
                               column(12, align = "center",
                             tags$h2("ABOUT THIS APP", width = 5))
                             
                             ),
                             
                             fluidRow(
                               column(6, align = "left",
                             tags$p("Our mission is to ensure the public awareness on the violent crime that pertain in selected the region."),
                             tags$p("We do this in the mindset of community safety and hopeful for the positive impact to communities where people work and lives."), offset = 3)),
                             
                             tags$hr(style="border-color: white;"),
                             
                             splitLayout(cellWidths = c("40%", "50%"),
                               img(src = "team.png", height = 180, width = 320),
                               
                               fluidRow(
                                 column(2, 
                               tags$h3("Our team"),
                               tags$i("A team that is competent, dynamic, and inclusive"))),
                               
                               ),
                             
                             tags$hr(style="border-color: white;"),
                             
                             splitLayout(cellWidths = c("60%", "40%"),
                                         
                                         fluidRow(
                                           column(7, 
                                                  tags$h3("Community Impact"),
                                                  tags$i("We are all part of the same world,"),
                                                  
                                                  tags$p(
                                                    tags$i("helping others is a part of responsibility")), offset = 2)),
                                         
                                         img(src = "https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/CrimeApp/www/community.png?token=GHSAT0AAAAAABQZ22SG7ANGNMEA3IO457EYYPK2MTA", height = 180, width = 320),
                                         
                             ),
                             
                             
      ),
              
               
      analysis_panel <-tabPanel("Analysis",icon = icon("chart-bar"),
                   sidebarLayout(
                     sidebarPanel( width = 5,
                       h3("Filter Data"),
                                  selectInput("checkYear", label = "Select Year",
                                              choices = list("2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011", "2012", "2015", "2016", "2017"), selected = "2003"),
                       selectInput("checkType", label = "Select Type of Crime",
                                   choices = list("Kidnapping", "Rape", "Sexual Assault", "Robbery", "Homicide", "Human Trafficking"), selected = "Type of Crime")),
      timeseries<- tabPanel("Time Analysis", 
             sidebarPanel(
              dateRangeInput("DateInput", sort(unique(df$Year)), min = min(df$Year), max = max(df$Year), range(min(df$Year), max(df$Year)), label = "Year range:"), #Date Range
              pickerInput("CountryInput","Select Country:", choices=sort(unique(df$Country)), options = list(`actions-box` = TRUE),multiple = T) #Country list
  
                   ),
                   mainPanel(
                  tabsetPanel(
                    tabPanel("Sales by Time Overview", #First tab for Time Analysis Nav. Tab
                          plotOutput("plot5", width = 800, height = 300), #plot output
                          br(),
                          br(),
                          plotOutput("plot4", width = 800, height = 300), #plot output
                          br(),
                          br()
                 ),
                 tabPanel("Customer Segmentation", #Second tab for Time Analysis Nav. Tab
                          plotOutput("plot6", width = 800, height = 300),
                          br(),
                          plotOutput("plot3", width = 800, height = 300)
                 )
               )
             )
    ),
                               
      map_panel <-tabPanel("World Map", icon = icon("globe-americas"),
                           mainPanel((leafletOutput("map", height = 1000))),
      )
    )
)
                 

      
      
        

# Define server logic 
server <- function(input, output) {
    
    # Filter for Time Analysis
  filtered2 <- reactive({
    retail %>%
      filter (Date >= input$DateInput[1],
              Date <= input$DateInput[-1],
              Country %in% input$CountryInput) %>% 
      group_by(Country, Time) %>% 
      summarise(numOfCust = n_distinct(CustomerID))
  })
  
  filtered3 <- reactive({
    retail %>%
      filter (Date >= input$DateInput[1],
              Date <= input$DateInput[-1],
              Country %in% input$CountryInput) %>% 
      group_by(Country, Time) %>% 
      summarise(Revenuebytime = sum(Total))
  })
  
  
  filtered5 <- reactive({
    retail %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Month) %>% 
      summarise(Revenuebymonth = sum(Total))
  })
  
  filtered6 <- reactive({
    retail %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Month) %>% 
      summarise(numOfcustbymonth = n_distinct(CustomerID))
  })
  
  #Output for Time Analysis
  output$plot3 <- renderPlot({
    ggplot(filtered2(), aes(x=Time, y=numOfCust, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Customer Count Across Time")+
      xlab("Time") + ylab("Number of customers") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot4 <- renderPlot({
    ggplot(filtered3(), aes(x=Time, y=Revenuebytime, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Revenue Across Time") +
      xlab("Time") + ylab("Revenue") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot5 <- renderPlot({
    ggplot(filtered5(), aes(x=Month, y=Revenuebymonth, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Monthly Revenue") +
      xlab("Month") + ylab("Revenue") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot6 <- renderPlot({
    ggplot(filtered6(), aes(x=Month, y=numOfcustbymonth, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Monthly Customer Count")+
      xlab("Month") + ylab("Number of customers") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_y_continuous(labels = scales::comma)
  })
  
    
}


# Run the application 
shinyApp(ui = ui, server = server)
