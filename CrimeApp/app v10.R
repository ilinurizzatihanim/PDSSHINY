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
library(ggridges)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(shinyWidgets)


#Loading data
crime <-read.csv("https://raw.githubusercontent.com/DineshRamachandran14/PDSSHINY/main/asiacrime.csv")
str(crime)


# Define UI for application
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(img(src = "banner.png", height = "100%", width = "100%")
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
                            
                            titlePanel("Introduction",
                              sidebarLayout(
                                sidebarPanel(
                                  h4(strong("The Kiterunner"), "a novel by", em("Khaled Hoseinni")),
                                ),
                                mainPanel()
                              )
                            ),
  
                            
                            fluidRow(
                              column(5,
                                img(src = "intro.png", height = "100%", width = "100%"), offset = 1),
                              
                              column(3,
                                img(src = "about.png")),
                              
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
                                     img(src = "analysis.png"), offset = 1),
                              
                              column(3,
                                     img(src = "worldmap.png")),
                              
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
                                 img(src = "datasc.png", height = 120, width = 550),
                                   p("The data science process ..."),
                                 br())),
                             
                             
               ),
               
      about_panel <-tabPanel("About", icon = icon("info-circle"),
                             
                             fluidRow(
                               column(12, align = "center",
                                 img(src = "our mission.png", height = "100%", width = "100%"))
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
                               img(src = "team.png", height = "100%", width = "100%"),
                               
                               fluidRow(
                                 column(2, 
                               tags$h3("Our team"),
                               tags$i("A team that is competent, dynamic, and inclusive"), offset = 2)),
                               
                               ),
                             
                             tags$hr(style="border-color: white;"),
                             
                             splitLayout(cellWidths = c("60%", "40%"),
                                         
                                         fluidRow(
                                           column(7, 
                                                  tags$h3("Community Impact"),
                                                  tags$i("We are all part of the same world,"),
                                                  
                                                  tags$p(
                                                    tags$i("helping others is a part of responsibility")), offset = 2)),
                                         
                                         img(src = "community.png", height = "50%", width = "100%"),
                                         
                             ),
                             
                             
      ),
      
      type_panel <-tabPanel("Type of Crime", icon = icon("fist-raised"),
                            
      ),
              
               
      analysis_panel <-tabPanel("Analysis",icon = icon("chart-bar"),
                   sidebarLayout(
                     sidebarPanel(width = 5,
                       h3("Filter Data"),
                       

                     sliderInput("years", # the id your server needs to use the selected value
                                 label = h3("Years"),
                                 min = 2003, max = 2017, # maximum range that can be selected
                                 value = c(2003, 2017)),
                    pickerInput("CountryInput","Select Country:", choices=sort(unique(crime$Country)), options = list(`actions-box` = TRUE),multiple = T)
                    
                    ),
                    
                    mainPanel(
                          tabsetPanel(
                            tabPanel("Kidnapping", #First tab 
                                 plotOutput("plot5", width = 800, height = 300)),  #plot output kidnapping
                            tabPanel('Serious Assault',
                                     plotOutput("plot6", width = 800, height = 300)),
                            tabPanel("Robbery",
                                     plotOutput("plot7", width = 800, height = 300)),
                                 
                                  br(),
                                  br(),
                          )
                    )
                   )
      )
    )
)
                     
       

# Define server logic 
server <- function(input, output, session) {
  
  #Date reactive
  Years_filtered <- reactive({
    filter(crime, between(Year ,input$years[1], input$years[2]))
  })
  
  #Kidnapping plot
  filtered5 <- reactive({ #graph 1A
    crime %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Year) %>% 
      summarise(KidnapRatebyYear = sum(Rate))
  })
  
  output$plot5 <- renderPlot({ #graph 1A
    ggplot(filtered5(), aes(x=Year, y=KidnapRatebyYear, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Kidnapping Rate by Year") +
      xlab("Year") + ylab("Kidnapping Rate (%)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
  #Serious Assault plot
  filtered6 <- reactive({ 
    crime %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Year) %>% 
      summarise(SeriousAssaultRatebyYear = sum(Rate.2))
  })
  
  output$plot6 <- renderPlot({ 
    ggplot(filtered6(), aes(x=Year, y=SeriousAssaultRatebyYear, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Serious Assault Rate by Year") +
      xlab("Year") + ylab("Serious Assault Rate (%)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })

  #Robbery plot + reactive
  filtered7 <- reactive({ 
    crime %>%
      filter (Country %in% input$CountryInput) %>% 
      group_by(Country, Year) %>% 
      summarise(RobberyRatebyYear = sum(as.numeric(Rate.1)))
  })
  
  output$plot7 <- renderPlot({ 
    ggplot(filtered7(), aes(x=Year, y=RobberyRatebyYear, group=Country)) +
      geom_line(aes(color=Country)) +
      theme_light() +
      labs(title="Robbery Rate by Year") +
      xlab("Year") + ylab("Robbery Rate (%)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      scale_y_continuous(labels = scales::comma)
  })
  
}
  





# Run the application 
shinyApp(ui = ui, server = server)
