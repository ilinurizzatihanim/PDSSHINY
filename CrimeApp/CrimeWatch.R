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
                titlePanel(img(src = "dataproduct.png", height = "100%", width = "100%")
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
                                                 tags$p(tags$i("This website is designed and developed as a useful application on demonstrating the analysis of violent crime in Asian region within the year 2003 to 2017."))), 
                                          
                                        ),
                                        
                                        fluidRow(
                                          tags$br(),
                                          
                                          column(10, align = "center",
                                                 tags$p(tags$h4("Before you start exploring this app, please read the following guideline")), offset = 1),
                                        ),
                                        
                                        fluidRow(
                                          tags$br()
                                        ),
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        #Introduction & About Row

                                        splitLayout(cellWidths = c("40%", "50%"),
                                                    img(src = "guide.intro.png", height = "100%", width = "100%"),
                                                    
                                                    fluidRow(
                                                      column(2, 
                                                             tags$h3("Introduction"),
                                                             tags$p(tags$i("Explanation based on the data science")), 
                                                             tags$p(tags$i("process implemented in developing this application.")), offset = 2)),
                                                    
                                        ),
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        splitLayout(cellWidths = c("40%", "50%"),
                                                    img(src = "guide.about.png", height = "100%", width = "100%"),
                                                    
                                                    fluidRow(
                                                      column(2, 
                                                             tags$h3("About"),
                                                             tags$p(tags$i("About our team, objective and motivation on developing this application.")), offset = 2)), 
                                                             
                                        ),
                                      
                                        
                                        
                                        #Analysis & World Map Row
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        splitLayout(cellWidths = c("40%", "50%"),
                                                    img(src = "guide.type.png", height = "100%", width = "100%"),
                                                    
                                                    fluidRow(
                                                      column(2, 
                                                             tags$h3("Type of Crime"),
                                                             tags$p(tags$i("Based on the UN Database, violent crimes are classified as kidnapping,")),
                                                             tags$p(tags$i("serious assault, and robbery.")),
                                                             br(),
                                                             tags$p(tags$i("Explanation on the violent crimes, namely kidnapping,")),
                                                             tags$p(tags$i("serious assault and robbery that will be analysed in the application.")), offset = 2)),
                                                    
                                        ),
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        splitLayout(cellWidths = c("40%", "50%"),
                                                    img(src = "guide.analysis.png", height = "100%", width = "100%"),
                                                    
                                                    fluidRow(
                                                      column(2, 
                                                             tags$h3("Analysis"),
                                                             tags$p(tags$i("Results of analysis that users may explore to")), 
                                                             tags$p(tags$i("compare violent crimes between different countries in Asia within the year 2003 to 2017.")), offset = 2)),
                                                    
                                        ),
                                        
                                        
                                        
                  ),
                  
                  
                  
                  
                  intro_panel <-tabPanel("Introduction",

                                         tags$hr(style="border-color: white;"),
                                         
                                         sidebarLayout(
                                           sidebarPanel(strong("Source of Dataset")),
                                           mainPanel(
                                             img(src = "UNODC logo.png", height = "100%", width = "100%"),
                                             tags$p(),
                                             p("There are a total of three datasets being used in developing this application."), 
                                             p("There are obtained from the United Nation Office on Drugs and Crime via link:"),
                                             tags$p(tags$a(href="https://dataunodc.un.org/data/crime/kidnapping", "Kidnapping")),
                                             tags$p(tags$a(href = "https://dataunodc.un.org/data/crime/Serious%20assault", "Serious Assault")),
                                             tags$p(tags$a(href = "https://dataunodc.un.org/data/crime/Robbery", "Robbery")),
                                             br(),
                                             p("The original datasets consist of crime counts and its rates for violent crime"), 
                                             p("-- kidnapping, serious assault and robbery from 2003 to 2017."),
                                             br())),
                                            
                                         
                                         tags$hr(style="border-color: white;"),
                                         
                                         sidebarLayout(
                                           sidebarPanel(strong("Data Preprocessing")),
                                           mainPanel(
                                             img(src = "datasc.png", height = "100%", width = "100%"),
                                             tags$p(),
                                             p("Data cleaning and exploration are carried out on these three datasets."),
                                             p("Firstly, all missing values are removed and the data are filtered to select only Asian countries."), 
                                             p("The datasets are then combined into a single dataset to ease the exploratory data analysis (EDA) and application’s development."),
                                             br(),
                                             p("Our aim is to do descriptive analysis which are describing, showing, and summarizing the dataset."),
                                             p("The result of analysis can be viewed on the graph under the 'analysis' tab."))),
                                         
                                         
                                         tags$hr(style="border-color: white;"),
                                         
                                         sidebarLayout(
                                           sidebarPanel(strong("Design Thinking")),
                                           mainPanel(
                                             img(src = "design thinking.png", height = "100%", width = "100%"),
                                             tags$p(),
                                             tags$ol(
                                               tags$strong(tags$li("Empathize")),
                                               tags$p("Focusing on social issues, we try to explore different problems by researching on news, article, websites and etc."),
                                               tags$p("From the results, we notice that the crime rates in some regions are increasing recently even during the Covid-19 pandemic and we are interested to look into the related topic."),
                                               tags$br(),
                                               tags$strong(tags$li("Define")),
                                               tags$p("Crime consist of different types from violent crime, drug demand and supply, homicide, corruption and bribery, sexual crime, trafficking in persons and many more."), 
                                               tags$p("In this project, after research and discussion, we aim to focus on crimes under category violent crime like kidnapping and robbery, which most of the people are familiar with, but having lack of concern about."),
                                               tags$br(),
                                               tags$strong(tags$li("Ideate")),
                                               tags$p("Brainstorming on a product that allows a wide range of users to obtain information and statistics of violent crimes conveniently."), 
                                               tags$p("This allows users to explore the information by selecting and comparing the crime based on years and countries on their own interactively."),
                                               tags$br(),
                                               tags$strong(tags$li("Prototype")),
                                               tags$p("Performing a prototype through a shiny application in R studio as it is free to utilise, easy to access and develop."),
                                               tags$br(),
                                               tags$strong(tags$li("Test")),
                                               tags$p("Testing on the “Crime Watch” application. Collect feedback based on user experiences and improve it from time to time accordingly."),
                               
                                             ))),
                                         
                                         tags$hr(style="border-color: white;"),
                                         
                                         
                                         
                                         
                                         
                  ),
                  
                  about_panel <-tabPanel("About", icon = icon("info-circle"),
                                         
                                         fluidRow(
                                           column(12, align = "center",
                                                  img(src = "mission.png", height = "100%", width = "100%"))
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
                                                              tags$p(tags$i("As the developers of this application named “Crime Watch”,")), 
                                                              tags$p(tags$i("we are students from University of Malaya with different races and backgrounds,")), 
                                                              tags$p(tags$i("who are competent, dynamic, inclusive and always concerned about social issues.")), offset = 3)),
                                                     
                                         ),
                                         
                                         tags$hr(style="border-color: white;"),
                                         
                                         splitLayout(cellWidths = c("60%", "40%"),
                                                     
                                                     fluidRow(
                                                       column(7, 
                                                              tags$h3("Our Motivation: Impacts Towards Community"),
                                                              tags$p(tags$i("Usually, people tend to pay less attention to social issues like crime and gender inequality.")), 
                                                              tags$p(tags$i("However, along with the saying, “We are all part of the same world, helping others is a part of responsibility”,")),
                                                              tags$p(tags$i("the public can react more proactively towards these issues.")),
                                                              br(),
                                                              tags$p(tags$i("Hence, by exploring this application, we are hoping that people")), 
                                                              tags$p(tags$i("will have higher exposure and concerns towards the topic on violent crime.")), offset = 2)),
                                                              
                                                     
                                                     img(src = "community.png", height = "50%", width = "100%"),
                                                     
                                         ),
                                         
                                         
                  ),
                  
                  type_panel <-tabPanel("Type of Crime", icon = icon("fist-raised"),
                                        
                                        fluidRow(
                                          column(12, align = "center",
                                                 tags$h2("Kidnapping", width = 2))
                                          
                                        ),
                                        
                                        fluidRow(
                                          column(6, align = "left",
                                                 tags$p("Any event or connected series of events of seizing, detaining or carrying away by force or fraud, of one or more insured person(s) (except a minor by his or her parent) for the purpose of demanding ransom monies."),
                                                 offset = 3)),
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        fluidRow(
                                          column(12, align = "center",
                                                 tags$h2("Serious Assault", width = 2))
                                          
                                        ),
                                                    
                                                    fluidRow(
                                                      column(6, 
                                                             tags$p("An assault involving violence upon another person, 
                                                                    for example occasioning serious bodily harm,
                                                                    sexual assault."), offset = 3)),
                                                    
                                        
                                        
                                        tags$hr(style="border-color: white;"),
                                        
                                        fluidRow(
                                          column(12, align = "center",
                                                 tags$h2("Robbery", width = 2))
                                          
                                        ),
    
                                                    fluidRow(
                                                      column(6,
                                                             tags$p("Taking of money or other property which may be the subject of larceny from the person or custody of another, with intent to either permanently or temporarily deprive the person or the owner of the money or other property, when in the course of the taking there is the use of force, violence, assault, or putting in fear."),
                                                             offset = 3)), 
                                                    
                                                    
                                                    
                                     
                  
                                        
                  ),
                  
                  
                  analysis_panel <-tabPanel("Analysis",icon = icon("chart-bar"),

                                            tags$br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                           h3("Filter Data"),
                                                           
                                                           
                                                           sliderInput("years", # the id your server needs to use the selected value
                                                                       label = h3("Year"),
                                                                       min = 2003, max = 2017, # maximum range that can be selected
                                                                       value = c(2003, 2017)),
                                                           pickerInput("CountryInput","Country Picker:", choices=sort(unique(crime$Country)), options = list(`actions-box` = TRUE),multiple = T)
                                                           
                                              ),
                                              
                                              mainPanel(
                                                tabsetPanel(
                                                  tabPanel("Kidnapping", #First tab 
                                                           plotOutput("plot5", width = 900, height = 600)),  #plot output kidnapping
                                                  tabPanel('Serious Assault',
                                                           plotOutput("plot6", width = 900, height = 600)),
                                                  tabPanel("Robbery",
                                                           plotOutput("plot7", width = 900, height = 600)),
                                                  
                                                  tags$p("Disclaimer: Data is presented as a rate per 100,000 population per year."),
                                                  
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
  
  #Kidnapping plot
  filtered5 <- reactive({ #graph 1A
    crime %>%
      filter (Country %in% input$CountryInput, between(Year ,input$years[1], input$years[2])) %>% 
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
  }, height = "auto")
  
  #Serious Assault plot
  filtered6 <- reactive({ 
    crime %>%
      filter (Country %in% input$CountryInput, between(Year ,input$years[1], input$years[2])) %>% 
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
  }, height = "auto")
  
  #Robbery plot + reactive
  filtered7 <- reactive({ 
    crime %>%
      filter (Country %in% input$CountryInput, between(Year ,input$years[1], input$years[2])) %>% 
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
  }, height = "auto")
  
}


# Run the application 
shinyApp(ui = ui, server = server)