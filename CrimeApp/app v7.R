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



# Define UI for application
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel(img(src = "banner.png", height = 180, width = 890)
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
                                img(src = "intro.png"), offset = 1),
                              
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
                                 img(src = "our mission.png", height = 180, width = 890))
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
                                         
                                         img(src = "community.png", height = 180, width = 320),
                                         
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

                   mainPanel(
                     plotOutput("scatterPlot")))
                   ),
                     
                   
      map_panel <-tabPanel("World Map", icon = icon("globe-americas"),
                           mainPanel((leafletOutput("map", height = 1000))),
      )
    )
)
                 

      
      
        

# Define server logic 
server <- function(input, output) {


  output$map <- renderLeaflet({
    leaflet(crime) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        # Popup content
        popup = paste("Offense", crime$entity, "<br>",
                      "Year:", crime$year))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
#dinesh
