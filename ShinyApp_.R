## World Shape Data Available at: http://thematicmapping.org/downloads/world_borders.php
## Statistical Capacity Data Available at: https://datacatalog.worldbank.org/dataset/data-statistical-capacity

library(shiny)
library(rgdal)
library(dplyr)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(scales)
library(rsconnect)


#LOADING DATA

#Measures displayed as rows - with one Percentage column - used for Choropleth map
SCI <- read.csv('SCI_Melted.csv',header=TRUE, sep=",")

#Measures divided into columns - used in Table
SCI_Table <- read.csv('SCI_Melted_Table.csv',header=TRUE, sep=",")


#world shape file
WorldMap <- readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')


#check if elements are presnt in both datasets

#remove unused datapoints for World Map
WorldMap <- subset(WorldMap, is.element(WorldMap$ISO3,SCI$Country_code))

#Align shapefile with SCI datasets so we can layer data
NewData <- SCI[order(match(SCI$Country_code, WorldMap$ISO3)),]

NewData_Table <- SCI_Table[order(match(SCI_Table$Country.Code, WorldMap$ISO3)),]



#Remove row names and drop 'X' column
rownames(NewData) <- c()

rownames(NewData_Table) <- c()

NewData <- select (NewData,-c(X))

NewData_Table <- select (NewData_Table,-c(X))

#changing columns names: 'NAME' = 'Country', 'Country.Code' = 'Country Code'
colnames(NewData_Table)[1] <- "Country"
colnames(NewData_Table)[2] <- "Country Code"

#Bins for choropleth
bins <- c(0,10,20,30,40,50,60,70,80,90,100)
pal <- colorBin('RdYlBu', domain = c(0,100), bins = bins)

#Sorting years in decreasing order
yearRange<-sort(unique(as.numeric(NewData$Year)), decreasing=TRUE)

countriesAplhabeticalOrder <- sort(unique(NewData$NAME), decreasing = FALSE)
countriesAplhabeticalOrder
#UI - DASHBOARD
ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title="Statistical Capacity Indicators: Global Overview",titleWidth = 450),
  dashboardSidebar(
    selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1]),
    radioButtons("dataMeasure", "Measure", choices=c('Average Score' = 'Average',
                                                     'Periodicity Score' = 'Periodicity',
                                                     'Source Score' = 'Source',
                                                     'Methodology Score' = 'Methodology')),
    selectInput('country','Country', choices = countriesAplhabeticalOrder, 
                multiple = FALSE, 
                selected = countriesAplhabeticalOrder[1])
  ),
  dashboardBody(
    fluidRow(
      column(width = 10,
             #Leaflet Map
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("worldMap", height=400)
             ),
             
             box(width = NULL,
                 plotOutput("countryPlot")
             ),
             #Table
             box(width=NULL,
                 dataTableOutput("worldTable")
             )
      )
      
    )
  )
)


#SERVER
server <- function(input, output){
  #Data used for Choropleth Map
  data_input <- reactive({
    NewData %>%
      filter(Year == input$dataYear) %>%
      filter(Indicator_Name == input$dataMeasure)
    
  })
  
  #align data
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$Country_code, WorldMap$ISO3)),]
  })
  
  #Data used for Table
  data_input_Table <- reactive({
    NewData_Table %>%
      filter(Year == input$dataYear) 
  })
  
  #align data
  data_input_Table_Ordered <- reactive({
    data_input_Table()[order(match(data_input_Table()$Country.Code, WorldMap$ISO3)),]
  })
  
  #Map labels when hovered over
  labels <- reactive({
    paste('<p>', '<b>', data_input_Table_Ordered()$NAME, '</b>', ' (', data_input_Table_Ordered()$Country.Code, ')', '<p>',
          '<p>', '<b>', 'Average Score: ', round(data_input_Table_Ordered()$Average, digits = 3),'</b>', '<p>',
          '<p>', 'Periodicity Score: ', round(data_input_Table_Ordered()$Periodicity, digits = 3),'<p>',
          '<p>', 'Source Score: ', round(data_input_Table_Ordered()$Source, digits = 3),'<p>',
          '<p>', 'Methodology Score: ', round(data_input_Table_Ordered()$Methodology, digits = 3),'<p>') 
  })
  
  #Choropleth output map
  output$worldMap <- renderLeaflet(
    leaflet() %>%
      setView(0, 32, 2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = WorldMap, 
                  weight = 1, 
                  smoothFactor = 0.5, 
                  color='white', 
                  fillOpacity = 0.8,
                  fillColor = pal(data_input_ordered()$Percentage),
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE),
                  label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal,
                title = "SCI Score",
                values = data_input_ordered()$Percentage,
                opacity = 0.7,
                position = 'topright')
    
  )
  
  
  #Table output
  output$worldTable <- renderDataTable(data_input_Table()[order(data_input_Table()$Average), ],
                                       server = FALSE, 
                                       options = list(pageLength = 5, autoWidth = TRUE),
                                       rownames= FALSE
  )
  
  #Line Plot Output
  data_input_plot <- reactive({
    NewData %>%
      filter(Indicator_Name == input$dataMeasure) %>%
      filter(NAME == input$country)
  })
  

  
  output$countryPlot = renderPlot({
    ggplot(data_input_plot()) +
      geom_line(mapping = aes(x = unique(NewData$Year),
                                       y = data_input_plot()$Percentage, 
                                       colour = data_input_plot()$NAME),
                color='darkblue') + 
      labs(x = "Years", y = "Score", 
           title = paste("SCI Score for", unique(data_input_plot()$NAME))) +
            scale_x_continuous(breaks=pretty_breaks()) + #used pretty_brakes() function 
                                                   #from 'scales' package to avoid 
                                                  #typing break manually
      scale_colour_discrete(name = "Country")
  })
} 

shinyApp(ui, server)

