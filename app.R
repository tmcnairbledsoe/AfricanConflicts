devtools::install_github("timelyportfolio/parsetR")
#devtools::install_github("hrbrmstr/streamgraph")
#devtools::install_github("rstudio/r2d3")
if(!require("shiny")) install.packages("shiny")
if(!require("shinydashboard")) install.packages("shinydashboard")
if(!require("dygraphs")) install.packages("dygraphs")
if(!require("devtools")) install.packages("devtools")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ROCR")) install.packages("ROCR")
if(!require("tm")) install.packages("tm")
if(!require("wordcloud")) install.packages("wordcloud")
if(!require("memoise")) install.packages("memoise")
if(!require("DT"))install.packages("DT")
if(!require("wordcloud")) install.packages("wordcloud")
if(!require("memoise")) install.packages("memoise")
if(!require("leaflet")) install.packages("leaflet")
if(!require("xts")) install.packages("xts")
#if(!require("parsetR")) devtools::install_github("timelyportfolio/parsetR")

# libraries
library(ROCR)
library(shiny)
#library(streamgraph)
library(lazyeval)
library(tidyverse) # much better to call tidyverse (includes dplyr, ggplot, etc)
library(leaflet)
library(parsetR)
#library(plotly)
#library(r2d3)
library(scales)
library(lubridate)
library(xts)
library(dygraphs)
library(shinydashboard)
library(DT)
library(tm)
library(wordcloud)
library(memoise)




#options(shiny.reactLog = TRUE)
# it's much better to use read_csv (from readr in tidyverse) than read.csv
# I modified your code to make it more tidyverse-compliant: read http://r4ds.had.co.nz/ (FREE ONLINE BOOK!)
africaData <- read_csv("ActorSpread.csv") %>%
  mutate(POINTLABEL = paste(LOCATION, FATALITIES),
         EVENT_DATE = as.Date(EVENT_DATE, "%m/%d/%Y"),
         MONTHYEAR = as.Date(MONTHYEAR, "%m/%d/%Y")) %>% drop_na()

# get drop down lists
africaData$EVENT_TYPE <- ifelse(africaData$EVENT_TYPE %in% c("Riots//Protests",
                                                             "Non-violent transfer of territory",
                                                             "Headquarters or base established",
                                                             "Battle-Government regains territory",
                                                             "Strategic Development"),
                                "All Other", africaData$EVENT_TYPE)
EventList = unique(africaData$EVENT_TYPE)
EventList = c("ALL", as.character(EventList))


RegionList = unique(africaData$REGION)
RegionList = c("ALL", as.character(RegionList))

ActorList = unique(c(africaData$ACTOR1, africaData$ACTOR2))
ActorList = c("ALL", as.character(ActorList))


##################################################
#  Word Cloud
##################################################

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(reactData) {
  
  
  text = paste(reactData['NOTES'], sep = "")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "killed"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


##################################################
#  Shiny UI
##################################################

ui <- dashboardPage(
  skin = "yellow", 
  
  dashboardHeader(title = "Visual Exploration of Africa Conflicts", titleWidth = 450), 
  dashboardSidebar(width = 300, 
                   sidebarMenu ( 
                     selectInput("input_events", "Event Types:",
                                 EventList, multiple = T, selectize = T, selected = EventList[1]),
                     
                     selectInput("input_regions", "Regions:",
                                 RegionList, multiple = T, selectize = T, selected = RegionList[1]),
                     
                     selectInput("input_actors", "Actors:",
                                 ActorList, multiple = T, selectize = T, selected = ActorList[1])
                     
                     #,uiOutput("actor_selector")
                     
                     
                     #sliderInput("input_Year", "Year:",
                     #           min = 1997, max = 2017, value = c(1997,2017), sep = "" )
                   )
                   
  ), 
  dashboardBody ( 
    tags$style(HTML("
                    .tabs-above > .nav > li[class=active] > a {
                    background-color: #3c8dbc;
                    color: #FFF;
                    }"))
    ,
    
    
    fluidRow(
      
      box(width = 8, title = "Fatalities Over Time", status = "primary", solidHeader = TRUE, dygraphOutput("dygraph"), height = "500px"),
      #box(width = 4, title = "Top Actors", status = "primary", solidHeader = TRUE, tableOutput("ActorTable"), height = "485px")
      box(width = 4, title = "Top Actors", status = "primary", solidHeader = TRUE, height = "500px",
          column(width = 12,DT::dataTableOutput("ActorDataTable"), style = "height:420px; overflow-y: scroll;")
      )
      
    ),
    fluidRow(textOutput('emptyline')),
    fluidRow (
      box (leafletOutput("mymap"), status = "primary", solidHeader = TRUE,
           width = 12, id = "mymap", title = "Leaflet Map"
      )
    )
    , 
    fluidRow(
      
      # box(width = 5, title = "Stream Graph", status = "primary", solidHeader = TRUE
      #        ,textOutput("selected_var")
      # ,streamgraphOutput("streamPlot")
      box(width = 5, title = "Word Cloud", status = "primary", solidHeader = TRUE,
          actionButton("generateWC", "Generate Word Cloud") , plotOutput("wordcloudplot", width = "490px", height = "400px"),
          height = "500px"),
      
      box (width = 7, parsetOutput(outputId = "parset_out", width = "700px", height = "400px"), 
           title = "Parallel Sets", 
           status = "primary", 
           solidHeader = TRUE,
           collapsible = TRUE, height = "500px"
      )
      
    )
    
    )
    )



##################################################
#  Shiny Server
##################################################

server <- function(input, output, session) {
  # Read data
  ReactiveData <- reactive({
    
    
    filterDf = africaData
    
    if(input$input_events != "ALL")
      filterDf = filterDf %>% filter(EVENT_TYPE %in% input$input_events)
    if(input$input_regions != "ALL")
      filterDf = filterDf %>% filter(REGION %in% input$input_regions)
    if(input$input_actors != "ALL")
      filterDf = filterDf %>% filter(ACTOR1 %in% input$input_actors)
    
    filterDf
  })
  
  # ReactiveActorData <- reactive({
  #   filterDf = ReactiveData()
  #   if(is.null(input$input_Actors)){
  #     filterDf
  #   } else if(input$input_Actors == "ALL"){
  #     filterDf
  #   } else {
  #     filter(filterDf, ACTOR1 %in% input$input_Actors)
  #   }
  # })
  
  ReactiveTimedData <- reactive({
    # time1 <- as.character(as.Date(strftime(req(input$dygraph_date_window[[1]]), "%Y-%m-%d")))
    # time2 <- as.character(as.Date(strftime(req(input$dygraph_date_window[[2]]), "%Y-%m-%d")))
    
    time1 <- as.Date(strftime(req(input$dygraph_date_window[[1]]), "%Y-%m-%d"))
    time2 <- as.Date(strftime(req(input$dygraph_date_window[[2]]), "%Y-%m-%d"))
    
    filter(ReactiveData(), time1 < MONTHYEAR & time2 > MONTHYEAR)
    
  })
  
  output$selected_var <- renderText({
    paste("You have selected ", input$input_regions)
  })
  
  output$emptyline = renderText({
    paste0("")
  })
  
  output$mymap <- renderLeaflet({
    leaflet_df <- ReactiveTimedData()
    
    # see https://www.r-bloggers.com/r-using-rcolorbrewer-to-colour-your-figures-in-r/
    pal <- colorFactor(RColorBrewer::brewer.pal(6, "Set1"), domain = unique(africaData$EVENT_TYPE))
    
    leaflet(leaflet_df) %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng = leaflet_df$LONGITUDE,
        lat = leaflet_df$LATITUDE,
        #radius = 2,
        radius = (leaflet_df$FATALITIES/50),
        popup = paste0("<br><strong>Fatalities:</strong> ",
                       leaflet_df$FATALITIES,
                       "<br><strong>Primary Belligerent:</strong> ",
                       leaflet_df$ACTOR1,
                       "<br><strong>Secondary Belligerent:</strong> ",
                       leaflet_df$ACTOR2,
                       "<br><strong>Location:</strong> ",
                       leaflet_df$LOCATION,
                       "<br><strong>Notes:</strong> ",
                       leaflet_df$NOTES),
        color = ~pal(leaflet_df$EVENT_TYPE), # see https://rstudio.github.io/leaflet/markers.html
        fillOpacity = .65,
        weight = 1) %>%
      setView(lng = mean(leaflet_df$LONGITUDE),
              lat = mean(leaflet_df$LATITUDE),
              zoom = 3) %>%
      leaflet::addLegend(position = "bottomright", values = leaflet_df$EVENT_TYPE,
                         pal = pal,
                         title = "Event Types",
                         opacity = 0.7
      )
    
  })
  
  output$dygraph <- renderDygraph({
    
    # count events by day
    aggTime <- ReactiveData() %>% count(EVENT_DATE) 
    
    createTimeSeries <- function(df){
      # change to xts data format
      africaTime <- xts(
        order.by = df$EVENT_DATE,
        x = df$n
      )
      # convert from daily to monthly, uses sum to aggregate the units
      apply.monthly(africaTime,sum)
    }
    
    africaTime <- createTimeSeries(aggTime)
    
    # call dygraphs: see http://rstudio.github.io/dygraphs/shiny.html
    dygraph(africaTime,  main = "Count of Fatalities Per Month") %>% # modify this to change dygraphs title
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Dark2"), includeZero = TRUE) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Fatalities Count by Month") %>%
      dyRangeSelector()
  })
  
  output$parset_out <- renderParset({
    ReactiveTimedData() %>%
      select(REGION, EVENT_TYPE, FATALITIES_RANGE, YEAR) %>%
      parset()
  })
  
  # output$actor_selector = renderUI({#Gets top actors depending on inputs
  #   filteredActors = ReactiveData() %>% 
  #     #filter(str_detect(ACTOR2, "^Unidentified") ) 
  #     group_by(ACTOR1) %>%
  #     summarize(count = n()) %>%
  #     arrange(desc(count)) %>%
  #     filter(count > 200)
  
  # Rajkumar - Commenting the ActorTable, and replacing it with ActorDataTable to implement the scrollbar.
  # output$ActorTable = renderTable (
  #   { 
  #     filteredActors = ReactiveTimedData() %>% 
  #       #filter(str_detect(ACTOR2, "^Unidentified") ) 
  #       group_by(ACTOR1) %>%
  #       summarize(Count = n()) %>%
  #       arrange(desc(Count))  %>%
  #       top_n(10)
  #       #filter(count > 200)
  #     }
  # )
  
  output$ActorDataTable = renderDataTable (
    { 
      filteredActors = ReactiveTimedData() %>% 
        #filter(str_detect(ACTOR2, "^Unidentified") ) 
        group_by(ACTOR1, ACTOR2) %>%
        summarize(Count = n()) %>%
        arrange(desc(Count))  %>%
        top_n(10)
      #filter(count > 200)
    }, options = list(paging = FALSE)
  )
  
  # Code to Stream graph output
  # output$streamPlot <- renderStreamgraph({
  # 
  # #streamgraph(subset(dat, Region == input$variable), "key", "value", "year")
  # #fatalities = subset(streamData, YEAR == input$input_Years & ACTOR1==input$input_Actors)
  # fatalities = subset(ReactiveTimedData())
  # validate(
  #   need(nrow(fatalities) != 0, "Please make another selection")
  # )
  # eval(call_new(streamgraph, fatalities, "EVENT_TYPE", "FATALITIES", "EVENT_DATE")) 
  # 
  # })
  
  #filteredActors = filteredActors['ACTOR1']
  #filteredActors = append("ALL", filteredActors$ACTOR1)
  
  #selectInput("input_Actors", "Top Actors: ",
  # filteredActors, multiple = T, selectize = F, selected = "ALL")
  #})
  
  wordcloud_rep <- repeatable(wordcloud)
  
  eventReactiveData <- eventReactive(input$generateWC, {
    ReactiveTimedData()
  })
  
  output$wordcloudplot <- renderPlot({
    v = getTermMatrix(eventReactiveData())
    
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 10, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
}

##################################################
#  Shiny Run App
##################################################

shinyApp(ui, server)


