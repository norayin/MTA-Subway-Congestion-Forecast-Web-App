library(shiny)
library(maps)
library(dplyr)
library(leaflet)
library(DT)

load("PredictionResult.RData")

shinyServer(function(input, output, session) {
  
  # Select input
  output$selectMonth <- renderUI({
    monthlist <- arrange(MTACongestion, Month)
    monthlist <- unique(monthlist$Month)
    selectInput("month", "Select a month", as.list(monthlist))
  })
  
  output$selectDay <- renderUI({
    
    if(is.null(input$month)) {
      MTACongestion.filtered <- filter(MTACongestion, Month == 7)
    } else {
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month)  
    }
    
    daylist <- arrange(MTACongestion.filtered, Day)
    daylist <- unique(daylist$Day)
    
    selectInput("day", "Select a day", as.list(daylist))
  })
  
  output$selectHour <- renderUI({
    
    if(is.null(input$month) || is.null(input$day)) {
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1)
    } else {
      MTACongestion.filtered <- filter(MTACongestion, 
                                       Month == input$month & Day == input$day) 
    }
    
    hourlist <- arrange(MTACongestion.filtered, Hour)
    hourlist <- unique(hourlist$Hour)
    
    selectInput("hour", "Select an hour", as.list(hourlist))
  })
  
  output$selectStation <- renderUI({
    
    if(is.null(input$month) || is.null(input$day) || is.null(input$hour)) {
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1 & Hour == 0)
    } else {
      MTACongestion.filtered <- filter(MTACongestion, 
                                       Month == input$month & Day == input$day & Hour == input$hour) 
    }
    
    stationlist <- arrange(MTACongestion.filtered, Station)
    stationlist <- unique(stationlist$Station)
    
    selectInput("station", "Select a station", as.list(stationlist))
  })
  
  # Output 1 - table
  output$table <- renderDataTable({
    
    if(is.null(input$month) && is.null(input$day) && is.null(input$hour)) {
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1 & Hour == 0)
    } else if (!is.null(input$month) && is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == 1 & Hour == 0)
    } else if (!is.null(input$month) && !is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == input$day & Hour == 0)
    } else if (!is.null(input$month) && is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == 1 & Hour == input$hour)
    } else if (is.null(input$month) && !is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == input$day & Hour == 0)
    } else if (is.null(input$month) && !is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == input$day & Hour == input$hour)
    } else if (is.null(input$month) && is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1 & Hour == input$hour)
    } else if (!is.null(input$month) && !is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, 
                                       Month == input$month & Day == input$day & Hour == input$hour)
    }
    
    return(as.data.frame(MTACongestion.filtered) %>%
             select(-geometry))
    
  })
  
  # Set map center and zoom level
  setview <- reactiveValues(longitude = -73.92291, latitude = 40.70661, zoomlevel = 11)
  
  observeEvent(input$recalc, {
    if(!is.null(input$station)){
      getStation <- filter(MTACongestion, Station == input$station)
      getStation <- getStation[1,]
      setview$longitude <- 0
      setview$longitude <- setview$longitude + getStation$lng
      setview$latitude <- 0
      setview$latitude <- setview$latitude + getStation$lat
      setview$zoomlevel <- 16
    } else {
      setview$longitude <- setview$longitude
      setview$latitude <- setview$latitude
      setview$zoomlevel <- setview$zoomlevel
    }
  })
  
  # Output 2 - map
  output$leaf=renderUI({
    leafletOutput('mymap', width = "100%", height = 500)
  })
  
  output$mymap <- renderLeaflet({
    
    if(is.null(input$month) && is.null(input$day) && is.null(input$hour)) {
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1 & Hour == 0)
    } else if (!is.null(input$month) && is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == 1 & Hour == 0)
    } else if (!is.null(input$month) && !is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == input$day & Hour == 0)
    } else if (!is.null(input$month) && is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == input$month & Day == 1 & Hour == input$hour)
    } else if (is.null(input$month) && !is.null(input$day) && is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == input$day & Hour == 0)
    } else if (is.null(input$month) && !is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == input$day & Hour == input$hour)
    } else if (is.null(input$month) && is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, Month == 7 & Day == 1 & Hour == input$hour)
    } else if (!is.null(input$month) && !is.null(input$day) && !is.null(input$hour)){
      MTACongestion.filtered <- filter(MTACongestion, 
                                       Month == input$month & Day == input$day & Hour == input$hour)
    }

    paletteFactor <- colorFactor(c("#41b6c4","#57C513","#F7E015","#F97F05","#CE0A0C"), 
                                 MTACongestion.filtered$Congestion.Level)
    
    leaflet(MTACongestion.filtered) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = setview$longitude, lat = setview$latitude, zoom = setview$zoomlevel) %>%
      addCircleMarkers(data=MTACongestion.filtered,
                       lng = ~lng,
                       lat = ~lat,
                       radius = 6,
                       fillOpacity = 1,
                       fillColor = ~paletteFactor(Congestion.Level),
                       color = "white",
                       opacity = 0.2,
                       stroke=T,
                       weight = 7,
                       popup= ~paste0("Congestion Score: ",Congestion.Score),
                       label = ~Station) %>%
      addLegend(pal = paletteFactor, 
                values = ~Congestion.Level, 
                position = "bottomright", 
                title = "Congestion Level",
                opacity = 1)
  })
})