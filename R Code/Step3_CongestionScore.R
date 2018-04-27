library(corrplot)
library(caret) 
library(AppliedPredictiveModeling)
library(stargazer)
library(ggmap)
library(tidyverse)
library(sf)
library(FNN)
library(data.table)
library(car)
library(spdep)
library(lubridate)
library(rgdal)
library(sp)
library(dplyr)
library(shiny)
library(leaflet)

setwd("G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working")
#load("G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working/Regression.RData")

subwayStation <- read.csv("NYC_Transit_Subway_Entrance_And_Exit_Data.csv")

# Change all values to upper case text for join later
MTAStation <- mutate_all(subwayStation, funs(toupper)) %>%
  mutate(Longitude = as.numeric(subwayStation$Station.Longitude),
         Latitude = as.numeric(subwayStation$Station.Latitude))

# Modify station name for join later
MTAStation <- MTAStation %>% 
  mutate(Station.Name = gsub('TH', '', Station.Name)) %>%
  mutate(Station.Name = gsub('RD', '', Station.Name)) %>%
  mutate(Station.Name = gsub('ND', '', Station.Name))

# Factor 1: Number of subway lines running at the station
MTAStationEntrances <- MTAStation
MTAStation <- MTAStation[!duplicated(MTAStation$Station.Name),]

wideformat <- MTAStation %>%
  select(Station.Name, Route1, Route2, Route3, Route4, Route5, Route6, Route7, Route8, Route9, Route10, Route11)

tallformat <- gather(wideformat, route, line, Route1:Route11, factor_key=TRUE)
tallformat <- na.omit(tallformat)
tallformat <- tallformat[!apply(tallformat, 1, function(x) any(x=="")),] 

count_line <- tallformat %>%
  group_by(Station.Name) %>%
  summarise(N_Lines = n())

# Factor 2: Number of entrances
count_entrance <- MTAStationEntrances %>%
  group_by(Station.Name) %>%
  summarise(N_Entrances = n())

# Factor 3: Number of turnstiles
allturnstile <- turnstileJulAug[!duplicated(turnstileJulAug$serial),]

count_turnstiles <- allturnstile %>%
  group_by(Station) %>%
  summarise(N_Turnstiles = n())

# Join factor 1 & 2
countDF <- left_join(count_line,count_entrance)
# Join filtered MTA stations with factor 3
countDF2 <- left_join(MTAStation2,count_turnstiles)

# Again...Station names are messy...
#write.csv(countDF, file="countDF.csv")
#write.csv(countDF2, file="countDF2.csv")

# Write modified file back
countDF <- read.csv("countDF.csv")
countDF2 <- read.csv("countDF2.csv")

# Join all three counts
finalcount <- left_join(countDF2,countDF)

# Join these counts with predicted vars data
joincount <- left_join(vars.predict, finalcount, by="Station")
congestionCalc <- joincount[,c(1:3,6:7,17:18,48:51,54:56)]

# Claculate factors used to calculate congestion score
congestionCalc <- congestionCalc %>%
  mutate(Longitude = congestionCalc$lng,
         Latitude = congestionCalc$lat,
         TrnstPerE = N_Turnstiles/N_Entrances,
         LinesPerE = N_Lines/N_Entrances,
         EntriesPerT = pred/N_Turnstiles,
         EntriesPerL = pred/N_Lines,
         EntriePerE = pred/N_Entrances)
congestionCalc <- st_as_sf(congestionCalc, coords = c("Longitude", "Latitude"), crs = 4326)

### Recode
# From 1 to 5, not congested to congested
# Factor 1: Weekday vs. Weekend (6,7,1 - score 5, other - score 1)
congestionCalc$s_weekday<-recode(congestionCalc$Weekday,"1=5;6:7=5;else=1")

# Factor 2: Rush hour vs. Not rush hour (1 - score 5, 0 - score 1)
congestionCalc$s_rushhr<-recode(congestionCalc$Rush_Hour,"1=5;else=1")

# Factor 3: Predicted net entries
quantile(congestionCalc$pred, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_pred<-recode(congestionCalc$pred,
                              "-945:384.1818=1; 384.1818:918.1196=2;
                              918.1196:1517.5255=3; 1517.5255:2686.9403=4;
                              2686.9403:332742=5")

# Factor 4: Net entries per turnstile
quantile(congestionCalc$EntriesPerT, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_entTrnst<-recode(congestionCalc$EntriesPerT,
                              "-300:36.34934=1; 36.34934:95.08506=2;
                              95.08506:165.33101=3; 165.33101:269.55590=4;
                              269.55590:15789=5")

# Factor 5: Net entries per line
congestionCalc$EntriesPerL[is.na(congestionCalc$EntriesPerL)] <- 0
quantile(congestionCalc$EntriesPerL, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_entLine<-recode(congestionCalc$EntriesPerL,
                                  "-945:215.3992=1; 215.3992:552.4522=2;
                                  552.4522:957.3827=3; 957.3827:1660.4129=4;
                                  1660.4129:157887=5")

# Factor 6: Net entries per entrance
congestionCalc$EntriePerE[is.na(congestionCalc$EntriePerE)] <- 0
quantile(congestionCalc$EntriePerE, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_entEnt<-recode(congestionCalc$EntriePerE,
                                 "-899:74.18413=1; 74.18413:191.43871=2;
                                 191.43871:341.49409=3; 341.49409:612.21923=4;
                                 612.21923:39472=5")

# Factor 7: Turnstiles per entrance
congestionCalc$TrnstPerE[is.na(congestionCalc$TrnstPerE)] <- 0
quantile(congestionCalc$TrnstPerE, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_trnstEnt<-recode(congestionCalc$TrnstPerE,
                                "0:1.5=1; 1.5:1.833333=2;
                                1.833333:2.4=3; 2.4:3.166667=4;
                                3.166667:27=5")

# Factor 8: Lines per entrance
congestionCalc$LinesPerE[is.na(congestionCalc$LinesPerE)] <- 0
quantile(congestionCalc$LinesPerE, c(0, .2, .4, .6, .8, 1))
congestionCalc$s_lineEnt<-recode(congestionCalc$LinesPerE,
                                  "0:0.2=1; 0.2:0.25=2;
                                  0.25:0.4705882=3; 0.4705882:0.75=4;
                                  0.75:5=5")

# Weight these 8 factors and calculate the final congestion score
# Rank of importance: 
# s_entLine - 5
# s_weekday - 4 
# s_entTrnst & s_entEnt - 3
# s_trnstEnt & s_lineEnt & s_rushhr - 2
# s_pred - 1

congestionCalc <- congestionCalc %>%
  mutate(CongestionScore = 5*s_entLine + 4*s_weekday + 3*(s_entTrnst+s_entEnt) +
           2*(s_trnstEnt+s_lineEnt+s_rushhr) + s_pred)

quantile(congestionCalc$CongestionScore, c(0, .1, .4, .6, .9, 1))

congestionCalc$alert<-recode(congestionCalc$CongestionScore,
                                 "22:37='Congestion Level 1'; 37:60='Congestion Level 2';
                                 60:70='Congestion Level 3'; 70:87='Congestion Level 4';
                                 87:110='Congestion Level 5'")

# Write to shapefile for creating Shiny app
st_write(congestionCalc, "CongestionScore.shp")

# Try make a leaflet map first
CongestionScoreCalc <- st_read("CongestionScore.shp")
MTACongestion <- CongestionScoreCalc[,c(1:2,4:5,3,10,27:28,8,9)]
colnames(MTACongestion)[colnames(MTACongestion) == 'pred'] <- 'Predicted.Net.Entries'
colnames(MTACongestion)[colnames(MTACongestion) == 'CngstnS'] <- 'Congestion.Score'
colnames(MTACongestion)[colnames(MTACongestion) == 'alert'] <- 'Congestion.Level'

MTACongestion.filtered <- filter(MTACongestion, Date == "7/1/2017")
MTACongestion.filtered <- filter(MTACongestion.filtered , Hour == 20|Hour == 21)

paletteFactor <- colorFactor(c("#41b6c4","#57C513","#F7E015","#F97F05","#CE0A0C"), 
                             MTACongestion.filtered$alert)

leaflet(MTACongestion.filtered) %>%
  #addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  setView(lng = -73.92291, lat = 40.70661, zoom = 11) %>%
  addCircleMarkers(data=MTACongestion.filtered,
                   lng = ~lng,
                   lat = ~lat,
                   radius = 6,
                   fillOpacity = 1,
                   fillColor = ~paletteFactor(alert),
                   color = "white",
                   opacity = 0.2,
                   stroke=T,
                   weight = 7,
                   #popup= ~paste0("Date: ",month,"/",day),
                   label = ~Station) %>%
  addLegend(pal = paletteFactor, 
            values = ~alert, 
            position = "bottomright", 
            title = "Congestion Level",
            opacity = 1)

#************************* For me to export the app online only *************************#
rsconnect::setAccountInfo(name='yinuoyin',token='201FBBE62B6C153502BA5CBC8BE51DA7',secret='LSnN5Y60LaV7Rhbkd7xhj+dirtFx+yRlPV65yUG7')
rsconnect::deployApp('G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working/ShinyApp')
#****************************************************************************************#
