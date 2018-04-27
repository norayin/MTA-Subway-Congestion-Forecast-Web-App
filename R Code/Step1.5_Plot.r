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

load("G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working/MyData2.RData")

myTheme <- function() {
  theme_void() + 
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 20, color = "#FEFFEF", hjust = 0.5, vjust = 0, face = "bold"), 
      plot.subtitle = element_text(size = 16, color = "#FEFFEF", hjust = 0.5, vjust = 0),
      axis.ticks = element_blank(),
      #panel.grid.major = element_line(colour = "gray95"),
      #panel.background = element_rect(fill = "gray95"),
      plot.background = element_rect(fill = "black", color="white"),
      panel.border = element_rect(colour = "grey", fill=NA, size=0),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
      legend.key.height = unit(0.4, "cm"), legend.key.width = unit(0.8, "cm"),
      legend.title = element_text(size = 14, color = "#FEFFEF", hjust = 0.5, vjust = 0, face = "bold"),
      legend.text = element_text(size = 12, color = "#FEFFEF", hjust = 0.5, vjust = 0),
      legend.direction = "horizontal", 
      legend.position = "bottom"
    )
}

#******************************* Map 1: subway locations ************************#
# Plot subway station on NYC map
# Set up basemap
baseMap <- get_map(location = c(lon = -73.9402338, lat = 40.7422241),
                   zoom = 11, 
                   maptype= 'toner')
# Plot
ggmap(baseMap) + 
  geom_point(data= MTAStation2,
             aes(x = Longitude, y = Latitude),
             colour = "orange",
             size = 2) +
  labs(title="MTA SUBWAY STATIONS",
       subtitle = "New York City, NY\n ") +
  myTheme()
#********************************************************************************#
#******************************* Map 2: total net entries by subway station  ************************#
totentries <- vars %>%
  group_by(Station) %>%
  summarise(Tot_Entries = sum(NetEntries))
totentries <- filter(totentries, Tot_Entries > 10000)
totentries <- left_join(totentries, MTAStation2)
totentries <- totentries %>%
  mutate(lng = totentries$Longitude,
         lat = totentries$Latitude) %>%
  arrange(Tot_Entries)
totentries <-  st_as_sf(totentries, coords = c("Longitude", "Latitude"), crs = 4326)

ggmap(baseMap) + 
  geom_point(data = totentries, aes(x=lng, y=lat, color=factor(ntile(Tot_Entries,5))), size = 2) +
  # Add title and subtital
  labs(title="Total Entries by Station July & August 2017",
       subtitle = "New York, NY\n ") +
  #scale_colour_viridis(option = "magma", name = "Ave. Price Per SQFT ($)  ", direction = -1) +
  #scale_colour_gradient(low = "white", high = "red", name = "Ave. Price Per SQFT ($)  ") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(totentries$Tot_Entries,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Total Entries") +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") +
  myTheme()
#********************************************************************************#
#***************** Map 3: Total entries at 34 ST-PENN STA  ***************#
# on the first week of July
pennsta <- filter(vars, Station == "34 ST-PENN STA")
pennsta$Day <- as.numeric(pennsta$Day)
pennstaJuly1 <- filter(pennsta, Month == 7 & Day < 8)
pennstaJuly1 <- pennstaJuly1 %>%
  group_by(Day) %>%
  summarise(Tot_Entries = sum(NetEntries))

# on July 4th, by hour
pennstaJ4th <- filter(pennsta, Month == 7 & Day == 4)

# by day
pennstaJuly <- filter(pennsta, Month == 7)
pennstaJuly <- pennstaJuly %>%
  group_by(Day) %>%
  summarise(Tot_Entries = sum(NetEntries))

pennstaJuly1 <- within(pennstaJuly1, Day[Day == 1] <- "Monday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 2] <- "Tuesday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 3] <- "Wednesday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 4] <- "Thursday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 5] <- "Friday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 6] <- "Saturday")
pennstaJuly1 <- within(pennstaJuly1, Day[Day == 7] <- "Sunday")
pennstaJuly1 <- pennstaJuly1 %>%
  mutate(valuelabel = as.factor(pennstaJuly1$Tot_Entries))

plotTheme <- theme(legend.position="none",
                   text = element_text(size = 12),
                   axis.text=element_text(size=12, color = "#BEBEBE", face="bold"),
                   plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
                   plot.title = element_text(colour="white", size=18, face ="bold"),
                   plot.subtitle = element_text(colour="white", size=14),
                   axis.title.x=element_text(size=12, color = "white", face="bold"), 
                   axis.title.y=element_text(size=12, color = "white", face="bold"),
                   panel.background = element_rect(fill = "black"),
                   plot.background = element_rect(fill = 'black'))

# plot by day
pennstaJuly <- within(pennstaJuly, Tot_Entries[Tot_Entries == -2863722] <- 105121)

ggplot(data=pennstaJuly, aes(x=Day, y=Tot_Entries, group=1)) +
  geom_line(color="#09AEF2",size=2) +
  scale_y_continuous(name="Total Entries", labels = scales::comma) +
  scale_x_discrete(name="Day of the Month",limits=c(1:31))+ 
  theme_minimal() + 
  plotTheme +
  ggtitle("Total Entries at 34 St-Penn Station, July, 2017")

# plot first week
ggplot(data=pennstaJuly1, aes(x=Day, y=Tot_Entries, group=1)) +
  geom_line(color="#6DCF00",size=2) +
  scale_y_continuous(name="Total Entries", labels = scales::comma) +
  scale_x_discrete(name="Day of the Week", 
                   limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+ 
  theme_minimal() + 
  plotTheme +
  ggtitle("Total Entries at 34 St-Penn Station, July 1th - 7th, 2017")

# plot by hour
ggplot(data=pennstaJ4th, aes(x=Hour, y=NetEntries, group=1)) +
  geom_line(color="#FCC000",size=2) +
  scale_y_continuous(name="Total Entries", labels = scales::comma) +
  scale_x_discrete(name="Hour of the Day", 
                   limits=c("0","4","8","12","16","20"))+ 
  theme_minimal() + 
  plotTheme +
  ggtitle("Total Entries at 34 St-Penn Station, July 4th, 2017")

#********************************************************************************#
#******************************* Map 4: proximity factors  ************************#
# distance to school
plotschool <- left_join(joincensus,MTAStation2)
plotschool <- plotschool %>%
  mutate(d_school = round(d_school, digits = 4))

ggmap(baseMap) + 
  geom_point(data = plotschool, aes(x=Longitude, y=Latitude, color=factor(ntile(d_school,5))), size = 3) +
  # Add title and subtital
  labs(title="Distance to School by MTA Station",
       subtitle = "New York, NY\n ") +
  #scale_colour_viridis(option = "magma", name = "Ave. Price Per SQFT ($)  ", direction = -1) +
  #scale_colour_gradient(low = "white", high = "red", name = "Ave. Price Per SQFT ($)  ") +
  scale_colour_manual(values = c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404"),
                      labels=as.character(quantile(plotschool$d_school,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="Total Entries") +
  theme(legend.direction = "horizontal", 
        legend.position = "bottom") +
  myTheme()
#********************************************************************************#
#******************************* Map 5: census factor  ************************#
# distance to school
plotschool <- left_join(joincensus,MTAStation2)
plotschool <- plotschool %>%
  mutate(d_school = round(d_school, digits = 4))

ggplot() +
  geom_sf(data = census, aes(fill = NLCDDevPt), lwd = 0) + 
  scale_fill_viridis(discrete = FALSE, direction = 1, option="viridis", name = "% of Development") +
  labs(title = 'PERCENTAGE OF DEVELOPMENT (ESRI)',
       subtitle = "By Census Block Group, New York, NY\n") +
  theme(panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  myTheme()
#********************************************************************************#
#******************************* Map 6: temperature & entries  ************************#
tempEn <- filter(vars, Month == 7 & NetEntries >-1 & NetEntries < 40001)
tempEn <- tempEn %>%
  arrange(NetEntries)

# Scatterplot 
ggplot(tempEn, aes(x=TEMP_C, y=NetEntries)) + 
  geom_point(col="#74a9cf", size=1) + 
  geom_smooth(method="loess", se=F,size=2,color="#f03b20") + 
  xlim(c(15, 35)) + 
  ylim(c(0, 40000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(subtitle="Correlation scatterplot", 
       y="Net Entries",
       x="Temperature", 
       title="Net Entries Vs Temperature") +
  plotTheme

#********************************************************************************#
#******************************* Map 7: taxi trips  ************************#
taxiride <- filter(vars, NetEntries >-1 & NetEntries < 40001)
taxiride <- taxiride %>%
  arrange(NetEntries) %>%
  mutate(TT = taxiride$TaxiTrips + taxiride$TaxiTrips2)

# Scatterplot 
ggplot(taxiride, aes(x=TaxiTrips2, y=NetEntries)) + 
  geom_point(col="#cccccc", size=1) + 
  geom_smooth(method="loess", se=F,size=2,color="#f03b20") + 
  xlim(c(1500, 24000)) + 
  ylim(c(0, 40000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(subtitle="Correlation scatterplot", 
       y="Net Entries",
       x="Taxi Trips", 
       title="Net Entries Vs Taxi Trips") +
  plotTheme

#********************************************************************************#
#******************************* Map 8: correlation  ************************#
vars$BikeTrips[is.na(vars$BikeTrips)] <- 0

unlist(lapply(vars, class))
bizCor <- vars[,c(11:14,16,17:18,22:37,39:40,45,53)]
M <- cor(bizCor)
corrplot(M, method = "number")

corrplot(M, method = "ellipse", order="hclust", type="lower", 
         tl.cex=0.75, add = FALSE, tl.pos="lower") 
corrplot(M, method = "number", order="hclust", type="upper", 
         tl.cex=0.75, add = TRUE, tl.pos="upper")

#********************************************************************************#
#*********************** Map 9: Average Ridership Each Day of the Week  ************************#
Day_of_Week_Filtered <- vars %>%
  arrange(NetEntries)
Day_of_Week_Filtered <- filter(vars, NetEntries >-1 & NetExits > -1 & NetEntries < 100000)
Day_of_Week_Filtered <- Day_of_Week_Filtered %>%
  arrange(NetEntries)

Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday== 7] <- "Monday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 1] <- "Tuesday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 2] <- "Wednesday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 3] <- "Thursday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 4] <- "Friday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 5] <- "Saturday")
Day_of_Week_Filtered <- within(Day_of_Week_Filtered, Weekday[Weekday == 6] <- "Sunday")

Day_of_Week_Filtered <- Day_of_Week_Filtered %>%
  group_by(Date, Weekday) %>%
  summarise(entries = sum(NetEntries)) %>%
  ungroup() %>%
  group_by(Weekday) %>%
  mutate(total_days = n(),
         avg_entries_per_day = sum(entries) / mean(total_days)) %>%
  select(Weekday, total_days, avg_entries_per_day) %>%
  distinct(Weekday, total_days, avg_entries_per_day)

ggplot(Day_of_Week_Filtered, aes(Weekday, avg_entries_per_day, fill = Weekday)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_y_continuous(labels=scales::comma) + 
  scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  guides(fill = FALSE) + 
  xlab("") + ylab("Total Entries") + 
  ggtitle("Average Ridership \nEach Day of the Week") +
  plotTheme

#********************************************************************************#
#******************* Map 10: problematic dependent variable ******************#
hist(vars$NetEntries)
#********************************************************************************#
#*********************** Map 11: Average Ridership Hours of the Day  ************************#

Day_of_Week_hour <- filter(vars, NetEntries >-1 & NetExits > -1 & NetEntries < 100000)

Day_of_Week_hour <- Day_of_Week_hour %>%
  group_by(Hour) %>%
  summarise(total_entries = sum(NetEntries),
            n = n(),
            normalized = total_entries / n)

Day_of_Week_hour1 <- filter(Day_of_Week_hour, Hour==1|Hour==5|Hour==9|Hour==13|Hour==17|Hour==21)
Day_of_Week_hour2 <- filter(Day_of_Week_hour, Hour==0|Hour==4|Hour==8|Hour==12|Hour==16|Hour==20)

ggplot(Day_of_Week_hour2, aes(x = Hour, y = normalized,
                             fill = Hour)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_discrete(limits=c("0","4","8","12","16","20"))+
  scale_fill_brewer(palette = 'Set2') +
  guides(fill = FALSE) +
  xlab("") + ylab("Total Entries") + 
  ggtitle("Average Ridership for each 4-hour Time Period") +
  plotTheme

#********************************************************************************#
#*********************** Map 12: Average Ridership Day of Week and Hour  ************************#

Day_of_Week_And_Hour <- filter(vars, NetEntries >-1 & NetExits > -1 & NetEntries < 100000)

Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday== 7] <- "Monday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 1] <- "Tuesday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 2] <- "Wednesday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 3] <- "Thursday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 4] <- "Friday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 5] <- "Saturday")
Day_of_Week_And_Hour <- within(Day_of_Week_And_Hour, Weekday[Weekday == 6] <- "Sunday")

Day_of_Week_And_Hour <- Day_of_Week_And_Hour %>%
  group_by(Weekday, Hour) %>%
  summarise(total_entries = sum(NetEntries), 
            n = n(),
            normalized = total_entries / n)

Day_of_Week_And_Hour$Weekday = factor(Day_of_Week_And_Hour$Weekday, 
                levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
Day_of_Week_And_Hour1 <- filter(Day_of_Week_And_Hour, Hour==1|Hour==5|Hour==9|Hour==13|Hour==17|Hour==21)
Day_of_Week_And_Hour2 <- filter(Day_of_Week_And_Hour, Hour==0|Hour==4|Hour==8|Hour==12|Hour==16|Hour==20)

ggplot(Day_of_Week_And_Hour1, aes(Hour, normalized, fill = Hour)) +
  geom_bar(stat = 'identity') +
  facet_grid(Weekday ~ .) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(limits=c("1","5","9","13","17","21"))+
  scale_fill_brewer(palette = 'Set2') +
  theme(legend.position = 'None',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "#626262")) +
  xlab("") + ylab("Total Entries") +
  plotTheme

ggplot(Day_of_Week_And_Hour2, aes(Hour, normalized, fill = Hour)) +
  geom_bar(stat = 'identity') +
  facet_grid(Weekday ~ .) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(limits=c("0","4","8","12","16","20"))+
  scale_fill_brewer(palette = 'Set2') +
  theme(legend.position = 'None',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "#626262")) +
  xlab("") + ylab("Total Entries") +
  plotTheme

#********************************************************************************#
#*********************** Map 13: Entried by Station  ************************#
stations <- filter(vars, NetEntries >-1 & NetExits > -1 & NetEntries < 100000)
stations <- stations %>%
  group_by(Station) %>%
  summarise(total_entries = sum(NetEntries)) %>%
  arrange(-total_entries)

ggplot(stations[1:50,], aes(x = reorder(Station, -total_entries), total_entries)) +
  geom_bar(stat = 'identity', fill = '#cbc9e2', color = 'black') +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma) + 
  xlab("Station Name") + ylab("Total Entries") +
  ggtitle("Total Entries for each Station - Top 50 Stations") +
  theme(legend.position = 'None',
        panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme
#********************************************************************************#
#*********************** Map 14: A Leaflet Map  ************************#
library(leaflet)
library(rgdal)
library(htmlwidgets)

forleaflet <- filter(vars, NetEntries >-1 & NetExits > -1 & NetEntries < 100000 & 
                       Month == 8 & Day ==17)
forleaflet <- filter(forleaflet, Hour == 16|Hour ==17)
forleaflet = forleaflet[!duplicated(forleaflet$Station),]

# Leaflet includes four "palette functions" for convenience.
#   colorNumeric():   continuous input, continuous colors
#   colorBin():       continuous input, discrete colors (pre defined bins)
#   colorQuantile():  continuous input, discrete colors (quantiles)
#   colorFactor():    categorical input, discrete colors

# Each palette function takes two required inputs: palette and domain
# palette can be:
#   1. The name of a Viridis or ColorBrewer palette (e.g. "magma", "inferno", "RdYlBu", "Greens")
#   2. A vector of colors, e.g. c("black", "grey", "#FFFFFF")

# personBins <- c(0, 1, 2, 5, 10, 20, Inf)
# paletteBins <- colorBin(palette = "OrRd", accidentpoints$persons, bins=-personBins)
# paletteFactor <- colorFactor(c("blue","green","yellow"), accidentpoints$drunk_dr)
# paletteContinuous <- colorNumeric(palette = "magma", domain = accidentpoints$latitude + accidentpoints$longitud)

palette <- colorQuantile("viridis", forleaflet$NetEntries, n = 6)

leaflet(forleaflet) %>%
  #addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(data=forleaflet,
                   lng = ~Longitude,
                   lat = ~Latitude,
                   radius = 4,
                   fillOpacity = 1,
                   fillColor = ~palette(NetEntries),
                   #fillColor = ~paletteFactor(drunk_dr),
                   #fillColor = ~paletteContinuous(latitude + longitud),
                   color = "white",
                   #opacity = 1,
                   stroke=F,
                   #stroke=T,
                   #weight = 1,
                   #popup= ~paste0("Date: ",month,"/",day),
                   label = ~Station) %>%
  addLegend(pal = palette, 
            values = ~NetEntries, 
            position = "bottomright", 
            title = "Congestion Level",
            opacity = 1)


#********************************************************************************#