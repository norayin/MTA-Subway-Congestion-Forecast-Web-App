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

setwd("G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working")
load("G:/Yinuo Yin/Study/Upenn/2018 Spring/LARP 745/MTA-Subway-Congestion/00Working/Regression.RData")

myvars <- read.csv("variables.csv")
myvars <- myvars %>%
  mutate(lat = Latitude,
         lng = Longitude)
myvars <- st_as_sf(myvars, coords = c("Longitude", "Latitude"), crs = 4326)
myvars <- myvars %>%
  select(-X)

############################  BEFORE REGRESSION  ########################

# Check dv distribution
hist(myvars$NetEntries)

# Re-arrange by dv
arrange_myvars <- myvars %>%
  arrange(NetEntries)

### Deal with problematic data
# Remove Net Entries < 0 and Net Exits < 0
filter_vars <- filter(myvars, NetEntries > -1 & NetExits > -1)
# Remove weirdly high number of net entries
filter_vars <- filter(filter_vars, NetEntries < 500000)

hist(filter_vars$NetEntries)

# Check how many zeros I have
zeros <- filter(filter_vars, NetEntries == 0)
# Which station has most zeros?
count_zero <- zeros %>%
  group_by(Station) %>%
  summarise(zeros = n()) %>%
  arrange(-zeros)
# Only keep stations with less than 10 zero Net Entries values
keep <- filter(count_zero, zeros < 10) # 262
keep <- as.data.frame(keep) %>%
  select(-zeros, -geometry)
filter_vars <- left_join(keep, filter_vars) # 105620

# DV is still not normally distributed
hist(filter_vars$NetEntries)
# When take log, it is
hist(log(filter_vars$NetEntries))

# Lots of NAs for independent variable BikeTrips
filter_vars$BikeTrips[is.na(filter_vars$BikeTrips)] <- 0

# Plot correlation matrix
unlist(lapply(filter_vars, class))
bizCor <- filter_vars[,c(9:11, 14:15,20:35,38:40,43,51)]
M <- cor(bizCor)
corrplot(M, method = "number")

# Based on the plot, modify vars if correlation > 0.8
# - Combine two taxi trips value
# - Delete d_hospital, d_cbd, d_college

filter_vars2 <- filter_vars %>%
  mutate(TaxiTrips = filter_vars$TaxiTrips + filter_vars$TaxiTrips2) %>%
  select(-d_hospital, -d_cbd, -d_college, -TaxiTrips2)

forRegression <- as.data.frame(filter_vars2) %>%
  select(Station, Date, Hour, NetEntries, Month, Day, HUMIDITY, PRESSURE, WIND_SPEED,
         TEMP_C, lag_NEntries, Weekday, Event_Count, Event_Count2, TaxiTrips, BikeTrips,
         d_plazamalls, d_recreation, d_parkinglot, d_office, d_subway, d_parks, d_busstop,
         d_school, POPDENS_CY, MEDHINC_CY, TOTHU_CY, S01_BUS, NLCDDevPt, lat, lng) %>%
  mutate(Month = as.factor(filter_vars2$Month),
         Hour = as.factor(filter_vars2$Hour),
         Day = as.factor(filter_vars2$Day),
         Weekday = as.factor(filter_vars2$Weekday))

# Lastly, get rid of negative values in lagged values
forRegression <- filter(forRegression, lag_NEntries > -1)
filter_vars2 <- filter(filter_vars2, lag_NEntries > -1 & lag_NExits > -1)

############################  IN-SAMPLE REGRESSION  ########################
# Only Station and Date
model1 <- lm(NetEntries ~ Station+Date, data = forRegression )
summary(model1)

# Station, Date, Hour
model2 <- lm(NetEntries ~ Station+Date+Hour, data = forRegression )
summary(model2)

# Add weather
model3 <- lm(NetEntries ~ Station+Date+Hour+HUMIDITY+PRESSURE+WIND_SPEED+TEMP_C, 
             data = forRegression )
summary(model3)

# Add census
model4 <- lm(NetEntries ~ Station+Date+Hour+NLCDDevPt++POPDENS_CY+MEDHINC_CY+TOTHU_CY+S01_BUS, 
             data = forRegression )
summary(model4)$r.squared

# Add time lag
model5 <- lm(NetEntries ~ Station+Date+Hour+lag_NEntries, 
             data = forRegression )
summary(model5)$r.squared

# Add trips and events
model6 <- lm(NetEntries ~ Station+Date+Hour+BikeTrips+TaxiTrips+Event_Count+Event_Count2, 
             data = forRegression )
summary(model6)$r.squared

# All variable 
model7 <- lm(NetEntries ~ ., data = as.data.frame(filter_vars2) %>% 
               select(-lat,-lng,-geometry))
summary(model7)$r.squared

# For the sake of test and training dataset, exnclude hour and station
model8 <- lm(NetEntries ~ ., data = as.data.frame(filter_vars2) %>% 
               select(-lat,-lng, -geometry, -Station, -Hour))
summary(model8)$r.squared

# Create model comparison table
regression_table <- data.frame()
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 1, 
                                     "Description" = "Only Station and Date", 
                                     "Adj_R_Squared" = summary(model1)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 2, 
                                     "Description" = "Station, Date, Hour", 
                                     "Adj_R_Squared" = summary(model2)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 3, 
                                     "Description" = "Add weather", 
                                     "Adj_R_Squared" = summary(model3)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 4, 
                                     "Description" = "Add census", 
                                     "Adj_R_Squared" = summary(model4)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 5, 
                                     "Description" = "Add time lag", 
                                     "Adj_R_Squared" = summary(model5)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 6, 
                                     "Description" = "Add trips and events", 
                                     "Adj_R_Squared" = summary(model6)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 7, 
                                     "Description" = "Include all prepared varibles", 
                                     "Adj_R_Squared" = summary(model7)$adj.r.squared))
regression_table <- rbind(regression_table, 
                          data.frame("Model_NO" = 8, 
                                     "Description" = "All prepared variables excluding Station and Hour", 
                                     "Adj_R_Squared" = summary(model8)$adj.r.squared))

# Final regression model
reg <- lm(NetEntries ~ ., data = as.data.frame(filter_vars2) %>% 
               select(-lat,-lng, -geometry, -Station, -Hour))
summary(reg)$r.squared

regFullset <-
  data.frame(observed = filter_vars2$NetEntries,
             predicted = reg$fitted.values,
             error = filter_vars2$NetEntries - reg$fitted.values) 

#******************** Plot 1: Predicted vs Observed *******************#
#Predicted net entries as a function of observed entries
ggplot(data = regFullset %>% filter(predicted < 200000 & observed <200000), aes(x = observed, y = predicted )) +
  geom_point(color="#F7DC6F", size = 1, shape = 1) +
  labs(title="PREDICTIED NET ENTRIES AS A FUNCTION OF OBSERVED NET ENTRIES",
       subtitle="Full Dataset Prediction Result",
       x="Observed",
       y="Predicted") +   
  geom_smooth(stat = 'smooth',method = lm, color = "#F39C12") +
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

#******************** Plot 2: Standardized Coefficient *******************#
library(QuantPsyc)

standardized <- as.data.frame(lm.beta(reg))
standardized$variable <- row.names(standardized)
colnames(standardized)[1] <- "std_coefficient"
standardized
standardized2 <- 
  standardized[-c(1,9:25,27), ] 

standardized2$absCoef <- abs(standardized2$std_coefficient)

ggplot(standardized2, aes(x=reorder(variable,-absCoef), y=absCoef, fill=variable)) + 
  geom_bar(stat="identity") +
  labs(title="STANDARDIZED OLS REGRESSION COEFFICIENTS",
       x="Variable",
       y="Absolute Standardized Coefficients") + 
  scale_x_discrete(limits = rev(levels(standardized$variable)))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        axis.title=element_text(size = 14), 
        plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) + 
  guides(fill=guide_legend(title="Variable Name")) +
  plotTheme
#***********************************************************************#


############################  OUT-OF-SAMPLE REGRESSION  ########################
# Generate randomly selected training set and test set
inTrain <- createDataPartition(
  y = filter_vars2$NetEntries, 
  p = .75, list = FALSE)
training <- filter_vars2[ inTrain,] #the training set
test <- filter_vars2[-inTrain,]  #the test set

#Training set regression
regTrain <- lm(NetEntries ~ ., data=training %>% select(-lat,-lng, -geometry, -Station, -Hour))
summary(regTrain)
#Training set goodness of fit metrics and indicators
regTrainingSet <-
  data.frame(observed = training$NetEntries,
             predicted = regTrain$fitted.values)
regTrainingSet <-
  regTrainingSet %>%
  mutate(error = predicted - observed) %>%
  mutate(absError = abs(predicted - observed)) %>%
  mutate(percentAbsError = abs(predicted - observed) / observed)

#Test set regression
regTest <- lm(NetEntries ~ ., data=test %>% select(-lat,-lng, -geometry, -Station, -Hour))
summary(regTest)
#Test set goodness of fit metrics and indicators
regTestSet <-
  data.frame(observed = test$NetEntries,
             predicted = regTest$fitted.values)
regTestSet <-
  regTestSet %>%
  mutate(error = predicted - observed) %>%
  mutate(absError = abs(predicted - observed)) %>%
  mutate(percentAbsError = abs(predicted - observed) / observed)

#Create a table summarizing the goodness of fit indicators calculated above
is.na(regTrainingSet)<-sapply(regTrainingSet, is.infinite)
regTrainingSet[is.na(regTrainingSet)]<-0
is.na(regTestSet)<-sapply(regTestSet, is.infinite)
regTestSet[is.na(regTestSet)]<-0

rsquared_training <- summary(regTrain)$adj.r.squared
rmse_training <- (mean((regTrainingSet$error)^2))^(1/2)
mae_training <- MAE(regTrainingSet$predicted, regTrainingSet$observed)
mape_training <- mean(regTrainingSet$percentAbsError)

rsquared_test <- summary(regTest)$adj.r.squared
rmse_test <- (mean((regTestSet$error)^2))^(1/2)
mae_test <- MAE(regTestSet$predicted, regTestSet$observed)
mape_test <- mean(regTestSet$percentAbsError)

Summary <- data.frame(dataset = c("training","test"),
                      Adjusted_RSquared = c(rsquared_training,rsquared_test),
                      RMSE = c(rmse_training,rmse_test),
                      MAE = c(mae_training,mae_test),
                      MAPE = c(mape_training,mape_test))
head(Summary)

#******************** Plot 3: Training set residuals *******************#
p1 <- ggplot() + geom_histogram(aes(regTrain$residuals), binwidth = 200, fill="darkorange") +
  xlab('Residuals') + ylab('Frequencies') +
  ggtitle('Histogram of Residuals - Training Set') +
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p2 <- ggplot(data = regTrainingSet %>% filter(predicted < 200000 & error < 50000), 
             aes(x = predicted, y = error)) +
  geom_point(color="darkorange", size = 1, shape = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 2, color = 'red') + 
  xlab('Predictions') + ylab('Residuals') + 
  ggtitle('Residuals against Predictions - Training Set')+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p3 <- ggplot(data = regTrainingSet %>% filter(predicted < 200000 & observed < 200000), 
             aes(x = predicted, y = observed)) +
  geom_point(color="darkorange", size = 1, shape = 1) +
  geom_abline(slope = 1, linetype = 'dotted', color = 'red', size = 2) + 
  xlab("Predictions") + ylab("Actual Hourly Entries") + 
  ggtitle("Predictions against Hourly Entries - Training Set")+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p4 <- ggplot() + geom_point(aes(1:nrow(training), regTrain$residuals), data = training,
                            color="darkorange", size = 1, shape = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 2, color = 'red') + 
  xlab('Index') + ylab('Residuals') + 
  ggtitle('Residuals Per Hour Per Station in Sequential Order\n - Training Set')+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

#******************** Plot 4: Test set residuals *******************#
p1_2 <- ggplot() + geom_histogram(aes(regTest$residuals), binwidth = 200, fill="darkorange") +
  xlab('Residuals') + ylab('Frequencies') +
  ggtitle('Histogram of Residuals - Test Set') +
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p2_2 <- ggplot(data = regTestSet %>% filter(predicted < 200000 & error > -30000), 
             aes(x = predicted, y = error)) +
  geom_point(color="darkorange", size = 1, shape = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 2, color = 'red') + 
  xlab('Predictions') + ylab('Residuals') + 
  ggtitle('Residuals against Predictions - Test Set')+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p3_2 <- ggplot(data = regTestSet %>% filter(predicted < 200000 & observed < 200000), 
             aes(x = predicted, y = observed)) +
  geom_point(color="darkorange", size = 1, shape = 1) +
  geom_abline(slope = 1, linetype = 'dotted', color = 'red', size = 2) + 
  xlab("Predictions") + ylab("Actual Hourly Entries") + 
  ggtitle("Predictions against Hourly Entries - Test Set")+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

p4_2 <- ggplot() + geom_point(aes(1:nrow(test), regTest$residuals), data = test,
                            color="darkorange", size = 1, shape = 1) +
  geom_hline(yintercept = 0, linetype = 'dotted', size = 2, color = 'red') + 
  xlab('Index') + ylab('Residuals') + 
  ggtitle('Residuals Per Hour Per Station in Sequential Order\n - Test Set')+
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme

grid.arrange(p1_2, p2_2, p3_2, p4_2, ncol = 2)

#***********************************************************************#
# Check how well our traininig set predict test set
regPred <- predict(regTrain, test)

predictPlot <- data.frame(observed = test$NetEntries,
                          predicted = regPred)

#******************** Plot 5: Test set prediction result *******************#
ggplot(data = predictPlot %>% filter(predicted < 200000 & observed <200000), aes(x = observed, y = predicted)) +
  geom_point(color="#58D68D", size = 1, shape = 1) +
  labs(title="PREDICTIED NET ENTRIES VS. OF OBSERVED NET ENTRIES",
       subtitle="Test Dataset Prediction Result",
       x="Observed",
       y="Predicted") + 
  geom_smooth(stat = 'smooth',method = lm, color = "#044389") +
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme
#***********************************************************************#

############################  CROSS VALIDATION  ########################
fitControl <- trainControl(method = "cv", number = 20)
set.seed(825)

varsCV <- filter_vars2 %>%
  select(-lat,-lng, -geometry, -Station, -Hour)

lmFit <- train(NetEntries ~ ., data = varsCV, 
               method = "lm", 
               trControl = fitControl)
lmFit

CVresults <-
  lmFit$resample %>%
  as.data.frame()
CVresults2 <-
  data.frame(TEST = "CROSS_VALIDATION",
             MEAN_RSQUARED = mean(CVresults$Rsquared),
             STANDARD_DEVIATION_RSQUARED = sd(CVresults$Rsquared))
#******************** Plot 6: CV Results R squared *******************#
# Histogram of CV R-SQUARED
ggplot(CVresults, aes(Rsquared)) + 
  geom_histogram(bins=20,
                 col="grey", 
                 fill="darkorange") +
  labs(title="CROSS-VALIDATION R-SQUARED",
       x="R-Squared Value",
       y="Frequency") + 
  theme(panel.grid.major = element_line(colour = "#626262"),
        panel.grid.minor = element_line(colour = "#626262")) +
  plotTheme
#***********************************************************************#
#******************** Plot 7: time series *******************#
plotTheme2 <- theme(panel.grid.major = element_line(colour = "#626262"),
                    panel.grid.minor = element_line(colour = "#626262"),
                    legend.position="bottom",
                    legend.direction = "horizontal",
                    text = element_text(size = 12),
                    axis.text=element_text(size=12, color = "#BEBEBE", face="bold"),
                    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "in"),
                    plot.title = element_text(colour="white", size=18, face ="bold",hjust = 0.5),
                    plot.subtitle = element_text(colour="white", size=14),
                    axis.title.x=element_text(size=12, color = "white", face="bold"), 
                    axis.title.y=element_text(size=12, color = "white", face="bold"),
                    panel.background = element_rect(fill = "black"),
                    plot.background = element_rect(fill = 'black'),
                    legend.background = element_rect(fill = 'black'),
                    legend.box.background = element_rect(fill = 'black'),
                    legend.text = element_text(color = "white", face="bold"))

vars.predict <- filter_vars2 %>% 
  mutate(pred = reg$fitted.values)

timeplot_July <- filter(vars.predict, Month == 7)
timeplot_August <- filter(vars.predict, Month == 8)

calc <- timeplot_July  %>% 
  filter(timeplot_July$Day == 1) %>%
  group_by(Hour) %>%
  arrange(Hour) %>%
  summarise(entries = sum(NetEntries),entries_pred = sum(pred))

title <- paste0("Day ", 1)

try <- ggplot(data = calc , aes(Hour),colour=variable) + 
  geom_line(aes(y = entries, colour = "Observed",group = 1), linetype = 'dotted',size=2) + 
  geom_line(aes(y = entries_pred, colour = "Predicted",  group = 1), size=2) +
  labs(colour='', 
       title=title,
       x="Hour",
       y="Sum of Net Entries at all Stations") + 
  scale_y_continuous(labels = scales::comma) +
  plotTheme2

imagename <- paste0("plot",".jpg") 
ggsave(filename=imagename, plot=last_plot())

for (i in 1:31){
  
  calc_July <- timeplot_July  %>% 
    filter(timeplot_July$Day == i) %>%
    group_by(Hour) %>%
    arrange(Hour) %>%
    summarise(entries = sum(NetEntries),entries_pred = sum(pred))
  
  title <- paste0("July - ","Day ", i)
  plot_July <- ggplot(data = calc_July, aes(Hour),colour=variable) + 
    geom_line(aes(y = entries, colour = "Observed",group = 1), linetype = 'dotted',size=2) + 
    geom_line(aes(y = entries_pred, colour = "Predicted",  group = 1), size=2) +
    labs(colour='', 
         title=title,
         x="Hour",
         y="Sum of Net Entries at all Stations") + 
    scale_y_continuous(labels = scales::comma) +
    plotTheme2
  
  imagename_July <- paste0("plot_July", i, ".jpg") 
  ggsave(filename=imagename_July, plot=last_plot(), width = 11, height = 7)
  
  }

#******************** Plot 8: error maps *******************#
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...))
}

errormap1 <- vars.predict  %>% 
  filter(vars.predict$Month == 7) %>%
  group_by(Station) %>%
  summarise(entries = sum(NetEntries),entries_pred = sum(pred)) %>%
  mutate(percent_error = abs(entries_pred - entries) / entries) %>%
  mutate(percentage = as.numeric(percent(percent_error)))

stations = vars.predict[!duplicated(vars.predict$Station),]
stations = stations[,c(1, 48, 49)]

errormap1 <- left_join(errormap1, stations)

m1 <- ggmap(baseMap) + 
  geom_point(data = errormap1, 
             aes(x=lng, y=lat, color=factor(ntile(percentage,5))), 
             size = 4) + 
  labs(title="Percent Error - July 2017\n") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(errormap1$percentage,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="% Error (Quantile Breaks)") +
  myTheme()

errormap2 <- vars.predict  %>% 
  filter(vars.predict$Month == 8) %>%
  group_by(Station) %>%
  summarise(entries = sum(NetEntries),entries_pred = sum(pred)) %>%
  mutate(percent_error = abs(entries_pred - entries) / entries) %>%
  mutate(percentage = as.numeric(percent(percent_error)))

errormap2 <- left_join(errormap2, stations)

m2 <- ggmap(baseMap) + 
  geom_point(data = errormap2, 
             aes(x=lng, y=lat, color=factor(ntile(percentage,5))), 
             size = 4) + 
  labs(title="Percent Error - August 2017\n") +
  scale_colour_manual(values = c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494"),
                      labels=as.character(quantile(errormap2$percentage,
                                                   c(.1,.2,.4,.6,.8),na.rm=T)),
                      name="% Error (Quantile Breaks)") +
  myTheme()

#***********************************************************************#

############################ MANHATTAN  ########################
## Based on the maps, the model predicts better for Manhattan
# Run regression again for only stations in Manhattan
manhattan <- st_read("Manhattan.shp")

stations <- stations %>%
  mutate(Longitude = stations$lng,
         Latitude = stations$lat)
stations <- st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)

# Spatial join
manhattan <- st_transform(manhattan, st_crs(stations))
manhattan_stations <- st_join(stations, manhattan)
manhattan_stations <- na.omit(manhattan_stations) # 69 stations

# Match with the full dataset
manhattan_stations <- manhattan_stations %>%
  select(Station, lat, lng)
vars.manhattan <- left_join(manhattan_stations, filter_vars2, by="Station")
vars.manhattan <- vars.manhattan[,c(1,4:51)]

# Start regression
reg_manhattan <- lm(NetEntries ~ ., data = as.data.frame(vars.manhattan) %>% 
            select(-lat.y,-lng.y, -geometry.x, -Station, -Hour))
summary(reg_manhattan)$r.squared # 0.6941281

regMan <-
  data.frame(observed = vars.manhattan$NetEntries,
             predicted = reg_manhattan$fitted.values,
             error = vars.manhattan$NetEntries - reg_manhattan$fitted.values) 

# Comparison of all stations vs. only manhattan stations
regFullset <-
  regFullset %>%
  mutate(absError = abs(error)) %>%
  mutate(percentAbsError = abs(error) / observed)

regMan <-
  regMan %>%
  mutate(absError = abs(error)) %>%
  mutate(percentAbsError = abs(error) / observed)

is.na(regFullset)<-sapply(regFullset, is.infinite)
regFullset[is.na(regFullset)]<-0

is.na(regMan)<-sapply(regMan, is.infinite)
regMan[is.na(regMan)]<-0

rsquared_full <- summary(reg)$adj.r.squared
rmse_full <- (mean((regFullset$error)^2))^(1/2)
mae_full <- MAE(regFullset$predicted, regFullset$observed)
mape_full <- mean(regFullset$percentAbsError)

rsquared_man <- summary(reg_manhattan)$adj.r.squared
rmse_man <- (mean((regMan$error)^2))^(1/2)
mae_man <- MAE(regMan$predicted, regMan$observed)
mape_man <- mean(regMan$percentAbsError)

Summary2 <- data.frame(dataset = c("All Stations","Manhattan Stations"),
                      Adjusted_RSquared = c(rsquared_full,rsquared_man),
                      RMSE = c(rmse_full,rmse_man),
                      MAE = c(mae_full,mae_man),
                      MAPE = c(mape_full,mape_man))
head(Summary2)

# Well..results do not match my expectations