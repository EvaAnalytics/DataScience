library(readr)
library(caret)
library(dplyr) # data wrangling
library(MASS)
library(missForest)
library(rsample)  # data splitting
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(tidyverse)
library(h2o)
library(DALEX)
library(DALEXtra)
library(iml)
library(skimr)
library(DataExplorer)
library(ggplot2)
library(ggpubr)
library(univariateML)
library(recipes)
library(olsrr)
library(GGally)
library(ModelMetrics)


deployStations <- read_csv("~/Documentos/UPV/Tareas/KAGGLE_COMPETITION/kaggle_data_files/deployStations.csv")
#names(deployStations)
#table(deployStations$station)
Stations_DistanceMatrix <- read_csv("~/Documentos/UPV/Tareas/KAGGLE_COMPETITION/kaggle_data_files/Stations_DistanceMatrix.csv")
trainingStations <- read_csv("~/Documentos/UPV/Tareas/KAGGLE_COMPETITION/kaggle_data_files/Training_missing.csv")
#names(deployStations)
#table(trainingStations$station)
testStations <- read_csv("~/Documentos/UPV/Tareas/KAGGLE_COMPETITION/kaggle_data_files/testStations.csv")
#names(testStations)
#table(testStations$station)

# UNIR BASES CON DISTANCIAS #####
#names(Stations_DistanceMatrix)
Training_Mayo<- trainingStations

# T113<- subset(Training_Mayo, station == 113)
# #T113<- T113[!duplicated(T113), ]
# #T113<- T113[-(1:4188 * 2), ]
# T113<-T113[!duplicated(T113[ , c("station", "latitude", "longitude", "numDocks", "timestamp", "year", "month", "day", "hour", "weekday", "weekhour", "isHoliday", "windMaxSpeed.m.s", "windMeanSpeed.m.s", "windDirection.grades", "temperature.C", "relHumidity.HR", "airPressure.mb", "precipitation.l.m2", "bikes_3h_ago", "full_profile_3h_diff_bikes", "full_profile_bikes", "short_profile_3h_diff_bikes", "short_profile_bikes", "bikes")]),]
# 
# Training_Mayo<- subset(Training_Mayo, station != 113)
# 
# Training_Mayo<- rbind(Training_Mayo, T113)
# 
deployStations<- subset(deployStations, select=-c(14,15))
deployStations$weekday <- factor(deployStations$weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                                    "Thursday", "Friday", "Saturday", "Sunday"),
                                 ordered = TRUE)
# # # 
deployStations$weekday<- as.integer(deployStations$weekday)
table(deployStations$weekday)


Base1<- rbind(Training_Mayo, deployStations)


#Base1<- rbind(Training_Mayo, deployStations)
# str(Base1)
table(Base1$precipitation.l.m2)

Base1$precipitation.l.m2<- ifelse(Base1$precipitation.l.m2 <= 0.4,"No", "Rain")

data<- merge(Base1, Stations_DistanceMatrix, by="station")

data.cmplt<- data


data.cmplt$season<-  ifelse(data.cmplt$month %in% 10:12, "Otoño", ifelse(data.cmplt$month %in% 1:3, "Invierno",ifelse(data.cmplt$month %in% 4:6, "Primavera","Verano")))
data.cmplt$season<- as.factor(data.cmplt$season)

#table(data.cmplt$year)
data.cmplt$year[data.cmplt$year == 0] = 2013
data.cmplt$year[data.cmplt$year == 1] = 2014
data.cmplt$year = factor(data.cmplt$year)


str(data.cmplt)

data.cmplt$precipitation.l.m2 = factor (data.cmplt$precipitation.l.m2)
data.cmplt$month = factor(data.cmplt$month)
data.cmplt$day = factor(data.cmplt$day)
data.cmplt$weekday = factor(data.cmplt$weekday)
data.cmplt$hour = factor(data.cmplt$hour)
data.cmplt$isHoliday = factor(data.cmplt$isHoliday)
data.cmplt$station = factor(data.cmplt$station)
summary(data.cmplt)

data.cmplt<- subset(data.cmplt, select=-c(5,11,24))
# "timestamp"  , "weekhour" . Id






##########
testStations$weekday<- factor(testStations$weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                                               "Thursday", "Friday", "Saturday", "Sunday"),ordered = TRUE)

testStations$weekday<- as.integer(testStations$weekday)


datatest<- merge(testStations, Stations_DistanceMatrix, by="station")
# Missing data treatment
imputdata<- missForest(datatest) 
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
datatest.cmplt<- imputdata$ximp

datatest.cmplt$season<-  ifelse(datatest.cmplt$month %in% 10:12, "Otoño", ifelse(datatest.cmplt$month %in% 1:3, "Invierno",ifelse(datatest.cmplt$month %in% 4:6, "Primavera","Verano")))
datatest.cmplt$season<- as.factor(datatest.cmplt$season)

datatest.cmplt$precipitation.l.m2<- ifelse(datatest.cmplt$precipitation.l.m2 <= 0.4,"No", "Rain")


datatest.cmplt$year[datatest.cmplt$year == 0] = 2013
datatest.cmplt$year[datatest.cmplt$year == 1] = 2014
datatest.cmplt$year = factor(datatest.cmplt$year)



str(datatest.cmplt)

datatest.cmplt$precipitation.l.m2 = factor (datatest.cmplt$precipitation.l.m2)
datatest.cmplt$month = factor(datatest.cmplt$month)
datatest.cmplt$day = factor(datatest.cmplt$day)
datatest.cmplt$weekday = factor(datatest.cmplt$weekday)
datatest.cmplt$hour = factor(datatest.cmplt$hour)
datatest.cmplt$isHoliday = factor(datatest.cmplt$isHoliday)
datatest.cmplt$station = factor(datatest.cmplt$station)
summary(datatest.cmplt)

datatest.cmplt<- subset(datatest.cmplt, select=-c(5,11,25))
# "timestamp"  , "weekhour" . Id




