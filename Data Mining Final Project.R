library(boot) 
library(glmnet)
library(leaps)
library(tidyverse)
library(repurrrsive)
library(dplyr)
library(mclust)
library(ISLR)
library(pracma)
library(magrittr)
library(caTools)
library(cowplot)
library(boot)
library(splines)
library(e1071) 
library(MASS)
library(broom)
library(ggplot2)
library(gridExtra)
library(gam)
library(expss)
library("gbm")
library(rpart)
library(rpart.plot)
library(tree)
library("randomForest")
library(readr)
library(readxl)
library(xlsx)

#import datasets

building_metadata = read.csv("C:/Users/we704/Desktop/building_metadata.csv")
weather = read.csv("C:/Users/we704/Desktop/weather_train.csv")
building_meter = read.csv("C:/Users/we704/Desktop/train.csv")

str(building_metadata)
summary(building_metadata)
str(weather)
summary(weather)
str(building_meter)
summary(building_meter)


#feature engineering

#merge above dataset into completed one

building = building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  left_join(weather, by = c('site_id', 'timestamp'))


#convert timestamp into variable time with 3 level: morning, afternoon, evening

building$timestamp = as.character(building$timestamp)

building = building%>%
  mutate(hour = as.numeric(substring(building$timestamp,12,13)))%>%
  mutate(time = ifelse(building$hour >= 5 & building$hour <= 11, "Morning",ifelse(building$hour > 11 & building$hour <= 16, "Afternoon",ifelse(building$hour > 16 & building$hour <= 20, "Evening", "Night"))))
  
building$time = as.factor(building$time)

summary(building$time)


#separate dataset into 3 sub datasets based on meter type

building_electricity = building%>%
  filter(building$meter == 0)

building_water = building%>%
  filter(building$meter == 1 | building$meter == 3)

building_steam = building%>%
  filter(building$meter == 2)





#missing value




#EDA, Cluster Analysis, any nonlinear pattern?



#transformation



#split data 80:20 into training set and test set



#ready for preliminary models building

## Linear Reg

## KNN

## Random Forest


#turn response into categorical variable? (research on energy usage and level with high/low)



#non linear model approach


#NN model






