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
  mutate(hour = as.numeric(substring(building$timestamp,12,13)))


building = building%>%
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




#EDA, Cluster Analysis, any nonlinear pattern?

#missing value
colSums(is.na(building_electricity))

#plot response variable
building_electricity %>% ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()

#generate dataset with outlier removed, create high energy usage data which is preserved for high energy consumption analysis 
summary(building_electricity$meter_reading)

boxplot(building_electricity$meter_reading)

outlier = boxplot(building_electricity$meter_reading)$out
summary(outlier)

remove = which(building_electricity$meter_reading %in% outlier)


#dataset with outliers removed
building_electricity_removeout = building_electricity[ -remove, ]

summary(building_electricity_removeout$meter_reading)


building_electricity_removeout %>% ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()


#high energy usage dataset, threshold is set as 3rd Qtr in outlier in original dataset
building_electricity_high = building_electricity %>%
  filter(building_electricity$meter_reading >= 1000)

summary(building_electricity_high$meter_reading)

building_electricity_high %>% ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()

outlier_inhigh = boxplot(building_electricity_high$meter_reading)$out

building_electricity_high_remove = building_electricity_high[ -which(building_electricity_high$meter_reading %in% outlier_inhigh), ]

summary(building_electricity_high_remove$meter_reading)

building_electricity_high_remove %>% ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()



#transformation

#response variable right skewed, log transformation






#split data 80:20 into training set and test set



#ready for preliminary models building

## Linear Reg

## KNN

## Random Forest


#turn response into categorical variable? (research on energy usage and level with high/low)



#non linear model approach


#NN model






