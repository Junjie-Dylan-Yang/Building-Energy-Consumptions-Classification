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
library(lubridate)
library(caret)
library(imputeTS)



#-------------------------------EDA, Feature Engineering------------------------------------------------
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


#merge building_metadata and building_meter dataset into completed one
building <- building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  mutate(site_id = as_factor(site_id))%>% 
  filter(meter == 0)


#inspect and handle NA value

colSums(is.na(weather))

weather$air_temperature <- na_kalman(weather$air_temperature)
weather$cloud_coverage <- na_kalman(weather$cloud_coverage)
weather$dew_temperature <- na_kalman(weather$dew_temperature)
weather$precip_depth_1_hr <- na_mean(weather$precip_depth_1_hr, option = "mode")
weather$sea_level_pressure <- na_kalman(weather$sea_level_pressure)
weather$wind_direction <- na_kalman(weather$wind_direction)
weather$wind_speed <- na_kalman(weather$wind_speed)
colSums(is.na(weather))

colSums(is.na(building))

tmp <- building %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(building_id, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading), .groups = "keep") %>%
  ungroup() %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour)))%>%
  dplyr::select(-hour,-day) %>%
  pivot_wider(names_from = date, values_from = avg_meter_reading) %>%
  left_join(building_metadata, by = 'building_id')

train_floor <- tmp %>% 
  dplyr::select(-primary_use, -building_id, -year_built) %>%
  drop_na()

test_floor <- tmp %>% 
  dplyr::select(-primary_use, -year_built) %>%
  filter(is.na(floor_count))

model_floor <- train(
  floor_count ~., data = train_floor, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k = c(9))
)

floor_est <- predict(model_floor, test_floor)
test_floor$floor_count <- round(floor_est)

tmp <- full_join(tmp, dplyr::select(test_floor, building_id, floor_count), by = "building_id") %>%
  mutate(floor_count = coalesce(floor_count.x, floor_count.y)) %>%
  dplyr::select(-floor_count.x, -floor_count.y)








#merge above dataset into completed one

building = building %>%
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


#any nonlinear pattern? #plot each preditor distribution, correlation, relationship with response





#transformation

#response variable right skewed, log transformation
building_electricity_removeout = building_electricity_removeout %>%
  mutate(meter_reading_log = log(building_electricity_removeout$meter_reading))

building_electricity_removeout %>% ggplot(aes(meter_reading_log))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()

#response variable right skewed, log(meter_reading+1) to avoid negative value
building_electricity_removeout = building_electricity_removeout %>%
  mutate(meter_reading_log = log(building_electricity_removeout$meter_reading+1))

building_electricity_removeout %>% ggplot(aes(meter_reading_log))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()



#Cluster Analysis (high comsumption vs low comsumption?)






#split data 80:20 into training set and test set



#ready for preliminary models building

## Linear Reg

## KNN

## Random Forest


#turn response into categorical variable? (research on energy usage and level with high/low)



#non linear model approach


#NN model






