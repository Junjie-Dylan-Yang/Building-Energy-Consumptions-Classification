library(tidyverse)
library(lubridate)
library(imputeTS)
library(here)
library(caret)
library(dplyr)



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
  mutate(site_id = as_factor(site_id))


#expend timestamp into detail time variables

building <- building %>% 
  mutate(timestamp_date = ymd(gsub( " .*$", "", timestamp)),
         timestamp_month = month(timestamp_date),
         timestamp_day = wday(timestamp_date, label = T, abbr = T),
         time_ymd_hms = ymd_hms(timestamp),
         time_hour = hour(time_ymd_hms))


#plot reponse variable

building_electricity = building%>%
  filter(building$meter == 0)

building_electricity%>%
  ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()

summary(building_electricity$meter_reading)


#Huge dense with value 0, try to remove outliers given by boxplot

outlier = boxplot(building_electricity$meter_reading)$out
summary(outlier)

remove = which(building_electricity$meter_reading %in% outlier)

building_electricity_removeout = building_electricity[ -remove, ]

summary(building_electricity_removeout$meter_reading)

building_electricity_removeout %>% ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()


#transformation, right skewed, log transformation

building_electricity_removeout = building_electricity_removeout %>%
  mutate(meter_reading_log = log(building_electricity_removeout$meter_reading))

building_electricity_removeout %>% ggplot(aes(meter_reading_log))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()


#response variable right skewed, log(meter_reading+1) to avoid negative value

building_electricity_removeout = building_electricity_removeout %>%
  mutate(meter_reading_log = log(building_electricity_removeout$meter_reading+1))

building_electricity_removeout %>% ggplot(aes(meter_reading_log))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()


#Need to look closer to the dataset with meter_reading = 0

building_electricity_zero = building_electricity %>%
  filter(building_electricity$meter_reading== 0)

summary(building_electricity_zero$site_id)

#site 0 has the most 0 value in meter_reading, look into site 0 data

building_electricity_site0 = building_electricity %>%
  filter(building_electricity$site_id == 0)

summary(building_electricity_site0)

hist(building_electricity_site0$building_id)

building_electricity_site0 = building_electricity_site0 %>% 
  group_by(timestamp_date)%>% 
  summarize(avg_meter_reading = mean(meter_reading))

building_electricity_site0%>%
  ggplot(aes(x=timestamp_date, y=avg_meter_reading))+geom_path()
###The meter reading of site 0 buildings are mostly 0 until mid-May. It suggests us to remove site 0 data prior mid_may. 
###However, our classification problem will look at series/path to distingush for building type and we will see the need of remove these data for better model performance.


#Other variables statistics/plot

## mean meter_reading by hour in a day

building_electricity %>% 
  group_by(time_hour)%>% 
  summarize(avg_meter_reading = mean(meter_reading))%>%
  ggplot(aes(x=time_hour, y=avg_meter_reading)) + geom_path()

## mean meter_reading by day in a week

building_electricity %>% 
  group_by(timestamp_day)%>% 
  summarize(avg_meter_reading = mean(meter_reading))%>%
  ggplot(aes(x=timestamp_day, y=avg_meter_reading)) + geom_point()


## mean meter_reading for days in a week of by building's use of purpose

building %>% 
  filter(meter == 0) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)

## focus on education

building %>% 
  filter(meter == 0, primary_use == "Education") %>%
  mutate(day = wday(timestamp), hour = hour(timestamp), month = month(timestamp)) %>%
  group_by(month, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-",month,"-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path()

## focus on Religious worship

building %>% 
  filter(meter == 0, primary_use == "Religious worship") %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)


#-------------------------------------------------------------------------------


#----------------------handle missing value---------------------------------------------------------

colSums(is.na(building))
colSums(is.na(weather))
colSums(is.na(building_metadata))

#handle missing data in building_metadata, variable year_built and floor_count
tmp <- building %>% 
  filter(meter == 0) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(building_id, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading), .groups = "keep") %>%
  ungroup() %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  select(-hour,-day) %>%
  pivot_wider(names_from = date, values_from = avg_meter_reading) %>%
  left_join(building_metadata, by = 'building_id')

train_floor <- tmp %>% 
  select(-primary_use, -building_id, -year_built) %>%
  drop_na()

test_floor <- tmp %>% 
  select(-primary_use, -year_built) %>%
  filter(is.na(floor_count))

model_floor <- train(
  floor_count ~., data = train_floor, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k = c(9))
)

floor_est <- predict(model_floor, test_floor)
test_floor$floor_count <- round(floor_est)

tmp <- full_join(tmp, select(test_floor, building_id, floor_count), by = "building_id") %>%
  mutate(floor_count = coalesce(floor_count.x, floor_count.y)) %>%
  select(-floor_count.x, -floor_count.y)


train_year <- tmp %>% 
  select(-primary_use, -building_id, -floor_count) %>%
  drop_na()

test_year <- tmp %>% 
  select(-primary_use, -floor_count) %>%
  filter(is.na(year_built))

model_year <- train(
  year_built ~., data = train_year, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k = 9)
)


year_est <- predict(model_year, test_year)
test_year$year_built <- round(year_est)

tmp <- full_join(tmp, select(test_year, building_id, year_built), by = "building_id") %>%
  mutate(year_built = coalesce(year_built.x, year_built.y)) %>%
  select(-year_built.x, -year_built.y)


bldg_meta <- select(tmp, building_id, floor_count, primary_use, square_feet, year_built, site_id)

#handle missing data in weather

weather$air_temperature <- na_kalman(weather$air_temperature)
weather$cloud_coverage <- na_kalman(weather$cloud_coverage)
weather$dew_temperature <- na_kalman(weather$dew_temperature)
weather$precip_depth_1_hr <- na_mean(weather$precip_depth_1_hr, option = "mode")
weather$sea_level_pressure <- na_kalman(weather$sea_level_pressure)
weather$wind_direction <- na_kalman(weather$wind_direction)
weather$wind_speed <- na_kalman(weather$wind_speed)

#-------------------------------------------------------------------------------




#------------------------------Use data with NA handled and EDA on remaining variables-------------------------------------------------
building_metadata <- read_csv("C:/Users/we704/Desktop/filled_bldg_meta.csv")


building <- building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  mutate(site_id = as_factor(site_id))

building_electricity = building%>%
  filter(building$meter == 0)

#EDA on primary_use

building_electricity %>% 
  group_by(primary_use)%>% 
  summarize(avg_meter_reading = mean(meter_reading))%>%
  ggplot(aes(x=reorder(primary_use,avg_meter_reading), y=avg_meter_reading)) + geom_col()

#Data transformation, each hour is a predictor

tmp <- building %>% 
  filter(meter == 0) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(building_id, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading), sd_meter_reading = sd(meter_reading), .groups = "keep") %>%
  ungroup() %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  select(-hour,-day) %>%
  pivot_wider(names_from = date,
              names_prefix = "electric_",
              values_from = c(avg_meter_reading, sd_meter_reading)) %>%
  left_join(building_metadata, by = 'building_id') %>% 
  select(-X1)

#EDA on floor

tmp %>%
  group_by(site_id)%>% 
  summarize(avg_floor = mean(floor_count))%>%
  ggplot(aes(x=reorder(site_id,avg_floor), y=avg_floor))+geom_col()

#EDA on weather data

summary(weather)

weather%>%
  group_by(site_id)%>%
  summarize(avg_tem = mean(air_temperature))%>%
  ggplot(aes(x=reorder(site_id,avg_tem), y=avg_tem))+geom_col()

weather%>%
  group_by(site_id)%>%
  summarize(avg_cloud = mean(cloud_coverage))%>%
  ggplot(aes(x=reorder(site_id,avg_cloud), y=avg_cloud))+geom_col()  

weather%>%
  group_by(site_id)%>%
  summarize(avg_precip = mean(precip_depth_1_hr))%>%
  ggplot(aes(x=reorder(site_id,avg_precip), y=avg_precip))+geom_col()

weather%>%
  group_by(site_id)%>%
  summarize(avg_wind = mean(wind_speed))%>%
  ggplot(aes(x=reorder(site_id,avg_wind), y=avg_wind))+geom_col()


#------------------------------Cluster Analysis-------------------------------------------------
full_data <- tmp
sum(is.na(full_data))

data_cluster = full_data[-2]

scale_data_cluster = scale(full_data)



scale.arrest = scale(USArrests)
summary(scale.arrest)

#------------------------------BUild LDA as base model-------------------------------------------------

full_data <- tmp
set.seed(1)
train.index = sample(1:nrow(full_data),nrow(full_data)*0.8)
train = full_data[train.index,]
test = full_data[-train.index,]


library(MASS)
lda.fit=lda(primary_use~.,data=train)
lda.fit




#data frame for classification stored in full_data
full_data <- tmp

trctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


lda_fit <- train(primary_use ~., data = full_data, method = "lda",
                 trControl=trctrl,
                 verbose = TRUE)

lda_fit



