library(tidyverse)
library(lubridate)
library(imputeTS)
library(here)
library(caret)
library(dplyr)

building_metadata <- read_csv(here("data","building_metadata.csv"))
building_meter <- read_csv(here("data", "train.csv"))
weather <- read_csv(here("data", "weather_train.csv"))


#-------------------------------------------------------------------------------


#----------------------EDA, feature Engineering---------------------------------------------------------

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


building_electricity = building%>%
  filter(building$meter == 0)

building_electricity%>%
  ggplot(aes(meter_reading))+geom_histogram(aes(y=..density..), color="blue", fill="light blue")+
  geom_density(color="red", size=1, fill="red", alpha=0.1)+theme_bw()

summary(building_electricity$meter_reading)



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



















