library(tidyverse)
library(lubridate)
library(here)


building_metadata = read_csv(here("data","building_metadata.csv"))
building_meter <- read_csv(here("data", "train.csv"))
weather <- read_csv(here("data", "weather_train.csv"))

building = building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  left_join(weather, by = c('site_id', 'timestamp'))

building %>% 
  filter(meter == 0) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
    geom_path() + facet_wrap(~primary_use)


building %>% 
  filter(meter == 1) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)


building %>% 
  filter(meter == 2) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)


building %>% 
  filter(meter == 3) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)

#-------------------------------------------------------------------------------

building %>% 
  filter(meter == 0, primary_use == "Education") %>%
  mutate(day = wday(timestamp), hour = hour(timestamp), month = month(timestamp)) %>%
  group_by(month, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-",month,"-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
    geom_path()




building %>% 
  filter(meter == 0, primary_use == "Religious worship") %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path() + facet_wrap(~primary_use)
  
