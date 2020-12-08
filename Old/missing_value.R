library(tidyverse)
library(lubridate)
library(caret)
library(imputeTS)
library(here)


building_metadata <- read_csv(here("data","building_metadata.csv"))
building_meter <- read_csv(here("data", "train.csv"))
weather <- read_csv(here("data", "weather_train.csv"))

building <- building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  mutate(site_id = as_factor(site_id))

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

#-------------------------------------------------------------------------------

weather$air_temperature <- na_kalman(weather$air_temperature)
weather$cloud_coverage <- na_kalman(weather$cloud_coverage)
weather$dew_temperature <- na_kalman(weather$dew_temperature)
weather$precip_depth_1_hr <- na_mean(weather$precip_depth_1_hr, option = "mode")
weather$sea_level_pressure <- na_kalman(weather$sea_level_pressure)
weather$wind_direction <- na_kalman(weather$wind_direction)
weather$wind_speed <- na_kalman(weather$wind_speed)