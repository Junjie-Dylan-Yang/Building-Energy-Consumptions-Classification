library(tidyverse)
library(caret)
library(xgboost)

building_metadata <- read_csv(here("data","filled_bldg_meta.csv"))
building_meter <- read_csv(here("data", "train.csv"))
weather <- read_csv(here("data", "weather_train.csv"))

building <- building_meter %>%
  left_join(building_metadata, by = 'building_id') %>%
  mutate(site_id = as_factor(site_id))


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

full_data <- tmp %>% select(-building_id)

trctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

xg_fit <- train(primary_use ~ ., data = full_data, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = grid_default,
                verbose = TRUE)

xg_fit
