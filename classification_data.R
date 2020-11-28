library(tidyverse)
library(lubridate)
library(imputeTS)
library(here)
library(caret)


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


#data frame for classification stored in full_data
full_data <- tmp

trctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)


lda_fit <- train(primary_use ~., data = full_data, method = "lda",
                trControl=trctrl,
                verbose = TRUE)

lda_fit




