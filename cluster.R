library(tidyverse)
library(here)
library(lubridate)

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
  filter(primary_use == 'Education') %>%
  select(-X1, -site_id, -primary_use, -floor_count, -square_feet, -year_built)

e_mat <- tmp %>% select(-building_id) %>% as.matrix()

e_mat <- e_mat %>% t() %>% scale() %>% t()

hc_complete <- hclust(dist(e_mat), method="complete")


#
vind <- cutree(hc_complete, 3)

tmpg1 <- tmp[which(vind == 1),]$building_id
tmpg2 <- tmp[which(vind == 2),]$building_id
tmpg3 <- tmp[which(vind == 3),]$building_id



building %>% 
  filter(meter == 0, building_id %in% tmpg1) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path()



building %>% 
  filter(meter == 0, building_id %in% tmpg3) %>%
  mutate(day = wday(timestamp), hour = hour(timestamp)) %>%
  filter(month(timestamp) == 2) %>%
  group_by(primary_use, day, hour) %>% 
  summarize(avg_meter_reading = mean(meter_reading)) %>%
  mutate(date = ymd_h(paste0("2020-11-",day, " ", hour))) %>%
  ggplot(aes(x=date, y=avg_meter_reading)) + 
  geom_path()