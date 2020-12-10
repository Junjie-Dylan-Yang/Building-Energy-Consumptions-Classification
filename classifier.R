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


full_data$primary_use <- fct_collapse(full_data$primary_use, Other = c("Other", "Warehouse/storage", "Utility",
                                                                       "Technology/science", "Services", "Retail",
                                                                       "Religious worship", "Food sales and service", "Manufacturing/industrial",
                                                                       "Healthcare", "Parking"))


trctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

grid_default <- expand.grid(
  nrounds = 150,
  max_depth = 8,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(100)
xg_fit <- train(primary_use ~ . -site_id, data = full_data, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = grid_default,
                verbose = TRUE)

xg_fit


set.seed(100)
lda_fit <- train(primary_use ~ .  -site_id, data = full_data, method = "lda",
                trControl=trctrl,
                verbose = TRUE)

lda_fit


set.seed(100)
mlp_fit <- train(primary_use ~ . -site_id, data = full_data, method = "mlp",
                 trControl=trctrl,
                 tuneGrid = expand.grid(size = c(5,10)),
                 verbose = TRUE)

mlp_fit


set.seed(100)
svm_fit <- train(primary_use ~ . -site_id, data = full_data, method = "svmLinear",
                 trControl=trctrl,
                 tuneGrid = expand.grid(C = c(17, 22, 25)),
                 verbose = TRUE)

svm_fit



set.seed(100)
rf_fit <- train(primary_use ~ . -site_id, data = full_data, method = "rf",
                 trControl=trctrl,
                 tuneGrid = expand.grid(mtry = c(15, 45, 100)),
                 verbose = TRUE)

rf_fit




ggplot(full_data, aes(x=primary_use))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  coord_flip()

tmp_colnames <- colnames(full_data[, 1:(length(full_data)-5)])
tmp <- full_data[, 1:(length(full_data)-5)] 
tmp <- apply(tmp, 1, scale) %>% t() %>% as_tibble()
colnames(tmp) <- tmp_colnames
tmp <- cbind(tmp, full_data[,(ncol(full_data)-4):ncol(full_data)])



set.seed(356)
trainIndex <- createDataPartition(tmp$primary_use, p = .7, 
                                  list = FALSE, 
                                  times = 1)
tmpTrain <- tmp[ trainIndex,]
tmpTest  <- tmp[-trainIndex,]

set.seed(100)
xg_fit <- train(primary_use ~ . , data = tmpTrain, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = grid_default,
                verbose = TRUE)

xg_fit


set.seed(100)
lda_fit <- train(primary_use ~ ., data = tmpTrain, method = "lda",
                 trControl=trctrl,
                 verbose = TRUE)

lda_fit


set.seed(100)
mlp_fit <- train(primary_use ~ . , data = tmpTrain, method = "mlp",
                 trControl=trctrl,
                 tuneGrid = expand.grid(size = c(5,10)),
                 verbose = TRUE)

mlp_fit


set.seed(100)
svm_fit <- train(primary_use ~ . , data = tmpTrain, method = "svmLinear",
                 trControl=trctrl,
                 tuneGrid = expand.grid(C = c(10, 17, 22)),
                 verbose = TRUE)

svm_fit



set.seed(100)
rf_fit <- train(primary_use ~ . , data = tmpTrain, method = "rf",
                trControl=trctrl,
                tuneGrid = expand.grid(mtry = c(15, 45, 100)),
                verbose = TRUE)

rf_fit


pc <- predict(lda_fit, tmpTest)
ac <- tmpTest$primary_use

length(which(pc == ac)) / length(ac)






d_multi <- tibble(target = tmpTest$primary_use, prediction = predict(xg_fit,tmpTest))

conf_mat <- confusion_matrix(targets = d_multi$target, predictions = d_multi$prediction)

plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE, font_counts = font(size = 8), font_normalized = font(size = 8),
                      font_row_percentages = font(size = 8), font_col_percentages = font(size = 8))





d_multi <- tibble(target = tmpTest$primary_use, prediction = predict(lda_fit,tmpTest))

conf_mat <- confusion_matrix(targets = d_multi$target, predictions = d_multi$prediction)

plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], add_sums = TRUE, font_counts = font(size = 8), font_normalized = font(size = 8),
                      font_row_percentages = font(size = 8), font_col_percentages = font(size = 8))

