library(tidyverse)
library(xgboost)

# household survey data -------------------------------------------------------------------

hhdata <- read_csv("data/hhfile_prepped_normalized.csv") %>% 
  select(-c("upm", "nvivi", "nhoga", "fex", "facpob", "area", "lnipcm",
            "linea_pobreza_total", "linea_pobreza_extrema", "totpov", "extpov")) %>% 
  mutate(lnipcm = log(ipcm)) %>% 
  select(-ipcm) %>% 
  select(year, lnipcm, everything()) 

# training and testing data -----------------------------------------------------------------

training_data <- 
  hhdata %>% 
  filter(year == 2018) %>% 
  select(-c(year))

testing_data <-    
  hhdata %>% 
  filter(year != 2018) %>% 
  select(-c(year))

vars <- names(training_data)[-1]

# gradient boosting --------------------------------------------------------------------------

cv <- xgb.cv(data = as.matrix(training_data %>% select(vars)),
             label = training_data$lnipcm,
             objective ="reg:squarederror",
             nrounds = 100, nfold = 5, eta = 0.3, depth = 6)

elog <- as.data.frame(cv$evaluation_log)
(nrounds <- which.min(elog$test_rmse_mean))

plot(elog$iter, elog$test_rmse_mean)

model <- xgboost(data = as.matrix(training_data %>% select(vars)),
             label = training_data$lnipcm,
             objective ="reg:squarederror",
             nrounds = nrounds, eta = 0.3, depth = 6)

# predictions ------------------------------------------------------------------------------

testing_data$pred <- predict(model, as.matrix(testing_data %>% select(vars)))

testing_data %>% 
  mutate(resid = pred - lnipcm) %>% 
  summarize(rmse = sqrt(mean(resid^2)))

ggplot(testing_data, aes(x = pred, y = lnipcm)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("log(ipcm) vs. linear model prediction (testing)")