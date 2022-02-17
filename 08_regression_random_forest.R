library(tidyverse)
library(ranger)

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

# random forests -----------------------------------------------------------------------------
seed <- 123
set.seed(123)

fmla <- as.formula("lnipcm ~ .")

inc_model_rf <- ranger(fmla, data = training_data, num.trees = 500, respect.unordered.factors = "order", seed = seed)

# predictions ----------------------------------------------------------------------------------

inc_model_rf

testing_data$pred <- predict(inc_model_rf, testing_data)$predictions

testing_data %>% 
  mutate(resid = pred - lnipcm) %>% 
  summarize(rmse = sqrt(mean(resid^2)))

ggplot(testing_data, aes(x = pred, y = lnipcm)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("log(ipcm) vs. linear model prediction (testing)")