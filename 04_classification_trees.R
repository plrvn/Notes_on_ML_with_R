library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)

# household survey data -------------------------------------------------------------------

hhdata <- read_csv("data/hhfile_prepped_normalized.csv") %>% 
  select(-c("upm", "nvivi", "nhoga", "fex", "facpob", "area", "ipcm", "lnipcm",
            "linea_pobreza_total", "linea_pobreza_extrema", "extpov")) %>% 
  select(year, totpov, everything())

# outcome -----------------------------------------------------------------------------------
# hhdata <- 
#   hhdata %>% 
#   mutate(totpov = as.numeric(totpov=="tot_pov"))

# training and testing data -----------------------------------------------------------------

training_data <- 
  hhdata %>% 
  filter(year == 2018) %>% 
  select(-c(year))

testing_data <-    
  hhdata %>% 
  filter(year != 2018) %>% 
  select(-c(year))

# poverty model ------------------------------------------------------------------------------
poverty_model <- rpart(totpov ~ ., data = training_data, method = "class", control = rpart.control(cp = 0))

testing_data$totpov_pred <- predict(poverty_model, testing_data, type = "class")

table(pred = testing_data$totpov_pred, obs = testing_data$totpov)
mean(testing_data$totpov_pred == testing_data$totpov)

# plot ---------------------------------------------------------------------------------------
rpart.plot(poverty_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# pruning the model -------------------------------------------------------------------------
plotcp(poverty_model)
pov_pruned <- prune(poverty_model, cp = 0.0089)

testing_data$totpov_pred <- predict(pov_pruned, testing_data, type = "class")
table(pred = testing_data$totpov_pred, obs = testing_data$totpov)
mean(testing_data$totpov_pred == testing_data$totpov)

rpart.plot(pov_pruned, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# prune control ----------------------------------------------------------------------------
prune_control <- rpart.control(maxdepth = 30, minsplit = 20)

poverty_model <- rpart(totpov ~ ., data = training_data, method = "class", control = prune_control)

testing_data$totpov_pred <- predict(poverty_model, testing_data, type = "class")
table(pred = testing_data$totpov_pred, obs = testing_data$totpov)
mean(testing_data$totpov_pred == testing_data$totpov)

rpart.plot(poverty_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

# random forest --------------------------------------------------------------------------------
pov_model <- randomForest(factor(totpov) ~ ., data = training_data)