library(tidyverse)
library(pROC)

# household survey data -------------------------------------------------------------------

hhdata <- read_csv("data/hhfile_prepped_normalized.csv") %>% 
  select(-c("upm", "nvivi", "nhoga", "fex", "facpob", "area", "ipcm", "lnipcm",
            "linea_pobreza_total", "linea_pobreza_extrema", "extpov")) %>% 
  select(year, totpov, everything())

# outcome -----------------------------------------------------------------------------------
hhdata <- 
  hhdata %>% 
  mutate(totpov = as.numeric(totpov=="tot_pov"))

# training and testing data -----------------------------------------------------------------

training_data <- 
  hhdata %>% 
  filter(year == 2018) %>% 
  select(-c(year))

testing_data <-    
  hhdata %>% 
  filter(year != 2018) %>% 
  select(-c(year))

# logistic regression ---------------------------------------------------------------------
poverty_model <- glm(totpov ~ ., data = training_data, family = "binomial")

# poverty probability training -----------------------------------------------------------------------
training_data$poverty_prob <- predict(poverty_model, training_data, type = "response")

mean(training_data$totpov)
mean(training_data$poverty_prob)
training_data$poverty_pred <- ifelse(training_data$poverty_prob > 0.29, 1, 0)

table(pred = training_data$poverty_pred, obs = training_data$totpov)
mean(training_data$poverty_pred == training_data$totpov)

# poverty probability testing -----------------------------------------------------------------------
testing_data$poverty_prob <- predict(poverty_model, testing_data, type = "response")

testing_data$poverty_pred <- ifelse(testing_data$poverty_prob > 0.29, 1, 0)

table(pred = testing_data$poverty_pred, obs = testing_data$totpov)
mean(testing_data$poverty_pred == testing_data$totpov)

# ROC curve -----------------------------------------------------------------------------------------
# Create a ROC curve
ROC <- roc(testing_data$totpov, testing_data$poverty_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

# stepwise regression for variable selection -------------------------------------------------------------------------------

null_model <- glm(totpov ~ 1, data = training_data, family = "binomial")
full_model <- poverty_model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(step_model)
