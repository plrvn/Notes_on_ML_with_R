library(tidyverse)

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

# linear regression ------------------------------------------------------------------------
fmla <- as.formula("lnipcm ~ .")

model <- lm(fmla, data = training_data)

# model.matrix(fmla, data = training_data) allows you to see data used in model

# model summary ----------------------------------------------------------------------------
print(model)
summary(model)
broom::glance(model)
sigr::wrapFTest(model)

# predictions ------------------------------------------------------------------------------
training_data$prediction <- predict(model)

ggplot(training_data, aes(x = prediction, y = lnipcm)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("log(ipcm) vs. linear model prediction (training)")

testing_data$prediction <- predict(model, newdata = testing_data)
testing_data$residuals <- testing_data$lnipcm - testing_data$prediction

ggplot(testing_data, aes(x = prediction, y = lnipcm)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("log(ipcm) vs. linear model prediction (testing)")

ggplot(testing_data, aes(x = prediction, y = residuals)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("log(ipcm) vs. linear model prediction (testing)")

WVPlots::GainCurvePlot(training_data, "prediction", "lnipcm", "model") #relative gini score = 1 is perfect sort order, 0 or negative is poor sorting.

# RMSE -------------------------------------------------------------------------------------------
RMSE <- sqrt(mean(testing_data$residuals^2))
sd <- sd(testing_data$lnipcm)

# R-squared -------------------------------------------------------------------------------------
rsq_train <-  1 - (sum((training_data$lnipcm - training_data$prediction)^2))/(sum((training_data$lnipcm - mean(training_data$lnipcm))^2))
rsq_test <-  1 - (sum((testing_data$lnipcm - testing_data$prediction)^2))/(sum((testing_data$lnipcm - mean(testing_data$lnipcm))^2))

#cross validation --------------------------------------------------------------------------------
library(vtreat)
nRows <- nrow(training_data)
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

k <- 3
training_data$pred.cv <- 0

for (i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(fmla, data = training_data[split$train,1:44])
  training_data$pred.cv[split$app] <- predict(model, newdata = training_data[split$app,1:44])
}

rmse_train <- sqrt(mean((testing_data$lnipcm-testing_data$prediction)^2))
rmse_cv <- sqrt(mean((training_data$lnipcm-training_data$pred.cv)^2))

# log transform -----------------------------------------------------------------------------------
# mean > median, predicting the mean will over predict typical values
mean(exp(hhdata$lnipcm))
median(exp(hhdata$lnipcm))

mean(hhdata$lnipcm)
median(hhdata$lnipcm)

