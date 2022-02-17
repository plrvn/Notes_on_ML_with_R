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

# log transform -----------------------------------------------------------------------------------
# mean > median, predicting the mean will over predict typical values
mean(exp(hhdata$lnipcm))
median(exp(hhdata$lnipcm))

mean(hhdata$lnipcm)
median(hhdata$lnipcm)

# linear regression ------------------------------------------------------------------------
fmla <- as.formula(paste0("lnipcm ~", paste(vars, collapse='+')))

model <- lm(fmla, data = training_data)

training_data$prediction <- predict(model)

training_data <- 
  training_data %>% mutate(ipcm = exp(lnipcm), 
                           resid = lnipcm - prediction,
                           prediction = exp(prediction),
                           duan_prediction = prediction*mean(exp(resid)))

# root mean squared relative error ---------------------------------------------------------------
# prediction errors are multiplicative 

modIncome <- lm(paste0("ipcm ~", paste(vars, collapse='+')), data = training_data)

modLogIncome <- lm(paste0("lnipcm ~", paste(vars, collapse='+')), data = training_data)

testing_data %>% 
  mutate(ipcm = exp(lnipcm),
         pred = predict(modIncome, newdata = testing_data),
         resid = ipcm - pred) %>% 
  summarize(rmse = sqrt(mean(resid^2)),
            rms.relerr = sqrt(mean((resid/ipcm)^2)))

testing_data %>% 
  mutate(ipcm = exp(lnipcm),
         predlog = predict(modLogIncome, newdata = testing_data),
         pred = exp(predlog),
         resid = ipcm - pred,
         pred_duan = pred*mean(exp(lnipcm-predlog)),
         resid_duan = ipcm - pred_duan) %>% 
  summarize(rmse = sqrt(mean(resid^2)),
            rms.relerr = sqrt(mean((resid/ipcm)^2)),
            rmse_duan = sqrt(mean(resid_duan^2)),
            rms.relerr_duan = sqrt(mean((resid_duan/ipcm)^2)),
            mean(ipcm),
            mean(pred),
            mean(pred_duan))
