library(tidyverse)
library(mgcv)

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

# GAM ----------------------------------------------------------------------------------------

model <- gam(lnipcm ~ hh_totpers + s(jefe_edad), data = training_data, family = gaussian)

summary(model)

plot(model)

predict(model, type = "response")