library(tidyverse)
library(tidymodels)
library(naivebayes)

# household survey data -------------------------------------------------------------------

hhdata <- read_csv("data/hhfile_prepped_normalized.csv") %>% 
  select(-c("upm", "nvivi", "nhoga", "fex", "facpob", "area", "ipcm", "lnipcm",
            "linea_pobreza_total", "linea_pobreza_extrema", "extpov")) %>% 
  select(year, totpov, everything())

# bin numeric variables ------------------------------------------------------------------------

hh_rec <- recipe(totpov ~ ., data = hhdata) %>%
  step_discretize(hh_totpers, jefe_edad, jefe_aniosestudio, perc_tiene_trabajo_remunerado, piezas_por_miembro, min_unique = 1, num_breaks = 5) %>%
  step_dummy(hh_totpers, jefe_edad, jefe_aniosestudio, perc_tiene_trabajo_remunerado, piezas_por_miembro)

hh_prep <- hh_rec %>% prep() %>% bake(new_data = hhdata)

# training and testing data -----------------------------------------------------------------

training_data <- 
  hh_prep %>% 
  filter(year == 2018) %>% 
  select(-c(year))

testing_data <-    
  hh_prep %>% 
  filter(year != 2018) %>% 
  select(-c(year))

# model --------------------------------------------------------------------------------------

povmodel <- naive_bayes(totpov ~ ., data = training_data, laplace = 1)

predictions <- bind_cols(testing_data, .pred = predict(povmodel, testing_data %>% select(-totpov)))

table(pred = predictions$.pred, obs = predictions$totpov)
mean(predictions$.pred == predictions$totpov)

predict(povmodel, testing_data %>% select(-totpov), type = "prob")