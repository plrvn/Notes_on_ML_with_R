library(tidyverse)
library(rsample)
library(Metrics)


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


# training-validating split  -------------------------------------------------------------------

cv_split <- vfold_cv(training_data, v = 20, strata = lnipcm, breaks = 10)

# extract the train and validate data for each split

cv_data <- cv_split %>% mutate(train = map(splits, ~training(.x)),
                               validate = map(splits, ~testing(.x)))

# linear regression ------------------------------------------------------------------------
fmla <- as.formula("lnipcm ~ .")

summary(lm(fmla, data = training_data))


# cross validation -------------------------------------------------------------------------
cv_models_lm <- cv_data %>% mutate(model = map(train, ~lm(fmla, data = .x)))

# extract actual values
cv_prep_lm <- 
  cv_models_lm %>% 
  mutate(validate_actual = map(validate, ~.x$lnipcm),
         validate_predicted = map2(model, validate, ~predict(.x, .y)))

# examples of manipulating lists
# cv_prep_lm %>% 
#   mutate(validate_test = map2(validate_actual, validate_predicted, ~(.x-.y))) %>% 
#   mutate(validate_test2 = map_dbl(train, ~mean((.x$lnipcm - .x$jefe_edad)))) %>% 
#   mutate(mean_test = map_dbl(validate_test, ~mean(.x)))
         
cv_eval_lm <- cv_prep_lm %>% #return vector instead of a list
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

mean(cv_eval_lm$validate_mae)

#test note
