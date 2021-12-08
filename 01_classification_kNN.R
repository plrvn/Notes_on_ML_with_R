library(class)
library(tidyverse)

# example data ----------------------------------------------------------------------------

signs <- read_csv("data/01_kNN_signs.csv")

training_data <- signs[-nrow(signs),-1]
testing_data <-  signs[nrow(signs),-1]

training_labels <- signs$sign_type[-nrow(signs)]
testing_labels <- signs$sign_type[nrow(signs)]

pred <- knn(training_data, testing_data, training_labels)


# household survey data -------------------------------------------------------------------

hhdata <- read_csv("data/hhfile_prepped_normalized.csv") %>% 
  select(-c("upm", "nvivi", "nhoga", "fex", "facpob", "area", "ipcm", "lnipcm",
            "linea_pobreza_total", "linea_pobreza_extrema", "extpov")) %>% 
  select(year, totpov, everything())

training_data <- 
  hhdata %>% 
  filter(year == 2018) %>% 
  select(-c(year, totpov))
  
testing_data <-    hhdata %>% 
  filter(year != 2018) %>% 
  select(-c(year, totpov))

training_labels <- 
  hhdata %>% 
  filter(year == 2018) %>% 
  pull(totpov)
  
testing_labels <- 
  hhdata %>% 
  filter(year != 2018) %>% 
  pull(totpov)

pred <- knn(training_data, testing_data, training_labels, k = 100)

pred_votes <- knn(training_data, testing_data, training_labels, k = 3, prob = TRUE)
pred_prob <- attr(pred_votes, "prob")

table(pred, testing_labels)
mean(pred == testing_labels)
