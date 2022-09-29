library(tidyverse)
library(dslabs)
library(broom)

data(gapminder) 

gapminder <- 
  gapminder %>% 
  drop_na()

# List Column Workflow 
# Nesting ------------------------------------------------------------------------------------------------------------

nested <- 
  gapminder %>% 
  group_by(country) %>% 
  nest()
  
nested$data[[9]]

nested %>% 
  unnest(data)

# map family of functions -----------------------------------------------------------------------------------------------
map(.x = nested$data, .f = ~mean(.x$population, na.rm = TRUE))

pop_df <- nested %>% 
  mutate(pop_mean = map(data, ~mean(.x$population, na.rm = TRUE)))

str(pop_df$pop_mean)

pop_df <- 
  pop_df %>% 
  unnest(pop_mean)

str(pop_df$pop_mean)

pop_df <- nested %>% 
  mutate(pop_mean = map_dbl(data, ~mean(.x$population, na.rm = TRUE)))

str(pop_df$pop_mean)

# many models ------------------------------------------------------------------------------------------------------------
models <- 
  nested %>% 
  mutate(model = map(data, ~lm(formula = population~fertility, data = .x)))

albania_model <- models$model[[1]]

summary(albania_model)

# broom to clean up ----------------------------------------------------------------------------------------------------
tidy(albania_model)
glance(albania_model)
augment(albania_model)

augment(albania_model) %>% 
  ggplot(mapping = aes(x=fertility)) +
  geom_point(mapping = aes(y=population)) +
  geom_line(mapping = aes(y = .fitted), color = "red")

# coefficients across models ------------------------------------------------------------------------------------------
models_le <- 
  nested %>% 
  mutate(model = map(data, ~lm(formula = life_expectancy~year, data = .x)))

tidy(models_le$model[[1]])

model_coef <- 
  models_le %>% 
  mutate(coef = map(model, ~tidy(.x))) %>% 
  unnest(coef)

model_coef %>% 
  filter(term == "year") %>% 
  ggplot(aes(x=estimate)) +
  geom_histogram()

# evaluating fit of many models -------------------------------------------------------------------------------------
model_perf <-
  models_le %>% 
  mutate(coef = map(model, ~glance(.x))) %>% 
  unnest(coef)

model_perf 

model_perf %>% 
  slice_max(order_by = r.squared, n = 2)

model_perf %>% 
  arrange(desc(r.squared))

model_perf %>% 
  arrange(r.squared)


# explore fit ------------------------------------------------------------------------------------------------------------
aug_model <- 
  models_le %>% 
  mutate(augmented = map(model, ~augment(.x))) %>% 
  unnest(augmented)

aug_model %>% filter(country == "Swaziland") %>% 
  ggplot(aes(x = year, y = life_expectancy)) +
  geom_point() +
  geom_line(aes(y=.fitted), color = "red")

# training, test, and validation splits ---------------------------------------------------------------------------------
library(rsample)
library(Metrics)
gap_split <- initial_split(gapminder, prop = 0.75)

training_data <- training(gap_split)
testing_data <- testing(gap_split)

nrow(testing_data)

cv_split <- vfold_cv(training_data, v = 3)

cv_data <- 
  cv_split %>% 
  mutate(train = map(splits, ~training(.x)),
         validate = map(splits, ~testing(.x)))

cv_models_lm <- 
  cv_data %>% 
  mutate(model = map(train, ~lm(formula = life_expectancy~year + gdp, data = .x)))

model_perf2 <-
  cv_models_lm %>% 
  mutate(coef = map(model, ~glance(.x))) %>% 
  unnest(coef)

cv_prep_lm <- 
  cv_models_lm %>% 
  mutate(validate_actual = map(validate, ~.x$life_expectancy),
         validate_predicted = map2(model, validate, ~predict(.x, .y)))

cv_eval_lm <- 
  cv_prep_lm %>% 
  mutate(validate_mae = map2_dbl(validate_actual, validate_predicted,
                                 ~mae(actual = .x, predicted = .y)))

cv_eval_lm






