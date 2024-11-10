library(tidyverse)
library(tidymodels)
library(naniar)
library(themis)

okcupid <- read_csv("lecture-6-feat-engineering-example/profiles_revised.csv")
head(okcupid) %>% View()
vis_miss(okcupid, warn_large_data = FALSE)

okcupid <- okcupid %>% 
  mutate(status = case_when(
    status == "available" ~ "single",
    status == "married" ~ "taken",
    status == "seeing someone" ~ "taken",
    status == "single" ~ "single"
  )) %>% 
  filter(status != "unknown")

okcupidv1 <- okcupid

okcupid$diet %>% table()
okcupid <- okcupid %>% 
  mutate(diet = case_when(
    str_detect(diet, "kosher") == TRUE ~ "kosher",
    str_detect(diet, "vegan") == TRUE ~ "vegan",
    str_detect(diet, "vegetarian") == TRUE ~ "vegetarian",
    str_detect(diet, "anything") == TRUE ~ "anything",
    str_detect(diet, "halal") == TRUE ~ "halal",
    str_detect(diet, "other") == TRUE ~ "other",
    .default = NA
  ))

okcupid <- okcupid %>%
  mutate(drinks = case_when(
    drinks == "not at all" ~ 1,
    drinks == "rarely" ~ 2,
    drinks == "socially" ~ 3,
    drinks == "often" ~ 4,
    drinks == "very often" ~ 5,
    drinks == "desperately" ~ 6
  ))

okcupid <- okcupid %>% 
  mutate(drugs = case_when(
    drugs == "never" ~ 1,
    drugs == "sometimes" ~ 2,
    drugs == "often" ~ 3
  ))

# this will turn "dropped out of high school," "graduated from high school," "working on high school," and "high school" into the same thing, etc
okcupid <- okcupid %>% 
  mutate(education = case_when(
    str_detect(education, "high school") ~ "high school",
    str_detect(education, "college") ~ "college",
    str_detect(education, "masters") ~ "grad school",
    str_detect(education, "ph.d") ~ "grad school",
    str_detect(education, "med school") ~ "medical school",
    str_detect(education, "law school") ~ "law school",
    str_detect(education, "space camp") ~ "other/not specified"
  ))

okcupid <- okcupid %>% 
  mutate(ethnicity = fct_lump(ethnicity, prop = .03))

okcupid <- okcupid %>% 
  mutate(has_kids = case_when(
    is.na(offspring) ~ "not specified",
    str_detect(offspring, "has a kid") ~ "yes",
    str_detect(offspring, "has kids") ~ "yes",
    str_detect(offspring, "doesn&rsquo;t have kids") ~ "no",
    str_detect(offspring, "doesn&rsquo;t want kids") ~ "no",
    str_detect(offspring, "might want kids") ~ "no",
    str_detect(offspring, "wants kids") ~ "no"
  )) %>% 
  mutate(wants_kids = case_when(
    is.na(offspring) ~ "not specified",
    str_detect(offspring, "might want more") ~ "yes",
    str_detect(offspring, "wants more") ~ "yes",
    str_detect(offspring, "might want kids") ~ "yes",
    str_detect(offspring, "wants kids") ~ "yes",
    str_detect(offspring, "wants them") ~ "yes",
    str_detect(offspring, "might want them") ~ "yes",
    .default = "no"
  )) %>% 
  select(-offspring)

okcupid <- okcupid %>% 
  mutate(has_pets = case_when(
    str_detect(pets, "has") ~ "yes",
    is.na(pets) ~ "not specified",
    .default = "no"
  )) %>% 
  mutate(dislikes_dogs = case_when(
    str_detect(pets, "dislikes dogs") ~ "yes",
    is.na(pets) ~ "not specified",
    .default = "no"
  )) %>% 
  mutate(dislikes_cats = case_when(
    str_detect(pets, "dislikes cats") ~ "yes",
    is.na(pets) ~ "not specified",
    .default = "no"
  )) %>% 
  select(-pets)

okcupid <- okcupid %>% 
  mutate(income = case_when(
    income <= 0 ~ NA,
    .default = income
  ))

set.seed(3400)
okcupid_split <- initial_split(okcupid, strata = status)

okcupid_train <- training(okcupid_split)

okcupid_test <- testing(okcupid_split)

okcupid_folds <- vfold_cv(okcupid_train, v = 5, strata = status)

okcupid_log <- recipe(status ~ age + body_type + diet +
                        drinks + drugs + education +
                        ethnicity + height + income +
                        orientation + sex + smokes + 
                        has_kids + wants_kids + 
                        has_pets + dislikes_dogs +
                        dislikes_cats, data = okcupid_train) %>% 
  step_impute_mode(body_type, diet, education, ethnicity, 
                   orientation, sex, smokes) %>%
  step_impute_mean(drinks, drugs) %>% 
  step_impute_linear(income, height, 
                     impute_with = imp_vars(age, drinks, drugs, sex)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_YeoJohnson(age, height, income) %>% 
  step_normalize(all_predictors()) %>% 
  step_interact(terms = ~ all_predictors():all_predictors()) %>% 
  step_upsample(over_ratio = 0.8)
prep(okcupid_log) %>% bake(okcupid_train)

en_grid <- grid_regular(penalty(), mixture(),
                        levels = 10)

log_mod <- logistic_reg(penalty = tune(),
                        mixture = tune()) %>% 
  set_engine("glmnet")

log_wkflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(okcupid_log)

okcupid_metrics <- metric_set(accuracy, roc_auc)

res_eng <- tune_grid(log_wkflow, okcupid_folds,
                        metrics = okcupid_metrics)

res_eng %>% 
  collect_metrics()

best_reg_eng <- select_best(res_eng, metric = "roc_auc")
results_part1 <- finalize_workflow(log_wkflow, best_reg_eng) %>% 
  fit(okcupid_train)
results_part1 %>% pluck() %>% 
  tidy() %>% arrange(estimate) %>% print(n = 100)
results <- results_part1 %>% 
  augment(okcupid_test)

results %>%
  mutate(status = factor(status)) %>% 
  roc_auc(truth = status, .pred_single)

okcupidv1 <- okcupidv1 %>%
  drop_na()

set.seed(3400)
okcupid_split <- initial_split(okcupidv1, strata = status)

okcupid_train <- training(okcupid_split)

okcupid_test <- testing(okcupid_split)

okcupid_folds <- vfold_cv(okcupid_train, v = 5, strata = status)

okcupid_log <- recipe(status ~ age + body_type + diet +
                        drinks + drugs + education +
                        ethnicity + height + income +
                        orientation + sex + smokes + pets +
                        offspring, data = okcupid_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

# prep(okcupid_log) %>% bake(okcupid_train)

log_mod <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

log_wkflow <- workflow() %>% 
  add_model(log_mod) %>% 
  add_recipe(okcupid_log)

res_no_eng <- tune_grid(log_wkflow, okcupid_folds, metrics = okcupid_metrics)

res_no_eng %>% 
  collect_metrics()

best_reg_no_eng <- select_best(res_no_eng, metric = "roc_auc")
results_b <- finalize_workflow(log_wkflow, best_reg_no_eng) %>% 
  fit(okcupid_train) %>% 
  augment(okcupid_test)

results_b %>%
  mutate(status = factor(status)) %>% 
  roc_auc(truth = status, .pred_single)

results %>% 
  mutate(status = factor(status)) %>% 
  roc_curve(truth = status, .pred_single) %>% 
  mutate(model = "mod1") %>% 
  bind_rows(results_b %>% 
              mutate(status = factor(status)) %>% 
              roc_curve(truth = status, .pred_single) %>% mutate(model = "mod2")) %>% 
  autoplot()

results %>% 
  ggplot(aes(x = .pred_single, fill = status)) +
  geom_boxplot()

