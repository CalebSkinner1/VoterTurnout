# Ok goals:

# 1. simple regression w/ and w/o demographic data
# 2. uncertainty quantification
# 3. weighted regression to deal with heteroskedasticity
# 4. robust regression using hyperbolic loss or absolute error
# 5. predicting 2024, using ridge/lasso

# libraries
library("tidyverse"); theme_set(theme_minimal())
library("here")
library("tidymodels")
library("gt")
library("gtExtras")
library("ggfortify")
library("janitor")
library("quantreg")

# data path
data_path <- "Data"

# functions
source(here("Functions.R"))

# read data
data <- read_csv(here(data_path, "complete_data.csv"))

# remove non predictors and prep for modeling
prep_data <- data %>%
  select(-state, -total_votes, -vep, #remove non-predictors
         -population_65, -female_population, -not_hispanic, -two_or_more_races #remove fully collinear covariates
  ) %>%
  mutate(
    nat_poll_margin = abs(nat_poll_margin/100),
    state_prev_margin = abs(state_prev_margin))

# Simple OLS --------------------------------------------------------------

# w/ demographic information
lm_fit <- lm_spec %>%
  fit(turnout_percentage ~ ., data = prep_data)

lm_fit %>%
  clean_data_table()

# w/o demographic information
lm_spec %>%
  fit(turnout_percentage ~ year + nat_poll_margin + state_prev_margin, data = prep_data) %>%
  clean_data_table()

# Uncertainty Quantification ----------------------------------------------

# is it normal? Probably
# qq plot
qq(lm_fit) +
  geom_point(color = "indianred3")

# normal - ya probably
shapiro(lm_fit)

# but let's do bootstrap anyway

# nonparametric bootstrap
np_boot <- prep_data %>% non_parametric_bootstrap()

# residual bootstrap
r_boot <- prep_data %>% residual_bootstrap()

# interval table
combine_intervals(np_boot, prep_data, r_boot, lm_fit) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(columns = where(is.numeric), decimals = 3) %>%
  tab_header(title = "95% Confidence Intervals") %>%
  tab_spanner(label = "Normality Assumption", columns = c(lower:upper)) %>%
  tab_spanner(label = "Non-Parametric BCa Bootstrap", columns = starts_with("np")) %>% 
  tab_spanner(label = "Residual BCa Bootstrap", columns = starts_with("res")) %>%
  cols_label("np_lower" ~ "lower") %>%
  cols_label("np_upper" ~ "upper") %>%
  cols_label("res_lower" ~ "lower") %>%
  cols_label("res_upper" ~ "upper")

# Weighted Regression -----------------------------------------------------

# is there constant variance? Probably
augment(lm_fit, new_data = prep_data) %>%
  ggplot(aes(x = .pred, y = abs(.resid))) +
  geom_point(color = "indianred3", size = 1) +
  geom_smooth(color = "cadetblue4", se = FALSE) +
  labs(x = "Fitted Value", y = "Absolute Residuals")

# but let's do weighted ols anyway

# create weights by predicting residual from smoothing spline
w <- 1/(predict(smooth.spline(lm_fit$fit$fitted.values, abs(lm_fit$fit$residuals)))$y)^2

weighted_lm_fit <- lm(turnout_percentage ~ ., data = prep_data, weights = w)

weighted_lm_fit %>% clean_data_table()

# check for homoscedasticity now
augment(weighted_lm_fit, new_data = prep_data) %>%
  ggplot(aes(x = .fitted, y = abs(.resid))) +
  geom_point(color = "indianred3", size = 1) +
  geom_smooth(color = "cadetblue4", se = FALSE) +
  labs(x = "Fitted Value", y = "Absolute Residuals")

# Robust Regression -------------------------------------------------------

# minimize mean absolute error (same as quantile regression set to minimize the median)
library("quantreg")

quant_fit <- rq(turnout_percentage ~ ., data = prep_data)

quant_fit %>%
  clean_data_table()

# Prediction --------------------------------------------------------------

train_data <- prep_data %>% filter(year != 2024)
test_data <- prep_data %>% filter(year == 2024)

train_fold <- vfold_cv(train_data, v = 8)

pred_recipe <- recipe(formula = turnout_percentage ~ ., data = train_data) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# OLS

ols_pred_fit <- lm_spec %>%
  fit(turnout_percentage ~ ., data = train_data)

augment(ols_pred_fit, new_data = test_data) %>%
  rsq(truth = turnout_percentage, estimate = .pred)

# Ridge

ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

ridge_workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(ridge_spec)

ridge_penalty_grid <- grid_regular(penalty(range = c(-5, 1.3)), levels = 50)

ridge_tune_res <- tune_grid(
  ridge_workflow,
  resamples = train_fold, 
  grid = penalty_grid)

ridge_best_penalty <- select_best(ridge_tune_res, metric = "rsq")

ridge_final <- finalize_workflow(ridge_workflow, ridge_best_penalty)

ridge_final_fit <- fit(ridge_final, data = train_data)

ridge_final_fit %>% tidy() %>%
  filter(estimate != 0)

augment(ridge_final_fit, new_data = test_data) %>%
  rsq(truth = turnout_percentage, estimate = .pred)


# Lasso!

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

lasso_workflow <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(lasso_spec)

lasso_penalty_grid <- grid_regular(penalty(range = c(-5, -1.7)), levels = 50)

lasso_tune_res <- tune_grid(
  lasso_workflow,
  resamples = train_fold, 
  grid = lasso_penalty_grid)

lasso_best_penalty <- select_best(lasso_tune_res, metric = "rsq")

lasso_final <- finalize_workflow(lasso_workflow, lasso_best_penalty)

lasso_final_fit <- fit(lasso_final, data = train_data)

lasso_final_fit %>% tidy() %>%
  filter(estimate != 0)

augment(lasso_final_fit, new_data = test_data) %>%
  rsq(truth = turnout_percentage, estimate = .pred)



