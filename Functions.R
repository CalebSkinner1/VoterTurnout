# Functions

library("tidymodels")
library("patchwork")
library("bcaboot")
library("boot")
library("modelr")
library("broom")

# clean estimates, std errors, and p value table
interesting_components <- c("year", "nat_poll_margin", "state_prev_margin")

clean_data_table <- function(fit){
  df <- fit %>%
    tidy() %>%
    clean_names() %>%
    filter(term %in% interesting_components) %>%
    mutate(
      term = recode(term,
                    "year" = "Year",
                    "nat_poll_margin" = "National Polling Margin",
                    "state_prev_margin" = "State Previous Margin"))
  
  if(sum(str_detect(colnames(df), "tau")) > 0){
    df %>%
      rename("upper" = "conf_high",
             "lower" = "conf_low") %>%
      select(-tau) %>% 
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = where(is.numeric), decimals = 3)
  }else{
    df %>%
      rename(
        "standard error" = "std_error",
        "p value" = "p_value") %>%
      gt() %>%
      gt_theme_538() %>%
      fmt_number(columns = where(is.numeric), decimals = 3)
  }
}

# set regression engine
lm_spec <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

# qqplot
qq <- function(model){
  autoplot(model, which = c(2,1)) +
    labs(title = "")
}

# shapiro-wilks test of normality
shapiro <- function(model){
  shapiro.test(model$fit$residuals)
}


# Bootstrap and Interval Functions ----------------------------------------
# these are taken from the functions I wrote for Homework 4 and slightly adjusted to allow for multiple variables

formula <- as.formula("turnout_percentage ~ .")

# formula for bootstrap
lm_est <- function(split, ...) {
  lm(formula, data = analysis(split)) %>%
    tidy()
}

# full non-parametric bootstrap, get boot models
non_parametric_bootstrap <- function(df){
  lm_est <- function(split, ...) {
    lm(formula, data = analysis(split)) %>%
      tidy()
  }
  
  set.seed(1234)
  bootstraps(df, 1e4, apparent = TRUE) %>%
    mutate(results = map(splits, lm_est))
}

# bootstrap histogram
boot_hist <- function(boots){
  results <- boots %>% 
    unnest(results)
  
  components <- interesting_components %>% as.list()
  names <-  components %>% str_replace_all("_", " ") %>%
    str_replace("nat", "national") %>%
    str_replace("prev", "previous") %>%
    str_to_title()
  
  p <- map2(components, names, ~{
    results %>%
      filter(term == .x) %>%
      select(estimate) %>%
      ggplot() +
      geom_histogram(aes(x = estimate), fill = "indianred3") +
      labs(y = "", x = .y) +
      theme(axis.text.y = element_blank())})
  
  p[[1]] + p[[2]] + p[[3]]
}

# ugg - residual method
stat_fun_lm_residuals <- function(df_boot, ind, mod){
  resid_mod <- residuals(mod)
  resid_mod_boot <- resid_mod[ind]
  lin_predict_mod <- predict(mod)
  response_boot <- lin_predict_mod + resid_mod_boot
  df_boot[[as.character(formula(mod)[2])]] <- response_boot
  mod_boot <- lm(formula(mod), data=df_boot)
  coef_ests <- coef(mod_boot)
  var_ests <- diag(vcov(mod_boot))
  out <- c(coef_ests, var_ests)
  names(out) <- paste(rep(names(coef_ests),2),rep(c("_est","_var"),each=2),sep="")
  return(out)
}

# residual boot
residual_bootstrap <- function(df){
  boot_out <-  boot(df, stat_fun_lm_residuals, 1e4, mod = lm(formula, data=df))
}

# combine intervals into one table
combine_intervals <- function(boots, df, boot_out, model, conf = .95){
  # 98% confidence interval
  res <- model$fit$df.residual
  conf_int <- model %>%
    tidy() %>%
    mutate(
      lower = (qt((1-conf)/2, res)*std.error + estimate) %>% round(digits = 4),
      upper = (qt((1+conf)/2, res)*std.error + estimate) %>% round(digits = 4)) %>%
    select(term, lower, upper) %>%
    filter(term %in% interesting_components)
  
  # bias corrected bootstrap interval
  # formula for bootstrap
  lm_est <- function(split, ...) {
    lm(formula, data = analysis(split)) %>%
      tidy()}
  
  bca <- int_bca(boots, results, .fn = lm_est, alpha = 1-conf) %>%
    mutate(across(where(is.numeric), ~round(.x, digits = 4))) %>%
    rename(np_lower = .lower, np_upper = .upper) %>%
    select(term, np_lower, np_upper) %>%
    filter(term %in% interesting_components)
  
  # residual bca
  bca_res <- broom::tidy(boot_out, conf.int=TRUE, conf.method = "bca") %>%
    mutate(across(where(is.numeric), ~round(.x, digits = 4))) %>%
    rename(res_lower = conf.low, res_upper = conf.high) %>%
    select(term, res_lower, res_upper) %>%
    slice(2:4) %>%
    mutate(term = recode(term,
                         "year_est" = "year",
                         "nat_poll_margin_var" = "nat_poll_margin",
                         "state_prev_margin_var" = "state_prev_margin"))
  
  combine <- conf_int %>%
    left_join(bca, by = join_by(term)) %>%
    left_join(bca_res, by = join_by(term)) %>%
    mutate(
      term = str_replace_all(term, "_", " "),
      term = str_replace(term, "nat", "national"),
      term = str_replace(term, "prev", "previous"),
      term = str_to_title(term))
  combine
}

