library(tidyverse)
library(gtsummary)   # tbl_uvregression()
library(MASS)        # polr()
library(rlang)

uv_polr <- function(data, outcome, ...) {
  # capture the symbols ---------------------------------------------
  outcome_sym <- enquo(outcome)
  expl_syms   <- enquos(...)
  
  # pre-process and run the univariable ordinal-logistic regressions
  data %>% 
    filter(!(.data[[as_name(outcome_sym)]] %in% 'I don\'t know how to answer')) %>% 
    mutate(
      !!outcome_sym :=
        factor(!!outcome_sym,
               levels = c('Strongly disagree',
                          'Somewhat disagree',
                          'Neither agree nor disagree',
                          'Somewhat agree',
                          'Strongly agree'))
    ) %>% 
    tbl_uvregression(
      include       = c(!!!expl_syms),   # country, gender, q5_category, …
      method        = polr,
      y             = !!outcome_sym,
      exponentiate  = TRUE,
      pvalue_fun    = ~ style_pvalue(.x, digits = 3)
    ) %>% 
    add_global_p()
}