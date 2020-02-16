library(pointblank)
library(tidyverse)

al <- action_levels(warn_at = 3)

small_table %>%
  col_vals_gt(vars(d), 1000, actions = al) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE, actions = al) %>%
  rows_distinct(vars(d, e), actions = al) %>%
  rows_distinct(vars(a, f), actions = al) %>%
  col_vals_gt(vars(d), 100, actions = al) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE, actions = al)
