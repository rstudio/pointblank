library(pointblank)
library(tidyverse)

small_table %>%
  col_vals_gt(vars(d), 1000) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f)) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_exists(columns = vars(a, h))
