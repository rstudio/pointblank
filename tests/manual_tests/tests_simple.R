library(pointblank)
library(tidyverse)

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

small_table %>%
  col_vals_gt(
    vars(d), 1000,
    actions = action_levels(warn_at = 3)
  )
