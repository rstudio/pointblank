library(pointblank)
library(tidyverse)

small_table %>%
  col_vals_gt(
    vars(d), 1000,
    actions = action_levels(warn_at = 3)
  )
