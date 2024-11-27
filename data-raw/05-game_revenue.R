library(tidyverse)
library(intendo)

game_revenue <-
  intendo::sj_all_revenue %>%
  dplyr::slice_head(n = 2000)
