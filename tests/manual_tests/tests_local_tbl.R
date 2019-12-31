library(pointblank)
library(tidyverse)

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

val <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(d), 100) %>%
  rows_not_duplicated(vars(d, e)) %>%
  interrogate()


val_summary <-
  val %>% get_interrogation_summary()

