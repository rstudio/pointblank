library(pointblank)
library(tidyverse)

small_table <-
  readr::read_csv(
    system.file("extdata", "small_table.csv", package = "pointblank"),
    col_types = "TDicddlc")

val <-
  create_agent(tbl = small_table) %>%
  rows_not_duplicated(vars(d, e)) %>%
  rows_not_duplicated(vars(a, f)) %>%
  col_vals_gt(vars(d), 100) %>%
  interrogate()


val_summary <-
  val %>% get_agent_report()


val %>% get_row_extracts(1)
