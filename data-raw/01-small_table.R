library(tidyverse)

small_table <-
  readr::read_csv(
    file = "data-raw/small_table.csv",
    col_types = "TDicddlc"
  )

small_table
