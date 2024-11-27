library(tidyverse)
library(DBI)
library(RSQLite)

small_table <-
  readr::read_csv(
    file = "data-raw/small_table.csv",
    col_types = "TDicddlc"
  )

con <-
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = "inst/data_files/small_table.db"
  )

DBI::dbWriteTable(
  conn = con,
  name = "small_table",
  value = small_table
)
