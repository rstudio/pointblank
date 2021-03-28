library(usethis)

source("data-raw/01-small_table.R")

# Create external datasets

usethis::use_data(
  small_table,
  internal = FALSE, overwrite = TRUE
)
