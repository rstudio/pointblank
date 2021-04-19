library(usethis)

source("data-raw/01-small_table.R")
source("data-raw/04-specifications.R")

# Create external datasets

usethis::use_data(
  small_table,
  specifications,
  internal = FALSE, overwrite = TRUE
)
