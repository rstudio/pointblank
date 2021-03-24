library(usethis)

source("data-raw/01-small_table.R")
source("data-raw/03-countries_subdivisions_currencies.R")

# Create external datasets

usethis::use_data(
  small_table, countries, subdivisions, currencies,
  internal = FALSE, overwrite = TRUE
)
