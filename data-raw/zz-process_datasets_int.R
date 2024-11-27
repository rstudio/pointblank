library(usethis)

source("data-raw/03-countries_subdivisions_currencies.R")

# Create internal datasets

usethis::use_data(
  countries, subdivisions, subd_list_main, currencies,
  internal = TRUE, overwrite = TRUE
)
