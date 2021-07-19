library(usethis)

source("data-raw/01-small_table.R")
source("data-raw/04-specifications.R")
source("data-raw/05-game_revenue.R")

# Create external datasets

usethis::use_data(
  small_table,
  specifications,
  game_revenue,
  internal = FALSE, overwrite = TRUE
)
