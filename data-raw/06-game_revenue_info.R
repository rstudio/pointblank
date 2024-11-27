library(tidyverse)

game_revenue_info <-
  dplyr::tibble(
    column = c(
      "player_id",
      "session_id",
      "session_start",
      "time",
      "item_type",
      "item_name",
      "item_revenue",
      "session_duration",
      "start_day",
      "acquisition",
      "country"
    ),
    info = c(
      "A `character` column with unique identifiers for each user/player.",
      "A `character` column that contains unique identifiers for each player session.",
      "A date-time column that indicates when the session (containing the revenue event) started.",
      "A date-time column that indicates exactly when the player purchase (or revenue event) occurred.",
      "A `character` column that provides the class of the item purchased.",
      "A `character` column that provides the name of the item purchased.",
      "A `numeric` column with the revenue amounts per item purchased.",
      "A `numeric` column that states the length of the session (in minutes) for which the purchase occurred.",
      "A `Date` column that provides the date of first login for the player making a purchase.",
      "A `character` column that provides the method of acquisition for the player.",
      "A `character` column that provides the probable country of residence for the player."
    )
  )
