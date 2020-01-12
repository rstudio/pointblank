library(pointblank)
library(tidyverse)
library(RSQLite)

# Create an in-memory SQLite database and connection
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# Copy the `small_table` dataset to the connection
# and name the table `"small_table"`.
dplyr::copy_to(
  dest = con, 
  df = small_table,
  name = "small_table",
  temporary = FALSE
)

tbl_sqlite <- dplyr::tbl(con, "small_table")

agent <-
  create_agent(tbl = tbl_sqlite) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_gte(vars(c), 2, na_pass = TRUE) %>%
  col_vals_equal(vars(e), 1, preconditions = ~tbl %>% dplyr::filter(e == 1)) %>%
  col_vals_not_equal(vars(e), 0) %>%
  col_vals_lt(vars(e), 10, preconditions = ~tbl %>% dplyr::mutate(e = e + 9)) %>%
  col_vals_in_set(vars(f), c("low", "medium", "high")) %>%
  col_vals_not_in_set(vars(e), 3:5) %>%
  col_vals_between(vars(d), 0, 10000) %>%
  col_vals_not_between(vars(d), 15000, 20000) %>%
  col_vals_null(vars(c), preconditions = ~tbl %>% dplyr::filter(b == "5-jdo-903")) %>%
  col_is_character(vars(b)) %>%
  col_is_numeric(vars(d, e)) %>%
  col_is_integer(vars(e)) %>%
  col_exists(vars(date_time, date, a)) %>%
  #col_vals_regex(vars(b), "[0-9]-[a-z]{3}-[0-9]{3}") %>%
  # conjointly(
  #   ~ col_vals_gt(., vars(d), 100),
  #   ~ col_vals_gte(., vars(c), 2, na_pass = TRUE)
  # ) %>%
  interrogate()

get_agent_report(agent)

# TODO the following step functions have problems
# - rows_distinct()
# - col_vals_regex()
# - conjointly()
