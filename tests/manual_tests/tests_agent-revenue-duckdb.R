library(pointblank)
library(intendo)
library(here)

# Create an `action_levels` object, setting
# *warn* and *stop* thresholds at 0.01 and 0.10
al <- action_levels(warn_at = 0.01, stop_at = 0.10)

all_revenue <- intendo::all_revenue()

agent_revenue_duckdb <-
  create_agent(
    tbl = 
      ~ db_tbl(
        table = all_revenue,
        dbname = ":memory:",
        dbtype = "duckdb"
      ),
    tbl_name = "revenue",
    label = "The **intendo** revenue table.", 
    actions = al
  ) %>%
  col_vals_lt(
    vars(item_revenue), value = vars(price),
    preconditions = ~ . %>% dplyr::filter(type != "ad"),
    label = "Comparison."
  ) %>%
  col_vals_lte(
    vars(size), value = "5", na_pass = TRUE,
    label = "Comparison."
  ) %>%
  col_vals_equal(
    vars(time), value = 2015,
    preconditions = ~ . %>% dplyr::mutate(time = YEAR(as.Date(time))),
    label = "Comparison."
  ) %>%
  col_vals_not_equal(
    vars(revenue), value = vars(price), na_pass = TRUE,
    label = "Comparison."
  ) %>%
  col_vals_not_equal(
    vars(name), value = vars(type),
    label = "Comparison."
  ) %>%
  col_vals_gte(
    vars(size), value = "1", na_pass = TRUE,
    label = "Comparison."
  ) %>%
  col_vals_gt(
    vars(revenue), 0,
    label = "Comparison."
  ) %>%
  col_vals_between(
    vars(revenue),
    left = 0, right = 150, inclusive = c(FALSE, TRUE),
    label = "Comparison."
  ) %>%
  col_vals_between(
    vars(size),
    left = "0", right = "5",
    label = "Comparison."
  ) %>%
  col_vals_between(
    vars(time),
    left = "2015-01-01", right = "2016-01-01",
    label = "Comparison."
  ) %>%
  col_vals_not_between(
    vars(revenue),
    left = 0, right = 0.75,
    preconditions = ~ . %>% dplyr::filter(!is.na(size)),
    label = "Comparison."
  ) %>%
  col_vals_in_set(
    vars(type),
    set = c("ad", "currency", "season_pass", "offer_agent", NA),
    label = "Values within sets."
  ) %>%
  col_vals_in_set(
    vars(name),
    set = c(
      "gems1", "gems2", "gems3", "gems4", "gems5",
      "gold1", "gold2", "gold3", "gold4", "gold5", "gold6", "gold7",
      "offer1", "offer2", "offer3", "offer4", "offer5",
      "ad_5sec", "pass"
    ),
    label = "Values within sets."
  ) %>%
  col_vals_not_in_set(
    vars(type),
    set = c("currency", "season_pass", "offer_agent"),
    preconditions = ~ . %>% dplyr::filter(is.na(size)),
    label = "Values within sets."
  ) %>%
  col_vals_null(
    vars(size, price),
    preconditions = ~ . %>% dplyr::filter(type == "ad"),
    label = "Presence of NULLs."
  ) %>%
  col_vals_not_null(
    vars(user_id, session_id, time, name, type, revenue),
    label = "Presence of NULLs."
  ) %>%
  col_vals_regex(
    vars(user_id), regex = "[A-Y]{12}",
    label = "Regular expression."
  ) %>%
  col_vals_regex(
    vars(session_id), regex = "[A-Z]{5}_[a-z]{8}",
    label = "Regular expression."
  ) %>%
  col_is_character(
    vars(user_id, session_id),
    label = "Column type."
  ) %>%
  col_schema_match(
    schema = col_schema(
      user_id = "character",
      session_id = "character",
      time = "POSIXct",
      name = "character",
      size = "character",
      type = "character",
      price = "numeric",
      revenue = "numeric"
    ),
    label = "Schema."
  ) %>%
  interrogate()

x_write_disk(
  agent_revenue_duckdb,
  filename = "agent_revenue_duckdb.rds",
  path = here::here("tests/manual_tests")
)
