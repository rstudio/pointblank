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
  col_vals_equal(
    vars(time), value = 2015,
    preconditions = ~ . %>% dplyr::mutate(time = YEAR(as.Date(time))),
    label = "Comparison."
  ) %>%
  col_vals_not_equal(
    vars(item_revenue), value = vars(item_revenue), na_pass = TRUE,
    label = "Comparison."
  ) %>%
  col_vals_not_equal(
    vars(item_name), value = vars(item_type),
    label = "Comparison."
  ) %>%
  col_vals_gt(
    vars(item_revenue), 0,
    label = "Comparison."
  ) %>%
  col_vals_between(
    vars(item_revenue),
    left = 0, right = 150, inclusive = c(FALSE, TRUE),
    label = "Comparison."
  ) %>%
  col_vals_between(
    vars(time),
    left = "2015-01-01", right = "2016-01-01",
    label = "Comparison."
  ) %>%
  col_vals_in_set(
    vars(item_type),
    set = c("ad", "currency", "season_pass", "offer_agent", NA),
    label = "Values within sets."
  ) %>%
  col_vals_in_set(
    vars(item_name),
    set = c(
      "ad_10sec", "ad_15sec", "ad_20sec", "ad_30sec", "ad_5sec", 
      "ad_playable", "ad_survey", "gems1", "gems2", "gems3", "gems4", 
      "gems5", "gold1", "gold2", "gold3", "gold4", "gold5", "gold6", 
      "gold7", "offer1", "offer2", "offer3", "offer4", "offer5"
    ),
    label = "Values within a sets."
  ) %>%
  col_vals_not_null(
    vars(player_id, session_id, time, item_name, item_type, item_revenue),
    label = "Presence of NULLs."
  ) %>%
  col_vals_regex(
    vars(player_id), regex = "[A-Y]{12}",
    label = "Regular expression."
  ) %>%
  col_vals_regex(
    vars(session_id), regex = "[A-Z]{5}_[a-z]{8}",
    label = "Regular expression."
  ) %>%
  col_is_character(
    vars(player_id, session_id),
    label = "Column type."
  ) %>%
  interrogate()

get_agent_report(agent = agent_revenue_duckdb)

x_write_disk(
  agent_revenue_duckdb,
  filename = "agent_revenue_duckdb.rds",
  path = here::here("tests/manual_tests")
)
