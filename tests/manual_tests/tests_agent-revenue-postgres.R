library(pointblank)
library(here)

# Create an `action_levels` object, setting
# *warn* and *stop* thresholds at 0.01 and 0.10
al <- action_levels(warn_at = 0.01, stop_at = 0.10)

agent_revenue_postgres <-
  create_agent(
    read_fn = 
      ~ db_tbl(
        db = "postgres", dbname = "intendo", table = "revenue",
        user = "PG_P_DB_USER", password = "PG_P_DB_PASS",
        host = "134.122.40.123"
      ),
    tbl_name = "revenue",
    label = "The **intendo** revenue table.", 
    actions = al
  ) %>%
  col_vals_between(
    vars(revenue),
    left = 0.01, right = 150
  ) %>%
  col_vals_not_null(vars(user_id, session_id, time, name, type, revenue)) %>%
  col_vals_between(
    vars(time),
    left = "2015-01-01", right = "2016-01-01"
  ) %>%
  col_vals_lt(
    vars(revenue),
    value = vars(price),
    preconditions = ~ . %>% dplyr::filter(type != "ad")
  ) %>%
  col_vals_in_set(
    vars(type),
    set = c("ad", "currency", "season_pass", "offer_agent", NA)
  ) %>%
  col_vals_in_set(
    vars(name),
    set = c(
        "gems1", "gems2", "gems3", "gems4", "gems5",
        "gold1", "gold2", "gold3", "gold4", "gold5", "gold6", "gold7",
        "offer1", "offer2", "offer3", "offer4", "offer5",
        "ad_5sec", "pass"
    )
  ) %>%
  col_vals_regex(vars(user_id), regex = "[A-Y]{12}") %>%
  col_vals_regex(vars(session_id), regex = "[A-Z]{5}_[a-z]{8}") %>%
  col_is_character(vars(user_id, session_id)) %>%
  col_schema_match(
    schema = col_schema(
      user_id = "character",
      session_id = "character",
      time = "character",
      name = "character",
      size = "character",
      type = "character",
      price = "numeric",
      revenue = "numeric"
    )
  ) %>%
  interrogate()

x_write_disk(
  agent_revenue_postgres,
  filename = "agent_revenue_postgres.rds",
  path = here::here("tests/manual_tests")
)
