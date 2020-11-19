library(pointblank)
library(here)
library(intendo)

informant_revenue_postgres <- 
  create_informant(
    read_fn = 
      ~ db_tbl(
        db = "postgres", dbname = "intendo", table = "revenue",
        user = "PG_P_DB_USER", password = "PG_P_DB_PASS",
        host = "134.122.40.123"
      ),
    tbl_name = "intendo::revenue", 
    label = "The **intendo** revenue table."
  ) %>%
  info_tabular(
    description = "This table contains the daily revenue data for
    Intendo's **Super Jetroid** game. All data is for the complete
    year of 2015. Each row represents a single revenue by a user
    `user_id` in a particular session `session_id`. Revenue could be
    earned through ad views (where `type == 'ad'`) or through in-app
    purchases."
  ) %>%
  info_columns(
    columns = "user_id",
    info = "This is the User ID field."
  ) %>%
  info_columns(
    columns = "session_id",
    info = "This is the Session ID field."
  ) %>%
  info_columns(
    ends_with("id"),
    info = "ID fields like this one are unique."
  ) %>%
  info_columns(
    columns = "time",
    info = "This is a date-time field."
  ) %>%
  info_columns(
    columns = "time",
    info = "Even though it's a character column, the times are in
    ISO-8601 format."
  ) %>%
  info_columns(
    columns = "name", 
    info = "These contain the names of buyable products."
  ) %>%
  info_columns(
    columns = "name",
    info = "Currently these products are {names}."
  ) %>%
  info_columns(
    columns = "size",
    info = "The `size` refers to the relative size of the product. Ads are
    always `NULL` but products like `gold` and `gems` have a size value."
  ) %>%
  info_columns(
    columns = "type",
    info = "These contain the names of buyable products. Currently there
    are the following types: {types}."
  ) %>%
  info_snippet(
    snippet_name = "types",
    fn = snip_list(column = "type")
  ) %>%
  info_snippet(
    snippet_name = "names",
    fn = snip_list(column = "name", limit = Inf)
  ) %>%
  info_columns(
    columns = "price",
    info = "The price (in USD) for the product in the `name` column. This
    value will always be greater than the corresponding `revenue` value
    (30% higher)."
  ) %>%
  info_columns(
    columns = "revenue",
    info = "The reported revenue (in USD) for the product. The value may
    change up to 3-4 weeks after the sale date due to processing of refunds."
  ) %>%
  info_columns(
    columns = "revenue",
    info = "The revenue total is ${revenue_total}."
  ) %>%
  info_columns(
    columns = vars(price, revenue),
    info = "((PARTNER))"
  ) %>%
  info_snippet(
    snippet_name = "revenue_total",
    fn = ~ . %>%
      dplyr::summarize(total = sum(revenue, na.rm = TRUE)) %>%
      dplyr::pull(total)
  ) %>%
  info_columns(
    columns = vars(name, type),
    `person responsible` = "Rita Mercer (r.mercer@example.com)"
  ) %>%
  info_columns(
    columns = "time",
    TODO = "Ensure that this becomes a `DATETIME` column."
  ) %>%
  info_section(
    section_name = "source", 
    database = "Data table hosted in a *PostgreSQL* database on Digital
    Ocean (`134.122.40.123`). Email [[roy@example.com]]<<color: green;>> 
    for access info.",
    repo = "Original datasets available in the
    [intendo repo](https://github.com/rich-iannone/intendo)"
  ) %>%
  incorporate()

informant_revenue_postgres

get_informant_report(informant_revenue_postgres, size = "standard")

get_informant_report(informant_revenue_postgres, size = "small")

x_write_disk(
  informant_revenue_postgres,
  filename = "informant_revenue_postgres.rds",
  path = here::here("tests/manual_tests")
)
