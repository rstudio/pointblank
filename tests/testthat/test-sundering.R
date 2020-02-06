context("Tests of the `get_sundered_data()` function")

library(dplyr)
library(RSQLite)
library(DBI)

# Create an agent, add validation steps, perform interrogation
agent <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f)) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

# Create an in-memory SQLite database and connection
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# Copy the `small_table` dataset to the connection
# and name the table `"small_table"`.
dplyr::copy_to(
  dest = con, 
  df = small_table %>% dplyr::distinct(),
  name = "small_table",
  temporary = FALSE
)

tbl_sqlite <- dplyr::tbl(con, "small_table")

agent_sqlite_no_id <-
  create_agent(tbl = tbl_sqlite) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f)) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

test_that("sundered data can be generated and retrieved with a `tbl_df`", {
  
  # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <- 
    get_sundered_data(agent, type = "pass")
  
  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <- 
    get_sundered_data(agent, type = "fail")
  
  # Expect that both tables are of the same type as the
  # input data
  expect_is(small_table, "tbl_df")
  expect_is(pass_data_tbl, "tbl_df")
  expect_is(fail_data_tbl, "tbl_df")
  
  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl), c(9, 8))
  expect_equal(dim(fail_data_tbl), c(4, 8))
  
  # Expect that the sum of rows from each data piece
  # should equal the number of rows in the input table
  expect_equal(nrow(pass_data_tbl) + nrow(fail_data_tbl), nrow(small_table))
  
  # Expect no common rows between the two
  # data pieces
  pass_data_tbl %>%
    dplyr::semi_join(
      fail_data_tbl,
      by = c("date_time", "date", "a", "b", "c", "d", "e", "f")
    ) %>%
    nrow() %>%
    expect_equal(0L)
  
  # Expect all column names to be the same between the
  # input table and the two data pieces
  expect_equal(
    colnames(small_table),
    colnames(pass_data_tbl),
    colnames(fail_data_tbl)
  )
  
  # Expect a list of data pieces if `NULL` provided
  # to the `type` argument
  data_tbl_list <- 
    get_sundered_data(agent, type = NULL)
  
  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_is(data_tbl_list, "list")
  expect_equal(names(data_tbl_list), c("pass", "fail"))
  expect_equal(length(data_tbl_list), 2)
  expect_equal(data_tbl_list$pass, pass_data_tbl)
  expect_equal(data_tbl_list$fail, fail_data_tbl)
  
  # Expect no common rows between the two
  # data pieces in the list object
  data_tbl_list$pass %>%
    dplyr::semi_join(
      data_tbl_list$fail,
      by = c("date_time", "date", "a", "b", "c", "d", "e", "f")
    ) %>%
    nrow() %>%
    expect_equal(0L)
  
  # Expect an error if the agent hasn't performed
  # an interrogation before calling `get_sundered_data()`
  expect_error(
    create_agent(tbl = small_table) %>%
      col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
      col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
      rows_distinct(vars(d, e)) %>%
      rows_distinct(vars(a, f)) %>%
      col_vals_gt(vars(d), 100) %>%
      col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
      col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
      get_sundered_data()
  )
})

test_that("sundered data can be generated and retrieved with a `tbl_dbi` (SQLite)", {
  
  # # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <-
    get_sundered_data(agent_sqlite_no_id, type = "pass", id_cols = "date_time")

  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <-
    get_sundered_data(agent_sqlite_no_id, type = "fail", id_cols = "date_time")

  # Expect that both tables are of the same type as the
  # input data
  expect_is(tbl_sqlite, "tbl_dbi")
  expect_is(pass_data_tbl, "tbl_dbi")
  expect_is(fail_data_tbl, "tbl_dbi")

  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl %>% dplyr::collect()), c(8, 8))
  expect_equal(dim(fail_data_tbl %>% dplyr::collect()), c(4, 8))
  
  # Expect that the sum of rows from each data piece
  # should equal the number of rows in the input table
  expect_equal(
    nrow(pass_data_tbl %>% dplyr::collect()) +
      nrow(fail_data_tbl %>% dplyr::collect()),
    nrow(tbl_sqlite %>% dplyr::collect())
  )
  
  # Expect all column names to be the same between the
  # input table and the two data pieces
  expect_equal(
    colnames(tbl_sqlite),
    colnames(pass_data_tbl),
    colnames(fail_data_tbl)
  )
  
  # Expect a list of data pieces if `NULL` provided
  # to the `type` argument
  data_tbl_list <-
    get_sundered_data(agent_sqlite_no_id, type = NULL, id_cols = "date_time")

  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_is(data_tbl_list, "list")
  expect_equal(names(data_tbl_list), c("pass", "fail"))
  expect_equal(length(data_tbl_list), 2)
  expect_equal(data_tbl_list$pass, pass_data_tbl)
  expect_equal(data_tbl_list$fail, fail_data_tbl)

  # Expect an error if the agent hasn't performed
  # an interrogation before calling `get_sundered_data()`
  expect_error(
    create_agent(tbl = tbl_sqlite) %>%
      col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
      col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
      rows_distinct(vars(d, e)) %>%
      rows_distinct(vars(a, f)) %>%
      col_vals_gt(vars(d), 100) %>%
      col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
      col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
      get_sundered_data()
  )
  
  # Expect an error if not providing `id_cols` for
  # a `tbl_dbi` table
  expect_error(get_sundered_data(agent_sqlite_no_id, type = "pass"))
  expect_error(get_sundered_data(agent_sqlite_no_id, type = "fail"))
  expect_error(get_sundered_data(agent_sqlite_no_id, type = NULL))
})
