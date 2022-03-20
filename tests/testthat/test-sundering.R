skip_on_cran()

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

# Create an agent with one validation step, perform interrogation
agent_1 <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(d), 1000) %>%
  interrogate()

# Create an agent with no validation steps, perform interrogation
agent_zero <-
  create_agent(tbl = small_table) %>%
  interrogate()

# Create a precondition formula
p_fn <- ~ . %>% dplyr::filter(f == "high")

# Create an agent, add validation steps with the same
# preconditions applied at each step, perform interrogation
agent_p_equal <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE, preconditions = p_fn) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE, preconditions = p_fn) %>%
  rows_distinct(vars(d, e), preconditions = p_fn) %>%
  rows_distinct(vars(a, f), preconditions = p_fn) %>%
  col_vals_gt(vars(d), 100, preconditions = p_fn) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE, preconditions = p_fn) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE, preconditions = p_fn) %>%
  interrogate()

# Create an agent, add validation steps with preconditions
# applied at certain steps, perform interrogation
agent_p_mixed <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE, preconditions = p_fn) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE, preconditions = p_fn) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f), preconditions = p_fn) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE, preconditions = p_fn) %>%
  col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
  interrogate()

# Create an agent, add validation steps with preconditions
# applied at non-considered steps (i.e., those steps that
# are not used to determine splitting), perform interrogation
agent_p_unused <-
  create_agent(tbl = small_table) %>%
  col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
  col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
  rows_distinct(vars(d, e)) %>%
  rows_distinct(vars(a, f), preconditions = p_fn) %>%
  col_vals_gt(vars(d), 100) %>%
  col_vals_equal(vars(d), vars(d), na_pass = TRUE, preconditions = p_fn, active = FALSE) %>%
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
  expect_s3_class(small_table, "tbl_df")
  expect_s3_class(pass_data_tbl, "tbl_df")
  expect_s3_class(fail_data_tbl, "tbl_df")
  
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
  data_tbl_list <- get_sundered_data(agent, type = NULL)
  
  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_type(data_tbl_list, "list")
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

test_that("sundered data can be combined to a single `tbl_df` with a single flag column", {
  
  # Use the `"combined"` option in `get_sundered_data()`
  combined_data_tbl <- get_sundered_data(agent, type = "combined")
  
  # Expect that the table is of the same type as the input data
  expect_s3_class(small_table, "tbl_df")
  expect_s3_class(combined_data_tbl, "tbl_df")
  
  # Expect there to be the same number of rows as the
  # input table but one more column
  expect_equal(nrow(combined_data_tbl), nrow(small_table))
  expect_equal(ncol(combined_data_tbl), ncol(small_table) + 1)
  
  # Expect the same column names as the input table but
  # with one more column at the end
  expect_equal(
    colnames(combined_data_tbl),
    c(colnames(small_table), ".pb_combined")
  )
  
  # Using different encodings for the 'pass' and 'label' status, expect
  # the column type and available values to change accordingly
  c_1 <- get_sundered_data(agent, type = "combined", pass_fail = c(1, 0))
  c_2 <- get_sundered_data(agent, type = "combined", pass_fail = c(TRUE, FALSE))
  c_3 <- get_sundered_data(agent, type = "combined", pass_fail = c("good", "bad"))
  c_4 <- get_sundered_data(agent, type = "combined", pass_fail = c(1L, 0L))
  expect_true(inherits(c_1$.pb_combined, "numeric"))
  expect_true(inherits(c_2$.pb_combined, "logical"))
  expect_true(inherits(c_3$.pb_combined, "character"))
  expect_true(inherits(c_4$.pb_combined, "integer"))
  expect_equal(unique(c_1$.pb_combined), c(1, 0))
  expect_equal(unique(c_2$.pb_combined), c(TRUE, FALSE))
  expect_equal(unique(c_3$.pb_combined), c("good", "bad"))
  expect_equal(unique(c_4$.pb_combined), c(1L, 0L))
  
  # Expect the rows to be in the same order as the input table
  expect_equivalent(combined_data_tbl %>% dplyr::select(-.pb_combined), small_table)
})

test_that("sundered data can be generated and retrieved with a `tbl_df` (one step)", {
  
  # These tests ensure that a single-step interrogation works
  
  # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <- 
    get_sundered_data(agent_1, type = "pass")
  
  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <- 
    get_sundered_data(agent_1, type = "fail")
  
  # Expect that both tables are of the same type as the
  # input data
  expect_s3_class(small_table, "tbl_df")
  expect_s3_class(pass_data_tbl, "tbl_df")
  expect_s3_class(fail_data_tbl, "tbl_df")
  
  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl), c(7, 8))
  expect_equal(dim(fail_data_tbl), c(6, 8))
  
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
  data_tbl_list <- get_sundered_data(agent_1, type = NULL)
  
  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_type(data_tbl_list, "list")
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
})

test_that("sundered data can be generated and retrieved with a `tbl_df` (no steps)", {
  
  # These tests ensure that a no-step interrogation works
  
  # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <- agent_zero %>% get_sundered_data(type = "pass")
  
  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <- agent_zero %>% get_sundered_data(type = "fail")
  
  # Expect that both tables are of the same type as the
  # input data
  expect_s3_class(small_table, "tbl_df")
  expect_s3_class(pass_data_tbl, "tbl_df")
  expect_s3_class(fail_data_tbl, "tbl_df")
  
  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl %>% dplyr::collect()), c(13, 8))
  expect_equal(dim(fail_data_tbl %>% dplyr::collect()), c(0, 8))
  
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
  data_tbl_list <- get_sundered_data(agent_zero, type = NULL)
  
  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_type(data_tbl_list, "list")
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
})

test_that("sundering can occur (with exceptions) when there are preconditions", {
  
  # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <- agent_p_equal %>% get_sundered_data(type = "pass")
  
  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <- agent_p_equal %>% get_sundered_data(type = "fail")
  
  # Get the 'combined' data table using `get_sundered_data()`
  combined_data_tbl <- agent_p_equal %>% get_sundered_data(type = "combined")
  
  # Expect that all tables are of the same type as the
  # input data
  expect_s3_class(small_table, "tbl_df")
  expect_s3_class(pass_data_tbl, "tbl_df")
  expect_s3_class(fail_data_tbl, "tbl_df")
  expect_s3_class(combined_data_tbl, "tbl_df")
  
  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl), c(4, 8))
  expect_equal(dim(fail_data_tbl), c(2, 8))
  expect_equal(dim(combined_data_tbl), c(6, 9))
  
  # Expect that the last column of `combined_data_tbl` is named `.pb_combined`
  expect_equal(rev(names(combined_data_tbl))[1], ".pb_combined")
  
  # Expect that the last column of `combined_data_tbl` contains
  # only `"pass"` and `"fail"` entries
  expect_equal(
    combined_data_tbl %>% dplyr::pull(.pb_combined) %>% unique(),
    c("pass", "fail")
  )
  
  # Expect that the sum of rows from each data piece
  # should equal the number of rows in the input table
  # after preconditions are applied
  expect_equal(
    nrow(pass_data_tbl) + nrow(fail_data_tbl),
    nrow(small_table %>% dplyr::filter(f == "high"))
  )
  
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
  data_tbl_list <- get_sundered_data(agent_p_equal, type = NULL)
  
  # Expect the resulting list to hold the
  # same two data pieces as before
  expect_type(data_tbl_list, "list")
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
  
  # Expect an error when sundering if there are 
  # mixed preconditions on the validations that
  # are ultimately used for splitting
  expect_error(agent_p_mixed %>% get_sundered_data(type = "pass"))
  expect_error(agent_p_mixed %>% get_sundered_data(type = "fail"))
  expect_error(agent_p_mixed %>% get_sundered_data(type = "combined"))
  expect_error(agent_p_mixed %>% get_sundered_data(type = NULL))
  
  # Expect no error if there are mixed preconditions across
  # the validations but the validation steps actually used for
  # sundering don't have mixed preconditions
  expect_error(regexp = NA, agent_p_unused %>% get_sundered_data(type = "pass"))
  expect_error(regexp = NA, agent_p_unused %>% get_sundered_data(type = "fail"))
  expect_error(regexp = NA, agent_p_unused %>% get_sundered_data(type = "combined"))
  expect_error(regexp = NA, agent_p_unused %>% get_sundered_data(type = NULL))
  
  # Get the 'pass' data piece using `get_sundered_data()` and `agent_p_unused`
  pass_data_tbl_u <- agent_p_unused %>% get_sundered_data(type = "pass")
  
  # Get the 'fail' data piece using `get_sundered_data()` and `agent_p_unused`
  fail_data_tbl_u <- agent_p_unused %>% get_sundered_data(type = "fail")
  
  # Get the 'combined' data table using `get_sundered_data()` and `agent_p_unused`
  combined_data_tbl_u <- agent_p_unused %>% get_sundered_data(type = "combined")
  
  # Expect that all tables are of the same type as the
  # input data
  expect_s3_class(pass_data_tbl_u, "tbl_df")
  expect_s3_class(fail_data_tbl_u, "tbl_df")
  expect_s3_class(combined_data_tbl_u, "tbl_df")
  
  # Expect certain dimensions for each table
  expect_equal(dim(pass_data_tbl_u), c(9, 8))
  expect_equal(dim(fail_data_tbl_u), c(4, 8))
  expect_equal(dim(combined_data_tbl_u), c(13, 9))
})

test_that("sundered data can be generated and retrieved with a `tbl_dbi` (SQLite)", {
  
  # Get the 'pass' data piece using `get_sundered_data()`
  pass_data_tbl <-
    get_sundered_data(agent_sqlite_no_id, type = "pass", id_cols = "date_time")

  # Get the 'fail' data piece using `get_sundered_data()`
  fail_data_tbl <-
    get_sundered_data(agent_sqlite_no_id, type = "fail", id_cols = "date_time")

  # Expect that both tables are of the same type as the
  # input data
  expect_s3_class(tbl_sqlite, "tbl_dbi")
  expect_s3_class(pass_data_tbl, "tbl_dbi")
  expect_s3_class(fail_data_tbl, "tbl_dbi")

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
  expect_type(data_tbl_list, "list")
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

test_that("an error occurs if using `get_sundered_data()` when agent has no intel", {
  
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
