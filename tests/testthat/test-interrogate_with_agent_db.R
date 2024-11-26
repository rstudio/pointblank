library(DBI)
library(RSQLite)

con <- 
  DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = system.file(
      "data_files", "small_table.db",
      package = "pointblank"
    )
  )

small_table <- dplyr::tbl(con, "small_table")

on.exit(DBI::dbDisconnect(conn = con)) 

test_that("Interrogating with an agent yields the correct results", {
  
  # Use the `col_schema_match()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_schema_match(schema = col_schema(.tbl = small_table)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_schema_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_s3_class(validation$validation_set[["values"]][[1]], "col_schema")
  expect_s3_class(validation$validation_set[["values"]][[1]], "r_type")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  expect_equal(nrow(validation$validation_set), 1)
  
  
  # Use the `col_schema_match()` function with a `col_schema` object
  # to create a validation step, then, `interrogate()`
  schema <- 
    col_schema(
      date_time = "numeric", date = "numeric", 
      a = "integer", b = "character", c = "numeric",
      d = "numeric", e = "integer", f = "character"
    )
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_schema_match(schema = schema) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_schema_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_s3_class(validation$validation_set[["values"]][[1]], "col_schema")
  expect_s3_class(validation$validation_set[["values"]][[1]], "r_type")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_schema_match()` function with a `col_schema` object
  # to create a validation step, then, `interrogate()`
  schema <- 
    col_schema(
      date_time = "REAL", date = "REAL", 
      a = "INTEGER", b = "TEXT", c = "REAL",
      d = "REAL", e = "INTEGER", f = "TEXT",
      .db_col_types = "sql"
    )
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_schema_match(schema = schema) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_schema_match")
  expect_true(is.na(validation$validation_set$column %>% unlist()))
  expect_s3_class(validation$validation_set[["values"]][[1]], "col_schema")
  expect_s3_class(validation$validation_set[["values"]][[1]], "sql_type")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_exists()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(b)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_exists")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_exists()` function to create
  # a validation step, then, `interrogate()`
  # (expecting a failed interrogation)
  validation <-
    create_agent(tbl = small_table) %>%
    col_exists(columns = vars(g)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_exists")
  expect_equal_unlist(validation$validation_set$column, "g")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 0)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0)
  expect_equal(validation$validation_set$f_failed, 1)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_is_character()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = vars(b)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_is_character")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_is_numeric()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equal_unlist(validation$validation_set$column, "a")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 0)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0)
  expect_equal(validation$validation_set$f_failed, 1)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # TODO: this validation step function should be disallowed for SQLite
  # Use the `col_is_posix()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = vars(date_time)) %>%
    interrogate()
  
  # TODO: this validation step function should be disallowed for SQLite
  # Use the `col_is_date()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = vars(date)) %>%
    interrogate()
  
  # Use the `col_is_integer()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = vars(a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_is_integer")
  expect_equal_unlist(validation$validation_set$column, "a")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 1)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # TODO: this validation step function should be disallowed for SQLite
  # Use the `col_is_logical()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = vars(e)) %>%
    interrogate()
})

test_that("Interrogating for valid row values", {
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`
  # validation <-
  #   create_agent(tbl = small_table) %>%
  #   col_vals_between(
  #     columns = vars(d),
  #     left = 0, right = 5000
  #   ) %>%
  #   interrogate()
  # 
  # # Expect certain values in `validation$validation_set`
  # expect_equal(validation$tbl_name, "small_table")
  # expect_equal(validation$validation_set$assertion_type, "col_vals_between")
  # expect_equal_unlist(validation$validation_set$column, "d")
  # expect_true(is.list(validation$validation_set[["values"]]))
  # expect_false(validation$validation_set$all_passed)
  # expect_equal(validation$validation_set$n, 13)
  # expect_equal(validation$validation_set$n_passed, 12)
  # expect_equal(validation$validation_set$n_failed, 1)
  # expect_equal(validation$validation_set$f_passed, 0.92308)
  # expect_equal(validation$validation_set$f_failed, 0.07692)
  # 
  # # Expect a single row in `validation$validation_set`
  # expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  # validation <-
  #   create_agent(tbl = small_table) %>%
  #   col_vals_between(
  #     columns = vars(d),
  #     left = 0, right = 5000,
  #     preconditions = ~ . %>% dplyr::filter(date > 16805)
  #   ) %>%
  #   interrogate()
  # 
  # # Expect certain values in `validation$validation_set`
  # expect_equal(validation$tbl_name, "small_table")
  # expect_equal(validation$validation_set$assertion_type, "col_vals_between")
  # expect_equal_unlist(validation$validation_set$column, "d")
  # expect_true(is.list(validation$validation_set[["values"]]))
  # expect_true(validation$validation_set$all_passed)
  # expect_equal(validation$validation_set$n, 10)
  # expect_equal(validation$validation_set$n_passed, 10)
  # expect_equal(validation$validation_set$n_failed, 0)
  # expect_equal(validation$validation_set$f_passed, 1)
  # expect_equal(validation$validation_set$f_failed, 0)
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`; using
  # column names for `left` and `right`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(c),
      left = vars(a), right = vars(d)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_between")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_type(validation$validation_set[["values"]], "list")
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 7)
  expect_equal(validation$validation_set$n_failed, 6)
  expect_equal(validation$validation_set$f_passed, 0.53846)
  expect_equal(validation$validation_set$f_failed, 0.46154)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_between()` function to create
  # a validation step, then, `interrogate()`; using
  # a column name for `right`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(
      columns = vars(c),
      left = 0, right = vars(d)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_between")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_type(validation$validation_set[["values"]], "list")
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 11)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.84615)
  expect_equal(validation$validation_set$f_failed, 0.15385)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_not_between()` function to create
  # a validation step, then, `interrogate()`
  # validation <-
  #   create_agent(tbl = small_table) %>%
  #   col_vals_not_between(
  #     columns = vars(d),
  #     left = 500, right = 1000
  #   ) %>%
  #   interrogate()
  # 
  # # Expect certain values in `validation$validation_set`
  # expect_equal(validation$tbl_name, "small_table")
  # expect_equal(validation$validation_set$assertion_type, "col_vals_not_between")
  # expect_equal_unlist(validation$validation_set$column, "d")
  # expect_true(is.list(validation$validation_set[["values"]]))
  # expect_false(validation$validation_set$all_passed)
  # expect_equal(validation$validation_set$n, 13)
  # expect_equal(validation$validation_set$n_passed, 9)
  # expect_equal(validation$validation_set$n_failed, 4)
  # expect_equal(validation$validation_set$f_passed, 0.69231)
  # expect_equal(validation$validation_set$f_failed, 0.30769)
  # 
  # # Expect a single row in `validation$validation_set`
  # expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_not_between()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  # validation <-
  #   create_agent(tbl = small_table) %>%
  #   col_vals_not_between(
  #     columns = vars(d),
  #     left = 500, right = 1000,
  #     preconditions = ~ . %>% dplyr::filter(date > 16805)
  #   ) %>%
  #   interrogate()
  # 
  # # Expect certain values in `validation$validation_set`
  # expect_equal(validation$tbl_name, "small_table")
  # expect_equal(validation$validation_set$assertion_type, "col_vals_not_between")
  # expect_equal_unlist(validation$validation_set$column, "d")
  # expect_true(is.list(validation$validation_set[["values"]]))
  # expect_false(validation$validation_set$all_passed)
  # expect_equal(validation$validation_set$n, 10)
  # expect_equal(validation$validation_set$n_passed, 6)
  # expect_equal(validation$validation_set$n_failed, 4)
  # expect_equal(validation$validation_set$f_passed, 0.6)
  # expect_equal(validation$validation_set$f_failed, 0.4)
  
  # Use the `col_vals_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = vars(d), value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equal_unlist(validation$validation_set$column, "d")
  expect_equal(validation$validation_set[["values"]] %>% unlist, 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 12)
  expect_equal(validation$validation_set$f_passed, 0.07692)
  expect_equal(validation$validation_set$f_failed, 0.92308)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ . %>% dplyr::filter(date > 16805)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equal_unlist(validation$validation_set$column, "d")
  expect_equal(validation$validation_set[["values"]] %>% unlist, 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 10)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 9)
  expect_equal(validation$validation_set$f_passed, 0.1)
  expect_equal(validation$validation_set$f_failed, 0.9)
  
  # Use the `col_vals_not_equal()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = vars(d), value = 283.94) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equal_unlist(validation$validation_set$column, "d")
  expect_equal(validation$validation_set[["values"]] %>% unlist, 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 12)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0.92308)
  expect_equal(validation$validation_set$f_failed, 0.07692)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_equal()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(
      columns = vars(d),
      value = 283.94,
      preconditions = ~ . %>% dplyr::filter(date > 16805)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equal_unlist(validation$validation_set$column, "d")
  expect_equal(validation$validation_set[["values"]] %>% unlist, 283.94)
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 10)
  expect_equal(validation$validation_set$n_passed, 9)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0.9)
  expect_equal(validation$validation_set$f_failed, 0.1)

  # Use the `rows_distinct()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct() %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "rows_distinct")
  expect_equal(validation$validation_set$column %>% unlist(), "date_time, date, a, b, c, d, e, f")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 11)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.84615)
  expect_equal(validation$validation_set$f_failed, 0.15385)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `rows_distinct()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct(
      preconditions = ~ . %>% dplyr::filter(date != 16820)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "rows_distinct")
  expect_equal(validation$validation_set$column %>% unlist(), "date_time, date, a, b, c, d, e, f")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 11)
  expect_equal(validation$validation_set$n_passed, 11)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Use the `rows_distinct()` function to create
  # a validation step for selected columns, then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct(columns = vars(date_time, a)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "rows_distinct")
  expect_equal(validation$validation_set$column %>% unlist(), "date_time, a")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 11)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.84615)
  expect_equal(validation$validation_set$f_failed, 0.15385)
  
  # Use the `col_vals_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_in_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 13)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_not_in_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = vars(f), set = c("lower", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 13)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Make another pass with the `col_vals_not_in_set()`
  # function to create a validation step, then, perform an
  # interrogation that fails every row
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = vars(f), set = c("lower", "middle", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 13)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 4)
  expect_equal(validation$validation_set$n_passed, 4)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Make another pass with `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "mid")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 3)
  expect_equal(validation$validation_set$n_passed, 2)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0.66667)
  expect_equal(validation$validation_set$f_failed, 0.33333)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Make another pass with `col_vals_make_set()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "middle")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 3)
  expect_equal(validation$validation_set$n_passed, 1)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.33333)
  expect_equal(validation$validation_set$f_failed, 0.66667)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid", "high")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 3)
  expect_equal(validation$validation_set$n_passed, 3)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Make another pass with the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_type(validation$validation_set[["values"]], "list")
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 2)
  expect_equal(validation$validation_set$n_passed, 2)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Make another pass with the `col_vals_make_subset()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "mid", "high", "higher")) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_true(is.list(validation$validation_set[["values"]]))
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 4)
  expect_equal(validation$validation_set$n_passed, 3)
  expect_equal(validation$validation_set$n_failed, 1)
  expect_equal(validation$validation_set$f_passed, 0.75)
  expect_equal(validation$validation_set$f_failed, 0.25)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_not_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = vars(c)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 11)
  expect_equal(validation$validation_set$n_failed, 2)
  expect_equal(validation$validation_set$f_passed, 0.84615)
  expect_equal(validation$validation_set$f_failed, 0.15385)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_not_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(
      columns = vars(c),
      preconditions = ~ . %>%
        dplyr::filter(date > 16809 & date < 16820)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 3)
  expect_equal(validation$validation_set$n_passed, 3)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
  
  # Use the `col_vals_null()` function to create
  # a validation step, then, `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = vars(c)) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_null")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_false(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 13)
  expect_equal(validation$validation_set$n_passed, 2)
  expect_equal(validation$validation_set$n_failed, 11)
  expect_equal(validation$validation_set$f_passed, 0.15385)
  expect_equal(validation$validation_set$f_failed, 0.84615)
  
  # Expect a single row in `validation$validation_set`
  expect_equal(nrow(validation$validation_set), 1)
  
  # Use the `col_vals_null()` function to create
  # a validation step (with a precondition), then,
  # `interrogate()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(
      columns = vars(c),
      preconditions = ~ . %>%
        dplyr::filter(date == 16806 | date == 16830)
    ) %>%
    interrogate()
  
  # Expect certain values in `validation$validation_set`
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$validation_set$assertion_type, "col_vals_null")
  expect_equal_unlist(validation$validation_set$column, "c")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(validation$validation_set$all_passed)
  expect_equal(validation$validation_set$n, 2)
  expect_equal(validation$validation_set$n_passed, 2)
  expect_equal(validation$validation_set$n_failed, 0)
  expect_equal(validation$validation_set$f_passed, 1)
  expect_equal(validation$validation_set$f_failed, 0)
})

test_that("Interrogating with an agent incorporates the `na_pass` option", {
  
  # Use the `col_vals_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(g),
      value = 1.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_equal(
      columns = vars(g),
      value = 1.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_equal()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(g),
      value = 2.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_not_equal(
      columns = vars(g),
      value = 2.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gt()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(g),
      value = 0.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_gt(
      columns = vars(g),
      value = 0.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_gte()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(g),
      value = 1.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_gte(
      columns = vars(g),
      value = 1.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lt()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(g),
      value = 3.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_lt(
      columns = vars(g),
      value = 3.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_lte()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(g),
      value = 1.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_lte(
      columns = vars(g),
      value = 1.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(g),
      left = 0, right = 3.0,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_between(
      columns = vars(g),
      left = 0, right = 3.0,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_between()` function to perform
  # a validation step with NAs, switching the
  # value of the `na_pass` option
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(g),
      left = 3.0, right = 4.5,
      na_pass = FALSE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table %>%
    dplyr::mutate(g = ifelse(!is.na(c), 1.5, NA_real_)) %>%
    create_agent() %>%
    col_vals_not_between(
      columns = vars(g),
      left = 3.0, right = 4.5,
      na_pass = TRUE,
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
})

test_that("The validations with sets can include NA values", {
  
  small_table_na_2 <- 
    small_table %>%
    dplyr::mutate(g = dplyr::case_when(
      is.na(c) ~ NA_character_,
      f == "low" ~ "one",
      f == "mid" ~ "two",
      f == "high" ~ "three"
    ))
  
  # Use the `col_vals_in_set()` function to perform
  # a validation step with NAs
  small_table_na_2 %>%
    create_agent() %>%
    col_vals_in_set(
      columns = vars(g),
      set = c("one", "two", "three", "four", "five"),
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table_na_2 %>%
    create_agent() %>%
    col_vals_in_set(
      columns = vars(g),
      set = c("one", "two", "three", "four", "five", NA),
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
  
  # Use the `col_vals_not_in_set()` function to perform
  # a validation step with NAs
  small_table_na_2 %>%
    create_agent() %>%
    col_vals_not_in_set(
      columns = vars(g),
      set = c("four", "five", "six", NA),
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_false()
  
  small_table_na_2 %>%
    create_agent() %>%
    col_vals_not_in_set(
      columns = vars(g),
      set = c("four", "five", "six"),
      actions = action_levels(warn_at = 1)
    ) %>%
    interrogate() %>%
    all_passed() %>%
    expect_true()
})
