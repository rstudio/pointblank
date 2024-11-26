test_that("Creating a `col_is_character()` step is possible", {
  
  # Use `col_is_character()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_character")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_date()` step is possible", {
  
  # Use `col_is_date()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name,  "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_date")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_date(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_factor()` step is possible", {
  
  # Use `col_is_factor()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_factor(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_factor")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_factor(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_integer()` step is possible", {
  
  # Use `col_is_integer()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_integer")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_integer(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_logical()` step is possible", {
  
  # Use `col_is_logical()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_logical")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_logical(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_numeric()` step is possible", {
  
  # Use `col_is_numeric()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_numeric")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_numeric(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_is_posix()` step is possible", {
  
  # Use `col_is_posix()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_is_posix")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_is_posix(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_between()` step is possible", {
  
  # Use `col_vals_between()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = vars(b), left = 2, right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_between")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c(2, 10))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = everything(), left = 2, right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_between()` step is possible", {
  
  # Use `col_vals_not_between()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(columns = vars(b), left = 2, right = 10)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_between")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c(2, 10))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_between(columns = everything(), left = 2, right = 10)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_equal()` step is possible", {
  
  # Use `col_vals_equal()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_equal")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_equal(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_equal()` step is possible", {
  
  # Use `col_vals_not_equal()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_equal")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_equal(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gt()` step is possible", {
  
  # Use `col_vals_gt()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_gt")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` select function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_gte()` step is possible", {
  
  # Use `col_vals_gte()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_gte")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_gte(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lt()` step is possible", {
  
  # Use `col_vals_lt()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_lt(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_lt")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_lt(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_lte()` step is possible", {
  
  # Use `col_vals_lte()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_lte(columns = vars(b), value = 5)
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_lte")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), 5)
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_lte(columns = everything(), value = 5)
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_in_set()` step is possible", {
  
  # Use `col_vals_in_set()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(b), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_in_set")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c("1-bcd-345", "5-jdo-903"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = everything(), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_in_set()` step is possible", {
  
  # Use `col_vals_not_in_set()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = vars(b), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_in_set")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c("1-bcd-345", "5-jdo-903"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_in_set(columns = everything(), set = c("1-bcd-345", "5-jdo-903"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_make_set()` step is possible", {
  
  # Use `col_vals_make_set()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = vars(f), set = c("low", "high", "mid"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_set")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c("low", "high", "mid"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_make_set(columns = everything(), set = c("low", "high", "mid"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_make_subset()` step is possible", {
  
  # Use `col_vals_make_subset()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = vars(f), set = c("low", "high"))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_make_subset")
  expect_equal_unlist(validation$validation_set$column, "f")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), c("low", "high"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_make_subset(columns = everything(), set = c("low", "high"))
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_regex()` step is possible", {
  
  # Use `col_vals_regex()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = vars(b), regex = "[0-9]-.*")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_regex")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), "[0-9]-.*")
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = everything(), regex = "[0-9]-.*")
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_within_spec()` step is possible", {
  
  # Use `col_vals_within_spec()` function to create
  # a validation step
  validation <-
    create_agent(tbl = specifications) %>%
    col_vals_within_spec(columns = vars(zip_codes), spec = "zip")
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "specifications")
  expect_equal(validation$col_names, c(
    "isbn_numbers", "vin_numbers", "zip_codes", "credit_card_numbers", 
    "iban_austria", "swift_numbers", "phone_numbers", "email_addresses", 
    "urls", "ipv4_addresses", "ipv6_addresses", "mac_addresses"
  ))
  expect_equal(validation$validation_set$assertion_type, "col_vals_within_spec")
  expect_equal_unlist(validation$validation_set$column, "zip_codes")
  expect_equal(validation$validation_set[["values"]] %>% unlist(), "postal[usa]")
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = specifications) %>%
    col_vals_within_spec(columns = everything(), spec = "zip")
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 12)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c(
      "isbn_numbers", "vin_numbers", "zip_codes", "credit_card_numbers", 
      "iban_austria", "swift_numbers", "phone_numbers", "email_addresses", 
      "urls", "ipv4_addresses", "ipv6_addresses", "mac_addresses"
    )
  )
})

test_that("Creating a `col_vals_null()` step is possible", {
  
  # Use `col_vals_null()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_null")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_null(validation$validation_set[["values"]][[1]])
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_null(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `col_vals_not_null()` step is possible", {
  
  # Use `col_vals_not_null()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = vars(b))
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "col_vals_not_null")
  expect_equal_unlist(validation$validation_set$column, "b")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Validate all available columns using the
  # `everything()` helper function
  validation_all <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(columns = everything())
  
  # Expect 8 rows in the `validation_all$validation_set` object
  expect_equal(nrow(validation_all$validation_set), 8)
  
  # Expect all column names in `validation_all$validation_set$column`
  expect_equal_unlist(
    validation_all$validation_set$column,
    c("date_time", "date", "a", "b", "c", "d", "e", "f"))
})

test_that("Creating a `rows_distinct()` step is possible", {
  
  # Use `rows_distinct()` function to create
  # a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    rows_distinct()
  
  # Expect the class name for the object
  # to be `ptblank_agent`
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(validation$tbl_name, "small_table")
  expect_equal(validation$col_names, c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  expect_equal(validation$validation_set$assertion_type, "rows_distinct")
  expect_equal(validation$validation_set$column %>% unlist(), "date_time, date, a, b, c, d, e, f")
  expect_true(is.null(validation$validation_set[["values"]][[1]]))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
})

test_that("Creating a `row_count_match()` step is possible", {
  
  # Use `row_count_match()` function to create a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = small_table)
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_true(inherits(validation$validation_set[["values"]][[1]], "tbl_df"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Use different inputs for `tbl` in `row_count_match()`
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = ~ small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = small_table ~ pointblank::small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = function() small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.function(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = 13)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.numeric(validation$validation_set[["values"]][[1]]))
  expect_equal(validation$validation_set[["values"]][[1]], 13)
  
  validation <-
    create_agent(tbl = small_table) %>%
    row_count_match(count = 13L)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.numeric(validation$validation_set[["values"]][[1]]))
  expect_equal(validation$validation_set[["values"]][[1]], 13)
})

test_that("Creating a `col_count_match()` step is possible", {
  
  # Use `col_count_match()` function to create a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = small_table)
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_true(inherits(validation$validation_set[["values"]][[1]], "tbl_df"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Use different inputs for `tbl` in `col_count_match()`
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = ~ small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = small_table ~ pointblank::small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = function() small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.function(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = 8)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.numeric(validation$validation_set[["values"]][[1]]))
  expect_equal(validation$validation_set[["values"]][[1]], 8)
  
  validation <-
    create_agent(tbl = small_table) %>%
    col_count_match(count = 8L)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.numeric(validation$validation_set[["values"]][[1]]))
  expect_equal(validation$validation_set[["values"]][[1]], 8)
})

test_that("Creating a `tbl_match()` step is possible", {
  
  # Use `tbl_match()` function to create a validation step
  validation <-
    create_agent(tbl = small_table) %>%
    tbl_match(tbl_compare = small_table)
  expect_s3_class(validation, "ptblank_agent")
  
  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_true(inherits(validation$validation_set[["values"]][[1]], "tbl_df"))
  expect_true(is.na(validation$validation_set$all_passed))
  expect_true(is.na(validation$validation_set$n))
  expect_true(is.na(validation$validation_set$n_passed))
  expect_true(is.na(validation$validation_set$n_failed))
  expect_true(is.na(validation$validation_set$f_passed))
  expect_true(is.na(validation$validation_set$f_failed))
  expect_true(is.na(validation$validation_set$warn))
  expect_true(is.na(validation$validation_set$notify))
  expect_true(is.na(validation$validation_set$row_sample))
  
  # Use different inputs for `tbl` in `tbl_match()`
  validation <-
    create_agent(tbl = small_table) %>%
    tbl_match(tbl_compare = ~ small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    tbl_match(tbl_compare = small_table ~ pointblank::small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(rlang::is_formula(validation$validation_set[["values"]][[1]]))
  
  validation <-
    create_agent(tbl = small_table) %>%
    tbl_match(tbl_compare = function() small_table)
  expect_s3_class(validation, "ptblank_agent")
  expect_true(is.function(validation$validation_set[["values"]][[1]]))
})
