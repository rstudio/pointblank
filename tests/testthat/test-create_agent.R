context("Creation of `agent` objects")

test_that("Creating a valid `agent` object is possible", {
  
  # Create an agent object
  agent <- create_agent()
  
  # Expect that names in an agent object match a
  # prescribed set of names
  expect_true(
    all(names(agent) ==
          c("validation_name", "validation_time",
            "focal_tbl_name", "focal_file_name",
            "focal_db_type", "focal_col_names",
            "focal_col_types",
            "focal_db_cred_file_path",
            "focal_init_sql", "validation_set")))
  
  # Expect an agent object of class `dgr_graph`
  expect_is(agent, "ptblank_agent")
  
  # Expect that the `validation_set` component is
  # a data frame
  expect_is(agent$validation_set, "tbl_df")
  
  # Expect certain classes for the different
  # `agent` components
  expect_is(agent$validation_name, "character")
  expect_is(agent$validation_time, "POSIXct")
  expect_is(agent$focal_tbl_name, "character")
  expect_is(agent$focal_file_name, "character")
  expect_is(agent$focal_db_type, "character")
  expect_is(agent$focal_col_names, "character")
  expect_is(agent$focal_col_types, "character")
  expect_is(agent$focal_db_cred_file_path, "character")
  expect_is(agent$focal_init_sql, "character")
  expect_is(agent$validation_set$tbl_name, "character")
  expect_is(agent$validation_set$db_type, "character")
  expect_is(agent$validation_set$assertion_type, "character")
  expect_is(agent$validation_set$column, "character")
  expect_is(agent$validation_set$value, "numeric")
  expect_is(agent$validation_set$set, "tbl_df")
  expect_is(agent$validation_set$regex, "character")
  expect_is(agent$validation_set$preconditions, "tbl_df")
  expect_is(agent$validation_set$n, "integer")
  expect_is(agent$validation_set$n_passed, "integer")
  expect_is(agent$validation_set$n_failed, "integer")
  expect_is(agent$validation_set$f_passed, "numeric")
  expect_is(agent$validation_set$f_failed, "numeric")
  expect_is(agent$validation_set$report_count, "numeric")
  expect_is(agent$validation_set$warn_count, "numeric")
  expect_is(agent$validation_set$notify_count, "numeric")
  expect_is(agent$validation_set$report, "logical")
  expect_is(agent$validation_set$warn, "logical")
  expect_is(agent$validation_set$notify, "logical")
  expect_is(agent$validation_set$init_sql, "character")
  expect_is(agent$validation_set$db_cred_file_path, "character")
  expect_is(agent$validation_set$file_path, "character")
  expect_is(agent$validation_set$col_types, "character")
  expect_is(agent$validation_set$time_processed, "POSIXct")
  expect_is(agent$validation_set$proc_duration_s, "numeric")
})
