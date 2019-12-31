context("Creation of `agent` objects")

test_that("Creating a valid `agent` object is possible", {
  
  df <-
    data.frame(
      a = c(1, 1, 1, 2, 2, 2),
      b = c(5, 5, 5, 3, 6, 3)
    )
  
  # Create an agent object
  agent <- create_agent(tbl = df)
  
  # Expect that names in an agent object match a
  # prescribed set of names
  expect_true(
    all(
      names(agent) ==
        c("validation_name", "validation_time",
          "focal_tbl", "focal_tbl_name", "focal_tbl_src",
          "focal_col_names", "focal_col_types", "validation_set")
    )
  )
  
  # Expect an agent object of class `ptblank_agent`
  expect_is(agent, "ptblank_agent")
  
  # Expect that the `validation_set` component is a `tbl_df`
  expect_is(agent$validation_set, "tbl_df")
  
  # Expect certain classes for the different `ptblank_agent` components
  expect_is(agent$validation_name, "character")
  expect_is(agent$validation_time, "POSIXct")
  expect_is(agent$focal_tbl, "data.frame")
  expect_is(agent$focal_tbl_name, "character")
  expect_is(agent$focal_tbl_src, "character")
  expect_is(agent$focal_col_names, "character")
  expect_is(agent$focal_col_types, "character")
  expect_is(agent$validation_set$assertion_type, "character")
  expect_is(agent$validation_set$column, "list")
  expect_is(agent$validation_set$value, "numeric")
  expect_is(agent$validation_set$set, "list")
  expect_is(agent$validation_set$regex, "character")
  expect_is(agent$validation_set$preconditions, "list")
  expect_is(agent$validation_set$n, "integer")
  expect_is(agent$validation_set$n_passed, "integer")
  expect_is(agent$validation_set$n_failed, "integer")
  expect_is(agent$validation_set$f_passed, "numeric")
  expect_is(agent$validation_set$f_failed, "numeric")
  expect_is(agent$validation_set$warn_count, "numeric")
  expect_is(agent$validation_set$notify_count, "numeric")
  expect_is(agent$validation_set$warn_fraction, "numeric")
  expect_is(agent$validation_set$notify_fraction, "numeric")
  expect_is(agent$validation_set$warn, "logical")
  expect_is(agent$validation_set$notify, "logical")
  expect_is(agent$validation_set$time_processed, "POSIXct")
  expect_is(agent$validation_set$proc_duration_s, "numeric")
  
  # Create an agent with a name
  agent_name <- create_agent(tbl = df, name = "test")
  
  # Expect that the agent name has been set
  expect_equivalent(agent_name$validation_name, "test")
})
