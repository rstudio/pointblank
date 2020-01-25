context("Creation of `agent` objects")

test_that("Creating a valid `agent` object is possible", {
  
  tbl <-
    dplyr::tibble(
      a = c(1, 1, 1, 2, 2, 2),
      b = c(5, 5, 5, 3, 6, 3)
    )
  
  # Create an agent object
  agent <- create_agent(tbl = tbl)
  
  # Expect that names in an agent object match a
  # prescribed set of names
  expect_true(
    all(
      names(agent) ==
        c("name", "time", "tbl", "tbl_name", "tbl_src", "tbl_src_details",
          "col_names", "col_types", "validation_set", "extracts")
    )
  )
  
  # Expect an agent object of class `ptblank_agent`
  expect_is(agent, "ptblank_agent")
  
  # Expect that the `validation_set` component is a `tbl_df`
  expect_is(agent$validation_set, "tbl_df")
  
  # Expect certain classes for the different `ptblank_agent` components
  expect_is(agent$name, "character")
  expect_is(agent$time, "POSIXct")
  expect_is(agent$tbl, class(tbl))
  expect_is(agent$tbl_name, "character")
  expect_is(agent$tbl_src, "character")
  expect_is(agent$tbl_src_details, "character")
  expect_is(agent$col_names, "character")
  expect_is(agent$col_types, "character")
  expect_is(agent$validation_set$i, "integer")
  expect_is(agent$validation_set$assertion_type, "character")
  expect_is(agent$validation_set$column, "list")
  expect_is(agent$validation_set$values, "list")
  expect_is(agent$validation_set$na_pass, "logical")
  expect_is(agent$validation_set$preconditions, "list")
  expect_is(agent$validation_set$actions, "list")
  expect_is(agent$validation_set$brief, "character")
  expect_is(agent$validation_set$eval_error, "logical")
  expect_is(agent$validation_set$eval_warning, "logical")
  expect_is(agent$validation_set$capture_stack, "list")
  expect_is(agent$validation_set$all_passed, "logical")
  expect_is(agent$validation_set$n, "integer")
  expect_is(agent$validation_set$n_passed, "integer")
  expect_is(agent$validation_set$n_failed, "integer")
  expect_is(agent$validation_set$f_passed, "numeric")
  expect_is(agent$validation_set$f_failed, "numeric")
  expect_is(agent$validation_set$warn, "logical")
  expect_is(agent$validation_set$notify, "logical")
  expect_is(agent$validation_set$stop, "logical")
  expect_is(agent$validation_set$row_sample, "numeric")
  expect_is(agent$validation_set$time_processed, "POSIXct")
  expect_is(agent$validation_set$proc_duration_s, "numeric")
  expect_is(agent$extracts, "list")
  
  # Create an agent with a name
  agent_name <- create_agent(tbl = tbl, name = "test")
  
  # Expect that the agent name has been set
  expect_equivalent(agent_name$name, "test")
})
