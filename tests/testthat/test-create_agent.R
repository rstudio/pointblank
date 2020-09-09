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
        c("tbl", "read_fn", "tbl_name", "label",
          "db_tbl_name", "tbl_src", "tbl_src_details", "col_names",
          "col_types", "db_col_types", "actions", "end_fns", "embed_report",
          "reporting", "lang", "locale", "time_start", "time_end",
          "validation_set", "extracts"
        )
    )
  )
  
  # Expect an agent object of class `ptblank_agent`
  expect_is(agent, "ptblank_agent")
  
  # Expect that the `validation_set` component is a `tbl_df`
  expect_is(agent$validation_set, "tbl_df")
  
  # Expect certain classes for the different `ptblank_agent` components
  expect_is(agent$tbl, class(tbl))
  expect_null(agent$read_fn)
  expect_is(agent$tbl_name, "character")
  expect_is(agent$label, "character")
  expect_is(agent$tbl_src, "character")
  expect_is(agent$tbl_src_details, "character")
  expect_is(agent$col_names, "character")
  expect_is(agent$col_types, "character")
  expect_is(agent$db_col_types, "character")
  expect_null(agent$actions)
  expect_is(agent$end_fns, "list")
  expect_is(agent$embed_report, "logical")
  expect_is(agent$lang, "character")
  expect_is(agent$time_start, "POSIXct")
  expect_is(agent$time_end, "POSIXct")
  expect_is(agent$validation_set$i, "integer")
  expect_is(agent$validation_set$assertion_type, "character")
  expect_is(agent$validation_set$column, "list")
  expect_is(agent$validation_set$values, "list")
  expect_is(agent$validation_set$na_pass, "logical")
  expect_is(agent$validation_set$preconditions, "list")
  expect_is(agent$validation_set$actions, "list")
  expect_is(agent$validation_set$brief, "character")
  expect_is(agent$validation_set$active, "logical")
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
  expect_is(agent$validation_set$tbl_checked, "list")
  expect_is(agent$validation_set$time_processed, "POSIXct")
  expect_is(agent$validation_set$proc_duration_s, "numeric")
  expect_is(agent$extracts, "list")
  
  # Expect that the agent label is a formatted date-time
  expect_match(agent$label, "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$")
  
  # Expect that the table name was correctly guessed as `tbl`
  expect_match(agent$tbl_name, "tbl")
  
  # Create an agent with a label and a table name
  agent_name <- create_agent(tbl = tbl, tbl_name = "table", label = "test")
  
  # Expect that the agent label has been set
  expect_equivalent(agent_name$tbl_name, "table")
  expect_equivalent(agent_name$label, "test")
})
