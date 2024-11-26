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
  expect_named(
    agent,
    c(
      "tbl", "read_fn", "tbl_name", "label", "db_tbl_name", "tbl_src",
      "tbl_src_details", "col_names", "col_types", "db_col_types",
      "actions", "end_fns", "embed_report", "reporting", "lang",
      "locale", "time_start", "time_end", "validation_set", "extracts"
    )
  )
  
  # Expect an agent object of class `ptblank_agent`
  expect_s3_class(agent, "ptblank_agent")
  
  # Expect that the `validation_set` component is a `tbl_df`
  expect_s3_class(agent$validation_set, "tbl_df")
  
  # Expect certain classes for the different `ptblank_agent` components
  expect_s3_class(agent$tbl, class(tbl), exact = TRUE)
  expect_null(agent$read_fn)
  expect_type(agent$tbl_name, "character")
  expect_type(agent$label, "character")
  expect_type(agent$tbl_src, "character")
  expect_type(agent$tbl_src_details, "character")
  expect_type(agent$col_names, "character")
  expect_type(agent$col_types, "character")
  expect_type(agent$db_col_types, "character")
  expect_null(agent$actions)
  expect_type(agent$end_fns, "list")
  expect_type(agent$embed_report, "logical")
  expect_type(agent$lang, "character")
  expect_s3_class(agent$time_start, "POSIXct")
  expect_s3_class(agent$time_end, "POSIXct")
  expect_type(agent$validation_set$i, "integer")
  expect_type(agent$validation_set$assertion_type, "character")
  expect_type(agent$validation_set$column, "list")
  expect_type(agent$validation_set$values, "list")
  expect_type(agent$validation_set$na_pass, "logical")
  expect_type(agent$validation_set$preconditions, "list")
  expect_type(agent$validation_set$actions, "list")
  expect_type(agent$validation_set$brief, "character")
  expect_type(agent$validation_set$active, "list")
  expect_type(agent$validation_set$eval_active, "logical")
  expect_type(agent$validation_set$eval_error, "logical")
  expect_type(agent$validation_set$eval_warning, "logical")
  expect_type(agent$validation_set$capture_stack, "list")
  expect_type(agent$validation_set$all_passed, "logical")
  expect_type(agent$validation_set$n, "integer")
  expect_type(agent$validation_set$n_passed, "integer")
  expect_type(agent$validation_set$n_failed, "integer")
  expect_type(agent$validation_set$f_passed, "double")
  expect_type(agent$validation_set$f_failed, "double")
  expect_type(agent$validation_set$warn, "logical")
  expect_type(agent$validation_set$notify, "logical")
  expect_type(agent$validation_set$stop, "logical")
  expect_type(agent$validation_set$row_sample, "double")
  expect_type(agent$validation_set$tbl_checked, "list")
  expect_s3_class(agent$validation_set$time_processed, "POSIXct")
  expect_type(agent$validation_set$proc_duration_s, "double")
  expect_type(agent$extracts, "list")
  
  # Expect that the agent label is a formatted date-time
  expect_match(agent$label, "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$")
  
  # Expect that the table name was correctly guessed as `tbl`
  expect_match(agent$tbl_name, "tbl")
  
  # Create an agent with a label and a table name
  agent_name <- create_agent(tbl = tbl, tbl_name = "table", label = "test")
  
  # Expect that the agent label has been set
  expect_equal(agent_name$tbl_name, "table")
  expect_equal(agent_name$label, "test")
  
  # Expect an error if nothing is provided for
  # either `tbl` or `read_fn`
  expect_error(create_agent())
  
  # Expect that when a table is piped into `create_agent()`
  # and also when `tbl_name` isn't provided, the table
  # name isn't known so it's assigned as `NA`
  expect_equal(
    small_table %>% create_agent() %>% .$tbl_name,
    NA_character_
  )
  
  # Expect that using a table-prep formula to read in a table but
  # not specifying `tbl_name` results in an variation of the formula
  # RHS for `tbl_name`
  expect_equal(
    create_agent(tbl = ~ pointblank::small_table) %>% .$tbl_name,
    "~pointblank::small_table"
  )
  
  # Expect that using a `read_fn` to read in a table but
  # not specifying `tbl_name` results in an `NA` for `tbl_name`; also,
  # expect a warning
  expect_equal(
    expect_warning(
      create_agent(read_fn = ~ pointblank::small_table)
    ) %>% .$tbl_name,
    NA_character_
  )
  
  # Expect that if a table is supplied to both `tbl` and `read_fn`
  # there is a preference for using the value from `tbl`; also,
  # expect a warning and a message
  agent_2 <- 
    expect_message(
      expect_warning(
        create_agent(
          tbl = tbl,
          read_fn = ~ pointblank::small_table
        )
      )
    )
  
  expect_equal(agent_2$col_names, c("a", "b"))
  
  agent_3 <-
    expect_message(
      expect_warning(
        create_agent(
          tbl = tbl,
          read_fn = function() pointblank::small_table
        )
      )
    )
  
  expect_equal(agent_3$col_names, c("a", "b"))
  
  # Expect that the `embed_report` option will always
  # be TRUE if we use any `end_fns` (even if we set
  # it to FALSE)
  agent_4 <- 
    create_agent(
      tbl = ~ pointblank::small_table,
      end_fns = ~ print(Sys.time()),
      embed_report = FALSE
    )
  
  expect_true(agent_4$embed_report)
})
