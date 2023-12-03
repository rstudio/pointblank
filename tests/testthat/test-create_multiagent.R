test_that("Creating a valid `multiagent` object is possible", {
  
  al <- action_levels(0.05, 0.10, 0.20)
  
  agent_1 <-
    create_agent(
      tbl = ~ small_table,
      label = "An example.",
      actions = al
    ) %>%
    col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
    col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
    rows_distinct() %>%
    col_vals_gt(vars(d), 100) %>%
    col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
    col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
    col_vals_not_between(vars(c), left = 10, right = 20, na_pass = TRUE) %>%
    rows_distinct(vars(d, e, f)) %>%
    col_is_integer(vars(a)) %>%
    interrogate()
  
  agent_2 <- 
    create_agent(
      tbl = ~ small_table,
      label = "An example.",
      actions = al
    ) %>%
    col_exists(vars(date, date_time)) %>%
    col_vals_regex(
      vars(b), "[0-9]-[a-z]{3}-[0-9]{3}",
      active = FALSE
    ) %>%
    rows_distinct() %>%
    interrogate()
  
  agent_3 <- 
    create_agent(
      tbl = ~ small_table,
      label = "An example.",
      actions = al
    ) %>%
    rows_distinct() %>%
    col_vals_gt(vars(d), 100) %>%
    col_vals_lte(vars(c), 5) %>%
    col_vals_equal(
      vars(d), vars(d),
      na_pass = TRUE
    ) %>%
    col_vals_in_set(
      vars(f),
      set = c("low", "mid", "high")
    ) %>%
    col_vals_between(
      vars(c),
      left = vars(a), right = vars(d),
      na_pass = TRUE
    ) %>%
    interrogate()
  
  agent_4 <-
    agent_3 %>%
    remove_steps(i = 1) %>%
    deactivate_steps(i = 1) %>%
    interrogate()
  
  agent_5 <-
    create_agent(
      tbl = ~ small_table,
      label = "An example.",
      actions = al
    ) %>%
    col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
    interrogate()
  
  # Create a multiagent object
  multiagent <- create_multiagent(agent_1, agent_2, agent_3, agent_5)
  
  # Expect a multiagent object of class `ptblank_multiagent`
  expect_is(multiagent, "ptblank_multiagent")
  
  # Expect that names in a multiagent object match a
  # prescribed set of names
  expect_true(all(names(multiagent) == c("overview_tbl", "agents")))
  
  # Expect that `multiagent$agents` has a length of 4 (one for each agent)
  expect_equal(length(multiagent$agents), 4)
  
  for (i in seq_len(length(multiagent$agents))) {
    
    agent_i <- multiagent$agents[[i]]
    
    # Expect that the `validation_set` component is a `tbl_df`
    expect_is(agent_i$validation_set, "tbl_df")
    
    # Expect certain classes for the different `ptblank_agent` components
    expect_is(agent_i$tbl, class(small_table))
    expect_is(agent_i$read_fn, "formula")
    expect_is(agent_i$tbl_name, "character")
    expect_is(agent_i$label, "character")
    expect_is(agent_i$tbl_src, "character")
    expect_is(agent_i$tbl_src_details, "character")
    expect_is(agent_i$col_names, "character")
    expect_is(agent_i$col_types, "character")
    expect_is(agent_i$db_col_types, "character")
    expect_is(agent_i$actions, "action_levels")
    expect_is(agent_i$end_fns, "list")
    expect_is(agent_i$embed_report, "logical")
    expect_is(agent_i$lang, "character")
    expect_is(agent_i$time_start, "POSIXct")
    expect_is(agent_i$time_end, "POSIXct")
    expect_is(agent_i$validation_set$i, "integer")
    expect_is(agent_i$validation_set$assertion_type, "character")
    expect_is(agent_i$validation_set$column, "list")
    expect_is(agent_i$validation_set$values, "list")
    expect_is(agent_i$validation_set$na_pass, "logical")
    expect_is(agent_i$validation_set$preconditions, "list")
    expect_is(agent_i$validation_set$actions, "list")
    expect_is(agent_i$validation_set$brief, "character")
    expect_is(agent_i$validation_set$active, "list")
    expect_is(agent_i$validation_set$eval_active, "logical")
    expect_is(agent_i$validation_set$eval_error, "logical")
    expect_is(agent_i$validation_set$eval_warning, "logical")
    expect_is(agent_i$validation_set$capture_stack, "list")
    expect_is(agent_i$validation_set$all_passed, "logical")
    expect_is(agent_i$validation_set$n, "numeric")
    expect_is(agent_i$validation_set$n_passed, "numeric")
    expect_is(agent_i$validation_set$n_failed, "numeric")
    expect_is(agent_i$validation_set$f_passed, "numeric")
    expect_is(agent_i$validation_set$f_failed, "numeric")
    expect_is(agent_i$validation_set$warn, "logical")
    expect_is(agent_i$validation_set$notify, "logical")
    expect_is(agent_i$validation_set$stop, "logical")
    expect_is(agent_i$validation_set$row_sample, "numeric")
    expect_is(agent_i$validation_set$tbl_checked, "list")
    expect_is(agent_i$validation_set$time_processed, "POSIXct")
    expect_is(agent_i$validation_set$proc_duration_s, "numeric")
    expect_is(agent_i$extracts, "list")
  }
})

test_that("multiagent checks for agent input", {
  expect_error(create_multiagent(1, "a"), "must be an agent")
})
