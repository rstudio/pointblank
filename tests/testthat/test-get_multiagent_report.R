test_that("Getting a multiagent report is possible", {

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

  # Get a multiagent report
  report <- get_multiagent_report(multiagent, display_table = FALSE)

  # Expect that the report has specific column names
  expect_match(
    colnames(report),
    "00[1-4]_[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}"
  )

  # Expect 14 rows in this report
  expect_equal(nrow(report), 14)

  for (col_name in colnames(report)) {

    tbl_c <- report[[col_name]]

    # Expect certain column types in each packed tibble
    expect_type(tbl_c$i, "integer")
    expect_type(tbl_c$step_id, "character")
    expect_type(tbl_c$assertion_type, "character")
    expect_type(tbl_c$column, "list")
    expect_type(tbl_c$values, "list")
    expect_type(tbl_c$na_pass, "logical")
    expect_type(tbl_c$preconditions, "list")
    expect_type(tbl_c$actions, "list")
    expect_type(tbl_c$label, "character")
    expect_type(tbl_c$brief, "character")
    expect_type(tbl_c$eval_active, "logical")
    expect_type(tbl_c$eval_error, "logical")
    expect_type(tbl_c$eval_warning, "logical")
    expect_type(tbl_c$eval_active, "logical")
    expect_type(tbl_c$all_passed, "logical")
    expect_type(tbl_c$n, "double")
    expect_type(tbl_c$f_passed, "double")
    expect_type(tbl_c$f_failed, "double")
    expect_type(tbl_c$warn, "logical")
    expect_type(tbl_c$notify, "logical")
    expect_type(tbl_c$stop, "logical")
  }
})
