context("Tests of functions dealing with YAML")

temp_dir <- tempdir()

test_that("YAML writing and reading works as expected", {
  
  al <- action_levels(warn_at = 0.1, stop_at = 0.2)

  agent <-
    create_agent(read_fn = ~ small_table, actions = al) %>%
    col_vals_in_set(vars(f), set = c("low", "mid", "high")) %>%
    col_vals_between(vars(a), 2, 8) %>%
    col_vals_lt(vars(a), vars(d), na_pass = TRUE, preconditions = ~. %>% dplyr::filter(a > 3)) %>%
    col_vals_gt(vars(d), 100) %>%
    col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
    col_vals_null(vars(c)) %>%
    col_vals_regex(vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}") %>%
    col_is_character(vars(b)) %>%
    col_exists(vars(a, b)) %>%
    col_vals_expr(expr(a %% 1 == 0)) %>%
    rows_distinct(vars(a, b, c)) %>%
    rows_distinct() %>%
    col_schema_match(
      schema = col_schema(
        date_time = c("POSIXct", "POSIXt"),
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character"
      )
    ) %>%
    conjointly(
      ~ col_vals_lt(., vars(a), 8),
      ~ col_vals_gt(., vars(c), vars(a)),
      ~ col_vals_not_null(., vars(b))
    ) %>%
    col_vals_between(vars(c), left = 2.03, right = vars(d), na_pass = TRUE)
  
  # Expect a `ptblank_agent` object but don't expect it to
  # have any intel (the `has_intel` class)
  expect_s3_class(agent, "ptblank_agent")
  expect_false(inherits(agent, "has_intel"))
  
  # Expect that `agent_yaml_string` shows a YAML string in the
  # console and returns nothing (when reading from an agent)
  expect_null(suppressMessages(agent_yaml_string(agent = agent)))
  expect_match(
    as.character(testthat::capture_message(agent_yaml_string(agent = agent))),
    "name: .*?read_fn: .*?action_levels:.*?warn_fraction: 0.1.*?stop_fraction: 0.2.*?embed_report: false.*?reporting_lang: en.*?steps:.*"
  )

  # Write the agent to a pointblank YAML file in the temp directory
  agent_yaml_write(agent, filename = "test.yaml", path = temp_dir)

  # Expect that the file was written to the temp directory
  expect_true("test.yaml" %in% list.files(path = temp_dir))

  # Expect that `agent_yaml_string()` shows a YAML string in the
  # console and returns nothing (when reading from a YAML file)
  expect_null(suppressMessages(agent_yaml_string(path = file.path(temp_dir, "test.yaml"))))
  expect_match(
    as.character(testthat::capture_message(agent_yaml_string(path = file.path(temp_dir, "test.yaml")))),
    "name: .*?read_fn: .*?action_levels:.*?warn_fraction: 0.1.*?stop_fraction: 0.2.*?embed_report: false.*?reporting_lang: en.*?steps:.*"
  )

  # Generate an agent with a plan defined by the YAML file
  agent_plan <- agent_yaml_read(file.path(temp_dir, "test.yaml"))

  # Expect a `ptblank_agent` object but don't expect it to
  # have any intel (the `has_intel` class)
  expect_s3_class(agent_plan, "ptblank_agent")
  expect_false(inherits(agent_plan, "has_intel"))

  # Expect the `tbl` data to be in the agent object (obtained
  # by way of the `read_fn`)
  expect_s3_class(agent_plan$tbl, "tbl_df")
  expect_true(inherits(agent_plan$read_fn, "formula"))

  # Expect there to be 16 validation steps available in the validation set
  expect_length(agent_plan$validation_set$i, n = 16)

  # Expect specific validation functions used as the bases for these
  # validation steps
  expect_equal(
    agent_plan$validation_set$assertion_type,
    c("col_vals_in_set", "col_vals_between", "col_vals_lt", "col_vals_gt",
      "col_vals_equal", "col_vals_null", "col_vals_regex", "col_is_character",
      "col_exists", "col_exists", "col_vals_expr", "rows_distinct",
      "rows_distinct", "col_schema_match", "conjointly", "col_vals_between"
    )
  )

  # Expect most columns in the validation set to be filled entirely
  # with NA values since no interrogation has yet been performed
  expect_true(all(is.na(agent_plan$validation_set$eval_error)))
  expect_true(all(is.na(agent_plan$validation_set$eval_warning)))
  expect_true(all(is.na(agent_plan$validation_set$all_passed)))
  expect_true(all(is.na(agent_plan$validation_set$n)))
  expect_true(all(is.na(agent_plan$validation_set$n_passed)))
  expect_true(all(is.na(agent_plan$validation_set$n_failed)))
  expect_true(all(is.na(agent_plan$validation_set$f_passed)))
  expect_true(all(is.na(agent_plan$validation_set$f_failed)))
  expect_true(all(is.na(agent_plan$validation_set$warn)))
  expect_true(all(is.na(agent_plan$validation_set$notify)))
  expect_true(all(is.na(agent_plan$validation_set$stop)))
  expect_true(all(is.na(agent_plan$validation_set$row_sample)))
  expect_true(all(is.na(agent_plan$validation_set$time_processed)))
  expect_true(all(is.na(agent_plan$validation_set$proc_duration_s)))

  # Expect that `agent_yaml_show_exprs()` shows string with pointblank
  # expression in the console and returns nothing (when reading from an agent)
  expect_null(suppressMessages(agent_yaml_show_exprs(path = file.path(temp_dir, "test.yaml"))))
  expect_match(
    as.character(testthat::capture_message(agent_yaml_show_exprs(path = file.path(temp_dir, "test.yaml")))),
    paste(
      c(
        "create_agent", "col_vals_in_set", "col_vals_between",
        "col_vals_lt", "col_vals_gt", "col_vals_equal", "col_vals_null",
        "col_vals_regex", "col_is_character", "col_exists", "col_exists",
        "col_vals_expr", "rows_distinct", "rows_distinct", "col_schema_match",
        "conjointly", "col_vals_between"),
      collapse = ".*?"
    )
  )

  # Create the agent from YAML and interrogate immediately
  agent_intel <- agent_yaml_interrogate(path = file.path(temp_dir, "test.yaml"))

  # Expect a `ptblank_agent` object and also expect it to
  # have intel (the `has_intel` class)
  expect_s3_class(agent_intel, "ptblank_agent")
  expect_s3_class(agent_intel, "has_intel")

  # Expect the `tbl` data to be in the agent object (obtained
  # by way of the `read_fn`)
  expect_s3_class(agent_intel$tbl, "tbl_df")
  expect_true(inherits(agent_intel$read_fn, "formula"))

  # Expect there to be 16 validation steps available in the validation set
  expect_length(agent_intel$validation_set$i, n = 16)

  # Expect specific validation functions used as the bases for these
  # validation steps
  expect_equal(
    agent_intel$validation_set$assertion_type,
    c("col_vals_in_set", "col_vals_between", "col_vals_lt", "col_vals_gt",
      "col_vals_equal", "col_vals_null", "col_vals_regex", "col_is_character",
      "col_exists", "col_exists", "col_vals_expr", "rows_distinct",
      "rows_distinct", "col_schema_match", "conjointly", "col_vals_between"
    )
  )

  # Expect interrogation data to be present in the validation set
  expect_equal(
    agent_intel$validation_set$n,
    c(13, 13, 6, 13, 13, 13, 13, 1, 1, 1, 13, 13, 13, 1, 13, 13)
  )
  expect_equal(
    agent_intel$validation_set$n_passed,
    c(13, 12, 6, 13, 13, 2, 13, 1, 1, 1, 13, 11, 11, 1, 6, 12)
  )
  expect_equal(
    agent_intel$validation_set$n_failed,
    c(0, 1, 0, 0, 0, 11, 0, 0, 0, 0, 0, 2, 2, 0, 7, 1)
  )
  
  # Expect an error if using `agent_yaml_string()` with both an
  # agent and a YAML file
  expect_error(agent_yaml_string(agent = agent_intel, path = file.path(temp_dir, "test.yaml")))
  
  # Expect an error if using `agent_yaml_string()` with neither an
  # agent nor a YAML file specified
  expect_error(agent_yaml_string())
  
  # Expect an error if using `as_agent_yaml_list()` with an agent
  # that doesn't have a table-reading function specified
  expect_error(agent_plan %>% remove_read_fn() %>% as_agent_yaml_list())
})
