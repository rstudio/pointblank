work_path <- "./generated_testthat_files"

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

fs::dir_create(path = work_path)

get_oneline_expr_str <- function(agent, expanded = FALSE) {

  agent %>%
    agent_get_exprs(expanded = expanded) %>%
    gsub("create_agent\\(.*?%>%\n", "", .) %>%
    gsub("\\s+$", "", .) %>%
    gsub("\n\\s*", "", .)
}

test_that("YAML writing and reading works as expected", {

  al <- action_levels(warn_at = 0.1, stop_at = 0.2)

  agent <-
    create_agent(tbl = ~ small_table, actions = al) %>%
    col_vals_in_set(vars(f), set = c("low", "mid", "high")) %>%
    col_vals_between("a", 2, 8) %>%
    col_vals_lt(vars(a), vars(d), na_pass = TRUE, preconditions = ~. %>% dplyr::filter(a > 3)) %>%
    col_vals_gt(vars(d), 100) %>%
    col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
    col_vals_null(vars(c)) %>%
    col_vals_not_null(matches("^.$")) %>%
    col_vals_regex("b", regex = "[0-9]-[a-z]{3}-[0-9]{3}") %>%
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

  # Expect that `yaml_agent_string` shows a YAML string in the
  # console and returns the string itself
  expect_type(
    suppressMessages(yaml_agent_string(agent = agent)),
    "character"
  )
  expect_match(
    as.character(testthat::capture_messages(yaml_agent_string(agent = agent))),
    "tbl: ~small_table.*?tbl_name: .*?label: .*?actions:.*?warn_fraction: 0.1.*?stop_fraction: 0.2.*?steps:.*"
  )

  # Write the agent to a pointblank YAML file in the temp directory
  yaml_write(agent, filename = "test.yaml", path = work_path)

  # Expect that the file was written to the temp directory
  expect_true("test.yaml" %in% list.files(path = work_path))

  # Expect that `yaml_agent_string` shows a YAML string in the
  # console and returns the string itself
  expect_type(
    suppressMessages(yaml_agent_string(filename = file.path(work_path, "test.yaml"))),
    "character"
  )
  expect_match(
    as.character(testthat::capture_messages(yaml_agent_string(filename = file.path(work_path, "test.yaml")))),
    "tbl: ~small_table.*?tbl_name: .*?label: .*?actions:.*?warn_fraction: 0.1.*?stop_fraction: 0.2.*?steps:.*"
  )

  # Generate an agent with a plan defined by the YAML file
  agent_plan <- yaml_read_agent(file.path(work_path, "test.yaml"))

  # Expect a `ptblank_agent` object but don't expect it to
  # have any intel (the `has_intel` class)
  expect_s3_class(agent_plan, "ptblank_agent")
  expect_false(inherits(agent_plan, "has_intel"))

  # Expect the `tbl` data to be NULL since the table only materializes
  # during an interrogation
  expect_null(agent_plan$tbl)
  expect_true(inherits(agent_plan$read_fn, "formula"))

  # Expect there to be 16 validation steps available in the validation set
  expect_length(agent_plan$validation_set$i, n = 22)

  # Expect specific validation functions used as the bases for these
  # validation steps
  expect_equal(
    agent_plan$validation_set$assertion_type,
    c("col_vals_in_set", "col_vals_between", "col_vals_lt", "col_vals_gt",
      "col_vals_equal", "col_vals_null", rep("col_vals_not_null", 6),
      "col_vals_regex", "col_is_character", "col_exists", "col_exists",
      "col_vals_expr", "rows_distinct", "rows_distinct", "col_schema_match",
      "conjointly", "col_vals_between"
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

  # Expect that `yaml_agent_show_exprs()` shows string with pointblank
  # expression in the console and returns nothing (when reading from an agent)
  expect_null(suppressMessages(yaml_agent_show_exprs(filename = file.path(work_path, "test.yaml"))))
  expect_match(
    as.character(testthat::capture_messages(yaml_agent_show_exprs(filename = file.path(work_path, "test.yaml")))),
    paste(
      c(
        "create_agent", "col_vals_in_set", "col_vals_between",
        "col_vals_lt", "col_vals_gt", "col_vals_equal", "col_vals_null",
        "col_vals_not_null", "col_vals_regex", "col_is_character", "col_exists",
        "col_vals_expr", "rows_distinct", "rows_distinct", "col_schema_match",
        "conjointly", "col_vals_between"),
      collapse = ".*?"
    )
  )

  # Create the agent from YAML and interrogate immediately
  agent_intel <- yaml_agent_interrogate(filename = file.path(work_path, "test.yaml"))

  # Expect a `ptblank_agent` object and also expect it to
  # have intel (the `has_intel` class)
  expect_s3_class(agent_intel, "ptblank_agent")
  expect_s3_class(agent_intel, "has_intel")

  # Expect the `tbl` data to be in the agent object (obtained
  # by way of the `read_fn`)
  expect_s3_class(agent_intel$tbl, "tbl_df")
  expect_true(inherits(agent_intel$read_fn, "formula"))

  # Expect there to be 16 validation steps available in the validation set
  expect_length(agent_intel$validation_set$i, n = 22)

  # Expect specific validation functions used as the bases for these
  # validation steps
  expect_equal(
    agent_intel$validation_set$assertion_type,
    c("col_vals_in_set", "col_vals_between", "col_vals_lt", "col_vals_gt",
      "col_vals_equal", "col_vals_null", rep("col_vals_not_null", 6),
      "col_vals_regex", "col_is_character", "col_exists", "col_exists",
      "col_vals_expr", "rows_distinct", "rows_distinct", "col_schema_match",
      "conjointly", "col_vals_between"
    )
  )

  # Expect interrogation data to be present in the validation set
  expect_equal(
    agent_intel$validation_set$n,
    c(13, 13, 6, rep(13, 10), 1, 1, 1, 13, 13, 13, 1, 13, 13)
  )
  expect_equal(
    agent_intel$validation_set$n_passed,
    c(
      13, 12, 6, 13, 13, 2, 13, 13, 11, 13, 13, 13, 13, 1, 1,
      1, 13, 11, 11, 1, 6, 12
    )
  )
  expect_equal(
    agent_intel$validation_set$n_failed,
    c(0, 1, 0, 0, 0, 11, 0, 0, 2, rep(0, 8), 2, 2, 0, 7, 1)
  )

  # Expect an error if using `yaml_agent_string()` with both an
  # agent and a YAML file
  expect_error(
    yaml_agent_string(
      agent = agent_intel,
      filename = file.path(temp_dir, "test.yaml")
    )
  )

  # Expect an error if using `yaml_agent_string()` with neither an
  # agent nor a YAML file specified
  expect_error(yaml_agent_string())

  # Expect an error if using `as_agent_yaml_list()` with an agent
  # that doesn't have a table-prep formula specified
  expect_error(
    agent_plan %>%
      remove_read_fn() %>%
      as_agent_yaml_list(expanded = FALSE)
  )
})

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

test_that("Individual validation steps make the YAML round-trip successfully", {

  agent <- create_agent(tbl = ~ small_table, label = "testthat")

  #
  # col_vals_lt()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a), 1)),
    "col_vals_lt(columns = vars(a),value = 1)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a), vars(c))),
    "col_vals_lt(columns = vars(a),value = vars(c))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1)),
    "col_vals_lt(columns = vars(a, c),value = 1)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, na_pass = TRUE)),
    "col_vals_lt(columns = vars(a, c),value = 1,na_pass = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_lt(
          vars(a), 1, na_pass = TRUE,
          preconditions = ~ . %>% dplyr::filter(a > 2)
          )
      ),
    "col_vals_lt(columns = vars(a),value = 1,na_pass = TRUE,preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_lt(
          vars(a), 1, na_pass = TRUE,
          preconditions = function(x) { x %>% dplyr::filter(a > 2) }
        )
    ),
    "col_vals_lt(columns = vars(a),value = 1,na_pass = TRUE,preconditions = function(x) {x %>% dplyr::filter(a > 2)})"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_lt(
          vars(a), 1, na_pass = TRUE,
          preconditions = function(x) {
            x %>% dplyr::filter(a > 2)
          }
        )
    ),
    "col_vals_lt(columns = vars(a),value = 1,na_pass = TRUE,preconditions = function(x) {x %>% dplyr::filter(a > 2)})"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_lt(
          vars(a, c), 1,
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_lt(columns = vars(a, c),value = 1,actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, label = "my_label")),
    "col_vals_lt(columns = vars(a, c),value = 1,label = \"my_label\")"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, active = FALSE)),
    "col_vals_lt(columns = vars(a, c),value = 1,active = FALSE)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, active = TRUE)),
    "col_vals_lt(columns = vars(a, c),value = 1)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, active = ~ . %>% has_columns(vars(a, c)))),
    "col_vals_lt(columns = vars(a, c),value = 1,active = ~. %>% has_columns(vars(a, c)))"
  )
  # expect_equal(
  #   get_oneline_expr_str(agent %>% col_vals_lt(vars(a), 1, brief = "Expect `a` < `1`.")),
  #   "col_vals_lt(columns = vars(a, c),value = 1,brief = \"Expect `a` < `1`.\")"
  # )
  # expect_equal(
  #   get_oneline_expr_str(agent %>% col_vals_lt(vars(a, c), 1, step_id = "my_id")),
  #   "col_vals_lt(columns = vars(a, c),value = 1,step_id = \"my_id\")"
  # )

  #
  # col_vals_between()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(a), left = -5, right = 15)),
    "col_vals_between(columns = vars(a),left = -5,right = 15)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = 15)),
    "col_vals_between(columns = vars(c),left = vars(a),right = 15)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = -5.2342, right = vars(d))),
    "col_vals_between(columns = vars(c),left = -5.2342,right = vars(d))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = vars(d))),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE)),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),na_pass = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_between(
          vars(c), left = vars(a), right = vars(d), na_pass = TRUE,
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),na_pass = TRUE,preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_between(
          vars(c), left = vars(a), right = vars(d),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = -5, right = 15, label = "my_label")),
    "col_vals_between(columns = vars(c),left = -5,right = 15,label = \"my_label\")"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = vars(d), inclusive = c(TRUE, FALSE))),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),inclusive = c(TRUE, FALSE))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = vars(d), inclusive = c(FALSE, TRUE))),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),inclusive = c(FALSE, TRUE))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_between(vars(c), left = vars(a), right = vars(d), inclusive = c(FALSE, FALSE))),
    "col_vals_between(columns = vars(c),left = vars(a),right = vars(d),inclusive = c(FALSE, FALSE))"
  )

  #
  # col_vals_not_between()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(a), left = -5, right = 15)),
    "col_vals_not_between(columns = vars(a),left = -5,right = 15)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(c), left = vars(a), right = 15)),
    "col_vals_not_between(columns = vars(c),left = vars(a),right = 15)"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(c), left = -5.2342, right = vars(d))),
    "col_vals_not_between(columns = vars(c),left = -5.2342,right = vars(d))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(c), left = vars(a), right = vars(d))),
    "col_vals_not_between(columns = vars(c),left = vars(a),right = vars(d))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE)),
    "col_vals_not_between(columns = vars(c),left = vars(a),right = vars(d),na_pass = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_not_between(
          vars(c), left = vars(a), right = vars(d), na_pass = TRUE,
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_not_between(columns = vars(c),left = vars(a),right = vars(d),na_pass = TRUE,preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_not_between(
          vars(c), left = vars(a), right = vars(d),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_not_between(columns = vars(c),left = vars(a),right = vars(d),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_between(vars(a), left = -5, right = 15, label = "my_label")),
    "col_vals_not_between(columns = vars(a),left = -5,right = 15,label = \"my_label\")"
  )

  #
  # col_vals_in_set()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_in_set(vars(f), set = c("low", "high"))),
    "col_vals_in_set(columns = vars(f),set = c(\"low\", \"high\"))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_in_set(vars(f), set = c("low", "high", NA))),
    "col_vals_in_set(columns = vars(f),set = c(\"low\", \"high\", NA))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_in_set(vars(c), set = c(1:10))),
    "col_vals_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_in_set(vars(c), set = c(1:10, NA))),
    "col_vals_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_in_set(
          vars(c), set = c(1:10),
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_in_set(
          vars(c), set = c(1:10),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_in_set(vars(f), set = c("low", "high"), label = "my_label")),
    "col_vals_in_set(columns = vars(f),set = c(\"low\", \"high\"),label = \"my_label\")"
  )

  #
  # col_vals_not_in_set()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_in_set(vars(f), set = c("low", "high"))),
    "col_vals_not_in_set(columns = vars(f),set = c(\"low\", \"high\"))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_in_set(vars(f), set = c("low", "high", NA))),
    "col_vals_not_in_set(columns = vars(f),set = c(\"low\", \"high\", NA))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_in_set(vars(c), set = c(1:10))),
    "col_vals_not_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_in_set(vars(c), set = c(1:10, NA))),
    "col_vals_not_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_not_in_set(
          vars(c), set = c(1:10),
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_not_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_not_in_set(
          vars(c), set = c(1:10),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_not_in_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_in_set(vars(f), set = c("low", "high"), label = "my_label")),
    "col_vals_not_in_set(columns = vars(f),set = c(\"low\", \"high\"),label = \"my_label\")"
  )

  #
  # col_vals_make_set()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_set(vars(f), set = c("low", "high"))),
    "col_vals_make_set(columns = vars(f),set = c(\"low\", \"high\"))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_set(vars(f), set = c("low", "high", NA))),
    "col_vals_make_set(columns = vars(f),set = c(\"low\", \"high\", NA))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_set(vars(c), set = c(1:10))),
    "col_vals_make_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_set(vars(c), set = c(1:10, NA))),
    "col_vals_make_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_make_set(
          vars(c), set = c(1:10),
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_make_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_make_set(
          vars(c), set = c(1:10),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_make_set(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_set(vars(f), set = c("low", "high"), label = "my_label")),
    "col_vals_make_set(columns = vars(f),set = c(\"low\", \"high\"),label = \"my_label\")"
  )

  #
  # col_vals_make_subset()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_subset(vars(f), set = c("low", "high"))),
    "col_vals_make_subset(columns = vars(f),set = c(\"low\", \"high\"))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_subset(vars(f), set = c("low", "high", NA))),
    "col_vals_make_subset(columns = vars(f),set = c(\"low\", \"high\", NA))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_subset(vars(c), set = c(1:10))),
    "col_vals_make_subset(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_subset(vars(c), set = c(1:10, NA))),
    "col_vals_make_subset(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_make_subset(
          vars(c), set = c(1:10),
          preconditions = ~ . %>% dplyr::filter(a > 2)
        )
    ),
    "col_vals_make_subset(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),preconditions = ~. %>% dplyr::filter(a > 2))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_make_subset(
          vars(c), set = c(1:10),
          actions = action_levels(warn_at = 0.1, stop_at = 0.2)
        )
    ),
    "col_vals_make_subset(columns = vars(c),set = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_make_subset(vars(f), set = c("low", "high"), label = "my_label")),
    "col_vals_make_subset(columns = vars(f),set = c(\"low\", \"high\"),label = \"my_label\")"
  )

  #
  # col_vals_increasing()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_increasing(vars(a))),
    "col_vals_increasing(columns = vars(a))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_increasing(vars(a), na_pass = TRUE)),
    "col_vals_increasing(columns = vars(a),na_pass = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_increasing(vars(a), allow_stationary = TRUE)
    ),
    "col_vals_increasing(columns = vars(a),allow_stationary = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_increasing(vars(a), decreasing_tol = 0.52)
    ),
    "col_vals_increasing(columns = vars(a),decreasing_tol = 0.52)"
  )

  #
  # col_vals_decreasing()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_decreasing(vars(a))),
    "col_vals_decreasing(columns = vars(a))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_decreasing(vars(a), na_pass = TRUE)),
    "col_vals_decreasing(columns = vars(a),na_pass = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_decreasing(vars(a), allow_stationary = TRUE)
    ),
    "col_vals_decreasing(columns = vars(a),allow_stationary = TRUE)"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>%
        col_vals_decreasing(vars(a), increasing_tol = 0.52)
    ),
    "col_vals_decreasing(columns = vars(a),increasing_tol = 0.52)"
  )

  #
  # col_vals_null()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_null(vars(c))),
    "col_vals_null(columns = vars(c))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_null(vars(a, c))),
    "col_vals_null(columns = vars(a, c))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_null(vars(c), label = "my_label")),
    "col_vals_null(columns = vars(c),label = \"my_label\")"
  )

  #
  # col_vals_not_null()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_null(vars(c))),
    "col_vals_not_null(columns = vars(c))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_null(vars(a, c))),
    "col_vals_not_null(columns = vars(a, c))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_not_null(vars(c), label = "my_label")),
    "col_vals_not_null(columns = vars(c),label = \"my_label\")"
  )

  #
  # col_vals_regex()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_regex(vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}")),
    "col_vals_regex(columns = vars(b),regex = \"[0-9]-[a-z]{3}-[0-9]{3}\")"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_vals_regex(vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}", label = "my_label")),
    "col_vals_regex(columns = vars(b),regex = \"[0-9]-[a-z]{3}-[0-9]{3}\",label = \"my_label\")"
  )

  #
  # col_vals_within_spec()
  #

  agent_spec <- create_agent(tbl = ~ specifications, label = "testthat")

  expect_equal(
    get_oneline_expr_str(agent_spec %>% col_vals_within_spec(columns = vars(isbn_numbers), spec = "isbn13")),
    "col_vals_within_spec(columns = vars(isbn_numbers),spec = \"isbn\")"
  )
  expect_equal(
    get_oneline_expr_str(agent_spec %>% col_vals_within_spec(columns = vars(vin_numbers), spec = "VIN", label = "my_label")),
    "col_vals_within_spec(columns = vars(vin_numbers),spec = \"vin\",label = \"my_label\")"
  )

  #
  # col_vals_expr()
  #

  tbl_1 <<-
    dplyr::tibble(
      a = c(1, 2, 1, 7, 8, 6),
      b = c(0, 0, 0, 1, 1, 1),
      c = c(0.5, 0.3, 0.8, 1.4, 1.9, 1.2),
    )

  agent_expr <- create_agent(tbl = ~ tbl_1, label = "testthat")

  expect_equal(
    get_oneline_expr_str(agent_expr %>% col_vals_expr(expr(a %% 1 == 0))),
    "col_vals_expr(expr = ~a%%1 == 0)"
  )
  # expect_equal(
  #   get_oneline_expr_str(
  #     agent_expr %>%
  #       col_vals_expr(~ case_when(
  #         b == 0 ~ a %>% between(0, 5) & c < 1,
  #         b == 1 ~ a > 5 & c >= 1
  #       ))
  #   ),
  #   "col_vals_expr(~ case_when(b == 0 ~ a %>% between(0, 5) & c < 1, b == 1 ~ a > 5 & c >= 1))"
  # )
  expect_equal(
    get_oneline_expr_str(agent_expr %>% col_vals_expr(expr = ~ dplyr::between(a, 1, 10))),
    "col_vals_expr(expr = ~dplyr::between(a, 1, 10))"
  )
  expect_equal(
    get_oneline_expr_str(agent_expr %>% col_vals_expr(expr = ~ dplyr::between(a, 1, 10), label = "my_label")),
    "col_vals_expr(expr = ~dplyr::between(a, 1, 10),label = \"my_label\")"
  )

  #
  # conjointly()
  #

  tbl_2 <<-
    dplyr::tibble(
      a = c(5, 2, 6),
      b = c(3, 4, 6),
      c = c(9, 8, 7)
    )

  agent_conjointly <- create_agent(tbl = ~ tbl_2, label = "testthat")

  expect_equal(
    get_oneline_expr_str(
      agent_conjointly %>%
        conjointly(
          ~ col_vals_lt(., vars(a), 8),
          ~ col_vals_gt(., vars(c), vars(a)),
          ~ col_vals_not_null(., vars(b))
        )
    ),
    "conjointly(~col_vals_lt(., vars(a), 8),~col_vals_gt(., vars(c), vars(a)),~col_vals_not_null(., vars(b)))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent_conjointly %>%
        conjointly(
          ~ col_vals_lt(., vars(a), 8),
          ~ col_vals_gt(., vars(c), vars(a)),
          ~ col_vals_not_null(., vars(b)),
          preconditions = ~ . %>% dplyr::filter(a > 0)
        )
    ),
    "conjointly(~col_vals_lt(., vars(a), 8),~col_vals_gt(., vars(c), vars(a)),~col_vals_not_null(., vars(b)),preconditions = ~. %>% dplyr::filter(a > 0))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent_conjointly %>%
        conjointly(
          ~ col_vals_lt(., vars(a), 8),
          ~ col_vals_gt(., vars(c), vars(a)),
          ~ col_vals_not_null(., vars(b)),
          label = "my_label"
        )
    ),
    "conjointly(~col_vals_lt(., vars(a), 8),~col_vals_gt(., vars(c), vars(a)),~col_vals_not_null(., vars(b)),label = \"my_label\")"
  )

  #
  # rows_distinct()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% rows_distinct()),
    "rows_distinct(columns = everything())"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% rows_distinct(columns = vars(a, b))),
    "rows_distinct(columns = vars(a, b))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% rows_distinct(columns = vars(a, b), preconditions = ~ . %>% dplyr::filter(a > 0))),
    "rows_distinct(columns = vars(a, b),preconditions = ~. %>% dplyr::filter(a > 0))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% rows_distinct(columns = vars(a, b), label = "my_label")),
    "rows_distinct(columns = vars(a, b),label = \"my_label\")"
  )

  #
  # col_is_character()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_is_character(vars(b))),
    "col_is_character(columns = vars(b))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_is_character(vars(b, f))),
    "col_is_character(columns = vars(b, f))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>% col_is_character(vars(b, f), actions = action_levels(warn_at = 0.1, stop_at = 0.2))
    ),
    "col_is_character(columns = vars(b, f),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_is_character(vars(b), label = "my_label")),
    "col_is_character(columns = vars(b),label = \"my_label\")"
  )

  #
  # col_exists()
  #

  expect_equal(
    get_oneline_expr_str(agent %>% col_exists(vars(b))),
    "col_exists(columns = vars(b))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_exists(vars(b, f))),
    "col_exists(columns = vars(b, f))"
  )
  expect_equal(
    get_oneline_expr_str(
      agent %>% col_exists(vars(b, f), actions = action_levels(warn_at = 0.1, stop_at = 0.2))
    ),
    "col_exists(columns = vars(b, f),actions = action_levels(warn_at = 0.1,stop_at = 0.2))"
  )
  expect_equal(
    get_oneline_expr_str(agent %>% col_exists(vars(b), label = "my_label")),
    "col_exists(columns = vars(b),label = \"my_label\")"
  )

  #
  # col_schema()
  #

  tbl_3 <<-
    dplyr::tibble(
      a = 1:5,
      b = letters[1:5]
    )

  schema_obj <-
    col_schema(
      a = "integer",
      b = "character"
    )

  agent_col_schema <- create_agent(tbl = ~ tbl_3, label = "testthat")

  expect_equal(
    get_oneline_expr_str(agent_col_schema %>% col_schema_match(schema_obj)),
    "col_schema_match(schema = col_schema(a = \"integer\",b = \"character\"))"
  )
  expect_equal(
    get_oneline_expr_str(agent_col_schema %>% col_schema_match(schema_obj, label = "my_label")),
    "col_schema_match(schema = col_schema(a = \"integer\",b = \"character\"),label = \"my_label\")"
  )
})

