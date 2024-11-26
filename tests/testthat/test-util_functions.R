skip_on_os(os = "windows")

test_that("Utility functions won't fail us", {
  
  # Use two validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_gt(columns = vars(c), value = 5) %>%
    col_vals_lt(columns = vars(d), value = 1000)
  
  is_ptblank_agent(x = agent) %>% expect_true()
  is_ptblank_agent(x = small_table) %>% expect_false()
  
  agent %>% get_tbl_object() %>% expect_s3_class("tbl_df")
  agent %>% get_tbl_object() %>% expect_equal(small_table)
  
  agent %>% has_agent_intel() %>% expect_false()
  agent %>% interrogate() %>% has_agent_intel() %>% expect_true()
  
  agent %>% is_agent_empty() %>% expect_false()
  small_table %>% is_agent_empty() %>% expect_false()
  create_agent(tbl = small_table) %>% is_agent_empty() %>% expect_true()
  
  agent %>% interrogate() %>% interrogation_time() %>% expect_s3_class("POSIXct")
  agent %>% interrogate() %>% interrogation_time() %>% expect_length(1)
  agent %>% interrogation_time() %>% expect_equal(NA)
  
  agent %>% number_of_validation_steps() %>% expect_equal(2)
  small_table %>% number_of_validation_steps() %>% expect_equal(NA)

  agent %>% get_assertion_type_at_idx(idx = 1) %>% expect_equal("col_vals_gt")
  agent %>% get_assertion_type_at_idx(idx = 2) %>% expect_equal("col_vals_lt")

  agent %>% get_column_as_sym_at_idx(idx = 1) %>% expect_s3_class("name")
  agent %>% get_column_as_sym_at_idx(idx = 1) %>% as.character() %>% expect_equal("c")
  agent %>% get_column_as_sym_at_idx(idx = 2) %>% expect_s3_class("name")
  agent %>% get_column_as_sym_at_idx(idx = 2) %>% as.character() %>% expect_equal("d")
  
  agent %>% get_values_at_idx(idx = 1) %>% expect_type("double")
  agent %>% get_values_at_idx(idx = 1) %>% expect_equal(5)
  agent %>% get_values_at_idx(idx = 2) %>% expect_type("double")
  agent %>% get_values_at_idx(idx = 2) %>% expect_equal(1000)
  
  agent %>% get_all_cols() %>% expect_type("character")
  agent %>% get_all_cols() %>%
    expect_equal(c("date_time", "date", "a", "b", "c", "d", "e", "f"))
  
  # Use set-based validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_in_set(columns = vars(e), set = c(TRUE, FALSE)) %>%
    col_vals_not_in_set(columns = vars(f), set = c("medium", "floor"))
  
  agent %>% get_values_at_idx(idx = 1) %>% expect_type("logical")
  agent %>% get_values_at_idx(idx = 1) %>% expect_equal(c(TRUE, FALSE))
  agent %>% get_values_at_idx(idx = 2) %>% expect_type("character")
  agent %>% get_values_at_idx(idx = 2) %>% expect_equal(c("medium", "floor"))
  
  # Use the `col_vals_regex()` validation step
  # function to create an agent with one validation step 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_regex(columns = vars(b), regex = "[0-9]-[a-z]*?-[0-9]*?")
  
  agent %>% get_values_at_idx(idx = 1) %>% expect_type("character")
  agent %>% get_values_at_idx(idx = 1) %>% expect_equal("[0-9]-[a-z]*?-[0-9]*?")
  
  #
  # get_column_na_pass_at_idx
  #
  
  # Use range-based validation step functions to create
  # an agent with two validation steps 
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_between(columns = vars(c), left = 2, right = 5, na_pass = TRUE) %>%
    col_vals_not_between(columns = vars(c), left = 2, right = 5, na_pass = FALSE)
  
  agent %>% get_column_na_pass_at_idx(idx = 1) %>% expect_type("logical")
  agent %>% get_column_na_pass_at_idx(idx = 1) %>% expect_equal(TRUE)
  agent %>% get_column_na_pass_at_idx(idx = 2) %>% expect_type("logical")
  agent %>% get_column_na_pass_at_idx(idx = 2) %>% expect_equal(FALSE)
  
  #
  # col_schema_from_names_types
  #
  
  # Expect that the `col_schema_from_names_types()` function generates
  # named list object from a character vector and an unnamed list
  col_names <- names(small_table)
  col_types <- unname(lapply(small_table, class))
  
  cs <- 
    col_schema_from_names_types(
      names = col_names,
      types = col_types
    )
  
  expect_type(cs, "list")
  expect_equal(names(cs), col_names)
  expect_equal(unname(cs), col_types)
  expect_equal(length(cs), length(col_names), length(col_types))
  
  #
  # normalize_step_id
  #
  
  # Expect that the `normalize_step_id()` function will make suitable
  # transformations of `step_id` based on the number of columns
  agent_0 <- create_agent(tbl = small_table)
  agent_1 <- create_agent(tbl = small_table) %>% col_exists(vars(a))
  agent_3 <- create_agent(tbl = small_table) %>% col_exists(vars(a, b, c))
  columns <- c("a", "b", "c")

  # Expect generated indices (with leading zeros) to be returned if `step_id`
  # is NULL (the number of  `columns` determines with number of steps, and
  # we're starting with an agent that has no validation steps)
  normalize_step_id(step_id = NULL, columns = columns[1], agent_0) %>% 
    expect_equal("0001")
  normalize_step_id(step_id = NULL, columns = columns[1:2], agent_0) %>% 
    expect_equal(c("0001", "0002"))
  normalize_step_id(step_id = NULL, columns = columns, agent_0) %>% 
    expect_equal(c("0001", "0002", "0003"))
  
  # Make similar expectations but use an agent that has a single validation
  # step as a starting point
  normalize_step_id(step_id = NULL, columns = columns[1], agent_1) %>% 
    expect_equal("0002")
  normalize_step_id(step_id = NULL, columns = columns[1:2], agent_1) %>% 
    expect_equal(c("0002", "0003"))
  normalize_step_id(step_id = NULL, columns = columns, agent_1) %>% 
    expect_equal(c("0002", "0003", "0004"))
  
  # Make similar expectations but use an agent that has three validation
  # step as a starting point
  normalize_step_id(step_id = NULL, columns = columns[1], agent_3) %>% 
    expect_equal("0004")
  normalize_step_id(step_id = NULL, columns = columns[1:2], agent_3) %>% 
    expect_equal(c("0004", "0005"))
  normalize_step_id(step_id = NULL, columns = columns, agent_3) %>% 
    expect_equal(c("0004", "0005", "0006"))
  
  # Expect a one-element `step_id` to be returned as-is for
  # the case of a single column
  normalize_step_id(step_id = "single", columns = columns[1], agent_0) %>%
    expect_equal("single")
  
  # Don't expect a warning for the above
  expect_no_warning(
    normalize_step_id(step_id = "single", columns = columns[1], agent_0)
  )
  
  # Expect a one-element `step_id` to be returned as a one-
  # element vector (first base name) in the case of a single column
  suppressWarnings(
    normalize_step_id(step_id = c("one", "two"), columns = columns[1], agent_0) %>%
      expect_equal("one")
  )
  
  # Expect a warning to be emitted for the previous statement
  expect_warning(normalize_step_id(step_id = c("one", "two"), columns = columns[1], agent_0))
  
  # Expect a two-element `step_id` to be returned as a three-
  # element vector (first base name) in the case of three columns
  # (`step_id` input not `1` or length of `columns`)
  suppressWarnings(
    normalize_step_id(step_id = c("one", "two"), columns = columns, agent_0) %>%
      expect_equal(c("one.0001", "one.0002", "one.0003"))
  )
  
  # Expect a warning to be emitted for the previous statement
  expect_warning(normalize_step_id(step_id = c("one", "two"), columns = columns, agent_0))
  
  # Expect a one-element `step_id` to be returned as a three-
  # element vector (base name + indices) in the case of three columns
  normalize_step_id(step_id = "single", columns = columns, agent_0) %>%
    expect_equal(c("single.0001", "single.0002", "single.0003"))
  
  # Don't expect a warning for the above
  expect_no_warning(
    normalize_step_id(step_id = "single", columns = columns, agent_0)
  )
  
  # Expect a two-element `step_id` to be returned as a three-
  # element vector (first base name + indices) in the case of three columns
  suppressWarnings(
    normalize_step_id(step_id = c("one", "two"), columns = columns, agent_0) %>%
      expect_equal(c("one.0001", "one.0002", "one.0003"))
  )
  
  # Expect a warning to be emitted for the previous statement
  expect_warning(normalize_step_id(step_id = c("one", "two"), columns = columns, agent_0))
  
  # Expect a three-element `step_id` to be returned as the same three-
  # element vector in the case of three columns
  normalize_step_id(step_id = c("one", "two", "three"), columns = columns, agent_0) %>%
    expect_equal(c("one", "two", "three"))
  
  # Don't expect a warning for the above
  expect_no_warning(
    normalize_step_id(step_id = c("one", "two", "three"), columns = columns, agent_0)
  )
  
  # Expect a three-element `step_id` as numeric values to be returned as
  # a character based, three-element vector in the case of three columns
  normalize_step_id(step_id = c(1.0, 2.0, 3.0), columns = columns, agent_0) %>%
    expect_equal(c("1", "2", "3"))
  
  # Expect a three-element `step_id` to be returned as a corrected three-
  # element vector (first base name + indices) in the case of some duplicated
  # `step_id` elements (for three columns)
  suppressWarnings(
    normalize_step_id(step_id = c("one", "two", "one"), columns = columns, agent_0) %>%
      expect_equal(c("one.0001", "one.0002", "one.0003"))
  )
  
  # Expect a warning to be emitted for the previous statement
  expect_warning(normalize_step_id(step_id = c("one", "two", "one"), columns = columns, agent_0))
  
  #
  # check_step_id_duplicates
  #
  
  # Expect that the `check_step_id_duplicates()` function will generate
  # an error if a `step_id` has been recorded in previous validation steps
  agent_0 <- create_agent(tbl = small_table)
  agent_1 <- create_agent(tbl = small_table) %>% col_exists(vars(a))
  agent_3 <- create_agent(tbl = small_table) %>% col_exists(vars(a, b, c))
  
  # There should be no duplicates (and no errors) when starting with
  # an agent that has no validation steps
  expect_no_error(
    check_step_id_duplicates(step_id = "1", agent_0)
  )
  expect_no_error(
    check_step_id_duplicates(step_id = c("one", "two", "three"), agent_0)
  )
  
  # Expect an error if re-using a past `step_id`
  expect_error(
    check_step_id_duplicates(step_id = "0001", agent_1)
  )
  expect_error(
    create_agent(tbl = small_table) %>%
      col_exists(vars(a), step_id = "one") %>%
      col_exists(vars(b), step_id = "one")
  )
  
  # Expect no errors if not providing any `step_id` values
  expect_no_error(
    create_agent(tbl = small_table) %>%
      col_exists(vars(a)) %>%
      col_exists(vars(b)) %>%
      col_is_character(vars(b)) %>%
      col_is_date(vars(date)) %>%
      col_is_posix(vars(date_time)) %>%
      col_is_integer(vars(a)) %>%
      col_is_numeric(vars(d)) %>%
      col_is_logical(vars(e)) %>%
      col_vals_between(vars(c), left = vars(a), right = vars(d), na_pass = TRUE) %>%
      col_vals_equal(vars(d), vars(d), na_pass = TRUE) %>%
      col_vals_expr(expr(c %% 1 == 0)) %>%
      col_vals_gt(vars(date_time), vars(date), na_pass = TRUE) %>%
      col_vals_gt(vars(b), vars(g), na_pass = TRUE) %>%
      col_vals_gte(vars(a, b, d), 0, na_pass = TRUE) %>%
      col_vals_regex(vars(b), "[1-9]-[a-z]{3}-[0-9]{3}") %>%
      rows_distinct() %>%
      col_vals_gt(vars(d), 100) %>%
      col_vals_not_null(vars(date_time))
  )
  
  #
  # normalize_reporting_language
  #
  
  # Expect different forms of two-letter language codes to be
  # accepted and transformed into lowercase versions
  expect_equal(normalize_reporting_language(lang = NULL), "en")
  expect_equal(normalize_reporting_language(lang = "en"), "en")
  expect_equal(normalize_reporting_language(lang = "EN"), "en")
  expect_equal(normalize_reporting_language(lang = "fr"), "fr")
  expect_equal(normalize_reporting_language(lang = "FR"), "fr")
  expect_equal(normalize_reporting_language(lang = "de"), "de")
  expect_equal(normalize_reporting_language(lang = "DE"), "de")
  expect_equal(normalize_reporting_language(lang = "it"), "it")
  expect_equal(normalize_reporting_language(lang = "IT"), "it")
  expect_equal(normalize_reporting_language(lang = "es"), "es")
  expect_equal(normalize_reporting_language(lang = "ES"), "es")
  expect_equal(normalize_reporting_language(lang = "pt"), "pt")
  expect_equal(normalize_reporting_language(lang = "PT"), "pt")
  expect_equal(normalize_reporting_language(lang = "zh"), "zh")
  expect_equal(normalize_reporting_language(lang = "ZH"), "zh")
  expect_equal(normalize_reporting_language(lang = "ru"), "ru")
  expect_equal(normalize_reporting_language(lang = "RU"), "ru")

  # Expect an error if the input doesn't correspond to
  # a supported reporting language
  expect_error(normalize_reporting_language(lang = "za"))
  
  #
  # get_lsv
  #
  
  # Obtain all of the available string vector names
  x <- 
    readRDS(
      file = system.file("text", "translations_built", package = "pointblank")
    )
  
  autobriefs_names <- names(x$autobriefs)
  agent_report_names <- names(x$agent_report)
  informant_report_names <- names(x$informant_report)
  email_names <- names(x$email)
  
  expect_equal(
    autobriefs_names,
    c(
      "precondition_text", "column_computed_text", "values_text", 
      "compare_expectation_text", "compare_failure_text", 
      "in_set_expectation_text", "in_set_failure_text",
      "make_set_expectation_text", "make_set_failure_text",
      "make_subset_expectation_text", "make_subset_failure_text",
      "not_in_set_expectation_text", "not_in_set_failure_text", 
      "between_expectation_text", "between_failure_text",
      "not_between_expectation_text", "not_between_failure_text",
      "null_expectation_text", "null_failure_text", 
      "not_null_expectation_text", "not_null_failure_text", 
      "increasing_expectation_text", "increasing_failure_text",
      "decreasing_expectation_text", "decreasing_failure_text",
      "col_vals_expr_expectation_text", "col_vals_expr_failure_text",
      "regex_expectation_text", "regex_failure_text", 
      "within_spec_expectation_text", "within_spec_failure_text", 
      "conjointly_expectation_text", "conjointly_failure_text",
      "serially_expectation_test_text", "serially_expectation_tests_text",
      "specially_expectation_text", "specially_failure_text",
      "col_exists_expectation_text", "col_exists_failure_text",
      "col_is_expectation_text", "col_is_failure_text", 
      "all_row_distinct_expectation_text", "all_row_distinct_failure_text", 
      "across_row_distinct_expectation_text", "across_row_distinct_failure_text", 
      "all_row_complete_expectation_text", "all_row_complete_failure_text", 
      "across_row_complete_expectation_text", "across_row_complete_failure_text", 
      "col_schema_match_expectation_text", "col_schema_match_failure_text",
      "row_count_match_expectation_text", "row_count_match_failure_text",
      "row_count_match_n_expectation_text", "row_count_match_n_failure_text",
      "col_count_match_expectation_text", "col_count_match_failure_text",
      "col_count_match_n_expectation_text", "col_count_match_n_failure_text",
      "tbl_match_expectation_text", "tbl_match_failure_text"
    )
  )
  
  expect_equal(
    agent_report_names,
    c(
      "pointblank_validation_title_text", "pointblank_validation_plan_text", 
      "no_interrogation_performed_text", "report_fail_rows_available", 
      "report_no_table_preconditions", "report_some_table_preconditions", 
      "report_on_segmentation", "report_no_evaluation_issues",
      "report_col_step", "report_col_steps", "report_col_columns",
      "report_col_values", "report_column_schema", "report_r_col_types",
      "report_r_sql_types"
    )
  )
  
  expect_equal(
    informant_report_names,
    c(
      "pointblank_information_title_text", "pointblank_table_text",
      "snip_list_more", "snip_list_and", "snip_list_or"
    )
  )
  
  expect_equal(
    email_names,
    c("agent_body", "footer_1", "footer_2", "footer_i", "informant_body")
  )
  
  # Get a localized string vectors for all items in 'autobriefs' and
  # determine that the same number of components exists in each
  for (i in seq_along(autobriefs_names)) {
    expect_equal(
      get_lsv(text = c("autobriefs", autobriefs_names[i])) %>% length(),
      length(reporting_languages)
    )
  }
  
  # Get a localized string vectors for all items in 'agent_report' and
  # determine that the same number of components exists in each
  for (i in seq_along(agent_report_names)) {
    expect_equal(
      get_lsv(text = c("agent_report", agent_report_names[i])) %>% length(),
      length(reporting_languages)
    )
  }
  
  # Get a localized string vectors for all items in 'informant_report' and
  # determine that the same number of components exists in each
  for (i in seq_along(informant_report_names)) {
    expect_equal(
      get_lsv(text = c("informant_report", informant_report_names[i])) %>% length(),
      length(reporting_languages)
    )
  }
  
  # Perform the same tests for all items in 'email' but express
  # the text as a length 1 vector
  for (i in seq_along(email_names)) {
    expect_equal(
      get_lsv(text = c("email", email_names[i])) %>% length(),
      length(reporting_languages)
    )
  }
  
  # Perform the same tests for all items in 'autobriefs' but express
  # the text as a length 1 vector
  for (i in seq_along(autobriefs_names)) {
    expect_equal(
      get_lsv(text = paste0("autobriefs/", autobriefs_names[i])) %>% length(),
      length(reporting_languages)
    )
  }
  
  # Expect an error if providing `text` in a length 3 vector
  expect_error(get_lsv(text = c("autobriefs", "/", autobriefs_names[1])))
  
  #
  # pb_str_catalog
  #
  
  l_vector <- letters[1:6]
  
  expect_equal(
    pb_str_catalog(l_vector),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"` (+1 more)"
  )
  expect_equal(
    pb_str_catalog(l_vector, limit = 20),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"`, and `\"f\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector, limit = Inf),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"`, and `\"f\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector, limit = 7),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"`, and `\"f\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector, limit = 2),
    "`\"a\"`, `\"b\"` (+4 more)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], limit = 1),
    "`\"a\"` and `\"b\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector, limit = 2, lang = "fr"),
    "`\"a\"`, `\"b\"` (+4 de plus)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], oxford = FALSE),
    "`\"a\"`, `\"b\"` and `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], sep = " |", and_or = ""),
    "`\"a\"` | `\"b\"` |  `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = FALSE, quot_str = FALSE),
    "a and b"
  )
  expect_equal(
    pb_str_catalog(l_vector, as_code = FALSE, quot_str = FALSE),
    "a, b, c, d, e (+1 more)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1], as_code = FALSE, quot_str = FALSE),
    "a"
  )
  expect_equal(
    pb_str_catalog(l_vector, and_or = "", oxford = TRUE),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"` (+1 more)"
  )
  expect_equal(
    pb_str_catalog(l_vector, and_or = "", oxford = FALSE),
    "`\"a\"`, `\"b\"`, `\"c\"`, `\"d\"`, `\"e\"` (+1 more)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], oxford = TRUE, lang = "en"),
    "`\"a\"`, `\"b\"`, and `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], oxford = FALSE, lang = "en"),
    "`\"a\"`, `\"b\"` and `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], oxford = FALSE, lang = "de"),
    "`\"a\"`, `\"b\"` und `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], oxford = FALSE, lang = "de"),
    pb_str_catalog(l_vector[1:3], oxford = TRUE, lang = "de")
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], and_or = "or"),
    "`\"a\"`, `\"b\"`, or `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1], and_or = "or"),
    "`\"a\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1], and_or = "and"),
    "`\"a\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1], and_or = ""),
    "`\"a\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1], and_or = NULL),
    "`\"a\"`"
  )
  expect_equal(
    pb_str_catalog(1:3),
    "`1`, `2`, and `3`"
  )
  expect_equal(
    pb_str_catalog(c(TRUE, FALSE, TRUE)),
    "`TRUE`, `FALSE`, and `TRUE`"
  )
  expect_equal(
    pb_str_catalog(as.Date(c("2015-05-26", "2020-06-17"))),
    "`2015-05-26` and `2020-06-17`"
  )
  expect_equal(
    pb_str_catalog(small_table$date_time[1:2]),
    "`2016-01-04 11:00:00` and `2016-01-04 00:32:00`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 1, and_or = "or", lang = "de"),
    "`\"a\"` (+2 mehr)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 2, and_or = "or", lang = "de"),
    "`\"a\"`, `\"b\"` (+1 mehr)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 3, and_or = "or", lang = "de"),
    "`\"a\"`, `\"b\"` oder `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 3, and_or = "or", lang = "de"),
    pb_str_catalog(l_vector[1:3], limit = 4, and_or = "or", lang = "de")
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 1, and_or = "and", lang = "de"),
    "`\"a\"` (+2 mehr)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 2, and_or = "and", lang = "de"),
    "`\"a\"`, `\"b\"` (+1 mehr)"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 3, and_or = "and", lang = "de"),
    "`\"a\"`, `\"b\"` und `\"c\"`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], limit = 3, and_or = "and", lang = "de"),
    pb_str_catalog(l_vector[1:3], limit = 4, and_or = "and", lang = "de")
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3]),
    pb_str_catalog(l_vector[1:3], as_code = TRUE, quot_str = TRUE)
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], as_code = TRUE, quot_str = FALSE),
    "`a`, `b`, and `c`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], as_code = FALSE, quot_str = TRUE),
    "\"a\", \"b\", and \"c\""
  )
  expect_equal(
    pb_str_catalog(l_vector[1:3], as_code = FALSE, quot_str = FALSE),
    "a, b, and c"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = TRUE, quot_str = FALSE),
    "`a` and `b`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = FALSE, quot_str = TRUE),
    "\"a\" and \"b\""
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = FALSE, quot_str = FALSE),
    "a and b"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = TRUE, quot_str = FALSE, lang = "fr"),
    "`a` et `b`"
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = FALSE, quot_str = TRUE, lang = "fr"),
    "\"a\" et \"b\""
  )
  expect_equal(
    pb_str_catalog(l_vector[1:2], as_code = FALSE, quot_str = FALSE, lang = "fr"),
    "a et b"
  )
  
  expect_error(pb_str_catalog(l_vector, and_or = NA))
  expect_error(pb_str_catalog(l_vector, and_or = "maybe"))
  
  #
  # pb_quantile_stats
  # 
  
  diamond_ducks <-
    db_tbl(table = ggplot2::diamonds, dbname = ":memory:", dbtype = "duckdb")
  
  expect_equal(pb_quantile_stats(dplyr::tibble(a = 0:100), 0.5), 50)
  expect_equal(pb_quantile_stats(dplyr::tibble(a = 0:100), 1), 100)
  expect_equal(pb_quantile_stats(dplyr::tibble(a = 0:100), 0), 0)
  expect_equal(pb_quantile_stats(dplyr::tibble(a = c(0:100, NA)), 0.5), 50)
  expect_equal(pb_quantile_stats(dplyr::tibble(a = c(0:100, NA)), 1), 100)
  expect_equal(pb_quantile_stats(dplyr::tibble(a = c(0:100, NA)), 0), 0)
  
  expect_equal(pb_quantile_stats(small_table %>% dplyr::select(c), 0.5), 7)
  expect_equal(pb_quantile_stats(small_table_sqlite() %>% dplyr::select(c), 0.5), 7)
  
  expect_equal(pb_quantile_stats(diamond_ducks %>% dplyr::select(depth), 0.5), 61.8)
  expect_equal(pb_quantile_stats(diamond_ducks %>% dplyr::select(table), 0.5), 57)
  expect_equal(pb_quantile_stats(diamond_ducks %>% dplyr::select(price), 0.5), 2401)
  
  #
  # pb_fmt_number
  #
  
  expect_equal(pb_fmt_number(5.235), "5.24")
  expect_equal(pb_fmt_number(5.235, decimals = 5), "5.23500")
  expect_equal(pb_fmt_number(5L, decimals = 5), "5.00000")
  expect_equal(pb_fmt_number(5.235, n_sigfig = 2), "5.2")
  expect_equal(pb_fmt_number(5L, n_sigfig = 2), "5.0")
  expect_equal(pb_fmt_number(5L, n_sigfig = 1), "5")
  expect_equal(
    pb_fmt_number(5L, n_sigfig = 1, drop_trailing_dec_mark = FALSE),
    "5."
  )
  expect_equal(pb_fmt_number(24245, decimals = 1), "24,245.0")
  expect_equal(pb_fmt_number(24245, decimals = 1, use_seps = FALSE), "24245.0")
  expect_equal(pb_fmt_number(24245, decimals = 1, suffixing = TRUE), "24.2K")
  expect_equal(pb_fmt_number(242, decimals = 1, pattern = "-{x}-"), "-242.0-")
  expect_equal(
    pb_fmt_number(24245, decimals = 1, dec_mark = ",", sep_mark = "."),
    "24.245,0"
  )
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "en"), "24,245.0")
  # expect_equal(pb_fmt_number(24245, decimals = 1, locale = "fr"), "24 245,0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "de"), "24.245,0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "it"), "24.245,0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "es"), "24.245,0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "pt"), "24.245,0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "zh"), "24,245.0")
  expect_equal(pb_fmt_number(24245, decimals = 1, locale = "ru"), "24 245,0")
  
  expect_null(pb_fmt_number(NULL))
  expect_equal(pb_fmt_number("text"), "text")
  
  #
  # add_icon_img / add_icon_svg
  #

  function_icons <-
    c(
      "col_vals_lt",
      "col_vals_lte",
      "col_vals_equal",
      "col_vals_not_equal",
      "col_vals_gte",
      "col_vals_gt",
      "col_vals_between",
      "col_vals_not_between",
      "col_vals_in_set",
      "col_vals_not_in_set",
      "col_vals_make_set",
      "col_vals_make_subset",
      "col_vals_increasing",
      "col_vals_decreasing",
      "col_vals_null",
      "col_vals_not_null",
      "col_vals_regex",
      "col_vals_within_spec",
      "col_vals_expr",
      "rows_distinct",
      "rows_complete",
      "col_is_character",
      "col_is_numeric",
      "col_is_integer",
      "col_is_logical",
      "col_is_date",
      "col_is_posix",
      "col_is_factor",
      "col_exists",
      "col_schema_match",
      "row_count_match",
      "col_count_match",
      "tbl_match",
      "conjointly",
      "serially",
      "specially"
    )

  for (i in seq(function_icons)) {

    expect_match(
      add_icon_img(icon = function_icons[i]),
      regexp = "^<img src=\"data:image/png;base64,"
    )
  }

  for (i in seq(function_icons)) {

    expect_match(
      add_icon_svg(icon = function_icons[i]),
      regexp = paste0(
        "<div style=\"margin:0;padding:0;display:inline-block;height:30px;",
        "vertical-align:middle;\">.*<svg width=\"30px\" height=\"30px\".*",
        "</svg></div>"
      )
    )
  }
  
  #
  # glue_safely
  #
  
  one <- 1
  two <- 2
  three <- 3
  
  expect_equal(
    glue_safely("Easy as {one}, {two}, {three}."),
    "Easy as 1, 2, 3."
  )
  expect_no_error(glue_safely("Easy as {one}, {two}, {three}."))
  expect_no_error(glue_safely("Easy as {LETTERS[1]}, B, C."))
  
  #
  # print_time
  #
  
  expect_equal(print_time(time_diff_s = 0.1), "")
  expect_equal(print_time(time_diff_s = 1.0), " {.time_taken (1.0 s)}")
  expect_equal(print_time(time_diff_s = 10.0), " {.time_taken (10.0 s)}")
  expect_equal(print_time(time_diff_s = 100.0), " {.time_taken (100.0 s)}")
  expect_equal(print_time(time_diff_s = 1000.0), " {.time_taken (1000.0 s)}")
  expect_equal(print_time(time_diff_s = 345.343234), " {.time_taken (345.3 s)}")
})
