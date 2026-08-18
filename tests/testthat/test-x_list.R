al <- action_levels(warn = 0.1, error = 0.2)

agent <-
  create_agent(tbl = small_table, actions = al) %>%
  col_vals_gt(
    vars(g), 100,
    preconditions = ~ . %>% dplyr::mutate(g = a + 95)
  ) %>%
  col_vals_lt(
    vars(c), vars(d),
    preconditions = ~ . %>% dplyr::mutate(d = d - 200)
  ) %>%
  col_vals_in_set(vars(f), c("low", "mid", "high", "higher"))

test_that("An x-list for a step is structurally correct", {

  # Get an x-list at step 1 before interrogation
  x_list_before <-
    agent %>%
    get_agent_x_list(i = 1)

  # Expect the class names for the object to be `x_list`
  # and `x_list_i`
  expect_s3_class(x_list_before, "x_list")
  expect_s3_class(x_list_before, "x_list_i")
  expect_true(is_ptblank_x_list(x_list_before))

  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_s3_class(x_list_before$time_start, "POSIXct")
  expect_equal(length(x_list_before$time_start), 0)
  expect_s3_class(x_list_before$time_end, "POSIXct")
  expect_equal(length(x_list_before$time_end), 0)
  expect_true(grepl("^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$", x_list_before$label))
  expect_type(x_list_before$tbl_name, "character")
  expect_equal(x_list_before$tbl_name, "small_table")
  expect_type(x_list_before$tbl_src, "character")
  expect_equal(x_list_before$tbl_src, "tbl_df")
  expect_type(x_list_before$tbl_src_details, "character")
  expect_equal(x_list_before$tbl_src_details, NA_character_)
  expect_s3_class(x_list_before$tbl, "tbl_df")
  expect_equal(x_list_before$tbl, small_table)
  expect_type(x_list_before$col_names, "character")
  expect_equal(x_list_before$col_names, colnames(small_table))
  expect_type(x_list_before$col_types, "character")
  expect_equal(
    x_list_before$col_types,
    c("POSIXct", "Date", "integer", "character", "numeric", "numeric",
      "logical", "character")
  )
  expect_type(x_list_before$i, "double")
  expect_equal(x_list_before$i, 1)
  expect_type(x_list_before$type, "character")
  expect_equal(x_list_before$type, "col_vals_gt")
  expect_type(x_list_before$columns, "character")
  expect_equal(x_list_before$columns, "g")
  expect_type(x_list_before$values, "double")
  expect_equal(x_list_before$values, 100)
  expect_type(x_list_before$values, "double")
  expect_equal(x_list_before$values, 100)
  expect_type(x_list_before$label, "character")
  expect_type(x_list_before$briefs, "character")
  expect_equal(
    x_list_before$briefs,
    paste0(
      "Expect that values in `g` (computed column) should be > `100`. ",
      "Precondition applied: `. %>% dplyr::mutate(g = a + 95)`."
      )
  )
  expect_type(x_list_before$eval_error, "logical")
  expect_equal(x_list_before$eval_error, NA)
  expect_type(x_list_before$eval_warning, "logical")
  expect_equal(x_list_before$eval_warning, NA)
  expect_null(x_list_before$capture_stack[[1]])
  expect_type(x_list_before$n, "integer")
  expect_equal(x_list_before$n, NA_integer_)
  expect_type(x_list_before$n_passed, "integer")
  expect_equal(x_list_before$n_passed, NA_integer_)
  expect_type(x_list_before$n_failed, "integer")
  expect_equal(x_list_before$n_failed, NA_integer_)
  expect_type(x_list_before$f_passed, "double")
  expect_equal(x_list_before$f_passed, NA_real_)
  expect_type(x_list_before$f_failed, "double")
  expect_equal(x_list_before$f_failed, NA_real_)
  expect_type(x_list_before$warn, "logical")
  expect_equal(x_list_before$warn, NA)
  expect_type(x_list_before$error, "logical")
  expect_equal(x_list_before$error, NA)
  expect_type(x_list_before$critical, "logical")
  expect_equal(x_list_before$critical, NA)
  expect_type(x_list_before$lang, "character")
  expect_equal(x_list_before$lang, "en")

  # Get an x-list at step 1 after interrogation
  x_list_after <-
    agent %>%
    interrogate() %>%
    get_agent_x_list(i = 1)

  # Expect the class names for the object to be `x_list`
  # and `x_list_i`
  expect_s3_class(x_list_after, "x_list")
  expect_s3_class(x_list_after, "x_list_i")
  expect_true(is_ptblank_x_list(x_list_after))

  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(length(x_list_after$time_start), 1)
  expect_s3_class(x_list_after$time_start, "POSIXct")
  expect_equal(length(x_list_after$time_end), 1)
  expect_s3_class(x_list_after$time_end, "POSIXct")
  expect_true(grepl("^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$", x_list_after$label))
  expect_type(x_list_after$tbl_name, "character")
  expect_equal(x_list_after$tbl_name, "small_table")
  expect_type(x_list_after$tbl_src, "character")
  expect_equal(x_list_after$tbl_src, "tbl_df")
  expect_type(x_list_after$tbl_src_details, "character")
  expect_equal(x_list_after$tbl_src_details, NA_character_)
  expect_s3_class(x_list_after$tbl, "tbl_df")
  expect_equal(x_list_after$tbl, small_table)
  expect_type(x_list_after$col_names, "character")
  expect_equal(x_list_after$col_names, colnames(small_table))
  expect_type(x_list_after$col_types, "character")
  expect_equal(
    x_list_after$col_types,
    c("POSIXct", "Date", "integer", "character", "numeric", "numeric",
      "logical", "character")
  )
  expect_type(x_list_after$i, "double")
  expect_equal(x_list_after$i, 1)
  expect_type(x_list_after$type, "character")
  expect_equal(x_list_after$type, "col_vals_gt")
  expect_type(x_list_after$columns, "character")
  expect_equal(x_list_after$columns, "g")
  expect_type(x_list_after$values, "double")
  expect_equal(x_list_after$values, 100)
  expect_type(x_list_after$values, "double")
  expect_equal(x_list_after$values, 100)
  expect_type(x_list_after$label, "character")
  expect_type(x_list_after$briefs, "character")
  expect_equal(
    x_list_after$briefs,
    paste0(
      "Expect that values in `g` (computed column) should be > `100`. ",
      "Precondition applied: `. %>% dplyr::mutate(g = a + 95)`."
    )
  )
  expect_type(x_list_after$eval_error, "logical")
  expect_equal(x_list_after$eval_error, FALSE)
  expect_type(x_list_after$eval_warning, "logical")
  expect_equal(x_list_after$eval_warning, FALSE)
  expect_type(x_list_after$capture_stack, "list")
  expect_equal(
    length(x_list_after$capture_stack %>% unlist(recursive = FALSE)),
    3
  )
  expect_equal(
    names(x_list_after$capture_stack %>% unlist(recursive = FALSE)),
    c("warning", "error", "pb_call")
  )
  expect_type(x_list_after$n, "double")
  expect_equal(x_list_after$n, 13)
  expect_type(x_list_after$n_passed, "double")
  expect_equal(x_list_after$n_passed, 3)
  expect_type(x_list_after$n_failed, "double")
  expect_equal(x_list_after$n_failed, 10)
  expect_type(x_list_after$f_passed, "double")
  expect_equal(x_list_after$f_passed, 0.23077)
  expect_type(x_list_after$f_failed, "double")
  expect_equal(x_list_after$f_failed, 0.76923)
  expect_type(x_list_after$warn, "logical")
  expect_equal(x_list_after$warn, TRUE)
  expect_type(x_list_after$error, "logical")
  expect_equal(x_list_after$error, TRUE)
  expect_type(x_list_after$critical, "logical")
  expect_equal(x_list_after$critical, NA)
  expect_type(x_list_after$lang, "character")
  expect_equal(x_list_after$lang, "en")
})


test_that("A complete x-list is structurally correct", {

  # Get an x-list at step 1 before interrogation
  x_list_before <-
    agent %>%
    get_agent_x_list()

  # Expect the class names for the object to be `x_list`
  # and `x_list_i`
  expect_s3_class(x_list_before, "x_list")
  expect_s3_class(x_list_before, "x_list_n")
  expect_true(is_ptblank_x_list(x_list_before))

  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(length(x_list_before$time_start), 0)
  expect_s3_class(x_list_before$time_start, "POSIXct")
  expect_equal(length(x_list_before$time_end), 0)
  expect_s3_class(x_list_before$time_end, "POSIXct")
  expect_true(grepl("^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$", x_list_before$label))
  expect_type(x_list_before$tbl_name, "character")
  expect_equal(x_list_before$tbl_name, "small_table")
  expect_type(x_list_before$tbl_src, "character")
  expect_equal(x_list_before$tbl_src, "tbl_df")
  expect_type(x_list_before$tbl_src_details, "character")
  expect_equal(x_list_before$tbl_src_details, NA_character_)
  expect_s3_class(x_list_before$tbl, "tbl_df")
  expect_equal(x_list_before$tbl, small_table)
  expect_type(x_list_before$col_names, "character")
  expect_equal(x_list_before$col_names, colnames(small_table))
  expect_type(x_list_before$col_types, "character")
  expect_equal(
    x_list_before$col_types,
    c("POSIXct", "Date", "integer", "character", "numeric", "numeric",
      "logical", "character")
  )
  expect_type(x_list_before$i, "integer")
  expect_equal(x_list_before$i, 1:3)
  expect_type(x_list_before$type, "character")
  expect_equal(
    x_list_before$type,
    c("col_vals_gt", "col_vals_lt", "col_vals_in_set")
  )
  expect_type(x_list_before$columns, "list")
  expect_equal(length(x_list_before$columns), 3)
  expect_equal(x_list_before$columns %>% unlist(), c("g", "c", "f"))
  expect_type(x_list_before$values, "list")
  expect_equal(length(x_list_before$values), 3)
  expect_equal(unlist(x_list_before$values[1]), 100)
  expect_type(unlist(x_list_before$values[2]), "list")
  expect_s3_class(unlist(x_list_before$values[2])[[1]], "quosure")
  expect_equal(
    unlist(x_list_before$values[3]),
    c("low", "mid", "high", "higher")
  )
  expect_type(x_list_before$label, "character")
  expect_type(x_list_before$briefs, "character")
  expect_equal(
    x_list_before$briefs,
    c(
      paste0(
        "Expect that values in `g` (computed column) should be > `100`. ",
        "Precondition applied: `. %>% dplyr::mutate(g = a + 95)`."
      ),
      paste0(
        "Expect that values in `c` should be < `d`. Precondition applied: ",
        "`. %>% dplyr::mutate(d = d - 200)`."
      ),
      paste0(
        "Expect that values in `f` should be in the set of `low`, `mid`, ",
        "`high` (and 1 more). "
      )
    )
  )
  expect_type(x_list_before$eval_error, "logical")
  expect_equal(x_list_before$eval_error, rep(NA, 3))
  expect_type(x_list_before$eval_warning, "logical")
  expect_equal(x_list_before$eval_warning, rep(NA, 3))
  expect_type(x_list_before$capture_stack, "list")
  expect_type(x_list_before$n, "integer")
  expect_equal(x_list_before$n, rep(NA_integer_, 3))
  expect_type(x_list_before$n_passed, "integer")
  expect_equal(x_list_before$n_passed, rep(NA_integer_, 3))
  expect_type(x_list_before$n_failed, "integer")
  expect_equal(x_list_before$n_failed, rep(NA_integer_, 3))
  expect_type(x_list_before$f_passed, "double")
  expect_equal(x_list_before$f_passed, rep(NA_real_, 3))
  expect_type(x_list_before$f_failed, "double")
  expect_equal(x_list_before$f_failed, rep(NA_real_, 3))
  expect_type(x_list_before$warn, "logical")
  expect_equal(x_list_before$warn, rep(NA, 3))
  expect_type(x_list_before$error, "logical")
  expect_equal(x_list_before$error, rep(NA, 3))
  expect_type(x_list_before$critical, "logical")
  expect_equal(x_list_before$critical, rep(NA, 3))
  expect_s3_class(x_list_before$validation_set, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x_list_before$validation_set), 3)
  expect_equal(ncol(x_list_before$validation_set), 35)
  expect_type(x_list_before$lang, "character")
  expect_equal(x_list_before$lang, "en")
  expect_s3_class(x_list_before$report_object, "gt_tbl")
  expect_type(x_list_before$report_object, "list")
  expect_null(x_list_before$email_object)
  expect_type(x_list_before$report_html, "character")
  expect_type(x_list_before$report_html_small, "character")

  # Get an x-list at step 1 after interrogation
  x_list_after <-
    agent %>%
    interrogate() %>%
    get_agent_x_list()

  # Expect the class names for the object to be `x_list`
  # and `x_list_i`
  expect_s3_class(x_list_after, c("x_list_n", "x_list"))
  expect_true(is_ptblank_x_list(x_list_after))

  # Expect elements of the object to be equivalent
  # to specific parameters
  expect_equal(length(x_list_after$time_start), 1)
  expect_s3_class(x_list_after$time_start, "POSIXct")
  expect_equal(length(x_list_after$time_end), 1)
  expect_s3_class(x_list_after$time_end, "POSIXct")
  expect_match(x_list_after$label, "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}\\|[0-9]{2}:[0-9]{2}:[0-9]{2}\\]$")
  expect_type(x_list_after$tbl_name, "character")
  expect_equal(x_list_after$tbl_name, "small_table")
  expect_type(x_list_after$tbl_src, "character")
  expect_equal(x_list_after$tbl_src, "tbl_df")
  expect_type(x_list_after$tbl_src_details, "character")
  expect_equal(x_list_after$tbl_src_details, NA_character_)
  expect_s3_class(x_list_after$tbl, "tbl_df")
  expect_equal(x_list_after$tbl, small_table)
  expect_type(x_list_after$col_names, "character")
  expect_equal(x_list_after$col_names, colnames(small_table))
  expect_type(x_list_after$col_types, "character")
  expect_equal(
    x_list_after$col_types,
    c("POSIXct", "Date", "integer", "character", "numeric", "numeric",
      "logical", "character")
  )
  expect_type(x_list_after$i, "integer")
  expect_equal(x_list_after$i, 1:3)
  expect_type(x_list_after$type, "character")
  expect_equal(
    x_list_after$type,
    c("col_vals_gt", "col_vals_lt", "col_vals_in_set")
  )
  expect_type(x_list_after$columns, "list")
  expect_length(x_list_after$columns, 3)
  expect_equal(x_list_after$columns %>% unlist(), c("g", "c", "f"))
  expect_type(x_list_after$values, "list")
  expect_equal(length(x_list_after$values), 3)
  expect_equal(unlist(x_list_after$values[1]), 100)
  expect_type(unlist(x_list_after$values[2]), "list")
  expect_s3_class(unlist(x_list_after$values[2])[[1]], "quosure")
  expect_equal(
    unlist(x_list_after$values[3]),
    c("low", "mid", "high", "higher")
  )
  expect_type(x_list_after$label, "character")
  expect_type(x_list_after$briefs, "character")
  expect_equal(
    x_list_after$briefs,
    c(
      paste0(
        "Expect that values in `g` (computed column) should be > `100`. ",
        "Precondition applied: `. %>% dplyr::mutate(g = a + 95)`."
      ),
      paste0(
        "Expect that values in `c` should be < `d`. Precondition applied: ",
        "`. %>% dplyr::mutate(d = d - 200)`."
      ),
      paste0(
        "Expect that values in `f` should be in the set of `low`, `mid`, ",
        "`high` (and 1 more). "
      )
    )
  )
  expect_type(x_list_after$eval_error, "logical")
  expect_equal(x_list_after$eval_error, rep(FALSE, 3))
  expect_type(x_list_after$eval_warning, "logical")
  expect_equal(x_list_after$eval_warning, rep(FALSE, 3))
  expect_type(x_list_after$capture_stack, "list")
  expect_type(x_list_after$n, "double")
  expect_equal(x_list_after$n, rep(13, 3))
  expect_type(x_list_after$n_passed, "double")
  expect_equal(x_list_after$n_passed, c(3, 10, 13))
  expect_type(x_list_after$n_failed, "double")
  expect_equal(x_list_after$n_failed, c(10, 3, 0))
  expect_type(x_list_after$f_passed, "double")
  expect_equal(x_list_after$f_passed, c(0.23077, 0.76923, 1))
  expect_type(x_list_after$f_failed, "double")
  expect_equal(x_list_after$f_failed, c(0.76923, 0.23077, 0))
  expect_type(x_list_after$warn, "logical")
  expect_equal(x_list_after$warn, c(TRUE, TRUE, FALSE))
  expect_type(x_list_after$error, "logical")
  expect_equal(x_list_after$error, c(TRUE, TRUE, FALSE))
  expect_type(x_list_after$critical, "logical")
  expect_equal(x_list_after$critical, rep(NA, 3))
  expect_s3_class(x_list_after$validation_set, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_equal(nrow(x_list_after$validation_set), 3)
  expect_equal(ncol(x_list_after$validation_set), 35)
  expect_type(x_list_after$lang, "character")
  expect_equal(x_list_after$lang, "en")
  expect_s3_class(x_list_after$report_object, "gt_tbl")
  expect_type(x_list_after$report_object, "list")
  expect_s3_class(x_list_after$email_object, c("blastula_message", "email_message"))
  expect_type(x_list_after$report_html, "character")
  expect_type(x_list_after$report_html_small, "character")
})
