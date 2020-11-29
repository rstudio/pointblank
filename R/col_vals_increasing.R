#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#

#' Are column data increasing by row?
#'
#' The `col_vals_increasing()` validation function, the
#' `expect_col_vals_increasing()` expectation function, and the
#' `test_col_vals_increasing()` test function all check whether column values in
#' a table are increasing when moving down a table. There are options for
#' allowing `NA` values in the target column, allowing stationary phases (where
#' consecutive values don't change), and even on for allowing decreasing
#' movements up to a certain threshold. The validation function can be used
#' directly on a data table or with an *agent* object (technically, a
#' `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table. The types of data tables that can be used include
#' data frames, tibbles, database tables (`tbl_dbi`), and Spark DataFrames
#' (`tbl_spark`). Each validation step or expectation will operate over the
#' number of test units that is equal to the number of rows in the table (after
#' any `preconditions` have been applied).
#'
#' If providing multiple column names to `columns`, the result will be an
#' expansion of validation steps to that number of column names (e.g.,
#' `vars(col_a, col_b)` will result in the entry of two validation steps). Aside
#' from column names in quotes and in `vars()`, **tidyselect** helper functions
#' are available for specifying columns. They are: `starts_with()`,
#' `ends_with()`, `contains()`, `matches()`, and `everything()`.
#'
#' This validation function supports special handling of `NA` values. The
#' `na_pass` argument will determine whether an `NA` value appearing in a test
#' unit should be counted as a *pass* or a *fail*. The default of `na_pass =
#' FALSE` means that any `NA`s encountered will accumulate failing test units.
#' 
#' Having table `preconditions` means **pointblank** will mutate the table just
#' before interrogation. Such a table mutation is isolated in scope to the
#' validation step(s) produced by the validation function call. Using
#' **dplyr** code is suggested here since the statements can be translated to
#' SQL if necessary. The code is most easily supplied as a one-sided **R**
#' formula (using a leading `~`). In the formula representation, the `.` serves
#' as the input data table to be transformed (e.g., 
#' `~ . %>% dplyr::mutate(col_a = col_b + 10)`). Alternatively, a function could
#' instead be supplied (e.g., 
#' `function(x) dplyr::mutate(x, col_a = col_b + 10)`).
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction of
#' test units failed, or, an absolute value), often using the `warn_at`
#' argument. This is especially true when `x` is a table object because,
#' otherwise, nothing happens. For the `col_vals_*()`-type functions, using 
#' `action_levels(warn_at = 0.25)` or `action_levels(stop_at = 0.25)` are good
#' choices depending on the situation (the first produces a warning when a
#' quarter of the total test units fails, the other `stop()`s at the same
#' threshold level).
#' 
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#' 
#' @inheritParams col_vals_gt
#' @param allow_stationary An option to allow pauses in decreasing values. For
#'   example if the values for the test units are `[85, 82, 82, 80, 77]` then
#'   the third unit (`82`, appearing a second time) would be marked with *fail*
#'   when `allow_stationary` is `FALSE` (the default). Using `allow_stationary =
#'   TRUE` will result in all the test units in `[85, 82, 82, 80, 77]` to be
#'   marked with *pass*.
#' @param decreasing_tol An optional threshold value that allows for movement of
#'   numerical values in the negative direction. By default this is `NULL` but
#'   using a numerical value with set the absolute threshold of negative travel
#'   allowed across numerical test units.
#' 
#' @family validation functions
#' 
#' @seealso The analogous function that moves in the opposite direction:
#' [col_vals_decreasing()].
#' 
#' @name col_vals_increasing
NULL

#' @rdname col_vals_increasing
#' @import rlang
#' @export
col_vals_increasing <- function(x,
                                columns,
                                allow_stationary = FALSE,
                                decreasing_tol = NULL,
                                na_pass = FALSE,
                                preconditions = NULL,
                                actions = NULL,
                                step_id = NULL,
                                label = NULL,
                                brief = NULL,
                                active = TRUE) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)
  
  # TODO: Ensure that `allow_stationary` is logical
  # TODO: Ensure that `decreasing_tol` is either `NULL` or numeric
  
  # Put `allow_stationary` and `decreasing_tol` into a length-2 numeric vector
  stat_tol <- 
    as.numeric(
      c(allow_stationary, if (is.null(decreasing_tol)) 0 else decreasing_tol)
    )
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_vals_increasing(
        columns = columns,
        allow_stationary = allow_stationary,
        decreasing_tol = decreasing_tol,
        na_pass = na_pass,
        preconditions = preconditions,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    brief <- 
      generate_autobriefs(
        agent, columns, preconditions,
        values = decreasing_tol, "col_vals_increasing"
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns, agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_increasing",
        column = columns[i],
        values = stat_tol,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = covert_actions(actions, agent),
        step_id = step_id[i],
        label = label,
        brief = brief[i],
        active = active
      )
  }
  
  agent
}

#' @rdname col_vals_increasing
#' @import rlang
#' @export
expect_col_vals_increasing <- function(object,
                                       columns,
                                       allow_stationary = FALSE,
                                       decreasing_tol = NULL,
                                       na_pass = FALSE,
                                       preconditions = NULL,
                                       threshold = 1) {
  
  fn_name <- "expect_col_vals_increasing"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_increasing(
      columns = {{ columns }},
      allow_stationary = allow_stationary,
      decreasing_tol = decreasing_tol,
      na_pass = na_pass,
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  x <- vs$notify %>% all()
  
  threshold_type <- get_threshold_type(threshold = threshold)
  
  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  # 
  # column_text <- prep_column_text(vs$column[[1]])
  # operator <- ">"
  # values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(
      failure_message_gluestring(
        fn_name = fn_name, lang = "en"
      )
    )
  )
  
  act$val <- object
  
  invisible(act$val)
}
