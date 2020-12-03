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


#' Do column data agree with a predicate expression?
#'
#' The `col_vals_expr()` validation function checks for whether column values in
#' a table match a user-defined predicate expression. The validation function
#' can be used directly on a data table or with an *agent* object (technically,
#' a `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table. The types of data tables that can be used include
#' data frames, tibbles, database tables (`tbl_dbi`), and Spark DataFrames
#' (`tbl_spark`). Each validation step or expectation will operate over the
#' number of test units that is equal to the number of rows in the table (after
#' any `preconditions` have been applied).
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
#' @param expr An expression to use for this test. This can either be in the
#'   form of a call made with the `expr()` function or as a one-sided **R**
#'   formula (using a leading `~`).
#'   
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#' 
#' @examples
#' # For all of the examples here, we'll
#' # use a simple table with three numeric
#' # columns (`a`, `b`, and `c`) and three
#' # character columns (`d`, `e`, and `f`)
#' tbl <-
#'   dplyr::tibble(
#'     a = c(1, 2, 1, 7, 8, 6),
#'     b = c(0, 0, 0, 1, 1, 1),
#'     c = c(0.5, 0.3, 0.8, 1.4, 1.9, 1.2),
#'   )
#'   
#' tbl
#' 
#' # A: Using an `agent` with validation
#' #    functions and then `interrogate()` 
#' 
#' # Validate that values in column `a`
#' # are integer-like by using the R modulo
#' # operator and expecting `0`
#' agent <-
#'   create_agent(tbl) %>%
#'   col_vals_expr(expr(a %% 1 == 0)) %>%
#'   interrogate()
#' 
#' # Determine if this validation
#' # had no failing test units (there
#' # are 6 test units, one for each row)
#' all_passed(agent)
#' 
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent)`
#' 
#' # B: Using the validation function
#' #    directly on the data (no `agent`)
#' 
#' # This way of using validation functions
#' # acts as a data filter: data is passed
#' # through but should `stop()` if there
#' # is a single test unit failing; the
#' # behavior of side effects can be
#' # customized with the `actions` option
#' tbl %>% 
#'   col_vals_expr(expr(a %% 1 == 0)) %>%
#'   dplyr::pull(a)
#'   
#' # C: Using the expectation function
#' 
#' # With the `expect_*()` form, we would
#' # typically perform one validation at a
#' # time; this is primarily used in
#' # testthat tests
#' expect_col_vals_expr(tbl, ~ a %% 1 == 0)
#' 
#' # D: Using the test function
#' 
#' # With the `test_*()` form, we should
#' # get a single logical value returned
#' # to us
#' test_col_vals_expr(tbl, ~ a %% 1 == 0)
#' 
#' # Variations
#' 
#' # We can do more complex things by
#' # taking advantage of the `case_when()`
#' # and `between()` functions (available
#' # for use in the pointblank package) 
#' tbl %>%
#'   test_col_vals_expr(~ case_when(
#'     b == 0 ~ a %>% between(0, 5) & c < 1,
#'     b == 1 ~ a > 5 & c >= 1
#'   ))
#' 
#' # If you only want to test a subset of
#' # rows, then the `case_when()` statement
#' # doesn't need to be exhaustive; any
#' # rows that don't fall into the cases
#' # will be pruned (giving us less test
#' # units overall) 
#' tbl %>%
#'   test_col_vals_expr(~ case_when(
#'     b == 1 ~ a > 5 & c >= 1
#'   ))
#' 
#' @family validation functions
#' @section Function ID:
#' 2-25
#' 
#' @seealso These reexported **dplyr** functions work nicely within
#'   `col_vals_expr()` and its variants: [dplyr::expr()], [dplyr::between()],
#'   and [dplyr::case_when()].
#' 
#' @name col_vals_expr
NULL

#' @rdname col_vals_expr
#' @import rlang
#' @export
col_vals_expr <- function(x,
                          expr,
                          preconditions = NULL,
                          actions = NULL,
                          step_id = NULL,
                          label = NULL,
                          brief = NULL,
                          active = TRUE) {
  
  if (!inherits(expr, "call")) {
    if (rlang::is_formula(expr)) {
      expr <- expr %>% rlang::f_rhs()
    } else {
      stop("The `expr` object must be a call or an R formula.\n",
           "* A call can be made with the `expr()` function.\n",
           "* An R formula can also be used, with the expression on the RHS.",
           call. = FALSE)
    }
  }
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      col_vals_expr(
        expr = expr,
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
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_expr"
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add a validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_vals_expr",
      column = NA_character_,
      values = expr,
      preconditions = preconditions,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )
  
  agent
}

#' @rdname col_vals_expr
#' @import rlang
#' @export
expect_col_vals_expr <- function(object,
                                 expr,
                                 preconditions = NULL,
                                 threshold = 1) {
  
  fn_name <- "expect_col_vals_expr"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_expr(
      expr = {{ expr }}, 
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

#' @rdname col_vals_expr
#' @import rlang
#' @export
test_col_vals_expr <- function(object,
                               expr,
                               preconditions = NULL,
                               threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_vals_expr(
      expr = {{ expr }}, 
      preconditions = {{ preconditions }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  all(!vs$notify)
}
