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


#' Perform multiple rowwise validations for joint validity
#'
#' The `conjointly()` validation function, the `expect_conjointly()` expectation
#' function, and the `test_conjointly()` test function all check whether test
#' units at each index (typically each row) all pass multiple validations with
#' `col_vals_*()`-type functions. Because of the imposed constraint on the
#' allowed validation functions, all test units are rows of the table (after any
#' common `preconditions` have been applied). Each of the functions (composed
#' with multiple validation function calls) ultimately perform a rowwise test of
#' whether all sub-validations reported a *pass* for the same test units. In
#' practice, an example of a joint validation is testing whether values for
#' column `a` are greater than a specific value while values for column `b` lie
#' within a specified range. The validation functions to be part of the conjoint
#' validation are to be supplied as one-sided **R** formulas (using a leading
#' `~`, and having a `.` stand in as the data object). The validation function
#' can be used directly on a data table or with an *agent* object (technically,
#' a `ptblank_agent` object) whereas the expectation and test functions can only
#' be used with a data table.
#'
#' If providing multiple column names in any of the supplied validation step
#' functions, the result will be an expansion of sub-validation steps to that
#' number of column names. Aside from column names in quotes and in `vars()`,
#' **tidyselect** helper functions are available for specifying columns. They
#' are: `starts_with()`, `ends_with()`, `contains()`, `matches()`, and
#' `everything()`.
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
#' @param ... a collection one-sided formulas that consist of validation step
#' functions that validate row units. Specifically, these functions should be
#' those with the naming pattern `col_vals_*()`. An example of this is
#' `~ col_vals_gte(., vars(a), 5.5), ~ col_vals_not_null(., vars(b)`).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#'
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#'
#' @examples
#' # For all examples here, we'll use
#' # a simple table with three numeric
#' # columns (`a`, `b`, and `c`); this is
#' # a very basic table but it'll be more
#' # useful when explaining things later
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 2, 6),
#'     b = c(3, 4, 6),
#'     c = c(9, 8, 7)
#'   )
#'   
#' tbl
#'   
#' # A: Using an `agent` with validation
#' #    functions and then `interrogate()`
#' 
#' # Validate a number of things on a
#' # row-by-row basis using validation
#' # functions of the `col_vals*` type
#' # (all have the same number of test
#' # units): (1) values in `a` are less
#' # than `4`, (2) values in `c` are
#' # greater than the adjacent values in
#' # `a`, and (3) there aren't any NA
#' # values in `b`
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   conjointly(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'     ) %>%
#'   interrogate()
#'   
#' # Determine if this validation
#' # had no failing test units (there
#' # are 3 test units, one for each row)
#' all_passed(agent)
#' 
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent)`
#' 
#' # What's going on? Think of there being
#' # three parallel validations, each
#' # producing a column of `TRUE` or `FALSE`
#' # values (`pass` or `fail`) and line them
#' # up side-by-side, any rows with any
#' # `FALSE` values results in a conjoint
#' # `fail` test unit
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
#'   conjointly(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'   )
#'
#' # C: Using the expectation function
#' 
#' # With the `expect_*()` form, we would
#' # typically perform one validation at a
#' # time; this is primarily used in
#' # testthat tests
#' expect_conjointly(
#'   tbl,
#'   ~ col_vals_lt(., vars(a), 8),
#'   ~ col_vals_gt(., vars(c), vars(a)),
#'   ~ col_vals_not_null(., vars(b))
#' )
#' 
#' # D: Using the test function
#' 
#' # With the `test_*()` form, we should
#' # get a single logical value returned
#' # to us
#' tbl %>%
#'   test_conjointly(
#'     ~ col_vals_lt(., vars(a), 8),
#'     ~ col_vals_gt(., vars(c), vars(a)),
#'     ~ col_vals_not_null(., vars(b))
#'   )
#'
#' @family validation functions
#' @section Function ID:
#' 2-14
#'
#' @name conjointly
NULL

#' @rdname conjointly
#' @import rlang
#' @export
conjointly <- function(x,
                       ...,
                       .list = list2(...),
                       preconditions = NULL,
                       actions = NULL,
                       step_id = NULL,
                       label = NULL,
                       brief = NULL,
                       active = TRUE) {

  # Obtain all of the group's elements
  list_elements <- .list
  
  dots_attrs <- list_elements[rlang::names2(list_elements) != ""]
  
  validation_formulas <-
    list_elements[
      vapply(
        list_elements,
        function(x) rlang::is_formula(x),
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE
      )
    ]
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      conjointly(
        .list = .list,
        preconditions = preconditions,
        actions = prime_actions(actions),
        label = label,
        brief = brief,
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
        assertion_type = "conjointly",
        preconditions = preconditions,
        values = validation_formulas
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
      assertion_type = "conjointly",
      column = NULL,
      values = validation_formulas,
      na_pass = NULL,
      preconditions = preconditions,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )
  
  agent
}

#' @rdname conjointly
#' @import rlang
#' @export
expect_conjointly <- function(object,
                              ...,
                              .list = list2(...),
                              preconditions = NULL,
                              threshold = 1) {
  
  fn_name <- "expect_conjointly"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    conjointly(
      .list = .list,
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
  
  # TODO: express warnings and errors here
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  values_text <- prep_values_text(values = vs$values, limit = 3, lang = "en")
  
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

#' @rdname conjointly
#' @import rlang
#' @export
test_conjointly <- function(object,
                            ...,
                            .list = list2(...),
                            preconditions = NULL,
                            threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    conjointly(
      .list = .list,
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
