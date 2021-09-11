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

#' Run several tests and a final validation in a serial manner
#'
#' @description 
#' The `specially()` validation function allows for ...
#' 
#' The function to provide to `specially()` must either return a table or a
#' logical vector:
#' 
#' @section Column Names:
#' If providing multiple column names in any of the supplied validation steps,
#' the result will be an expansion of sub-validation steps to that number of
#' column names. Aside from column names in quotes and in `vars()`,
#' **tidyselect** helper functions are available for specifying columns. They
#' are: `starts_with()`, `ends_with()`, `contains()`, `matches()`, and
#' `everything()`.
#' 
#' @section Preconditions:
#' Providing expressions as `preconditions` means **pointblank** will preprocess
#' the target table table during interrogation as a preparatory step. It might
#' happen that a particular validation requires a calculated column, some
#' filtering of rows, or the addition of columns via a join, etc. Especially for
#' an *agent*-based report this can be advantageous since we can develop a large
#' validation plan with a single target table and make minor adjustments to it,
#' as needed, along the way.
#'
#' The table mutation is totally isolated in scope to the validation step(s)
#' where `preconditions` is used. Using **dplyr** code is suggested here since
#' the statements can be translated to SQL if necessary (i.e., if the target
#' table resides in a database). The code is most easily supplied as a one-sided
#' **R** formula (using a leading `~`). In the formula representation, the `.`
#' serves as the input data table to be transformed (e.g., `~ . %>%
#' dplyr::mutate(col_b = col_a + 10)`). Alternatively, a function could instead
#' be supplied (e.g., `function(x) dplyr::mutate(x, col_b = col_a + 10)`).
#' 
#' @section Actions:
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
#' @section Briefs:
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#' 
#' @section YAML:
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). When
#' `serially()` is represented in YAML (under the top-level `steps` key as a
#' list member), the syntax closely follows the signature of the validation
#' function. Here is an example of how a complex call of `serially()` as a
#' validation step is expressed in R code and in the corresponding YAML
#' representation.
#' 
#' ```
#' # R statement
#' agent %>% 
#'   specially(
#'     fn = function(x) { ... },
#'     preconditions = ~ . %>% dplyr::filter(a < 10),
#'     actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
#'     label = "The `specially()` step.",
#'     active = FALSE
#'   )
#' 
#' # YAML representation
#' steps:
#' - specially:
#'     fn: function(x) { ... }
#'     preconditions: ~. %>% dplyr::filter(a < 10)
#'     actions:
#'       warn_fraction: 0.1
#'       stop_fraction: 0.2
#'     label: The `specially()` step.
#'     active: false
#' ```
#' 
#' In practice, both of these will often be shorter as only the expressions for
#' validation steps are necessary. Arguments with default values won't be
#' written to YAML when using [yaml_write()] (though it is acceptable to include
#' them with their default when generating the YAML by other means). It is also
#' possible to preview the transformation of an agent to YAML without any
#' writing to disk by using the [yaml_agent_string()] function.
#'
#' @inheritParams col_vals_gt
#' @param fn A function performs the specialized validation on the data. It must
#'   either return a table with a logical column in the rightmost position, or,
#'   a logical vector.
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
#'     b = c(6, 4, 9),
#'     c = c(1, 2, 3)
#'   )
#'   
#' tbl
#'   
#' # A: Using an `agent` with validation
#' #    functions and then `interrogate()`
#' 
#' # The `specially()` function can be set
#' # up to perform a series of tests and
#' # then perform a validation (only if
#' # all tests pass); here, we are going
#' # to (1) test whether columns `a` and
#' # `b` are numeric, (2) check that both
#' # don't have any `NA` values, and (3)
#' # perform a finalizing validation that
#' # checks whether values in `b` are
#' # greater than values in `a`
#' agent_1 <-
#'   create_agent(tbl = tbl) %>%
#'   specially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b)),
#'     ~ col_vals_gt(., vars(b), vars(a))
#'     ) %>%
#'   interrogate()
#'   
#' # Determine if this validation
#' # had no failing test units (there are
#' # are 4 tests and a final validation)
#' all_passed(agent_1)
#' 
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent_1)`
#' 
#' # What's going on? All four of the tests
#' # passed and so the final validation
#' # occurred; there were no failing test
#' # units in that either!
#' 
#' # The final validation is optional; here
#' # is a different agent where only the
#' # serial tests are performed
#' agent_2 <-
#'   create_agent(tbl = tbl) %>%
#'   specially(
#'     ~ test_col_is_numeric(., vars(a, b)),
#'     ~ test_col_vals_not_null(., vars(a, b))
#'   ) %>%
#'   interrogate()
#'   
#' # Everything is good here too:
#' all_passed(agent_2)
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
#'   specially(
#'   )
#'
#' # C: Using the expectation function
#' 
#' # With the `expect_*()` form, we would
#' # typically perform one validation at a
#' # time; this is primarily used in
#' # testthat tests
#' expect_specially(
#'   tbl,
#' )
#' 
#' # D: Using the test function
#' 
#' # With the `test_*()` form, we should
#' # get a single logical value returned
#' # to us
#' tbl %>%
#'   test_specially(
#'   )
#'
#' @family validation functions
#' @section Function ID:
#' 2-22
#' 
#' @name specially
NULL

#' @rdname specially
#' @import rlang
#' 
#' @export
specially <- function(x,
                      fn,
                      preconditions = NULL,
                      actions = NULL,
                      step_id = NULL,
                      label = NULL,
                      brief = NULL,
                      active = TRUE) {
  
  segments <- NULL
  
  # Resolve segments into list
  segments_list <-
    resolve_segments(
      x = x,
      seg_expr = segments,
      preconditions = preconditions
    )
  
  if (is_a_table_object(x)) {
    
    secret_agent <-
      create_agent(x, label = "::QUIET::") %>%
      specially(
        fn = fn,
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
        assertion_type = "specially"
      )
  }
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Get the next step number for the `validation_set` tibble
  i_o <- get_next_validation_set_row(agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add one or more validation steps based on the
  # length of `segments_list`
  for (i in seq_along(segments_list)) {
    
    seg_col <- names(segments_list[i])
    seg_val <- unname(unlist(segments_list[i]))
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "specially",
        i_o = i_o,
        columns_expr = NULL,
        column = NULL,
        values = fn,
        na_pass = NULL,
        preconditions = preconditions,
        seg_expr = segments,
        seg_col = seg_col,
        seg_val = seg_val,
        actions = covert_actions(actions, agent),
        step_id = step_id,
        label = label,
        brief = brief,
        active = active
      )
  }
  
  agent
}

#' @rdname specially
#' @import rlang
#' @export
expect_specially <- function(object,
                             fn,
                             preconditions = NULL,
                             threshold = 1) {
  
  fn_name <- "expect_specially"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    specially(
      fn = fn,
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

#' @rdname specially
#' @import rlang
#' @export
test_specially <- function(object,
                           fn,
                           preconditions = NULL,
                           threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    specially(
      fn = fn,
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
