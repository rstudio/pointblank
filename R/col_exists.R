#' Do one or more columns actually exist?
#'
#' The `col_exists()` validation step function and the `expect_col_exists()`
#' expectation function both check whether one or more columns exist in the
#' target table. The only requirement is specification of the column names. The
#' validation step function can be used directly on a data table or with an
#' *agent* object (technically, a `ptblank_agent` object) whereas the
#' expectation function can only be used with a data table. The types of data
#' tables that can be used include data frames, tibbles, and even database
#' tables of the `tbl_dbi` class. Each validation step or expectation will
#' operate over a single test unit, which is whether the column exists or not.
#' 
#' If providing multiple column names, the result will be an expansion of
#' validation steps to that number of column names (e.g., `vars(col_a, col_b)`
#' will result in the entry of two validation steps). Aside from column names
#' in quotes and in `vars()`, **tidyselect** helper functions are available for
#' specifying columns. They are: `starts_with()`, `ends_with()`, `contains()`,
#' `matches()`, and `everything()`.
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation step function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction test
#' units failed, or, an absolute value), often using the `warn_at` argument.
#' Using `action_levels(warn_at = 1)` or `action_levels(stop_at = 1)` are good
#' choices depending on the situation (the first produces a warning, the other
#' `stop()`s).
#' 
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#'
#' @inheritParams col_vals_gt
#' @param columns One or more columns from the table in focus. This can be
#'   provided as a vector of column names using `c()` or bare column names
#'   enclosed in [vars()].
#'   
#' @return For the validation step function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   
#' @examples
#' # Create a simple table with
#' # two columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # Validate that columns `a` and `b`
#' # exist in the `tbl` table
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_exists(vars(a, b)) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-23
#' @name col_exists
NULL

#' @rdname col_exists
#' @import rlang
#' @export
col_exists <- function(x,
                       columns,
                       actions = NULL,
                       brief = NULL,
                       active = TRUE) {

  preconditions <- NULL
  values <- NULL
  
  # Normalize the `columns` expression
  if (inherits(columns, "quosures")) {
    
    columns <- 
      vapply(
        columns,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) as.character(rlang::get_expr(x))
      )
  }

  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_exists(
        columns = columns,
        actions = prime_actions(actions),
        brief = brief,
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x

  if (is.null(brief)) {
    brief <- generate_autobriefs(agent, columns, preconditions, values, "col_exists")
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (i in seq(columns)) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_exists",
        column = columns[i],
        preconditions = NULL,
        actions = actions,
        brief = brief[i],
        active = active
      )
  }

  agent
}

#' @rdname col_exists
#' @import rlang
#' @export
expect_col_exists <- function(object,
                              columns,
                              threshold = 1) {

  fn_name <- "expect_col_exists"
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_exists(
      columns = {{ columns }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>% .$validation_set
  
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
  
  column_text <- prep_column_text(vs$column[[1]])
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))
  )
  
  act$val <- object
  
  invisible(act$val)
}
