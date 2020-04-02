#' Perform multiple rowwise validations for joint validity
#'
#' The `conjointly()` validation step function checks whether the same test
#' units all pass multiple validations with `col_vals_*()`-type functions.
#' Because of the imposed constraint on the allowed validation step functions,
#' all test units are rows of the table (after any common `preconditions` have
#' been applied). This validation step function (internally composed of multiple
#' steps) ultimately performs a rowwise test of whether all sub-validations
#' reported a *pass* for the same test units. In practice, an example of a
#' joint validation is testing whether values for column `a` are greater than
#' a specific value while values for column `b` lie within a specified range.
#' The validation step functions to be part of the conjoint validation are to be
#' supplied as one-sided **R** formulas (using a leading `~`, and having a `.`
#' stand in as the data object). This function can be used directly on a data
#' table or with an *agent* object (technically, a `ptblank_agent` object).
#' 
#' If providing multiple column names in any of the supplied validation step
#' functions, the result will be an expansion of sub-validation steps to that
#' number of column names. Aside from column names in quotes and in `vars()`,
#' **tidyselect** helper functions are available for specifying columns. They
#' are: `starts_with()`, `ends_with()`, `contains()`, `matches()`, and
#' `everything()`.
#' 
#' Having table `preconditions` means **pointblank** will mutate the table just
#' before interrogation. It's isolated to the validation steps produced by this
#' validation step function. Using **dplyr** code is suggested here since the
#' statements can be translated to SQL if necessary. The code is to be supplied
#' as a one-sided **R** formula (using a leading `~`). In the formula
#' representation, the obligatory `tbl` variable will serve as the input
#' data table to be transformed (e.g.,
#' `~ tbl %>% dplyr::mutate(col_a = col_b + 10)`. A series of expressions can be
#' used by enclosing the set of statements with `{ }` but note that the `tbl`
#' variable must be ultimately returned.
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation step function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction test
#' units failed, or, an absolute value), often using the `warn_at` argument.
#' This is especially true when `x` is a table object because, otherwise,
#' nothing happens. For the `col_vals_*()`-type functions, using 
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
#' @examples
#' # Create a simple table with three
#' # columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(3, 4, 6, 8, 9, 11),
#'     c = c(2, 6, 8, NA, 3, 8)
#'   )
#'
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   conjointly(
#'     ~ col_vals_gt(., vars(a), 6),
#'     ~ col_vals_lt(., vars(b), 10),
#'     ~ col_vals_not_null(., vars(c))
#'     ) %>%
#'   interrogate()
#'
#' @family Validation Step Functions
#' @section Function ID:
#' 2-14
#'
#' @import rlang
#' @export
conjointly <- function(x,
                       ...,
                       .list = list2(...),
                       preconditions = NULL,
                       actions = NULL,
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
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      conjointly(
        .list = .list,
        preconditions = preconditions,
        actions = prime_actions(actions),
        brief = brief,
        active = active
      ) %>% interrogate()
    
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

  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "conjointly",
      column = NULL,
      values = validation_formulas,
      na_pass = NULL,
      preconditions = preconditions,
      actions = actions,
      brief = brief,
      active = active
    )
  
  agent
}  
  