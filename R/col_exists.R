#' Do one or more columns actually exist?
#'
#' The `col_exists()` validation step function checks whether one or more
#' columns exist in the target table. The only requirement is a specification of
#' the column names. Each validation step will operate over a single test unit,
#' which is whether the column exists or not.
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
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
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
#' 
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
