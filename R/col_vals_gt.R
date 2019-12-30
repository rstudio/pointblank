#' Are numerical column data greater than a specific value?
#'
#' Verification step where numeric values in a table column should be greater
#' than a specified value.
#'
#' @param x A data frame, tibble, or an agent object of class `ptblank_agent`.
#' @param column The column (or a set of columns, provided as a character
#'   vector) to which this validation should be applied.
#' @param value A numeric value used for this test. Any column values `>value`
#'   are considered passing.
#' @param incl_na Should `NA` values be a part of the condition? This is by
#'   default `FALSE`.
#' @param preconditions expressions used for mutating the input table before
#'   proceeding with the validation. This is ideally as a one-sided R formula
#'   using a leading `~`. In the formula representation, the `tbl` serves as the
#'   input data table to be transformed (e.g.,
#'   `~ tbl %>% dplyr::mutate(col = col + 10)`. A series of expressions can be
#'   used by enclosing the set of statements with `{ }` but note that the `tbl`
#'   object must be ultimately returned.
#' @param brief An optional, text-based description for the validation step.
#' @param warn_count,notify_count The threshold number for individual
#'   validations returning a `FALSE` result before applying the `warn` or
#'   `notify` flag.
#' @param warn_fraction,notify_fraction The threshold fraction for individual
#'   validations returning a `FALSE` over all the entire set of individual
#'   validations. Beyond this threshold, either the `warn` or `notify` flag will
#'   be applied.
#' @param stop_count,stop_fraction The threshold number or fraction of `FALSE`
#'   validation results before stopping a simple validation or stopping an
#'   agent-based validation.
#'   
#' @return Either a `ptblank_agent` object or a table object, depending on what
#'   was passed to `x`.
#'   
#' @examples
#' # Create a simple data frame with
#' # a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7)
#'   )
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_gt(column = vars(a), value = 4) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @import rlang
#' @export
col_vals_gt <- function(x,
                        column,
                        value,
                        incl_na = FALSE,
                        preconditions = NULL,
                        brief = NULL,
                        warn_count = NULL,
                        stop_count = NULL,
                        notify_count = NULL,
                        warn_fraction = NULL,
                        stop_fraction = NULL,
                        notify_fraction = NULL) {
  
  # Capture the `column` expression
  column <- rlang::enquo(column)
  
  # Resolve the columns based on the expression
  column <- resolve_columns(x = x, var_expr = column, preconditions)

  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_vals_gt",
          column = column,
          value = value,
          incl_na = incl_na,
          preconditions = preconditions,
          warn_count = warn_count,
          stop_count = stop_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          stop_fraction = stop_fraction,
          notify_fraction = notify_fraction
        )
    )
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_gt",
        preconditions = preconditions,
        column = column,
        value = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `column` variable
  for (col in column) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_gt",
        column = col,
        value = value,
        incl_na = incl_na,
        preconditions = preconditions,
        brief = brief,
        warn_count = warn_count,
        stop_count = stop_count,
        notify_count = notify_count,
        warn_fraction = warn_fraction,
        stop_fraction = stop_fraction,
        notify_fraction = notify_fraction
      )
  }

  agent
}
