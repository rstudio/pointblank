#' Are numerical column data greater than a specific value?
#'
#' Verification step where numeric values in a table column should be greater
#' than a specified value.
#'
#' @param x A data frame, tibble, or an agent object of class `ptblank_agent`.
#' @param columns The column (or a set of columns, provided as a character
#'   vector) to which this validation should be applied.
#' @param value A numeric value used for this test. Any column values `>value`
#'   are considered passing.
#' @param na_pass Should any encountered `NA` values be allowed to pass a
#'   validation unit? This is by default `FALSE`. Set to `TRUE` to give `NA`s
#'   a pass.
#' @param preconditions expressions used for mutating the input table before
#'   proceeding with the validation. This is ideally as a one-sided R formula
#'   using a leading `~`. In the formula representation, the `tbl` serves as the
#'   input data table to be transformed (e.g.,
#'   `~ tbl %>% dplyr::mutate(col = col + 10)`. A series of expressions can be
#'   used by enclosing the set of statements with `{ }` but note that the `tbl`
#'   object must be ultimately returned.
#' @param brief An optional, text-based description for the validation step.
#' @param actions A list containing threshold levels so that the validation step
#'   can react accordingly when exceeding the set levels. This is to be created
#'   with the [action_levels()] helper function.
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
#'   col_vals_gt(columns = vars(a), value = 4) %>%
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
                        columns,
                        value,
                        na_pass = FALSE,
                        preconditions = NULL,
                        actions = NULL,
                        brief = NULL) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions)

  if (is_a_table_object(x)) {

    secret_agent <- create_agent(x) %>%
      col_vals_gt(
        columns = columns,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        brief = brief,
        actions = prime_actions(actions)
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_gt",
        preconditions = preconditions,
        column = columns,
        value = value
      )
  }
  
  # Add one or more validation steps based on the
  # length of the `columns` variable
  for (column in columns) {
    
    agent <-
      create_validation_step(
        agent = agent,
        assertion_type = "col_vals_gt",
        column = column,
        value = value,
        na_pass = na_pass,
        preconditions = preconditions,
        actions = actions,
        brief = brief
      )
  }

  agent
}
