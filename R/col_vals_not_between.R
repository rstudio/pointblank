#' Are numerical column data not between two specified values?
#'
#' Set a verification step where column data should not be between two values.
#' @inheritParams col_vals_gt
#' @param left the lower bound for the range. The validation includes this bound
#'   value in addition to values greater than \code{left}. Any values \code{>=
#'   left} and \code{<= right} will be considered as failing.
#' @param right the upper bound for the range. The validation includes this
#'   bound value in addition to values lower than \code{right}. Any values
#'   \code{<= right} and \code{>= left} will be considered as failing.
#' @return an agent object.
#' @examples
#' # Create a simple data frame
#' # with a column a numerical values
#' df <-
#'   data.frame(
#'     a = c(5.6, 8.2, 6.3, 7.8, 3.4))
#' 
#' # Validate that none of the values 
#' # in column `a` are between 9 and 10,
#' # or, between 0 and 2
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_not_between(
#'     column = a,
#'     left = 9,
#'     right = 10) %>%
#'   col_vals_not_between(
#'     column = a,
#'     left = 0,
#'     right = 2) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed by
#' # using `all_passed()`
#' all_passed(agent)
#' @importFrom dplyr bind_rows tibble
#' @importFrom rlang enquo expr_text
#' @importFrom stringr str_replace_all
#' @export
col_vals_not_between <- function(...,
                                 column,
                                 left,
                                 right,
                                 incl_na = TRUE,
                                 incl_nan = TRUE,
                                 preconditions = NULL,
                                 brief = NULL,
                                 warn_count = NULL,
                                 notify_count = NULL,
                                 warn_fraction = NULL,
                                 notify_fraction = NULL,
                                 tbl_name = NULL,
                                 db_type = NULL,
                                 creds_file = NULL,
                                 initial_sql = NULL,
                                 file_path = NULL,
                                 col_types = NULL) {
  
  # Collect the object provided
  object <- list(...)
  
  # Get the column name
  column <- 
    rlang::enquo(column) %>%
    rlang::expr_text() %>%
    stringr::str_replace_all("~", "") %>%
    stringr::str_replace_all("\"", "'")
  
  if (inherits(object[[1]] , c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      object[[1]] %>%
        evaluate_single(
          type = "col_vals_not_between",
          column = column,
          left = left,
          right = right,
          incl_na = incl_na,
          incl_nan = incl_nan,
          warn_count = warn_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          notify_fraction = notify_fraction)
    )
  }
  
  agent <- object[[1]]
  
  # Get the preconditions
  preconditions <- 
    rlang::enquo(preconditions) %>%
    rlang::expr_text() %>%
    stringr::str_replace_all("~", "") %>%
    stringr::str_replace_all("\"", "'")
  
  if (length(preconditions) == 0) {
    preconditions <- NULL
  }
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_vals_not_between",
        column = column,
        left = left,
        right = right)
  }
  
  # If "*" is provided for `column`, select all
  # table columns for this verification
  if (column[1] == "all_cols()") {
    column <- get_all_cols(agent = agent)
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_vals_not_between",
      column = column,
      set = c(left, right),
      incl_na = incl_na,
      incl_nan = incl_nan,
      preconditions = preconditions,
      brief = brief,
      warn_count = warn_count,
      notify_count = notify_count,
      warn_fraction = warn_fraction,
      notify_fraction = notify_fraction,
      tbl_name = ifelse(is.null(tbl_name), as.character(NA), tbl_name),
      db_type = ifelse(is.null(db_type), as.character(NA), db_type),
      creds_file = ifelse(is.null(creds_file), as.character(NA), creds_file),
      init_sql = ifelse(is.null(initial_sql), as.character(NA), initial_sql),
      file_path = ifelse(is.null(file_path), as.character(NA), file_path),
      col_types = ifelse(is.null(col_types), as.character(NA), col_types))
  
  # If no `brief` provided, set as NA
  if (is.null(brief)) {
    brief <- as.character(NA)
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    dplyr::bind_rows(
      agent$logical_plan,
      dplyr::tibble(
        component_name = "col_vals_not_between",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
