#' Are numerical column data between two specified values?
#'
#' Verification step where column data should be between two values.
#' 
#' @inheritParams col_vals_gt
#' @param left the lower bound for the range. The validation includes this bound
#'   value in addition to values greater than \code{left}.
#' @param right the upper bound for the range. The validation includes this
#'   bound value in addition to values lower than \code{right}.
#'   
#' @examples
#' # Create a simple data frame
#' # with a column of numerical
#' # values
#' df <-
#'   data.frame(
#'     a = c(5.6, 8.2, 6.3, 7.8, 3.4))
#' 
#' # Validate that values in
#' # column `a` are all between
#' # 1 and 9
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_between(
#'     column = a,
#'     left = 1,
#'     right = 9) %>%
#'   interrogate()
#' 
#' # Determine if this column
#' # validation has passed by using
#' # `all_passed()`
#' all_passed(agent)
#' 
#' @return an agent object.
#' @import rlang
#' @export
col_vals_between <- function(x,
                             column,
                             left,
                             right,
                             incl_na = FALSE,
                             incl_nan = FALSE,
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

  # Get the column name
  column <- 
    rlang::enquo(column) %>%
    rlang::expr_text() %>%
    stringr::str_replace_all("~", "") %>%
    stringr::str_replace_all("\"", "'")
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "col_vals_between",
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
  
  agent <- x
  
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
        assertion_type = "col_vals_between",
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
      assertion_type = "col_vals_between",
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
        component_name = "col_vals_between",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
