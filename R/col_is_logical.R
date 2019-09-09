#' Do the columns contain logical values?
#'
#' Verification step where a table column is expected to consist of
#' \code{logical} values.
#' 
#' @inheritParams col_vals_gt
#' @param column the name of a single table column, multiple columns in the same
#'   table, or, a helper function such as \code{\link{all_cols}()}.
#'   
#' @examples
#' # Create a simple data frame
#' # with a column containing data
#' # classed as `logical`
#' df <-
#'   data.frame(
#'     a = c(TRUE, FALSE))
#' 
#' # Validate that column `a` in
#' # the data frame is classed as
#' # `logical`
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_is_logical(column = a) %>%
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
col_is_logical <- function(x,
                           column,
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
          type = "col_is_logical",
          column = column,
          value = value,
          warn_count = warn_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          notify_fraction = notify_fraction)
    )
  }
  
  agent <- x
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_is_logical",
        column = column)
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
      assertion_type = "col_is_logical",
      column = column,
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
        component_name = "col_is_logical",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
