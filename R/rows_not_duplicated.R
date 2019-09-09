#' Verify that row data are not duplicated
#'
#' Verification step where row data should contain no duplicates.
#' 
#' @inheritParams col_vals_gt
#' @param x an agent object of class \code{ptblank_agent}.
#' @param cols an optional grouping of columns to check for duplication. If not
#'   provided, the validation checks for duplicate records using data across all
#'   columns.
#'   
#' @examples
#' # Validate that column `a` exists in
#' # the `small_table` CSV file; do this
#' # by creating an agent, focussing on
#' # that table, creating a
#' # `rows_not_duplicated()` step, and then
#' # interrogating the table
#' agent <-
#'   create_agent() %>%
#'   focus_on(
#'     file_name = 
#'       system.file(
#'         "extdata", "small_table.csv",
#'         package = "pointblank"),
#'     col_types = "TDicidlc") %>%
#'   rows_not_duplicated(
#'     cols = a & b) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @return an agent object.
#' @import rlang
#' @export
rows_not_duplicated <- function(x,
                                cols = NULL,
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
  
  agent <- x
  
  # Get the values supplied for `cols`
  cols <- 
    rlang::enquo(cols) %>%
    rlang::get_expr() %>%
    as.character()
  
  if (length(cols) > 0) {
    
    cols <- 
      cols %>%
      base::setdiff("&") %>%
      paste(collapse = ", ")
    
  } else {
    
    cols <- NULL
  }
  
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
        assertion_type = "rows_not_duplicated",
        column = cols)
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "rows_not_duplicated",
      column = ifelse(is.null(cols), as.character(NA), cols),
      value = NULL,
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
      col_types = ifelse(is.null(col_types), as.character(NA), col_types)
    )
  
  # If no `brief` provided, set as NA
  if (is.null(brief)) {
    brief <- as.character(NA)
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    dplyr::bind_rows(
      agent$logical_plan,
      dplyr::tibble(
        component_name = "rows_not_duplicated",
        parameters = as.character(NA),
        brief = brief
      )
    )
  
  agent
}
