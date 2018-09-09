#' Verify that one or more columns exist
#'
#' Set a verification step that checks whether one or several specified columns
#' exist in the target table.
#' @inheritParams col_vals_gt
#' @param column the name of a single table column or multiple columns in the
#'   same table.
#' @return an agent object.
#' @examples
#' # Validate that column `a` exists in
#' # the `small_table` CSV file; do this
#' # by creating an agent, focussing on
#' # that table, creating a `col_exists()`
#' # step, and then interrogating the table
#' agent <-
#'   create_agent() %>%
#'   focus_on(
#'     file_name = 
#'       system.file(
#'         "extdata", "small_table.csv",
#'         package = "pointblank"),
#'     col_types = "TDicidlc") %>%
#'   col_exists(column = a) %>%
#'   interrogate()
#' 
#' # Determine if this column validation
#' # passed by using `all_passed()`
#' all_passed(agent)
#' #> [1] TRUE
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang enquo get_expr
#' @importFrom stringr str_replace_all
#' @export
col_exists <- function(...,
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
          type = "col_exists",
          column = column,
          value = value,
          warn_count = warn_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          notify_fraction = notify_fraction)
    )
  }
  
  agent <- object[[1]]
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_exists",
        column = column)
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_exists",
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
      tibble::tibble(
        component_name = "col_exists",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
