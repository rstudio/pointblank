#' Verify whether numerical column data are
#' greater than a specified value
#' @description Set a verification step where
#' numeric values in a table column should be
#' greater than a specified value.
#' @return an agent object.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export verify_col_gt

verify_col_gt <- function(agent,
                          column,
                          value,
                          report_count = 0,
                          warn_count = 1,
                          notify_count = 2,
                          tbl_name = NULL,
                          db_type = NULL,
                          credentials_file = NULL) {
  
  validation_component <-
    tibble::tibble(
      tbl_name = as.character(agent$focal_tbl_name),
      db_type = as.character(agent$focal_db_type),
      db_cred_file_path = as.character(agent$focal_db_cred_file_path),
      assertion_type = "verify_col_gt",
      column = as.character(column),
      value = as.numeric(value),
      passed = as.logical(NA),
      report_count = as.numeric(report_count),
      warn_count = as.numeric(warn_count),
      notify_count = as.numeric(notify_count))
  
  # If just `tbl_name` provided, assume it is
  # a local data frame
  if (!is.null(tbl_name)) {
    validation_component$tbl_name <- tbl_name
  }
  
  if (!is.null(db_type)) {
    validation_component$db_type <- db_type
  }
  
  if (!is.null(credentials_file)) {
    validation_component$db_cred_file_path <- credentials_file
  }
  
  # Append `validation_component` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(
      agent$validation_set,
      validation_component)
  
  return(agent)
}
