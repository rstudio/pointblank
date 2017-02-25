#' Create a properly formatted validation step
#' @description Get a validation step as a
#' tbl row.
#' @return a validation step as a tbl object.
#' @importFrom tibble tibble
#' @export create_validation_step

create_validation_step <- function(agent,
                                   assertion_type,
                                   column,
                                   value,
                                   report_count,
                                   warn_count,
                                   notify_count,
                                   tbl_name = as.character(NA),
                                   db_type = as.character(NA),
                                   credentials_file = as.character(NA)) {
  
  validation_step <-
    tibble::tibble(
      tbl_name = as.character(agent$focal_tbl_name),
      db_type = as.character(agent$focal_db_type),
      db_cred_file_path = as.character(agent$focal_db_cred_file_path),
      assertion_type = assertion_type,
      column = as.character(column),
      value = as.numeric(value),
      passed = as.logical(NA),
      report_count = as.numeric(report_count),
      warn_count = as.numeric(warn_count),
      notify_count = as.numeric(notify_count))
  
  # If just `tbl_name` provided, assume it is
  # a local data frame
  if (!is.na(tbl_name)) {
    validation_step$tbl_name <- tbl_name
  }
  
  if (!is.na(db_type)) {
    validation_step$db_type <- db_type
  }
  
  if (!is.na(credentials_file)) {
    validation_step$db_cred_file_path <- credentials_file
  }
  
  return(validation_step)
}