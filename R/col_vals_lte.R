#' Verify whether numerical column data are
#' less than or equal to a specified value
#' @description Set a verification step where
#' numeric values in a table column should be
#' less than or equal to a specified value.
#' @return an agent object.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export col_vals_lte

col_vals_lte <- function(agent,
                         column,
                         value,
                         report_count = 0,
                         warn_count = 1,
                         notify_count = 2,
                         tbl_name = NULL,
                         db_type = NULL,
                         creds_file = NULL) {
  
  assertion_type <- "col_vals_lte"
  
  validation_step <-
    create_validation_step(
      agent = agent,
      assertion_type = assertion_type,
      column = column,
      value = value,
      report_count = report_count,
      warn_count = warn_count,
      notify_count = notify_count,
      tbl_name = ifelse(is.null(tbl_name), as.character(NA), tbl_name),
      db_type = ifelse(is.null(db_type), as.character(NA), db_type),
      creds_file = ifelse(is.null(creds_file), as.character(NA), creds_file))
  
  # Append `validation_component` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(
      agent$validation_set,
      validation_step)
  
  return(agent)
}
