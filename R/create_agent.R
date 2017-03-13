#' Create an agent object
#' @description Creates an agent object.
#' @return an agent object.
#' @importFrom dplyr filter
#' @importFrom tibble tibble as_tibble
#' @export create_agent

create_agent <- function() {
  
  agent <-
    list(
      focal_tbl_name = as.character(NA)[-1],
      focal_file_name = as.character(NA)[-1],
      focal_db_type = as.character(NA)[-1],
      focal_col_names = as.character(NA)[-1],
      focal_db_cred_file_path = as.character(NA)[-1],
      focal_init_sql = as.character(NA)[-1],
      validation_set =
        tibble::tibble(
          tbl_name = as.character(NA),
          db_type = as.character(NA),
          assertion_type = as.character(NA),
          column = as.character(NA),
          value = as.numeric(NA),
          set = as.numeric(NA),
          regex = as.character(NA),
          preconditions = as.character(NA),
          all_passed = as.logical(NA),
          n = as.integer(NA),
          n_passed = as.integer(NA),
          n_failed = as.integer(NA),
          f_passed = as.numeric(NA),
          f_failed = as.numeric(NA),
          report_count = as.numeric(NA),
          warn_count = as.numeric(NA),
          notify_count = as.numeric(NA),
          report = as.logical(NA),
          warn = as.logical(NA),
          notify = as.logical(NA),
          row_sample = as.numeric(NA),
          init_sql = as.character(NA),
          db_cred_file_path = as.character(NA),
          file_path = as.character(NA))
    )
  
  agent$validation_set$set <- tibble::as_tibble(NA)
  
  agent$validation_set$preconditions <- tibble::as_tibble(NA)
  
  agent$validation_set <-
    agent$validation_set %>%
    dplyr::filter(n == 1)
  
  # Assign the class attribute value `ptblank_agent` to
  # the `agent object`
  attr(agent, "class") <- "ptblank_agent"
  
  return(agent)
}
