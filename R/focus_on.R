#' Place certain access details more to the fore 
#' @description Allows for a change of the 
#' focus on the table name, database type, and
#' the location of the credentials file. 
#' @return an agent object.
#' @export focus_on

focus_on <- function(agent,
                     tbl_name,
                     db_type = NULL,
                     creds_file = NULL,
                     initial_sql = NULL) {
  
  agent$focal_tbl_name <- tbl_name
  
  if (is.null(db_type)) {
    agent$focal_db_type <- "local"
  } else if (!is.null(db_type)) {
    agent$focal_db_type <- db_type
  }
  
  if (is.null(creds_file)) {
    agent$focal_db_cred_file_path <- as.character(NA)
  } else if (!is.null(creds_file)) {
    agent$focal_db_cred_file_path <- creds_file
  }
  
  if (is.null(initial_sql)) {
    agent$focal_init_sql <- as.character(NA)
  } else if (!is.null(initial_sql)) {
    agent$focal_init_sql <- initial_sql
  }
  
  return(agent)
}