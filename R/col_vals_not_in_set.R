#' Verify whether column data are not part of
#' a set of values
#' @description Set a verification step where
#' numeric values in a table column should be
#' part of a set of values.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param report_count the threshold number for 
#' individual validations returning a \code{FALSE}
#' result before applying the \code{report} flag.
#' @param warn_count the threshold number for 
#' individual validations returning a \code{FALSE}
#' result before applying the \code{warn} flag.
#' @param notify_count the threshold number for 
#' individual validations returning a \code{FALSE}
#' result before applying the \code{notify} flag.
#' @param tbl_name the name of the local or remote
#' table.
#' @param db_type if the table is located in a
#' database, the type of database is required here.
#' Currently, this can be either \code{PostgreSQL}
#' or \code{MySQL}.
#' @param creds_file if a connection to a database
#' is required for reaching the table specified in
#' \code{tbl_name}, then a path to a credentials file
#' can be used to establish that connection. The
#' credentials file is an \code{RDS} containing a
#' character vector with the following items in the
#' specified order: (1) database name (\code{dbname}),
#' (2) the \code{host} name, (3) the \code{port},
#' (4) the username (\code{user}), and (5) the
#' \code{password}. This file can be easily created
#' using the \code{create_creds_file()} function.
#' @param initial_sql when accessing a remote table,
#' this provides an option to provide an initial
#' query component before conducting validations. 
#' An entire SQL statement can be provided here, or,
#' as a shortcut, the initial \code{SELECT...}
#' statement can be omitted for simple queries (e.g.,
#' \code{WHERE a > 1 AND b = 'one'}).
#' @param preconditions a optional vector of filtering
#' statements for filtering the table before this
#' validation step.
#' @return an agent object.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export col_vals_not_in_set

col_vals_not_in_set <- function(agent,
                                column,
                                set,
                                report_count = 0,
                                warn_count = 1,
                                notify_count = 2,
                                tbl_name = NULL,
                                db_type = NULL,
                                creds_file = NULL,
                                initial_sql = NULL,
                                preconditions = NULL) {
  
  assertion_type <- "col_vals_not_in_set"
  
  # If "*" is provided for `column`, select all
  # table columns for this verification
  if (column[1] == "*") {
    column <- get_all_cols(agent = agent)
  }
  
  validation_step <-
    create_validation_step(
      agent = agent,
      assertion_type = assertion_type,
      column = column,
      set = set,
      report_count = report_count,
      warn_count = warn_count,
      notify_count = notify_count,
      preconditions = preconditions,
      tbl_name = ifelse(is.null(tbl_name), as.character(NA), tbl_name),
      db_type = ifelse(is.null(db_type), as.character(NA), db_type),
      creds_file = ifelse(is.null(creds_file), as.character(NA), creds_file),
      init_sql = ifelse(is.null(initial_sql), as.character(NA), initial_sql))
  
  # Append `validation_component` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(
      agent$validation_set,
      validation_step)
  
  return(agent)
}
