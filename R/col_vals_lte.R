#' Verify whether numerical column data are
#' less than or equal to a specified value
#' @description Set a verification step where
#' numeric values in a table column should be
#' less than or equal to a specified value.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param column the column (or a set of columns,
#' provided as a character vector) to which this
#' validation should be applied. Aside from a single
#' column name, column operations can be used to
#' create one or more computed columns (e.g., 
#' \code{"a + b"} or \code{"a + sum(a)"}).
#' @param value a numeric value used for this test.
#' Any column values \code{<= value} are considered
#' passing.
#' @param warn_count the threshold number for 
#' individual validations returning a \code{FALSE}
#' result before applying the \code{warn} flag.
#' @param notify_count the threshold number for 
#' individual validations returning a \code{FALSE}
#' result before applying the \code{notify} flag.
#' @param warn_fraction the threshold fraction for 
#' individual validations returning a \code{FALSE}
#' over all the entire set of individual validations.
#' Beyond this threshold, the \code{warn} flag will
#' be applied.
#' @param notify_fraction the threshold fraction for 
#' individual validations returning a \code{FALSE}
#' over all the entire set of individual validations.
#' Beyond this threshold, the \code{notify} flag will
#' be applied.
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
#' @param file_path an optional path for a tabular data
#' file to be loaded for this verification step. Valid
#' types are CSV and TSV files.
#' @param col_types if validating a CSV or TSV file,
#' an optional column specification can be provided
#' here as a string. This string representation is
#' where each character represents one column and the
#' mappings are: \code{c} -> character, \code{i} ->
#' integer, \code{n} -> number, \code{d} -> double, 
#' \code{l} -> logical, \code{D} -> date, \code{T} ->
#' date time, \code{t} -> time, \code{?} -> guess, 
#' or \code{_/-}, which skips the column.
#' @param preconditions an optional vector of filtering
#' statements for filtering the table before this
#' validation step.
#' @param description an optional, text-based
#' description for the validation step. Used primarily
#' in the Logical Plan section of the report generated
#' by the \code{html_summary} function.
#' @return an agent object.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export col_vals_lte

col_vals_lte <- function(agent,
                         column,
                         value,
                         warn_count = 1,
                         notify_count = NULL,
                         warn_fraction = NULL,
                         notify_fraction = NULL,
                         tbl_name = NULL,
                         db_type = NULL,
                         creds_file = NULL,
                         initial_sql = NULL,
                         file_path = NULL,
                         col_types = NULL,
                         preconditions = NULL,
                         description = NULL) {
  
  assertion_type <- "col_vals_lte"
  
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
      value = value,
      warn_count = warn_count,
      notify_count = notify_count,
      warn_fraction = warn_fraction,
      notify_fraction = notify_fraction,
      preconditions = preconditions,
      tbl_name = ifelse(is.null(tbl_name), as.character(NA), tbl_name),
      db_type = ifelse(is.null(db_type), as.character(NA), db_type),
      creds_file = ifelse(is.null(creds_file), as.character(NA), creds_file),
      init_sql = ifelse(is.null(initial_sql), as.character(NA), initial_sql),
      file_path = ifelse(is.null(file_path), as.character(NA), file_path),
      col_types = ifelse(is.null(col_types), as.character(NA), col_types))
  
  # Append `validation_component` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(
      agent$validation_set,
      validation_step)
  
  # If no `description` provided, set as `NA`
  if (is.null(description)) {
    description <- as.character(NA)
  }
  
  # Place the validation step in the logical plan
  agent$logical_plan <-
    dplyr::bind_rows(
      agent$logical_plan,
      tibble::tibble(
        component_name = "col_vals_lte",
        parameters = as.character(NA),
        description = description))
  
  return(agent)
}
