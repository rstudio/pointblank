#' Are numerical column data greater than a specific value?
#'
#' Verification step where numeric values in a table column should be greater
#' than a specified value.
#'
#' @param x A data frame, tibble, or an agent object of class `ptblank_agent`.
#' @param column The column (or a set of columns, provided as a character
#'   vector) to which this validation should be applied. Aside from a single
#'   column name, column operations can be used to create one or more computed
#'   columns (e.g., `a + b` or `a + sum(a)`).
#' @param value A numeric value used for this test. Any column values `>value`
#'   are considered passing.
#' @param incl_na Should `NA` values be a part of the condition? This is by
#'   default `FALSE`.
#' @param preconditions An optional statement of filtering conditions that may
#'   reduce the number of rows for validation for the current validation step.
#'   The statements are executed for every row of the table in focus and are
#'   often referred as predicate statements (they either return `TRUE` or
#'   `FALSE` for every row evaluated, where rows evaluated as `TRUE` are the
#'   rows that are retained for the validation step). For example, if a table
#'   has columns `a`, `b`, and `c`, and, column `a` has numerical data, we can
#'   write a statement `a < 5` that filters all rows in the table where values
#'   in column a are less than five.
#' @param brief An optional, text-based description for the validation step.
#' @param warn_count,notify_count The threshold number for individual
#'   validations returning a `FALSE` result before applying the `warn` or
#'   `notify` flag.
#' @param warn_fraction,notify_fraction The threshold fraction for individual
#'   validations returning a `FALSE` over all the entire set of individual
#'   validations. Beyond this threshold, either the `warn` or `notify` flag will
#'   be applied.
#' @param stop_count,stop_fraction The threshold number or fraction of `FALSE`
#'   validation results before stopping a simple validation or stopping an
#'   agent-based validation.
#' @param tbl_name The name of the local or remote table.
#' @param db_type If the table is located in a database, the type of database is
#'   required here. Currently, this can be either `PostgreSQL` or `MySQL`.
#' @param creds_file If a connection to a database is required for reaching the
#'   table specified in `tbl_name`, then a path to a credentials file can be
#'   used to establish that connection. The credentials file is an `RDS`
#'   containing a character vector with the following items in the specified
#'   order: (1) database name (`dbname`), (2) the `host` name, (3) the `port`,
#'   (4) the username (`user`), and (5) the `password`. This file can be easily
#'   created using the [create_creds_file()] function.
#' @param initial_sql When accessing a remote table, this provides an option to
#'   provide an initial query component before conducting validations. An entire
#'   SQL statement can be provided here, or, as a shortcut, the initial
#'   `SELECT...` statement can be omitted for simple queries (e.g., `WHERE a > 1
#'   AND b = 'one'`).
#' @param file_path An optional path for a tabular data file to be loaded for
#'   this verification step. Valid types are CSV and TSV files.
#' @param col_types If validating a CSV or TSV file, an optional column
#'   specification can be provided here as a string. This string representation
#'   is where each character represents one column and the mappings are: `c` ->
#'   character, `i` -> integer, `n` -> number, `d` -> double, `l` -> logical,
#'   `D` -> date, `T` -> date time, `t` -> time, `?` -> guess, or `_/-`, which
#'   skips the column.
#'   
#' @examples
#' # Create a simple data frame
#' # with a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7))
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_gt(
#'     column = a,
#'     value = 4) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @return Either a \pkg{pointblank} agent object or a table object, depending
#'   on what was passed to `x`.
#' @import rlang
#' @export
col_vals_gt <- function(x,
                        column,
                        value,
                        incl_na = FALSE,
                        preconditions = NULL,
                        brief = NULL,
                        warn_count = NULL,
                        stop_count = NULL,
                        notify_count = NULL,
                        warn_fraction = NULL,
                        stop_fraction = NULL,
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
  
  if (inherits(x , c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    preconditions <- rlang::enquo(preconditions)
    
    return(
      x %>%
        evaluate_single(
          type = "col_vals_gt",
          column = column,
          value = value,
          incl_na = incl_na,
          preconditions = preconditions,
          warn_count = warn_count,
          stop_count = stop_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          stop_fraction = stop_fraction,
          notify_fraction = notify_fraction
        )
    )
  }
  
  agent <- x
  
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
        assertion_type = "col_vals_gt",
        preconditions = preconditions,
        column = column,
        value = value
      )
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
      assertion_type = "col_vals_gt",
      column = column,
      value = value,
      incl_na = incl_na,
      preconditions = preconditions,
      brief = brief,
      warn_count = warn_count,
      stop_count = stop_count,
      notify_count = notify_count,
      warn_fraction = warn_fraction,
      stop_fraction = stop_fraction,
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
        component_name = "col_vals_gt",
        parameters = as.character(NA),
        brief = brief
      )
    )
  
  agent
}
