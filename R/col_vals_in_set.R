#' Verify whether column data are part of a set of values
#'
#' Set a verification step where numeric values in a table column should be part
#'   of a set of values.
#' @inheritParams col_vals_gt
#' @param set a vector of numeric or string-based elements, where column values
#'   found within this \code{set} will be considered as passing.
#' @return an agent object.
#' @examples
#' # Create a simple data frame with
#' # 2 columns: one with numerical
#' # values and the other with strings
#' df <-
#'   data.frame(
#'     a = c(1, 2, 3, 4),
#'     b = c("one", "two", "three", "four"),
#'     stringsAsFactors = FALSE)
#' 
#' # Validate that all numerical values
#' # in column `a` belong to a numerical
#' # set, and, create an analogous 
#' # validation check for column `b` with
#' # a set of string values 
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df") %>%
#'   col_vals_in_set(
#'     column = a,
#'     set = 1:4) %>%
#'   col_vals_in_set(
#'     column = b,
#'     set = c("one", "two",
#'             "three", "four")) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' #> [1] TRUE
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang enquo expr_text
#' @importFrom stringr str_replace_all
#' @export
col_vals_in_set <- function(...,
                            column,
                            set,
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
          type = "col_vals_in_set",
          column = column,
          set = set,
          warn_count = warn_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          notify_fraction = notify_fraction)
    )
  }
  
  agent <- object[[1]]
  
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
        assertion_type = "col_vals_in_set",
        column = column,
        set = set)
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
      assertion_type = "col_vals_in_set",
      column = column,
      set = set,
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
        component_name = "col_vals_in_set",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
