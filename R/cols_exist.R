#' Do one or more columns actually exist?
#'
#' Verification step that checks whether one or several specified columns
#' exist in the target table.
#' 
#' @inheritParams col_vals_gt
#' @param cols one or more columns from the table in focus. This can be provided
#'   as a vector of column names using \code{c()} or bare column names enclosed
#'   in \code{\link{vars}()}.
#'   
#' @examples
#' # Validate that columns `a`, `c`, and
#' # `f` exist in the `small_table` CSV file;
#' # do this by creating an agent, focussing
#' # on that table, creating a `cols_exist()`
#' # step, and then interrogating the table
#' agent <-
#'   create_agent() %>%
#'   focus_on(
#'     file_name = 
#'       system.file(
#'         "extdata", "small_table.csv",
#'         package = "pointblank"),
#'     col_types = "TDicidlc") %>%
#'   cols_exist(cols = vars(a, c, f)) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' @return an agent object.
#' @import rlang
#' @export
cols_exist <- function(x,
                       cols,
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
  
  # Get the column names
  if (inherits(cols, "quosures")) {
    
    cols <- 
      cols %>% as.character() %>%
      gsub("~", "", .)
  }
  
  if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
    
    return(
      x %>%
        evaluate_single(
          type = "cols_exist",
          column = cols,
          value = value,
          warn_count = warn_count,
          notify_count = notify_count,
          warn_fraction = warn_fraction,
          notify_fraction = notify_fraction
        )
    )
  }
  
  agent <- x
  
  preconditions <- NULL
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "cols_exist",
        column = cols)
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "cols_exist",
      column = cols,
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
      dplyr::tibble(
        component_name = "cols_exist",
        parameters = as.character(NA),
        brief = brief))
  
  agent
}
