#' Verify that row data are not duplicated
#'
#' Verification step where row data should contain no duplicates.
#'
#' @inheritParams col_vals_gt
#' @param x An agent object of class `ptblank_agent`.
#' @param cols An optional grouping of columns to check for duplication. If not
#'   provided, the validation checks for duplicate records using data across all
#'   columns.
#'   
#' @examples
#' # Create a simple data frame
#' # with three columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 8, 3),
#'     c = c(1, 1, 1, 3, 3, 3)
#'   )
#' 
#' # Validate that when considering only
#' # data in columns `a` and `b`, there
#' # are no duplicate rows (i.e., all
#' # rows are distinct)
#' agent <-
#'   create_agent(tbl = df) %>%
#'   rows_not_duplicated(
#'     cols = a & b
#'   ) %>%
#'   interrogate()
#' 
#' # Determine if these column
#' # validations have all passed
#' # by using `all_passed()`
#' all_passed(agent)
#' 
#' @return A \pkg{pointblank} agent object.
#' @import rlang
#' @export
rows_not_duplicated <- function(x,
                                cols = NULL,
                                preconditions = NULL,
                                brief = NULL,
                                warn_count = NULL,
                                stop_count = NULL,
                                notify_count = NULL,
                                warn_fraction = NULL,
                                stop_fraction = NULL,
                                notify_fraction = NULL) {

  agent <- x
  
  # Get the values supplied for `cols`
  cols <- 
    rlang::enquo(cols) %>%
    rlang::get_expr() %>%
    as.character()
  
  if (length(cols) > 0) {
    
    cols <- 
      cols %>%
      base::setdiff("&") %>%
      paste(collapse = ", ")
    
  } else {
    
    cols <- NULL
  }
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "rows_not_duplicated",
        column = cols
      )
  }
  
  # Add one or more validation steps
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "rows_not_duplicated",
      column = ifelse(is.null(cols), as.character(NA), cols),
      value = NULL,
      preconditions = preconditions,
      brief = brief,
      warn_count = warn_count,
      stop_count = stop_count,
      notify_count = notify_count,
      warn_fraction = warn_fraction,
      stop_fraction = stop_fraction,
      notify_fraction = notify_fraction
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
        component_name = "rows_not_duplicated",
        parameters = as.character(NA),
        brief = brief
      )
    )
  
  agent
}
