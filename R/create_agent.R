#' Create a pointblank agent object
#'
#' Creates an agent object.
#'
#' @param tbl The input table that will be the focus of the validation. This can
#'   be a data frame, a tibble, or a `tbl_dbi` object.
#' @param name An optional name for the validation pipeline that the agent will
#'   eventually carry out during the interrogation process. If no value is
#'   provided, a name will be generated based on the current system time.
#'   
#' @return A `ptblank_agent` object.
#'   
#' @examples 
#' # Create a simple data frame
#' # with a column of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7)
#'   )
#' 
#' # Create a pointblank `agent` object
#' agent <- create_agent(tbl = df)
#'
#' # Then, as with any `ptblank_agent`
#' # object, we can focus on a table,
#' # add validation steps, and then
#' # eventually use `interrogate()`
#' # to perform the validations;
#' # here, in a single validation
#' # step, we expect that values in
#' # column `a` are always greater
#' # than 4
#' agent <-
#'   agent %>%
#'   col_vals_gt(columns = vars(a), value = 4) %>%
#'   interrogate()
#'  
#' # A summary can be produced using
#' # `get_interrogation_summary()`; we
#' # we will just obtain the first
#' # 7 columns of its output
#' (agent %>%
#'   get_interrogation_summary())[, 1:7]
#'   
#' @export
create_agent <- function(tbl,
                         name = NULL) {

  # Generate an agent name if none provided
  if (is.null(name)) {
    name <- paste0("agent_", gsub(" ", "_", Sys.time() %>% as.character()))
    brief <- "Create agent with auto-assigned validation name"
  } else {
    brief <- "Create agent with an assigned validation name"
  }

  tbl_name <- deparse(match.call()$tbl)
  
  if (tbl_name == ".") {
    tbl_name <- "table"
  }

  suppressWarnings(
    column_names_types <-
      tbl %>%
      dplyr::filter(dplyr::row_number() == 1L) %>%
      dplyr::collect() %>%
      vapply(
        FUN.VALUE = character(1),
        FUN = function(x) class(x)[1]
      )
  )
  
  column_names <- names(column_names_types)
  column_types <- unname(unlist(column_names_types))

  # Create the agent list object
  agent <-
    list(
      name = name,
      time = as.POSIXct(NA)[-1],
      tbl = tbl,
      tbl_name = tbl_name,
      tbl_src = character(0),
      col_names = column_names,
      col_types = column_types,
      validation_set =
        dplyr::tibble(
          i = integer(0),
          assertion_type = character(0),
          column = list(NULL),
          value = numeric(0),
          set = list(NULL),
          regex = character(0),
          incl_na = logical(0),
          preconditions = list(NULL),
          brief = character(0),
          all_passed = logical(0),
          n = integer(0),
          n_passed = integer(0),
          n_failed = integer(0),
          f_passed = numeric(0),
          f_failed = numeric(0),
          warn_count = numeric(0),
          stop_count = numeric(0),
          notify_count = numeric(0),
          warn_fraction = numeric(0),
          stop_fraction = numeric(0),
          notify_fraction = numeric(0),
          warn = logical(0),
          notify = logical(0),
          row_sample = numeric(0),
          time_processed = as.POSIXct(NA)[-1],
          proc_duration_s = numeric(0)
        ),
      extracts = list()
    )
  
  # Assign the class attribute value `ptblank_agent` to
  # the `agent object`
  attr(agent, "class") <- "ptblank_agent"
  
  agent
}
