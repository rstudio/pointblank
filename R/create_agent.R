#' Create a pointblank agent object
#'
#' Creates an *agent* object, which is used in a *data quality reporting*
#' workflow. The overall aim of this workflow is to generate some reporting on
#' the state of data quality for a target table. We can supply as many
#' validation functions as the user wishes to write to get validation
#' coverage on that table. We supply a single table to `create_agent()`, which
#' becomes the sole focus of the data quality analysis. After application of
#' one or more validation functions, we need to use the [interrogate()]
#' function to complete the process; the validation functions, when called
#' on an agent, are merely instructions up to that point. The *agent* then has
#' information on how the interrogation went. Reporting of the interrogation can
#' be performed with the [get_agent_report()] function. If we just need to know
#' whether all validations completely passed (i.e., all steps had no failing
#' test units), the [all_passed()] function should be used.
#'
#' @param tbl The input table. This can be a data frame, a tibble, or a
#'   `tbl_dbi` object.
#' @param name An optional name for the validation plan that the agent will
#'   eventually carry out during the interrogation process. If no value is
#'   provided, a name will be generated based on the current system time.
#' @param actions A list containing threshold levels so that all validation
#'   steps can react accordingly when exceeding the set levels. This is to be
#'   created with the [action_levels()] helper function. Should an action levels
#'   list be used for specific validation step, any default set here will be
#'   overridden.
#' @param end_fns A list of functions that should be performed at the end of an
#'   interrogation.
#' @param embed_report An option to embed a **gt**-based validation report into
#'   the `ptblank_agent` object. If `FALSE` (the default) then the table object
#'   will be not generated and available with the agent upon returning from the
#'   interrogation.
#' @param reporting_lang The language to use for automatic creation of briefs
#'   (short descriptions for each validation step). By default, `NULL` will
#'   create English (`"en"`) text. Other options include French (`"fr"`),
#'   German (`"de"`), Italian (`"it"`), and Spanish (`"es"`).
#'   
#' @return A `ptblank_agent` object.
#'   
#' @examples
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- 
#'   dplyr::tibble(a = c(5, 7, 8, 7))
#' 
#' # Create a pointblank `agent` object
#' agent <- create_agent(tbl = tbl)
#'
#' # Then, as with any `ptblank_agent`
#' # object, we can add validation steps
#' # to the validation plan and then
#' # eventually use `interrogate()`
#' # to perform the validations; here,
#' # with a single validation step, we
#' # expect that values in column `a`
#' # are always greater than 4
#' agent <-
#'   agent %>%
#'   col_vals_gt(vars(a), 4) %>%
#'   interrogate()
#'  
#' @family Planning and Prep
#' @section Function ID:
#' 1-2
#'   
#' @export
create_agent <- function(tbl,
                         name = NULL,
                         actions = NULL,
                         end_fns = NULL,
                         embed_report = FALSE,
                         reporting_lang = NULL) {

  # Generate an agent name if none provided
  if (is.null(name)) {
    name <- paste0("agent_", gsub(" ", "_", Sys.time() %>% as.character()))
    brief <- "Create agent with auto-assigned validation name"
  } else {
    brief <- "Create agent with an assigned validation name"
  }
  
  # Normalize the reporting language identifier and stop if necessary
  reporting_lang <- normalize_reporting_language(reporting_lang)

  tbl_name <- deparse(match.call()$tbl)
  
  if (tbl_name == ".") {
    tbl_name <- "table"
  }
  
  tbl_information <- get_tbl_information(tbl = tbl)

  # If any `end_fns` are specified we always attempt to
  # embed the validation report
  if (!is.null(end_fns)) {
    embed_report <- TRUE
  }

  # Create the agent list object
  agent <-
    list(
      name = name,
      time = as.POSIXct(NA)[-1],
      tbl = tbl,
      tbl_name = tbl_name,
      db_tbl_name = tbl_information$db_tbl_name,
      tbl_src = tbl_information$tbl_src,
      tbl_src_details = tbl_information$tbl_src_details,
      col_names = tbl_information$col_names,
      col_types = tbl_information$r_col_types,
      db_col_types = tbl_information$db_col_types,
      actions = actions,
      end_fns = list(end_fns),
      embed_report = embed_report,
      reporting = NULL,
      reporting_lang = reporting_lang,
      validation_set =
        dplyr::tibble(
          i = integer(0),
          assertion_type = character(0),
          column = list(NULL),
          values = list(NULL),
          na_pass = logical(0),
          preconditions = list(NULL),
          actions = list(NULL),
          brief = character(0),
          active = logical(0),
          eval_error = logical(0),
          eval_warning = logical(0),
          capture_stack = list(NULL),
          all_passed = logical(0),
          n = integer(0),
          n_passed = integer(0),
          n_failed = integer(0),
          f_passed = numeric(0),
          f_failed = numeric(0),
          warn = logical(0),
          notify = logical(0),
          stop = logical(0),
          row_sample = numeric(0),
          tbl_checked = list(NULL),
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
