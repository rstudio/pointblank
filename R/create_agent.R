#' Create a pointblank agent object
#'
#' @description
#' The `create_agent()` function creates an *agent* object, which is used in a
#' *data quality reporting* workflow. The overall aim of this workflow is to
#' generate useful reporting information for assessing the level of data quality
#' for the target table. We can supply as many validation functions as the user
#' wishes to write, thereby increasing the level of validation coverage for that
#' table. The *agent* assigned by the `create_agent()` call takes validation
#' functions, which expand to validation steps (each one is numbered). This
#' process is known as developing a *validation plan*.
#'
#' The validation functions, when called on an *agent*, are merely instructions
#' up to the point the [interrogate()] function is called. That kicks off the
#' process of the *agent* acting on the *validation plan* and getting results
#' for each step. Once the interrogation process is complete, we can say that
#' the *agent* has intel. Calling the *agent* itself will result in a reporting
#' table. This reporting of the interrogation can also be accessed with the
#' [get_agent_report()] function, where there are more reporting options.
#'
#' @details A very detailed list object, known as the x-list, can be obtained by
#' using the [get_agent_x_list()] function on the *agent*. This font of
#' information can be taken as a whole, or, broken down by the step number (with
#' the `i` argument).
#'
#' Sometimes it is useful to see which rows were the failing ones. By using the
#' [get_data_extracts()] function on the *agent*, we either get a list of
#' tibbles (for those steps that have data extracts) or one tibble if the
#' validation step is specified with the `i` argument.
#'
#' If we just need to know whether all validations completely passed (i.e., all
#' steps had no failing test units), the [all_passed()] function could be used
#' on the *agent*. However, in practice, it's not often the case that all data
#' validation steps are free from any failing units.
#'
#' @param tbl The input table. This can be a data frame, a tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object. Alternatively, a function can be used to
#'   read in the input data table with the `read_fn` argument (in which case,
#'   `tbl` can be `NULL`).
#' @param read_fn A function that's used for reading in the data. If a `tbl` is
#'   not provided, then this function will be invoked. However, if both a `tbl`
#'   *and* a `read_fn` is specified, then the supplied `tbl` will take priority.
#'   There are two ways to specify a `read_fn`: (1) using a function (e.g.,
#'   `function() { <table reading code> }`) or, (2) with an R formula expression
#'   (e.g., `~ { <table reading code> }`).
#' @param tbl_name A optional name to assign to the input table object. If no
#'   value is provided, a name will be generated based on whatever information
#'   is available.
#' @param actions A list containing threshold levels so that all validation
#'   steps can react accordingly when exceeding the set levels. This is to be
#'   created with the [action_levels()] helper function. Should an action levels
#'   list be used for specific validation step, any default set here will be
#'   overridden.
#' @param end_fns A list of functions that should be performed at the end of an
#'   interrogation.
#' @param label An optional label for the validation plan. If no value is
#'   provided, a label will be generated based on the current system time.
#'   Markdown can be used here to make the label more visually appealing (it
#'   will appear in the header area of the agent report).
#' @param embed_report An option to embed a **gt**-based validation report into
#'   the `ptblank_agent` object. If `FALSE` (the default) then the table object
#'   will be not generated and available with the *agent* upon returning from
#'   the interrogation.
#' @param reporting_lang The language to use for automatic creation of briefs
#'   (short descriptions for each validation step) and for the *agent report* (a
#'   summary table that provides the validation plan and the results from the
#'   interrogation. By default, `NULL` will create English (`"en"`) text. Other
#'   options include French (`"fr"`), German (`"de"`), Italian (`"it"`), and
#'   Spanish (`"es"`).
#'   
#' @return A `ptblank_agent` object.
#'   
#' @examples
#' # Let's walk through a data quality
#' # analysis of an extremely small table;
#' # it's actually called `small_table` and
#' # we can find it as a dataset in this
#' # package
#' small_table
#' 
#' # We ought to think about what's
#' # tolerable in terms of data quality so
#' # let's designate proportional failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # static thresholds provided by `al` will
#' # make the reporting a bit more useful
#' agent <- 
#'   create_agent(
#'     small_table,
#'     label = "example",
#'     actions = al
#'   )
#'
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want; then, we use `interrogate()` to
#' # physically perform the validations and
#' # gather intel
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), 100) %>%
#'   col_vals_lte(vars(c), 5) %>%
#'   col_vals_equal(
#'     vars(d), vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   col_vals_between(
#'     vars(c),
#'     left = vars(a), right = vars(d),
#'     na_pass = TRUE
#'   ) %>%
#'   interrogate()
#'   
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent)`
#' report <- get_agent_report(agent)
#' class(report)
#' 
#' # What can you do with the report?
#' # Print it from an R Markdown code
#' # chunk, use it in a **blastula** email,
#' # put it in a webpage, or further
#' # modify it with the **gt** package
#' 
#' # From the report we know that Step
#' # 4 had two test units (rows, really)
#' # that failed; we can see those rows
#' # with `get_data_extracts()` 
#' agent %>% get_data_extracts(i = 4)
#' 
#' # We can get an x-list for the whole
#' # validation (8 steps), or, just for
#' # the 4th step with `get_agent_x_list()`
#' xl_step_4 <-
#'   agent %>% get_agent_x_list(i = 4)
#'  
#' # And then we can peruse the different
#' # parts of the list; let's get the
#' # fraction of test units that failed
#' xl_step_4$f_failed
#' 
#' # Just printing the x-list will tell
#' # us what's available therein
#' xl_step_4
#' 
#' # An x-list not specific to any step
#' # will have way more information and a
#' # slightly different structure; see
#' # `help(get_agent_x_list)` for more info
#' # get_agent_x_list(agent)
#'  
#' @family Planning and Prep
#' @section Function ID:
#' 1-2
#'   
#' @export
create_agent <- function(tbl = NULL,
                         read_fn = NULL,
                         tbl_name = NULL,
                         actions = NULL,
                         end_fns = NULL,
                         label = NULL,
                         embed_report = FALSE,
                         reporting_lang = NULL) {

  # Generate a label if none provided
  if (is.null(label)) {
    label <- paste0("[", gsub(" ", "|", Sys.time() %>% as.character()), "]")
  }
  
  # Normalize the reporting language identifier and stop if necessary
  reporting_lang <- normalize_reporting_language(reporting_lang)

  # If nothing is provided for either `tbl` or `read_fn`,
  # this function needs to be stopped
  if (is.null(tbl) && is.null(read_fn)) {
    
    stop(
      "A table object or table-reading function must be supplied:\n",
      " * Use a table object in the `tbl` argument.\n",
      " * Or supply a table-reading function in `read_fn`.\n",
      " * You can do both: the supplied `tbl` will take priority.",
      call. = FALSE
    )
  }
  
  if (!is.null(tbl) && is.null(tbl_name)) {
    tbl_name <- deparse(match.call()$tbl)
    if (tbl_name == ".") {
      tbl_name <- NA_character_
    }
  } 
  if (is.null(tbl_name)) {
    tbl_name <- NA_character_
  }
  
  if (!is.null(read_fn) && is.null(tbl)) {
    if (inherits(read_fn, "function")) {
      tbl <- rlang::exec(read_fn)
    } else if (rlang::is_formula(read_fn)) {
      tbl <- read_fn %>% rlang::f_rhs() %>% rlang::eval_tidy()
    } else {
      stop(
        "The `read_fn` object must be a function or an R formula.\n",
        "* A function can be made with `function()` {<table reading code>}.\n",
        "* An R formula can also be used, with the expression on the RHS.",
        call. = FALSE
      )
    }
  }
  
  # Get some basic information on the table, which will be
  # returned as a list
  tbl_information <- get_tbl_information(tbl = tbl)

  # If any `end_fns` are specified we always attempt to
  # embed the validation report
  if (!is.null(end_fns)) {
    embed_report <- TRUE
  }

  # Create the agent list object
  agent <-
    list(
      tbl = tbl,
      read_fn = read_fn,
      tbl_name = tbl_name,
      label = label,
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
      time_start = as.POSIXct(NA)[-1],
      time_end = as.POSIXct(NA)[-1],
      validation_set =
        dplyr::tibble(
          i = integer(0),
          step_id = character(0),
          sha1 = character(0),
          assertion_type = character(0),
          column = list(NULL),
          values = list(NULL),
          na_pass = logical(0),
          preconditions = list(NULL),
          actions = list(NULL),
          label = character(0),
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
