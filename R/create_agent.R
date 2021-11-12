#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Create a **pointblank** *agent* object
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
#' @section Data Products Obtained from an Agent:
#' A very detailed list object, known as an x-list, can be obtained by using the
#' [get_agent_x_list()] function on the *agent*. This font of information can be
#' taken as a whole, or, broken down by the step number (with the `i` argument).
#'
#' Sometimes it is useful to see which rows were the failing ones. By using the
#' [get_data_extracts()] function on the *agent*, we either get a list of
#' tibbles (for those steps that have data extracts) or one tibble if the
#' validation step is specified with the `i` argument.
#'
#' The target data can be split into pieces that represent the 'pass' and 'fail'
#' portions with the [get_sundered_data()] function. A primary requirement is an
#' agent that has had [interrogate()] called on it. In addition, the validation
#' steps considered for this data splitting need to be those that operate on
#' values down a column (e.g., the `col_vals_*()` functions or [conjointly()]).
#' With these in-consideration validation steps, rows with no failing test units
#' across all validation steps comprise the 'pass' data piece, and rows with at
#' least one failing test unit across the same series of validations constitute
#' the 'fail' piece.
#'
#' If we just need to know whether all validations completely passed (i.e., all
#' steps had no failing test units), the [all_passed()] function could be used
#' on the *agent*. However, in practice, it's not often the case that all data
#' validation steps are free from any failing units.
#' 
#' While printing an *agent* will display the *agent* report in the Viewer, we
#' can alternatively use the [get_agent_report()] to take advantage of other
#' options (e.g., overriding the language, modifying the arrangement of report
#' rows, etc.), and to return the report as independent objects. For example,
#' with the `display_table = TRUE` option (the default), [get_agent_report()]
#' will return a **gt** table object (`"gt_tbl"`). If `display_table` is set to
#' `FALSE`, we'll get a data frame back instead.
#' 
#' @section YAML: 
#' A **pointblank** agent can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an agent (with [yaml_read_agent()])
#' or interrogate the target table (via [yaml_agent_interrogate()]). Here is an
#' example of how a complex call of `create_agent()` is expressed in R code and
#' in the corresponding YAML representation.
#' 
#' ```
#' # R statement
#' create_agent(
#'   read_fn = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "An example.",
#'   actions = action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   ), 
#'   end_fns = list(
#'     ~ beepr::beep(2),
#'     ~ Sys.sleep(1)
#'   ), 
#'   embed_report = TRUE,
#'   lang = "fr", 
#'   locale = "fr_CA"
#' )
#' 
#' # YAML representation
#' type: agent
#' read_fn: ~small_table
#' tbl_name: small_table
#' label: An example.
#' lang: fr
#' locale: fr_CA
#' actions:
#'   warn_fraction: 0.1
#' stop_fraction: 0.25
#' notify_fraction: 0.35
#' end_fns:
#' - ~beepr::beep(2)
#' - ~Sys.sleep(1)
#' embed_report: true
#' ```
#' 
#' In practice, this block of YAML will be shorter since arguments with default
#' values won't be written to YAML when using [yaml_write()] (though it is
#' acceptable to include them with their default when generating the YAML by
#' other means). The only requirement for writing the YAML representation of an
#' *agent* is having `read_fn` specified (any table supplied to `tbl` is
#' ignored).
#' 
#' What typically follows this chunk of YAML is a `steps` part, and that
#' corresponds to the addition of validation steps via validation functions.
#' Help articles for each validation function have a *YAML* section that
#' describes how a given validation function is translated to YAML.
#' 
#' Should you need to preview the transformation of an *agent* to YAML (without
#' any committing anything to disk), use the [yaml_agent_string()] function. If
#' you already have a `.yml` file that holds an *agent*, you can get a glimpse
#' of the R expressions that are used to regenerate that agent with
#' [yaml_agent_show_exprs()].
#' 
#' @section Writing an Agent to Disk:
#' An *agent* object can be written to disk with the [x_write_disk()] function.
#' This can be useful for keeping a history of validations and generating views
#' of data quality over time. Agents are stored in the serialized RDS format and
#' can be easily retrieved with the [x_read_disk()] function.
#'
#' It's recommended that table-prep formulas are supplied to the `read_fn`
#' argument of `create_agent()`. In this way, when an *agent* is read from disk
#' through [x_read_disk()], it can be reused to access the target table (which
#' may change, hence the need to use an expression for this).
#' 
#' @section Combining Several Agents in a *multiagent* Object:
#' Multiple *agent* objects can be part of a *multiagent* object, and two
#' functions can be used for this: [create_multiagent()] and
#' [read_disk_multiagent()]. By gathering multiple agents that have performed
#' interrogations in the past, we can get a *multiagent* report showing how data
#' quality evolved over time. This use case is interesting for data quality
#' monitoring and management, and, the reporting (which can be customized with
#' [get_multiagent_report()]) is robust against changes in validation steps for
#' a given target table.
#'
#' @param tbl The input table. This can be a data frame, a tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object. Alternatively, a function can be used to
#'   read in the input data table with the `read_fn` argument (in which case,
#'   `tbl` can be `NULL`).
#' @param read_fn A table-prep formula that's used to access the target table.
#'   Even if a `tbl` is provided, this formula will be invoked to obtain the
#'   data (i.e., the `read_fn` takes priority). There are two ways to specify a
#'   `read_fn`: (1) with a right-hand side (RHS) formula expression (e.g.,
#'   `~ { <table reading code>}`) or (2) as a function (e.g., 
#'   `function() { <table reading code>}`).
#' @param tbl_name A optional name to assign to the input table object. If no
#'   value is provided, a name will be generated based on whatever information
#'   is available. This table name will be displayed in the header area of the
#'   agent report generated by printing the *agent* or calling
#'   [get_agent_report()].
#' @param label An optional label for the validation plan. If no value is
#'   provided, a label will be generated based on the current system time.
#'   Markdown can be used here to make the label more visually appealing (it
#'   will appear in the header area of the agent report).
#' @param actions A option to include a list with threshold levels so that all
#'   validation steps can react accordingly when exceeding the set levels. This
#'   is to be created with the [action_levels()] helper function. Should an
#'   action levels list be used for a specific validation step, the default set
#'   specified here will be overridden.
#' @param end_fns A list of expressions that should be invoked at the end of an
#'   interrogation. Each expression should be in the form of a one-sided R
#'   formula, so overall this construction should be used: `end_fns = list(~ <R
#'   statements>, ~ <R statements>, ...)`. An example of a function included in
#'   **pointblank** that can be sensibly used here is [email_blast()], which
#'   sends an email of the validation report (based on a sending condition).
#' @param embed_report An option to embed a **gt**-based validation report into
#'   the `ptblank_agent` object. If `FALSE` (the default) then the table object
#'   will be not generated and available with the *agent* upon returning from
#'   the interrogation.
#' @param lang The language to use for automatic creation of briefs (short
#'   descriptions for each validation step) and for the *agent report* (a
#'   summary table that provides the validation plan and the results from the
#'   interrogation. By default, `NULL` will create English (`"en"`) text. Other
#'   options include French (`"fr"`), German (`"de"`), Italian (`"it"`), Spanish
#'   (`"es"`), Portuguese (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`), Russian
#'   (`"ru"`), Polish (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and Dutch
#'   (`"nl"`).
#' @param locale An optional locale ID to use for formatting values in the
#'   *agent report* summary table according the locale's rules. Examples include
#'   `"en_US"` for English (United States) and `"fr_FR"` for French (France);
#'   more simply, this can be a language identifier without a country
#'   designation, like "es" for Spanish (Spain, same as `"es_ES"`).
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
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
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
#'     vars(b),
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   col_vals_equal(
#'     vars(d), value = vars(d),
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
#' @section Figures:
#' \if{html}{\figure{man_create_agent_1.png}{options: width=100\%}}
#'  
#' @family Planning and Prep
#' @section Function ID:
#' 1-2
#'   
#' @export
create_agent <- function(tbl = NULL,
                         read_fn = NULL,
                         tbl_name = NULL,
                         label = NULL,
                         actions = NULL,
                         end_fns = NULL,
                         embed_report = FALSE,
                         lang = NULL,
                         locale = NULL) {

  # Generate a label if none provided
  label <- generate_label(label = label)

  # Normalize the reporting language identifier and stop if necessary
  lang <- normalize_reporting_language(lang)
  
  # Set the `locale` to the `lang` value if `locale` isn't set
  if (is.null(locale)) locale <- lang

  # If nothing is provided for either `tbl` or `read_fn`,
  # this function needs to be stopped
  if (is.null(tbl) && is.null(read_fn)) {
    
    stop(
      "A table object or table-prep formula must be supplied:\n",
      " * Use a table object in the `tbl` argument.\n",
      " * Or supply a table-prep formula in `read_fn`.\n",
      " * You can do both, the table-prep formula will take priority though.",
      call. = FALSE
    )
  }
  
  # Try to infer the table name if one isn't
  # explicitly given in `tbl_name`
  if (!is.null(tbl) && is.null(tbl_name)) {
    tbl_name <- deparse(match.call()$tbl)
    if (tbl_name == ".") {
      tbl_name <- NA_character_
    }
  } 
  if (is.null(tbl_name)) {
    tbl_name <- NA_character_
  }
  
  # Prefer reading a table from a `read_fn` if it's available
  # TODO: Verify that the table is a table object
  # and provide an error if it isn't
  if (!is.null(read_fn)) {
    
    if (inherits(read_fn, "function")) {
      
      tbl <- rlang::exec(read_fn)
      
    } else if (rlang::is_formula(read_fn)) {
      
      tbl <- 
        read_fn %>% 
        rlang::f_rhs() %>% 
        rlang::eval_tidy(env = caller_env(n = 1))
      
      if (inherits(tbl, "read_fn")) {

        if (inherits(tbl, "with_tbl_name") && is.na(tbl_name)) {
          tbl_name <- tbl %>% rlang::f_lhs() %>% as.character()
        }
        
        tbl <-
          tbl %>%
          rlang::f_rhs() %>%
          rlang::eval_tidy(env = caller_env(n = 1))
      }
      
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
      lang = lang,
      locale = locale,
      time_start = as.POSIXct(NA)[-1],
      time_end = as.POSIXct(NA)[-1],
      validation_set =
        dplyr::tibble(
          i = integer(0),
          i_o = integer(0),
          step_id = character(0),
          sha1 = character(0),
          assertion_type = character(0),
          columns_expr = character(0),
          column = list(NULL),
          values = list(NULL),
          na_pass = logical(0),
          seg_expr = list(NULL),
          seg_col = character(0),
          seg_val = character(0),
          preconditions = list(NULL),
          actions = list(NULL),
          label = character(0),
          brief = character(0),
          active = list(NULL),
          eval_active = logical(0),
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
          interrogation_notes = list(NULL),
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
