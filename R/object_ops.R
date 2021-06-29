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


#' Write a **pointblank** *agent*, *informant*, or table scan to disk
#' 
#' @description 
#' Writing an *agent*, *informant*, or even a table scan to disk with
#' `x_write_disk()` can be useful for keeping data validation intel or table
#' information close at hand for later retrieval (with [x_read_disk()]). By
#' default, any data table that the *agent* or *informant* may have held before
#' being committed to disk will be expunged (not applicable to any table scan
#' since they never hold a table object). This behavior can be changed by
#' setting `keep_tbl` to `TRUE` but this only works in the case where the table
#' is not of the `tbl_dbi` or the `tbl_spark` class.
#'
#' @details
#' It is recommended to set up a table-prep formula so that the *agent* and
#' *informant* can access refreshed data after being read from disk through
#' [x_read_disk()]. This can be done initially with the `read_fn` argument of
#' [create_agent()]/[create_informant()] or, later, with [set_read_fn()].
#' Alternatively, we can reintroduce the *agent* or *informant* to a data table
#' with the [set_tbl()] function.
#' 
#' @param x An *agent* object of class `ptblank_agent`, an *informant* of class
#'   `ptblank_informant`, or an table scan of class `ptblank_tbl_scan`.
#' @param filename The filename to create on disk for the `agent`, `informant`,
#'   or table scan.
#' @param path An optional path to which the file should be saved (this is
#'   automatically combined with `filename`).
#' @param keep_tbl An option to keep a data table that is associated with the
#'   *agent* or *informant* (which is the case when the *agent*, for example, is
#'   created using `create_agent(tbl = <data table, ...)`). The default is
#'   `FALSE` where the data table is removed before writing to disk. For
#'   database tables of the class `tbl_dbi` and for Spark DataFrames
#'   (`tbl_spark`) the table is always removed (even if `keep_tbl` is set to
#'   `TRUE`).
#' @param keep_extracts An option to keep any collected extract data for failing
#'   rows. Only applies to *agent* objects. By default, this is `FALSE` (i.e.,
#'   extract data is removed).
#' @param quiet Should the function *not* inform when the file is written? By
#'   default this is `FALSE`.
#'   
#' @return Invisibly returns `TRUE` if the file has been written.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # A: Writing an `agent` to disk 
#' 
#' # Let's go through the process of (1)
#' # developing an agent with a validation
#' # plan (to be used for the data quality
#' # analysis of the `small_table` dataset),
#' # (2) interrogating the agent with the
#' # `interrogate()` function, and (3) writing
#' # the agent and all its intel to a file
#' 
#' # Creating an `action_levels` object is a
#' # common workflow step when creating a
#' # pointblank agent; we designate failure
#' # thresholds to the `warn`, `stop`, and
#' # `notify` states using `action_levels()`
#' al <- 
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Now create a pointblank `agent` object
#' # and give it the `al` object (which
#' # serves as a default for all validation
#' # steps which can be overridden); the
#' # data will be referenced in a `read_fn`
#' agent <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`",
#'     actions = al
#'   )
#' 
#' # Then, as with any `agent` object, we
#' # can add steps to the validation plan by
#' # using as many validation functions as we
#' # want; then, we `interrogate()`
#' agent <-
#'   agent %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(vars(d), value = 100) %>%
#'   col_vals_lte(vars(c), value = 5) %>%
#'   interrogate()
#'
#' # The `agent` can be written to a file with
#' # the `x_write_disk()` function
#' x_write_disk(
#'   agent,
#'   filename = "agent-small_table.rds"
#' )
#' 
#' # We can read the file back into an agent
#' # with the `x_read_disk()` function and
#' # we'll get all of the intel along with the
#' # 'restored' agent
#' 
#' # If you're consistently storing agents
#' # from periodic runs of checking data, we
#' # could make use of the `affix_date()` or
#' # `affix_datetime()` depending on the
#' # granularly you need; here's an example
#' # that writes the file with the format:
#' # 'agent-small_table-YYYY-mm-dd_HH-MM-SS.rds'
#' x_write_disk(
#'   agent,
#'   filename = affix_datetime(
#'     "agent-small_table.rds"
#'   )
#' )
#' 
#' # B: Writing an `informant` to disk
#' 
#' # Let's go through the process of (1)
#' # creating an informant object that
#' # minimally describes the `small_table`
#' # dataset, (2) ensuring that data is
#' # captured from the target table using
#' # the `incorporate()` function, and (3)
#' # writing the informant to a file
#' 
#' # Create a pointblank `informant`
#' # object with `create_informant()`
#' # and the `small_table` dataset;
#' # `incorporate()` so that info snippets
#' # are integrated into the text
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "high_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "low_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "From {low_a} to {high_a}."
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. ((CALC))"
#'   ) %>%
#'   incorporate()
#'
#' # The `informant` can be written to a
#' # file with `x_write_disk()`; let's do
#' # this with the `affix_date()` so the
#' # filename has a datestamp
#' x_write_disk(
#'   informant,
#'   filename = affix_date(
#'     "informant-small_table.rds"
#'   )
#' )
#' 
#' # We can read the file back into the
#' # same informant object (as when it
#' # was saved) by using `x_read_disk()`
#' 
#' }
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-1
#' 
#' @export
x_write_disk <- function(x,
                         filename,
                         path = NULL,
                         keep_tbl = FALSE,
                         keep_extracts = FALSE,
                         quiet = FALSE) {

  if (!any(inherits(x, "ptblank_agent") | inherits(x, "ptblank_informant"))) {
    stop(
      "The object given as `x` is neither an agent nor an informant.", 
      call. = FALSE
    )
  }
  
  if (inherits(x, "ptblank_agent")) {

    x$validation_set$tbl_checked <- NULL
    
    x$validation_set <- 
      x$validation_set %>%
      dplyr::mutate(tbl_checked = list(NULL))
    
    if (keep_tbl) {
      
      if (inherits(x$tbl, "tbl_dbi") || inherits(x$tbl, "tbl_spark")) {
        
        warning(
          "A table of class `tbl_dbi` or `tbl_spark` cannot be ",
          "kept with the object.",
          call. = FALSE
        )
        
        x <- remove_tbl(x)
      }
      
    } else if (!keep_tbl) {
      x <- remove_tbl(x)
    }
    
    if (!keep_extracts) {
      x$extracts <- list()
    }
    
    object_type <- "agent"
  } else {
    object_type <- "informant"
  }
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
    
  filename <- as.character(fs::path_norm(fs::path_expand(filename)))

  # Write the object to disk
  saveRDS(x, file = filename)
  
  # Generate cli message w.r.t. written RDS file
  if (!quiet) {
    cli_bullet_msg(
      msg = "The {object_type} file has been written to `{filename}`",
      bullet = cli::symbol$tick,
      color = "green"
    )
  }
  
  invisible(TRUE)
}

#' Read a **pointblank** *agent* or *informant* from disk
#' 
#' @description 
#' An *agent* or *informant* that has been written to disk (with
#' [x_write_disk()]) can be read back into memory with the `x_read_disk()`
#' function. Once the *agent* or *informant* has been generated in this way, it
#' may not have a data table associated with it (depending on whether the
#' `keep_tbl` option was `TRUE` or `FALSE` when writing to disk) but it should
#' still be able to produce reporting (by printing the *agent* or *informant* to
#' the console or using [get_agent_report()]/[get_informant_report()]). An
#' *agent* will return an x-list with [get_agent_x_list()] and yield any
#' available data extracts with [get_data_extracts()]. Furthermore, all of an
#' *agent*'s validation steps will still be present (along with results from the
#' last interrogation).
#' 
#' @details
#' Should the *agent* or *informant* possess a table-prep formula (can be set
#' any time with [set_read_fn()]) or a specific table (settable with
#' [set_tbl()]) we could use the [interrogate()] or [incorporate()] function
#' again. For a *data quality reporting* workflow, it is useful to
#' [interrogate()] target tables that evolve over time. While the same
#' validation steps will be used, more can be added before calling
#' [interrogate()]. For an *information management* workflow with an *informant*
#' object, using [incorporate()] will update aspects of the reporting such as
#' table dimensions, and info snippets/text will be regenerated.
#' 
#' @param filename The name of a file that was previously written by
#'   [x_write_disk()].
#' @param path An optional path to the file (combined with `filename`).
#' 
#' @return Either a `ptblank_agent` or a `ptblank_informant` object.
#' 
#' @examples
#' if (interactive()) {
#' 
#' # A: Reading an `agent` from disk 
#' 
#' # The process of developing an `agent`
#' # and writing it to disk with the
#' # `x_write_disk()` function is explained
#' # in that function's documentation;
#' # suppose we have such a written file
#' # that's named "agent-small_table.rds",
#' # we could read that to a new agent
#' # with `x_read_disk()`
#' agent <-
#'   x_read_disk("agent-small_table.rds")
#' 
#' # B: Reading an `informant` from disk
#' 
#' # If there is an `informant` written
#' # to disk via `x_write_disk()` and it's
#' # named "informant-small_table.rds",
#' # we could read that to a new informant
#' # with `x_read_disk()`
#' informant <-
#'   x_read_disk("informantt-small_table.rds")
#' 
#' }
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-2
#' 
#' @export
x_read_disk <- function(filename,
                        path = NULL) {
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  
  readRDS(filename)
}

#' Set a data table to an *agent* or *informant*
#' 
#' @description 
#' Setting a data table to an *agent* or *informant* with `set_tbl()` replaces
#' any associated table (a data frame, a tibble, objects of class `tbl_dbi` or
#' `tbl_spark`). If a data table is associated with an *agent* or *informant*
#' through the `tbl` argument *and* the same object has a table-prep formula
#' (settable in [create_agent()] and [create_informant()]'s `read_fn` argument
#' or with [set_read_fn()]), the table-prep formula will take precedence. If
#' this is undesirable, it be removed with the [remove_read_fn()] function. The
#' association to a table can be removed with with [remove_tbl()].
#'
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' @param tbl The input table for the `agent`. This can be a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Any table already
#'   associated with the *agent* or *informant* will be overwritten.
#' 
#' @examples
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that has
#' # `small_table` set as the target
#' # table via `tbl`; apply the actions,
#' # add some validation steps and then
#' # interrogate the data
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # Replace the agent's association to
#' # `small_table` with a mutated version
#' # of it (one that removes duplicate rows);
#' # then, interrogate the new target table
#' agent_2 <-
#'   agent_1 %>%
#'   set_tbl(
#'     tbl = small_table %>% dplyr::distinct()
#'   ) %>%
#'   interrogate()
#' 
#' @family Object Ops
#' @section Function ID:
#' 9-3
#' 
#' @export
set_tbl <- function(x,
                    tbl) {
  
  # Set the table on the `$tbl` list element
  x$tbl <- tbl
  
  if (is_ptblank_agent(x)) {
    
    # TODO: only attempt to set a `tbl_name` if it is NULL
    
    # Get the name of the table and set it to
    # the `$tbl_name` list element
    tbl_name <- deparse(match.call()$tbl)
    if (tbl_name == ".") {
      tbl_name <- "table"
    }
    
    x$tbl_name <- tbl_name
  }
  
  if (is_ptblank_agent(x)) {
    
    # Obtain basic information on the table and
    # set the relevant list elements
    tbl_information <- get_tbl_information(tbl = tbl)
    
    x$db_tbl_name <- tbl_information$db_tbl_name
    x$tbl_src <- tbl_information$tbl_src
    x$tbl_src_details <- tbl_information$tbl_src_details
    x$col_names <- tbl_information$col_names
    x$col_types <- tbl_information$r_col_types
    x$db_col_types <- tbl_information$db_col_types
    
    # Remove any data extracts
    x$extracts <- NULL
  }
  
  invisible(x)
}

#' Remove a data table associated with an *agent* or *informant*
#' 
#' @description 
#' Removing an *agent* or *informant*'s association to a data table can be done
#' with the `remove_tbl()` function. This can be useful to ensure that the table
#' data isn't unintentionally written to disk. It is usually best to avoid
#' directly associating a table to an *agent* or *informant* through the `tbl`
#' argument, instead opting for setting a table-prep formula (via
#' [create_agent()] and [create_informant()]'s `read_fn` argument, or, with
#' [set_read_fn()]). If necessary, the association to a table can be set again
#' with [set_tbl()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' 
#' @examples
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that has
#' # `small_table` set as the target
#' # table via `tbl`; apply the actions,
#' # add some validation steps and then
#' # interrogate the data
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # In this case where `small_table`
#' # changes (and the aim is to have
#' # validations run periodically) it is
#' # better to obtain the table from the
#' # source with a table-prep formula;
#' # while doing this, the direct
#' # association to `small_table` can be
#' # removed with `remove_tbl()` so it's
#' # no longer part of the agent object
#' agent_2 <-
#'   agent_1 %>%
#'   remove_tbl() %>%
#'   set_read_fn(read_fn = ~ small_table) %>%
#'   interrogate()
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-4
#'   
#' @export
remove_tbl <- function(x) {
  
  x$tbl <- NULL
  invisible(x)
}

#' Set a table-prep formula to an *agent* or *informant*
#'
#' @description
#' A table-prep formula can be associated with an *agent* or *informant* with
#' `set_read_fn()`. Should both a `tbl` *and* a `read_fn` be associated with the
#' *agent* or *informant*, the `read_fn` will take priority. We can specify a
#' value for `read_fn` with an RHS formula expression (e.g., `~ { <table reading
#' code> }`). The table-prep formula can removed with [remove_read_fn()] or
#' replaced with `set_read_fn()`.
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' @param read_fn An R formula expression (e.g., `~ { <table reading code> }`)
#'   that is used to prepare a table.
#'
#' @examples
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that reads in
#' # `small_table` with a table-prep
#' # formula; apply the actions,
#' # add some validation steps and then
#' # interrogate the data
#' agent_1 <- 
#'   create_agent(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # Change the table-prep formula to use
#' # a mutated version of `small_table`
#' # (one that removes duplicate rows);
#' # then, interrogate the target table
#' # again
#' agent_2 <-
#'   agent_1 %>%
#'   set_read_fn(
#'     read_fn = ~ small_table %>% dplyr::distinct()
#'   ) %>%
#'   interrogate()
#'
#' @family Object Ops
#' @section Function ID:
#' 9-5
#'
#' @export
set_read_fn <- function(x,
                        read_fn) {
  
  x$read_fn <- read_fn
  invisible(x)
}

#' Remove a table-prep formula associated with an *agent* or *informant*
#' 
#' @description 
#' Removing an *agent* or an *informant*'s association to a table-pre formula
#' can be done with `remove_read_fn()`. This may be good idea in an interactive
#' session when needing to rely on the direct association of a 'fixed' data
#' table (settable in [create_agent()] and [create_informant()]'s `tbl` argument
#' or with [set_tbl()]) instead of using a table-prep formula that might produce
#' different a different table than expected. The table-prep formula can always
#' be set again with [set_read_fn()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#'   
#' @examples 
#' # Set proportional failure thresholds
#' # to the `warn`, `stop`, and `notify`
#' # states using `action_levels()`
#' al <- 
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' 
#' # Create an agent that directly ingests
#' # the `small_table` object and also has
#' # a table-prep formula (when both are
#' # present the latter always obtains the
#' # table); apply the actions, add some
#' # validation steps and then interrogate
#' # the data that was read in
#' agent_1 <- 
#'   create_agent(
#'     tbl = small_table,
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#'   
#' # In a situation where `small_table`
#' # changes frequently and it's desirable
#' # to have a snapshot of the table, we
#' # can remove the table-prep formula so
#' # that the ingested `small_table` will
#' # be used
#' agent_2 <-
#'   agent_1 %>%
#'   remove_read_fn() %>%
#'   interrogate()
#'   
#' @family Object Ops
#' @section Function ID:
#' 9-6
#'   
#' @export
remove_read_fn <- function(x) {
  
  x$read_fn <- NULL
  
  # Remove any data extracts
  x$extracts <- NULL
  
  invisible(x)
}
