#' Write an agent to disk
#' 
#' Writing an *agent* to disk with `agent_write()` is good practice for keeping
#' data validation intel close at hand for later retrieval (with
#' [agent_read()]). By default, any data table that the *agent* may have before
#' being committed to disk will be expunged. This behavior can be changed by
#' setting `keep_tbl` to `TRUE` but this only works in the case where the table
#' is not of the `tbl_dbi` or the `tbl_spark` class.
#'
#' It can be helpful to set a table-reading function to later reuse the *agent*
#' when read from disk through [agent_read()]. This can be done with the
#' `read_fn` argument of [create_agent()] or, later, with [set_read_fn()].
#' Alternatively, we can reintroduce the *agent* to a data table with the
#' [set_tbl()] function.
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#' @param filename The file name to create on disk for the `agent`.
#' @param path An optional path to which the file should be saved (combined with
#'   `filename`).
#' @param keep_tbl An option to keep a data table that is associated with the
#'   *agent* (which is the case when the *agent* is created using 
#'   `create_agent(tbl = <data table, ...)`). The default is `FALSE` where the
#'   data table is removed before writing to disk. For database tables of the
#'   class `tbl_dbi` and for Spark DataFrames (`tbl_spark`) the table is always
#'   removed (even if `keep_tbl` is set to `TRUE`).
#'   
#' @family Agent Ops
#' @section Function ID:
#' 8-1
#' 
#' @export
agent_write <- function(agent,
                        filename,
                        path = NULL,
                        keep_tbl = FALSE) {
  
  if (keep_tbl) {
    
    if (inherits(agent$tbl, "tbl_dbi") || inherits(agent$tbl, "tbl_spark")) {
      
      warning(
        "A table of class `tbl_dbi` or `tbl_spark` cannot be kept with the agent.",
        call. = FALSE
      )
      
      agent <- remove_tbl(agent)
    }
    
  } else if (!keep_tbl) {
    agent <- remove_tbl(agent)
  }
  

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
    
  filename <- fs::path_expand(filename)
  
  con <- xzfile(filename)
  on.exit(close(con), add = TRUE)
  saveRDS(agent, con)
  invisible(agent)
}

#' Read an agent from disk
#' 
#' An *agent* that has been written to disk (with [agent_write()]) can be read
#' back into memory with the `agent_read()` function. Once the *agent* has been
#' read, it may not have a data table associated with it (depending on whether
#' the `keep_tbl` option was `TRUE` or `FALSE` when writing to disk) but it
#' should still be able to produce an agent report (by printing the *agent* to
#' the console or using [get_agent_report()]), return an agent x-list (with
#' [get_agent_x_list()]), and yield any available data extracts with
#' [get_data_extracts()]. Furthermore, all of its validation steps will still be
#' present (along with results from any interrogation).
#' 
#' Should the *agent* possess a table-reading function (can be set any time with
#' [set_read_fn()]) or a specific table (settable with [set_tbl()]) we could use
#' the [interrogate()] function again. This is useful for tables that evolve
#' over time and need periodic data quality assessments with the same validation
#' steps (and more steps can be added as well).
#' 
#' @param path The path to the file that was previously written by
#'   [agent_write()].
#' 
#' @family Agent Ops
#' @section Function ID:
#' 8-2
#' 
#' @export
agent_read <- function(path) {
  readRDS(path)
}

#' Set a data table to an agent
#' 
#' Setting a data table to *agent* with `set_tbl()` replaces any table (a data
#' frame, a tibble, objects of class `tbl_dbi` or `tbl_spark`) associated with
#' the *agent*. If no data table is associated with an *agent*, setting one will
#' mean the data table takes precedence over table-reading function (settable in
#' [create_agent()]'s `read_fn` argument or with [set_read_fn()]).
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#' @param tbl The input table for the `agent`. This can be a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Any table already
#'   associated with the *agent* will be overwritten.
#' 
#' @family Agent Ops
#' @section Function ID:
#' 8-3
#' 
#' @export
set_tbl <- function(agent,
                    tbl) {
  
  # Set the table on the `$tbl` list element
  agent$tbl <- tbl
  
  # Get the name of the table and set it to
  # the `$tbl_name` list element
  tbl_name <- deparse(match.call()$tbl)
  if (tbl_name == ".") {
    tbl_name <- "table"
  }
  
  agent$tbl_name <- tbl_name
  
  # Obtain basic information on the table and
  # set the relevant list elements
  tbl_information <- get_tbl_information(tbl = tbl)
  
  agent$db_tbl_name <- tbl_information$db_tbl_name
  agent$tbl_src <- tbl_information$tbl_src
  agent$tbl_src_details <- tbl_information$tbl_src_details
  agent$col_names <- tbl_information$col_names
  agent$col_types <- tbl_information$r_col_types
  agent$db_col_types <- tbl_information$db_col_types

  # Remove any data extracts
  agent$extracts <- NULL
  
  invisible(agent)
}

#' Remove a data table associated with an agent
#' 
#' Removing an *agent*'s association to a data table can be done with the
#' `remove_tbl()` function. This may be preferable when relying on a
#' table-reading function (settable in [create_agent()]'s `read_fn` argument or
#' with [set_read_fn()]) instead of directly using a table. If interrogating
#' again with [interrogate()] then there must be either an association to a
#' table or a table-reading function available in the *agent* ([set_read_fn()]
#' can help in this regard if the the `read_fn` argument in [create_agent()] was
#' left as `NULL` when creating the *agent*).
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#'   
#' @family Agent Ops
#' @section Function ID:
#' 8-4
#'   
#' @export
remove_tbl <- function(agent) {
  
  agent$tbl <- NULL
  invisible(agent)
}

#' Set a table-reading function to an agent
#'
#' A table-reading function can be associated with an *agent* with
#' `set_read_fn()`. If a data table is already associated with an *agent*, it
#' will act as the target table (i.e., the *agent* will disregard the
#' table-reading function). However, if there is no data table associated with
#' the *agent* then the table-reading function will be invoked. We can always
#' remove a data table associated with an *agent* with the [remove_tbl()]
#' function. There are two ways to specify a `read_fn`: (1) using a function
#' (e.g., `function() { <table reading code> }`) or, (2) with an R formula
#' expression (e.g., `~ { <table reading code> }`).
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#' @param read_fn A function that's used for reading in the data. If a table is
#'   not associated with the `agent` then this function will be invoked. Should
#'   both a `tbl` *and* a `read_fn` be associated with the `agent`, the `tbl`
#'   will take priority. There are two ways to specify a `read_fn`: (1) using a
#'   function (e.g., `function() { <table reading code> }`) or, (2) with an R
#'   formula expression (e.g., `~ { <table reading code> }`).
#'
#' @family Agent Ops
#' @section Function ID:
#' 8-5
#'
#' @export
set_read_fn <- function(agent,
                        read_fn) {
  
  agent$read_fn <- read_fn
  invisible(agent)
}

#' Remove a table-reading function associated with an agent
#' 
#' Removing an *agent*'s association to a table-reading function can be done
#' with `remove_read_fn()`. This may be good idea when instead relying on the
#' direct association of a data table (settable in [create_agent()]'s `tbl`
#' argument or with [set_tbl()]), where the table-reading function is no longer
#' relevant.
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#'   
#' @family Agent Ops
#' @section Function ID:
#' 8-6
#'   
#' @export
remove_read_fn <- function(agent) {
  
  agent$read_fn <- NULL
  invisible(agent)
}
