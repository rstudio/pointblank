#' Write a **pointblank** *agent* or *informant* to disk
#' 
#' Writing an *agent* or *informant* to disk with `x_write_disk()` can be useful
#' for keeping data validation intel or table information close at hand for
#' later retrieval (with [x_read_disk()]). By default, any data table that the
#' *agent* or *informant* may have held before being committed to disk will be
#' expunged. This behavior can be changed by setting `keep_tbl` to `TRUE` but
#' this only works in the case where the table is not of the `tbl_dbi` or the
#' `tbl_spark` class.
#'
#' It is recommended to set a table-reading function for later reuse of the
#' *agent* and *informant* after being read from disk through [x_read_disk()].
#' This can be done initially with the `read_fn` argument of
#' [create_agent()]/[create_informant()] or, later, with [set_read_fn()].
#' Alternatively, we can reintroduce the *agent* or *informant* to a data table
#' with the [set_tbl()] function.
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' @param filename The filename to create on disk for the `agent` or
#'   `informant`.
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
#'   rows. By default, this is `FALSE`.
#'   
#' @family Object Ops
#' @section Function ID:
#' 8-1
#' 
#' @export
x_write_disk <- function(x,
                         filename,
                         path = NULL,
                         keep_tbl = FALSE,
                         keep_extracts = FALSE) {
  
  x$validation_set$tbl_checked <- NULL
  x$validation_set <-
    x$validation_set %>%
    dplyr::mutate(tbl_checked = list(NULL))
  
  if (keep_tbl) {
    
    if (inherits(x$tbl, "tbl_dbi") || inherits(x$tbl, "tbl_spark")) {
      
      warning(
        "A table of class `tbl_dbi` or `tbl_spark` cannot be kept with the object.",
        call. = FALSE
      )
      
      x <- remove_tbl(x)
      x$validation_set$tbl_checked <- list()
      x$validation_set <-
        x$validation_set %>%
        dplyr::mutate(tbl_checked = list(NULL))
    }
    
  } else if (!keep_tbl) {
    x <- remove_tbl(x)
  }
  
  if (!keep_extracts) {
    x$extracts <- list()
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
    
  filename <- as.character(fs::path_expand(filename))

  saveRDS(x, file = filename)
}

#' Read a **pointblank** *agent* or *informant* from disk
#' 
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
#' Should the *agent* or *informant* possess a table-reading function (can be
#' set any time with [set_read_fn()]) or a specific table (settable with
#' [set_tbl()]) we could use the [interrogate()] or [incorporate()] function
#' again. For a *data quality reporting* workflow, it is useful to
#' [interrogate()] target tables that evolve over time. While the same
#' validation steps will be used, more can be added before calling
#' [interrogate()]. For an *information management* workflow with an *informant*
#' object, using [incorporate()] will update aspects of the reporting such as
#' table dimensions, and info snippets/text will be regenerated.
#' 
#' @param path The path to a file that was previously written by
#'   [x_write_disk()].
#' 
#' @family Object Ops
#' @section Function ID:
#' 8-2
#' 
#' @export
x_read_disk <- function(path) {
  readRDS(path)
}

#' Set a data table to an *agent* or *informant*
#' 
#' Setting a data table to an *agent* or *informant* with `set_tbl()` replaces
#' any associated table (a data frame, a tibble, objects of class `tbl_dbi` or
#' `tbl_spark`). If a data table is associated with an *agent* or *informant*
#' along with a table-reading function (settable in [create_agent()] and
#' [create_informant()]'s `read_fn` argument or with [set_read_fn()]), the
#' table-reading function will take precedence. If this is undesirable, it be
#' removed with the [remove_read_fn()] function. The association to a table can
#' be removed with with [remove_tbl()].
#'
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' @param tbl The input table for the `agent`. This can be a data frame, a
#'   tibble, a `tbl_dbi` object, or a `tbl_spark` object. Any table already
#'   associated with the *agent* or *informant* will be overwritten.
#' 
#' @family Object Ops
#' @section Function ID:
#' 8-3
#' 
#' @export
set_tbl <- function(x,
                    tbl) {
  
  # Set the table on the `$tbl` list element
  x$tbl <- tbl
  
  if (is_ptblank_agent(x)) {
    
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
#' Removing an *agent* or *informant*'s association to a data table can be done
#' with the `remove_tbl()` function. This can be useful to ensure that the table
#' data isn't unintentionally written to disk. It is usually best to avoid
#' directly associating a table to an *agent* or *informant*, instead opting for
#' setting a table-reading function (via [create_agent()] and
#' [create_informant()]'s `read_fn` argument, or, with [set_read_fn()]). The 
#' association to a table can be set again with [set_tbl()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#'   
#' @family Object Ops
#' @section Function ID:
#' 8-4
#'   
#' @export
remove_tbl <- function(x) {
  
  x$tbl <- NULL
  invisible(x)
}

#' Set a table-reading function to an *agent* or *informant*
#'
#' A table-reading function can be associated with an *agent* or *informant*
#' with `set_read_fn()`. Should both a `tbl` *and* a `read_fn` be associated
#' with the *agent* or *informant*, the `read_fn` will take priority. There are
#' two ways to specify a `read_fn`: (1) using a function (e.g., 
#' `function() { <table reading code> }`) or, (2) with an R formula expression
#' (e.g., `~ { <table reading code> }`). The table-reading function can removed
#' with [remove_read_fn()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#' @param read_fn A function that's used for reading in the data. This can be
#'   specified by using a function (e.g., `function() { <table reading code> }`)
#'   or an R formula expression (e.g., `~ { <table reading code> }`).
#'
#' @family Object Ops
#' @section Function ID:
#' 8-5
#'
#' @export
set_read_fn <- function(x,
                        read_fn) {
  
  x$read_fn <- read_fn
  invisible(x)
}

#' Remove a table-reading function associated with an *agent* or *informant*
#' 
#' Removing an *agent* or an *informant*'s association to a table-reading
#' function can be done with `remove_read_fn()`. This may be good idea in an
#' interactive session when instead relying on the direct association of a data
#' table (settable in [create_agent()] and [create_informant()]'s `tbl` argument
#' or with [set_tbl()]). The table-reading function can be set again with
#' [set_read_fn()].
#' 
#' @param x An *agent* object of class `ptblank_agent`, or, an *informant* of
#'   class `ptblank_informant`.
#'   
#' @family Object Ops
#' @section Function ID:
#' 8-6
#'   
#' @export
remove_read_fn <- function(x) {
  
  x$read_fn <- NULL
  invisible(x)
}
