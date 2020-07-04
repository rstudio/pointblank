#' Write an agent to disk (in reserve)
#' 
#' Writing an *agent* to disk with `agent_write()` is good practice for keeping
#' data validation intel close at hand for later retrieval (with
#' [agent_read()]). By default, any data table that the *agent* may have before
#' being committed to disk will be expunged. This behavior can be changed by
#' setting `keep_tbl` to `TRUE` but this only works in the case where the table
#' is not of the `tbl_dbi` class.
#'
#' It can be helpful to set a table-reading function to later reuse the agent
#' when read from disk through [agent_read()]. This can be done with the
#' `read_fn` argument of [create_agent()] or, later, with [set_read_fn()].
#' Alternatively, we can reintroduce the agent to a data table with the
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
#'   class `tbl_dbi`, the table is always removed (even if `keep_tbl` is set to
#'   `TRUE`).
#' 
#' @export
agent_write <- function(agent,
                        filename,
                        path = NULL,
                        keep_tbl = FALSE) {
  
  if (keep_tbl) {
    
    if (inherits(agent$tbl, "tbl_dbi")) {
      
      warning(
        "A table of class `tbl_dbi` cannot be kept with the agent.",
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

#' Read an agent from disk (redeployment)
#' 
#' @param path The path to the file that was previously written by
#'   [agent_write()].
#' 
#' @export
agent_read <- function(path) {
  readRDS(path)
}

#' Set a data table to an agent
#' 
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#' @param tbl The input table for the `agent`. This can be a data frame, a
#'   tibble, or a `tbl_dbi` object. Any table already associated with the
#'   *agent* will be overwritten.
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
#' @param agent An *agent* object of class `ptblank_agent` that is created with
#'   [create_agent()].
#'   
#' @export
remove_tbl <- function(agent) {
  
  agent$tbl <- NULL
  invisible(agent)
}

#' Set a table-reading function to an agent
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
#' @export
set_read_fn <- function(agent,
                        read_fn) {
  
  agent$read_fn <- read_fn
  invisible(agent)
}
