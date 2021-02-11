#' Define a store of tables with table-reading functions
#' 
#' @description
#' It can be useful to set up all the data sources you need and just draw from
#' them when necessary. This upfront configuration with `tbl_store()` can be
#' quite useful since we can define the methods for obtaining tabular data from
#' mixed sources (e.g., database tables, tables generated from flat files, etc.)
#' and provide names for these data pulling procedures as a way to access the
#' data with [tbl_get()]. Data preparation could be a part of what's in the
#' store (imagine procuring several mutated variations of the same source table
#' or pre-filtering a database table according to the system time). Another nice
#' aspect of organizing table-reading functions in a single object is supplying
#' it to the `read_fn` argument of [create_agent()] or [create_informant()] via
#' `$` notation (e.g, `create_agent(read_fn = <tbl_store>$<name>)`).
#' 
#' @param ... Expressions that contain table-reading functions and table names
#'   for data retrieval. Two-sided formulas (e.g, `<LHS> ~ <RHS>`) are to be
#'   used, where the left-hand side is a given name and the right-hand is a
#'   table-reading function call (i.e., code that is used to obtain the table).
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' 
#' @return A `tbl_store` object that contains table-reading functions.
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-8
#' 
#' @export
tbl_store <- function(...,
                      .list = list2(...)) {
  
  # Collect a fully or partially named list of tables
  tbl_list <- .list
  
  # Check that every list element is a formula
  for (i in seq_along(tbl_list)) {
    
    if (!inherits(tbl_list[[i]], "formula")) {
      stop(
        "Each entry to `tbl_catalog()` must be a formula.",
        call. = FALSE
      )
    }
  }
  
  # Get names for every entry in the list
  name_list <- c()
  has_given_name <- c()
  for (i in seq_along(tbl_list)) {
    
    if (is.null(rlang::f_lhs(tbl_list[[i]]))) {
      
      # TODO: Try to get the name if present in `db_tbl()` or `file_tbl()`;
      # for now, just use the index number formatted as string
      name_list <- c(name_list, formatC(i, width = 3, flag = "0"))
      has_given_name <- c(has_given_name, FALSE)
      
    } else if (inherits(rlang::f_lhs(tbl_list[[i]]), "name")) {
      name_list <- c(name_list, as.character(rlang::f_lhs(tbl_list[[i]])))
      has_given_name <- c(has_given_name, TRUE)
    }
  }

  tbl_list <- rlang::set_names(tbl_list, name_list)
  
  for (i in seq_along(tbl_list)) {
    if (has_given_name[i]) {
      class(tbl_list[[i]]) <- c("with_tbl_name", "read_fn")
    } else {
      class(tbl_list[[i]]) <- "read_fn"  
    }
  }
  
  class(tbl_list) <- "tbl_store"
  
  tbl_list
}

#' Obtain a table from a `tbl_store` object
#' 
#' @description
#' With a `tbl_store` object at the ready, any table referenced in that store
#' can be materialized by providing a matching table name. The [tbl_store()]
#' function is used to create a store of tables, which is a catalog of table-
#' reading functions with names supplied for each of the tables. The `tbl_get()`
#' function does the work of evaluating a table-reading function and returning
#' the requested table.
#' 
#' @param tbl The table to retrieve from a table `store`. This table could be
#'   identified by its name (e.g., `tbl = "large_table"`) or by supplying a
#'   reference using a subset (with `$`) of the `tbl_store` object (e.g.,
#'   `tbl = store$large_table`). If using the latter method then nothing needs
#'   to be supplied to `store`.
#' @param store The table store object created by the [tbl_store()] function.
#' 
#' @return A table object.
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-9
#' 
#' @export
tbl_get <- function(tbl,
                    store = NULL) {
  
  # TODO: store can be a `tbl_store` object or a
  # YAML file with entries under `tbls` or `tbl_store`
  if (is.character(tbl) && tbl %in% names(store)) {
    tbl_entry <- store[[tbl]]
  } else if (inherits(tbl, "read_fn")) {
    tbl_entry <- tbl
  }
  
  # Obtain the table object
  tbl_obj <- rlang::f_rhs(tbl_entry) %>% rlang::eval_tidy()
  
  # Add the in-store table name to the `pb_tbl_name` attribute
  # of the retrieved table
  if (
    !is.null(rlang::f_lhs(tbl_entry)) &&
    is.null(attr(tbl, "pb_tbl_name", exact = TRUE))
  ) {
    table_name <- as.character(rlang::f_lhs(tbl_entry))
    attr(tbl_obj, "pb_tbl_name") <- table_name
  }
  
  # Add the retrieval time to the `pb_tbl_name` attribute
  # of the table if it isn't present
  if (is.null(attr(tbl, "pb_access_time", exact = TRUE))) {
    
    access_time <- Sys.time()
    attr(tbl_obj, "pb_access_time") <- access_time
  }
  
  suppressWarnings(tbl_obj)
}