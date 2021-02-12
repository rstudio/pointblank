#' Define a store of tables with table-prep formulas
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
#' aspect of organizing table-prep formulas in a single object is supplying
#' it to the `read_fn` argument of [create_agent()] or [create_informant()] via
#' `$` notation (e.g, `create_agent(read_fn = <tbl_store>$<name>)`).
#' 
#' @param ... Expressions that contain table-prep formulas and table names for
#'   data retrieval. Two-sided formulas (e.g, `<LHS> ~ <RHS>`) are to be used,
#'   where the left-hand side is a given name and the right-hand is the portion
#'   that is is used to obtain the table.
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' 
#' @return A `tbl_store` object that contains table-prep formulas.
#' 
#' @examples 
#' # Define a `tbl_store` object by adding
#' # table-prep formulas inside the
#' # `tbl_store()` call
#' # tbls <- 
#' #   tbl_store(
#' #     small_table_duck ~ db_tbl(
#' #       table = small_table,
#' #       dbname = ":memory:",
#' #       dbtype = "duckdb"
#' #     ),
#' #     ~ db_tbl(
#' #       table = "rna",
#' #       dbname = "pfmegrnargs",
#' #       dbtype = "postgres",
#' #       host = "hh-pgsql-public.ebi.ac.uk",
#' #       port = 5432,
#' #       user = I("reader"),
#' #       password = I("NWDMCE5xdipIjRrp")
#' #     ),
#' #     all_revenue ~ db_tbl(
#' #       table = file_tbl(
#' #         file = from_github(
#' #           file = "all_revenue_large.rds",
#' #           repo = "rich-iannone/intendo",
#' #           subdir = "data-large"
#' #         )
#' #       ),
#' #       dbname = ":memory:",
#' #       dbtype = "duckdb"
#' #     ),
#' #     sml_table ~ pointblank::small_table
#' #   )
#' 
#' # Once this object is available, you can
#' # check that the table of interest is
#' # produced to your specification with the
#' # `tbl_get()` function
#' # tbl_get(
#' #   tbl = "small_table_duck",
#' #   store = tbls
#' # )
#' 
#' # Another simpler way to get the same
#' # table materialized is by using `$` to
#' # get the entry of choice for `tbl_get()`
#' # tbls$small_table_duck %>% tbl_get()
#' 
#' # Creating an agent is easy when all
#' # table-prep formulas are encapsulated
#' # in a `tbl_store` object; use `$` notation
#' # to pass the appropriate procedure for
#' # reading a table to the `read_fn` argument
#' # agent <-
#' #   create_agent(read_fn = tbls$small_table_duck)
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
      name_list <- 
        c(name_list, paste0("tbl_", formatC(i, width = 3, flag = "0")))
      
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

#' Obtain a table-prep formula from a `tbl_store` object
#' 
#' @description
#' With a `tbl_store` object at the ready, any table-prep formula within it can
#' be extracted with the `tbl_source()` function. Should you need to obtain the
#' table itself (generated via the table-prep formula), then the [tbl_get()]
#' function is useful for that.
#' 
#' @param tbl The table name associated with a table-prep formula. This is part
#'   of the table `store`. This table could be identified by its name (e.g.,
#'   `tbl = "large_table"`) or by supplying a reference using a subset (with
#'   `$`) of the `tbl_store` object (e.g., `tbl = store$large_table`). If using
#'   the latter method then nothing needs to be supplied to `store`.
#' @param store Either a table store object created by the [tbl_store()]
#'   function or a path to a table store YAML file created by [yaml_write()].
#' 
#' @return A table-prep formula.
#' 
#' @examples 
#' # Let's create a `tbl_store` object by
#' # giving two table-prep formulas to
#' # `tbl_store()`
#' tbls <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' 
#' # We can pass a table-prep formula
#' # to `create_agent()` and interrogate
#' # the table shortly thereafter
#' agent <- 
#'   create_agent(
#'     read_fn = ~ tbl_source("sml_table", tbls),
#'     label = "An example that uses a table store.",
#'     actions = action_levels(warn_at = 0.10)
#'   ) %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   col_vals_regex(
#'     vars(b), "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   interrogate()
#'
#' # Both the `tbl_store` object and the
#' # `agent` can be transformed to YAML with
#' # the `yaml_write()` function
#' 
#' # This writes the `tbl_store.yml` file
#' # by default (a different name could be used)
#' # yaml_write(tbls)
#' 
#' # Let's modify the agent's `read_fn` to point
#' # to the YAML representation of the `tbl_store`
#' agent <-
#'   agent %>% 
#'   set_read_fn(
#'     ~tbl_source("sml_table", "tbl_store.yml")
#'   )
#' 
#' # Then we can write agent to a YAML file
#' # (it's `agent-sml_table.yml` by default)
#' # yaml_write(agent)
#' 
#' # Now that both are in this on-disk format
#' # an interrogation can be done by accessing
#' # the agent YAML
#' # yaml_agent_interrogate("agent-sml_table.yml")
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-9
#' 
#' @export
tbl_source <- function(tbl,
                       store = NULL) {
  
  # If `store` is supplied as a character vector, assume it is a
  # file path to a YAML file
  if (is.character(store)) {
    store <- yaml_read_tbl_store(filename = store)
  }
  
  # TODO: store can be a `tbl_store` object or a
  # YAML file with entries under `tbls` or `tbl_store`
  if (is.character(tbl) && tbl %in% names(store)) {
    tbl_entry <- store[[tbl]]
  } else if (inherits(tbl, "read_fn")) {
    tbl_entry <- tbl
  }
  
  tbl_entry
}

#' Obtain a table from a `tbl_store` object
#' 
#' @description
#' With a `tbl_store` object at the ready, any table referenced in that store
#' can be materialized by providing a matching table name. The [tbl_store()]
#' function is used to create a store of tables, which is a catalog of table-
#' prep formulas with names supplied for each of the tables. The `tbl_get()`
#' function does the work of evaluating a table-prep formula and returning the
#' requested table.
#' 
#' @param tbl The table to retrieve from a table `store`. This table could be
#'   identified by its name (e.g., `tbl = "large_table"`) or by supplying a
#'   reference using a subset (with `$`) of the `tbl_store` object (e.g.,
#'   `tbl = store$large_table`). If using the latter method then nothing needs
#'   to be supplied to `store`.
#' @param store Either a table store object created by the [tbl_store()]
#'   function or a path to a table store YAML file created by [yaml_write()].
#' 
#' @return A table object.
#' 
#' @examples 
#' # Define a `tbl_store` object by adding
#' # table-prep formulas in `tbl_store()`
#' # tbls <- 
#' #   tbl_store(
#' #     small_table_duck ~ db_tbl(
#' #       table = small_table,
#' #       dbname = ":memory:",
#' #       dbtype = "duckdb"
#' #     ),
#' #     ~ db_tbl(
#' #       table = "rna",
#' #       dbname = "pfmegrnargs",
#' #       dbtype = "postgres",
#' #       host = "hh-pgsql-public.ebi.ac.uk",
#' #       port = 5432,
#' #       user = I("reader"),
#' #       password = I("NWDMCE5xdipIjRrp")
#' #     ),
#' #     all_revenue ~ db_tbl(
#' #       table = file_tbl(
#' #         file = from_github(
#' #           file = "all_revenue_large.rds",
#' #           repo = "rich-iannone/intendo",
#' #           subdir = "data-large"
#' #         )
#' #       ),
#' #       dbname = ":memory:",
#' #       dbtype = "duckdb"
#' #     ),
#' #     sml_table ~ pointblank::small_table
#' #   )
#' 
#' # Once this object is available, you can
#' # check that the table of interest is
#' # produced to your specification
#' # tbl_get(
#' #   tbl = "small_table_duck",
#' #   store = tbls
#' # )
#' 
#' # An alternative method for getting the
#' # same table materialized is by using `$`
#' # to get the formula of choice from `tbls`
#' # tbls$small_table_duck %>% tbl_get()
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-10
#' 
#' @export
tbl_get <- function(tbl,
                    store = NULL) {
  
  # Get the table-prep formula with the `tbl_source()` function
  tbl_entry <- tbl_source(tbl = tbl, store = store)
  
  # Obtain the table object
  tbl_obj <- 
    rlang::f_rhs(tbl_entry) %>%
    rlang::eval_tidy()
  
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



yaml_read_tbl_store <- function(filename) {
  
  # Read the YAML file with `yaml::read_yaml()`
  y <- yaml::read_yaml(file = filename)
  
  table_names <- names(y[[1]])
  table_formulas <- 
    unlist(y, recursive = FALSE, use.names = FALSE) %>%
    unlist()
  
  statements <- paste(table_names, table_formulas)
  
  # Generate the expression string
  expr_str <-
    paste0(
      "tbl_store(\n",
      paste(paste0("  ", statements), collapse = ",\n"), "\n",
      ")"
    )

  tbl_store <- 
    expr_str %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
    
  tbl_store
}
