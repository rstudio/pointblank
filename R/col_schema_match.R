#' Do columns in the table (and their types) match a predefined schema?
#'
#' The `col_schema_match()` validation step function works in conjunction with a
#' `col_schema` object (generated through the [col_schema()] function) to
#' determine whether the expected schema matches the target table. This
#' validation step operates over a single test unit, which is whether the schema
#' exactly matches that of the table. If the target table is a `tbl_sql` object,
#' we can choose to validate the column schema that is based on R column types
#' (e.g., `"numeric"`, `"character"`, etc.), or, SQL column types (e.g.,
#' `"double"`, `"varchar"`, etc.). That option is defined in the [col_schema()]
#' function (with the `.db_col_types` argument).
#' 
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation step function, takes a specially-crafted list
#' object that is best produced by the [action_levels()] function. Read that
#' function's documentation for the lowdown on how to create reactions to
#' above-threshold failure levels in validation. The basic gist is that you'll
#' want at least a single threshold level (specified as either the fraction test
#' units failed, or, an absolute value), often using the `warn_at` argument.
#' Using `action_levels(warn_at = 1)` or `action_levels(stop_at = 1)` are good
#' choices depending on the situation (the first produces a warning, the other
#' `stop()`s).
#' 
#' Want to describe this validation step in some detail? Keep in mind that this
#' is only useful if `x` is an *agent*. If that's the case, `brief` the agent
#' with some text that fits. Don't worry if you don't want to do it. The
#' *autobrief* protocol is kicked in when `brief = NULL` and a simple brief will
#' then be automatically generated.
#' 
#' @inheritParams col_vals_gt
#' @param schema A table schema of type `col_schema` which can be generated
#' using the [col_schema()] function.
#' 
#' @examples
#' # Create a simple table with
#' # two columns: one `integer` and
#' # the other `character`
#' tbl <- 
#'   dplyr::tibble(
#'     a = 1:5,
#'     b = letters[1:5]
#'   )
#' 
#' # Create a column schema object
#' # that describes the columns and
#' # their types (in the expected
#' # order)
#' schema_obj <- 
#'   col_schema(
#'     a = "integer",
#'     b = "character"
#'   )
#' 
#' # Validate that the schema object
#' # `col_schema_x` exactly defines
#' # the column names and column types
#' # of the `tbl_x` table
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' @family Validation Step Functions
#' @section Function ID:
#' 2-24
#' 
#' @name col_schema_match
NULL

#' @rdname col_schema_match
#' @import rlang
#' @export
col_schema_match <- function(x,
                             schema,
                             actions = NULL,
                             brief = NULL,
                             active = TRUE) {
  
  
  if (is_a_table_object(x)) {
    
    secret_agent <- create_agent(x, name = "::QUIET::") %>%
      col_schema_match(
        schema = schema,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>% interrogate()
    
    return(x)
  }
  
  agent <- x
  
  if (is.null(brief)) {
    
    brief <-
      create_autobrief(
        agent = agent,
        assertion_type = "col_schema_match"
      )
  }
  
  # Add a validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_schema_match",
      column = NA_character_,
      values = schema,
      preconditions = NULL,
      actions = actions,
      brief = brief,
      active = active
    )
}

#' @rdname col_schema_match
#' @import rlang
#' @export
expect_col_schema_match <- function(object,
                                    schema,
                                    threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, name = "::QUIET::") %>%
    col_schema_match(
      schema = {{ schema }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>% .$validation_set
  
  x <- vs$notify %>% all()
  f_failed <- vs$f_failed
  
  # TODO: express warnings and errors here
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue("The supplied `schema` did not match that of the table.")
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' Generate a table column schema manually or with a reference table
#' 
#' A table column schema object, as can be created by `col_schema()`, is
#' necessary when using the [col_schema_match()] validation step function (which
#' checks whether the table object under study matches a known column schema).
#' The `col_schema` object can be made by carefully supplying the column names
#' and their types as a set of named arguments, or, we could provide a table
#' object, which could by of the `data.frame`, `tbl_df`, or `tbl_dbi` varieties.
#' There's an additional option, which is just for validating the schema of a
#' `tbl_dbi` object: we can validate the schema based on R column types (e.g.,
#' `"numeric"`, `"character"`, etc.), or, SQL column types (e.g., `"double"`,
#' `"varchar"`, etc.). This is great if we want to validate table column schemas
#' both on the server side and when tabular data is collected and loaded into R.
#' 
#' @param ... A set of named arguments where the names refer to column names and
#'   the values are one or more column types.
#' @param .tbl An option to use a table object to define the schema. If this is
#'   provided then any values provided to `...` will be ignored.
#' @param .db_col_types Determines whether the column types refer to R column
#'   types (`"r"`) or SQL column types (`"sql"`).
#'   
#' @examples 
#' # Create a simple table with two
#' # columns: one `integer` and the
#' # other `character`
#' tbl <- 
#'   dplyr::tibble(
#'     a = 1:5,
#'     b = letters[1:5]
#'   )
#' 
#' # Create a column schema object
#' # that describes the columns and
#' # their types (in the expected
#' # order)
#' schema_obj <- 
#'   col_schema(
#'     a = "integer",
#'     b = "character"
#'   )
#' 
#' # Validate that the schema object
#' # `col_schema_x` exactly defines
#' # the column names and column types
#' # of the `tbl_x` table
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate()
#' 
#' # Determine if these three validation
#' # steps passed by using `all_passed()`
#' all_passed(agent)
#' 
#' # We can alternatively create
#' # a column schema object from a
#' # `tbl_df` object
#' schema_obj <-
#'   col_schema(
#'     .tbl = dplyr::tibble(
#'       a = integer(0),
#'       b = character(0)
#'     )
#'   )
#'
#' # This should provide the same
#' # interrogation results as in the
#' # previous example
#' create_agent(tbl = tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate() %>%
#'   all_passed()
#'   
#' @family Planning and Prep
#' @section Function ID:
#' 1-4
#' 
#' @export
col_schema <- function(...,
                       .tbl = NULL,
                       .db_col_types = c("r", "sql")) {
  
  db_col_types <- match.arg(.db_col_types)

  x <- list(...)
  
  # Apply the `col_schema` and the `r_type`/`sql_type` classes
  class(x) <- c(paste0(db_col_types, "_type"), "col_schema")
  
  if (!is.null(.tbl)) {
    
    # Validate .tbl object
    
    # Generate schema from tbl object
    if (inherits(.tbl, "data.frame")) {
      
      x <- create_col_schema_from_df(tbl = .tbl)
      
      # Apply the `col_schema` class
      class(x) <- c("r_type", "col_schema")
    }
    
    if (inherits(.tbl, "tbl_dbi")) {
      
      tbl_info <- get_tbl_information(tbl = .tbl)
      
      x <- 
        switch(
          db_col_types,
          "r" = create_col_schema_from_names_types(tbl_info$col_names, tbl_info$r_col_types),
          "sql" = create_col_schema_from_names_types(tbl_info$col_names, tbl_info$db_col_types)
        )
      
      # Apply the `col_schema` and the `r_type`/`sql_type` classes
      class(x) <- c(paste0(db_col_types, "_type"), "col_schema")
    }
  }
  
  x
}


r_col_type <- function(type) {
  # Generate a standardized vector for an `r_type`
}

db_col_type <- function(db_type) {
  # Generate a standardized vector for an `db_col_type`
}

create_col_schema_from_df <- function(tbl) {
  
  lapply(tbl, class)
}

create_col_schema_from_names_types <- function(names, types) {
  
  as.list(stats::setNames(types, names))
}
