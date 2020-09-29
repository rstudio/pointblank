#' Do columns in the table (and their types) match a predefined schema?
#'
#' The `col_schema_match()` validation function, the `expect_col_schema_match()`
#' expectation function, and the `test_col_schema_match()` test function all
#' work in conjunction with a `col_schema` object (generated through the
#' [col_schema()] function) to determine whether the expected schema matches
#' that of the target table. The validation function can be used directly on a
#' data table or with an *agent* object (technically, a `ptblank_agent` object)
#' whereas the expectation and test functions can only be used with a data
#' table. The types of data tables that can be used include data frames,
#' tibbles, database tables (`tbl_dbi`), and Spark DataFrames (`tbl_spark`).
#' Each validation step or expectation will operate over a single test unit,
#' which is whether the column is an integer-type column or not. The validation
#' step or expectation operates over a single test unit, which is whether the
#' schema matches that of the table (within the constraints enforced by the
#' `complete` and `in_order` options). If the target table is a `tbl_dbi` or a
#' `tbl_spark` object, we can choose to validate the column schema that is based
#' on R column types (e.g., `"numeric"`, `"character"`, etc.), SQL column types
#' (e.g., `"double"`, `"varchar"`, etc.), or Spark SQL types (e.g,.
#' `"DoubleType"`, `"StringType"`, etc.). That option is defined in the
#' [col_schema()] function (it is the `.db_col_types` argument).
#'
#' Often, we will want to specify `actions` for the validation. This argument,
#' present in every validation function, takes a specially-crafted list object
#' that is best produced by the [action_levels()] function. Read that function's
#' documentation for the lowdown on how to create reactions to above-threshold
#' failure levels in validation. The basic gist is that you'll want at least a
#' single threshold level (specified as either the fraction of test units
#' failed, or, an absolute value), often using the `warn_at` argument. Using
#' `action_levels(warn_at = 1)` or `action_levels(stop_at = 1)` are good choices
#' depending on the situation (the first produces a warning, the other
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
#'   using the [col_schema()] function.
#' @param complete A requirement to account for all table columns in the
#'   `schema`. By default, this is `TRUE` and so that all column names in the
#'   target table must be present in the schema object. This restriction can be
#'   relaxed by using `FALSE`, where we can provide a subset of table columns in
#'   the schema.
#' @param in_order A stringent requirement for enforcing the order of columns in
#'   the provided `schema`. By default, this is `TRUE` and the order of columns
#'   in both the schema and the target table must match. By setting to `FALSE`,
#'   this strict order requirement is removed.
#' 
#' @return For the validation function, the return value is either a
#'   `ptblank_agent` object or a table object (depending on whether an agent
#'   object or a table was passed to `x`). The expectation function invisibly
#'   returns its input but, in the context of testing data, the function is
#'   called primarily for its potential side-effects (e.g., signaling failure).
#'   The test function returns a logical value.
#' 
#' @examples
#' 
#' # For all examples here, we'll use
#' # a simple table with two columns:
#' # one `integer` (`a`) and the other
#' # `character` (`b`); the following
#' # examples will validate that the
#' # table columns abides match a schema
#' # object as created by `col_schema()`
#' tbl <- 
#'   dplyr::tibble(
#'     a = 1:5,
#'     b = letters[1:5]
#'   )
#'   
#' tbl
#' 
#' # Create a column schema object with
#' # the helper function `col_schema()`
#' # that describes the columns and
#' # their types (in the expected order)
#' schema_obj <- 
#'   col_schema(
#'     a = "integer",
#'     b = "character"
#'   )
#'   
#' # A: Using an `agent` with validation
#' #    functions and then `interrogate()`
#' 
#' # Validate that the schema object
#' # `schema_obj` exactly defines
#' # the column names and column types
#' agent <-
#'   create_agent(tbl) %>%
#'   col_schema_match(schema_obj) %>%
#'   interrogate()
#' 
#' # Determine if this validation
#' # had no failing test units (there is
#' # a single test unit governed by
#' # whether there is a match)
#' all_passed(agent)
#' 
#' # Calling `agent` in the console
#' # prints the agent's report; but we
#' # can get a `gt_tbl` object directly
#' # with `get_agent_report(agent)`
#' 
#' # B: Using the validation function
#' #    directly on the data (no `agent`)
#' 
#' # This way of using validation functions
#' # acts as a data filter: data is passed
#' # through but should `stop()` if there
#' # is a single test unit failing; the
#' # behavior of side effects can be
#' # customized with the `actions` option
#' tbl %>% col_schema_match(schema_obj)
#'
#' # C: Using the expectation function
#' 
#' # With the `expect_*()` form, we would
#' # typically perform one validation at a
#' # time; this is primarily used in
#' # testthat tests
#' expect_col_schema_match(tbl, schema_obj)
#' 
#' # D: Using the test function
#' 
#' # With the `test_*()` form, we should
#' # get a single logical value returned
#' # to us
#' tbl %>% test_col_schema_match(schema_obj)
#' 
#' @family validation functions
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
                             complete = TRUE,
                             in_order = TRUE,
                             actions = NULL,
                             step_id = NULL,
                             label = NULL,
                             brief = NULL,
                             active = TRUE) {

  if (!inherits(schema, "col_schema")) {
    stop("A `col_schema` object must be provided to `schema`:\n",
         "* A schema can be defined using the `col_schema()` function",
         call. = FALSE)
  }
  
  # Incorporate `complete` and `in_order` options into
  # the `schema` object
  if (is.null(schema$`__complete__`) && is.null(schema$`__in_order__`)) {
    
    schema <- 
      structure(
        c(schema, list(`__complete__` = complete, `__in_order__` = in_order)),
        class = c("match_options", class(schema))
      )
  }
  
  if (is_a_table_object(x)) {

    secret_agent <- 
      create_agent(x, label = "::QUIET::") %>%
      col_schema_match(
        schema = schema,
        label = label,
        brief = brief,
        actions = prime_actions(actions),
        active = active
      ) %>%
      interrogate()
    
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
  
  # Normalize any provided `step_id` value(s)
  step_id <- normalize_step_id(step_id, columns = "column", agent)
  
  # Check `step_id` value(s) against all other `step_id`
  # values in earlier validation steps
  check_step_id_duplicates(step_id, agent)
  
  # Add a validation step
  agent <-
    create_validation_step(
      agent = agent,
      assertion_type = "col_schema_match",
      column = NA_character_,
      values = schema,
      preconditions = NULL,
      actions = covert_actions(actions, agent),
      step_id = step_id,
      label = label,
      brief = brief,
      active = active
    )
}

#' @rdname col_schema_match
#' @import rlang
#' @export
expect_col_schema_match <- function(object,
                                    schema,
                                    complete = TRUE,
                                    in_order = TRUE,
                                    threshold = 1) {
  
  fn_name <- "expect_col_schema_match"
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_schema_match(
      schema = {{ schema }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  x <- vs$notify %>% all()
  
  threshold_type <- get_threshold_type(threshold = threshold)
  
  if (threshold_type == "proportional") {
    failed_amount <- vs$f_failed
  } else {
    failed_amount <- vs$n_failed
  }
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  act <- testthat::quasi_label(enquo(x), arg = "object")
  
  testthat::expect(
    ok = identical(!as.vector(act$val), TRUE),
    failure_message = glue::glue(failure_message_gluestring(fn_name = fn_name, lang = "en"))
  )
  
  act$val <- object
  
  invisible(act$val)
}

#' @rdname col_schema_match
#' @import rlang
#' @export
test_col_schema_match <- function(object,
                                  schema,
                                  complete = TRUE,
                                  in_order = TRUE,
                                  threshold = 1) {
  
  vs <- 
    create_agent(tbl = object, label = "::QUIET::") %>%
    col_schema_match(
      schema = {{ schema }},
      actions = action_levels(notify_at = threshold)
    ) %>%
    interrogate() %>%
    .$validation_set
  
  if (inherits(vs$capture_stack[[1]]$warning, "simpleWarning")) {
    warning(conditionMessage(vs$capture_stack[[1]]$warning))
  }
  if (inherits(vs$capture_stack[[1]]$error, "simpleError")) {
    stop(conditionMessage(vs$capture_stack[[1]]$error))
  }
  
  all(!vs$notify)
}

#' Generate a table column schema manually or with a reference table
#' 
#' A table column schema object, as can be created by `col_schema()`, is
#' necessary when using the [col_schema_match()] validation function (which
#' checks whether the table object under study matches a known column schema).
#' The `col_schema` object can be made by carefully supplying the column names
#' and their types as a set of named arguments, or, we could provide a table
#' object, which could be of the `data.frame`, `tbl_df`, `tbl_dbi`, or
#' `tbl_spark` varieties. There's an additional option, which is just for
#' validating the schema of a `tbl_dbi` or `tbl_spark` object: we can validate
#' the schema based on R column types (e.g., `"numeric"`, `"character"`, etc.),
#' SQL column types (e.g., `"double"`, `"varchar"`, etc.), or Spark SQL column
#' types (`"DoubleType"`, `"StringType"`, etc.). This is great if we want to
#' validate table column schemas both on the server side and when tabular data
#' is collected and loaded into R.
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
#' # `schema_obj` exactly defines
#' # the column names and column types
#' # of the `tbl` table
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
#' 1-7
#' 
#' @export
col_schema <- function(...,
                       .tbl = NULL,
                       .db_col_types = c("r", "sql")) {
  
  db_col_types <- match.arg(.db_col_types)

  x <- list(...)
  
  # Transform SQL column types to lowercase to allow
  # both uppercase and lowercase conventions while
  # standardizing the input
  if (db_col_types == "sql") {
    x <- lapply(x, tolower)
  }
  
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
    
    if (inherits(.tbl, "tbl_dbi") || inherits(.tbl, "tbl_spark")) {
      
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
