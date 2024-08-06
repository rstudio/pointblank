#------------------------------------------------------------------------------#
# 
#                 _         _    _      _                _    
#                (_)       | |  | |    | |              | |   
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |                                                        
#  |_|                                                        
#  
#  This file is part of the 'rstudio/pointblank' project.
#  
#  Copyright (c) 2017-2024 pointblank authors
#  
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
# 
#------------------------------------------------------------------------------#


is_ptblank_agent <- function(x) {
  inherits(x, "ptblank_agent")
}

is_ptblank_informant <- function(x) {
  inherits(x, "ptblank_informant")
}

is_ptblank_x_list <- function(x) {
  inherits(x, "x_list")
}

has_agent_intel <- function(agent) {
  inherits(agent, "has_intel")
}

get_tbl_object <- function(agent) {
  
  if (!is.null(agent$tbl)) {
    
    tbl <- agent$tbl
    
  } else if (!is.null(agent$read_fn)) {
    
    tbl <- materialize_table(agent$read_fn)
    
  } else {
    tbl <- NULL
  }
  
  tbl
}

is_a_table_object <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl_dbi", "tbl_spark"))
}

is_tbl_df <- function(x) {
  inherits(x, c("data.frame", "tbl_df"))
}

is_tbl_dbi <- function(x) {
  inherits(x, "tbl_dbi")
}

# nocov start

is_tbl_spark <- function(x) {
  inherits(x, "tbl_spark")
}

is_tbl_bigquery <- function(x) {
  inherits(x, "tbl_BigQueryConnection")
}

is_arrow_object <- function(x) {
  inherits(x, "ArrowObject")
}

is_tbl_mssql <- function(x) {
  
  if (!is_tbl_dbi(x)) {
    return(FALSE)
  } 
  
  tbl_src_details <- tolower(get_tbl_dbi_src_details(x))
  grepl("sql server|sqlserver", tbl_src_details)
}

# nocov end

# Generate a label for the `agent` or `informant` object
generate_label <- function(label = NULL) {
  
  if (!is.null(label)) return(as.character(label))
  
  paste0("[", gsub(" ", "|", strftime(Sys.time())), "]")
}

safely_transformer <- function(otherwise = NA) {
  
  oth <- otherwise
  
  function(text, envir) {
    tryCatch(
      eval(parse(text = text, keep.source = FALSE), envir),
      error = function(e) if (is.language(oth)) eval(oth) else oth)
  }
}

glue_safely <- function(
    ...,
    .otherwise = NA,
    .envir = parent.frame()
) {
  
  as.character(
    glue::glue(
      ...,
      .transformer = safely_transformer(.otherwise),
      .envir = .envir
    )
  )
}

is_agent_empty <- function(agent) {
  if (!is_ptblank_agent(agent)) return(FALSE)
  if (nrow(agent$validation_set) == 0) TRUE else FALSE
}

interrogation_time <- function(agent) {
  if (has_agent_intel(agent)) agent$time_start else NA
}

number_of_validation_steps <- function(agent) {
  if (is_ptblank_agent(agent)) agent$validation_set %>% nrow() else NA
}

# Get the next step number for the `validation_set` tibble
get_next_validation_set_row <- function(agent) {
  
  if (nrow(agent$validation_set) == 0) {
    step <- 1L
  } else {
    step <- max(agent$validation_set$i) + 1L
  }
  step
}

tidyselect_helpers <- function() {
  names(tidyselect::vars_select_helpers)
}

get_assertion_type_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "assertion_type"]]
}

get_column_as_sym_at_idx <- function(agent, idx) {
  column <- unlist(agent$validation_set[[idx, "column"]])
  if (!is.na(column)) {
    column <- rlang::sym(gsub("'", "", column))
  }
  column
}

get_values_at_idx <- function(agent, idx) {
  
  # Get list-column element (`values` is always a length-1 list)
  values <- agent$validation_set[[idx, "values"]]
  
  # Expressions (via `col_vals_expr()`) and functions (via `specially()`)
  # can get the old `unlist()` treatment
  if (rlang::is_expression(values[[1]]) || rlang::is_function(values[[1]])) {
    values <- unlist(values, recursive = FALSE)
  } else {
    # In other cases (e.g., `values`, `left`, `right`), flatten with subsetting
    # to preserve class
    values <- values[[1]]
  }
  
  values
}

get_column_na_pass_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "na_pass"]]
}

get_all_cols <- function(agent) {
  agent$col_names
}

materialize_table <- function(tbl, check = TRUE) {
  
  if (is.null(tbl)) {
    stop("A table must be provided.", call. = FALSE)
  }
  
  if (is_a_table_object(tbl)) {
    return(tbl)
  } else if (inherits(tbl, "function")) {
    tbl <- rlang::exec(tbl)
  } else if (rlang::is_formula(tbl)) {
    
    tbl <- tbl %>% rlang::f_rhs() %>% rlang::eval_tidy()
    
    if (inherits(tbl, "read_fn")) {
      tbl <- tbl %>% rlang::f_rhs() %>% rlang::eval_tidy()
    }
    
  } else {
    
    stop(
      "The `tbl` object must either be a table, a function, or a formula.\n",
      "* A table-prep formula can be used (with the expression on the RHS).\n",
      "* A function can be made with `function()` {<tbl reading code>}.",
      call. = FALSE
    )
  }  
  
  if (check) {
    is_a_table_object(tbl)
  }
  
  tbl
}

as_columns_expr <- function(columns) {
  columns_expr <- gsub("^\"|\"$", "", rlang::as_label(columns))
  # Treat NULL and missing `columns` as the same
  if (columns_expr == "<empty>") {
    columns_expr <- "NULL"
  }
  columns_expr
}

is_secret_agent <- function(x) {
  is_ptblank_agent(x) && (x$label == "::QUIET::")
}

resolve_label <- function(label, columns = "", segments = "") {
  n_columns <- length(columns)
  n_segments <- length(segments)
  n_combinations <- n_columns * n_segments
  # If label is NULL, turn into NA for vector storage
  if (is.null(label)) {
    label <- NA_character_
  }
  # If length-1, match length of col-x-seg combination
  if (length(label) == 1) {
    label <- rep_len(label, n_combinations)
  }
  # Check for length match
  if (length(label) != n_combinations) {
    rlang::abort(paste0("`label` must be length 1 or ", n_combinations,
                        ", not ", length(label)))
  }
  # Create a columns * segments matrix of the (recycled) label vector
  # - Fill by row to preserve order (for loops iterate the j before the i)
  out <- matrix(label, nrow = n_columns, ncol = n_segments, byrow = TRUE)
  # If missing columns and/or segments, collapse to vector/scalar
  if (missing(columns) || missing(segments)) {
    out <- as.vector(out)
  }
  # A matrix/vector subsettable via `out[col]`, `out[seg]`, or `out[col,seg]`
  out
}

#' The `resolve_segments()` function works with input from the `segments`
#' argument, present is a variety of row-based validation functions.
#' 
#' @return A segment list with <column_name> = <column_value>
#' @noRd
resolve_segments <- function(x, seg_expr, preconditions) {
  
  # Return a list with an NA_character_ vector if the `seg_expr` is NULL
  if (is.null(seg_expr)) {
    
    empty_vec <- c("empty" = NA_character_)
    names(empty_vec) <- NA_character_
    
    return(as.list(empty_vec))
  }
  
  # Upgrade single item to a list
  if (
    rlang::is_formula(seg_expr) ||
    inherits(seg_expr, "quosures")
  ) {
    seg_expr <- list(seg_expr)
  }
  
  # Verify that `seg_expr` is a list
  if (!is.list(seg_expr)) {
    stop(
      "The `segments` value should be a list of two-sided formulas",
      call. = FALSE
    )
  }
  
  segments_list <- list()
  
  # Process each `seg_expr` element
  for (i in seq_along(seg_expr)) {
    
    if (inherits(seg_expr[[i]], "quosures")) {
      
      if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
        tbl <- x
      } else if (inherits(x, ("ptblank_agent"))) {
        tbl <- get_tbl_object(agent = x)
      }
      
      if (!is.null(preconditions)) {
        tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)
      }
      
      for (j in seq_along(seg_expr[[i]])) {
        
        column_name <- rlang::as_label(seg_expr[[i]][[j]])
        
        col_seg_vals <- 
          tbl %>%
          dplyr::select(tidyselect::all_of(column_name)) %>%
          dplyr::distinct() %>%
          dplyr::pull()
        
        names(col_seg_vals) <- rep(column_name, length(col_seg_vals))
        
        segments_list <- c(segments_list, as.list(col_seg_vals))
      }
    }
    
    if (rlang::is_formula(seg_expr[[i]])) {
  
      group_formula <- seg_expr[[i]]
      
      # Determine if this is a two-sided formula
      if (
        is.null(rlang::f_lhs(group_formula)) ||
        is.null(rlang::f_rhs(group_formula))
      ) {
        stop(
          "Any formulas provided for `segments` must be two-sided",
          call. = FALSE
        )
      }
      
      col_seg_vals <- rlang::eval_bare(rlang::f_rhs(group_formula))
      
      column_name <- rlang::as_label(rlang::f_lhs(group_formula))
      
      if (grepl("^vars\\(.+\\)$", column_name)) {
        column_name <- gsub("(vars\\(|\\))", "", column_name)
      }
      
      names(col_seg_vals) <- rep(column_name, length(col_seg_vals))
      
      segments_list <- c(segments_list, as.list(col_seg_vals))
    }
  }
  
  segments_list
}

normalize_step_id <- function(step_id, columns, agent) {

  if (is.null(step_id)) {

    if (length(agent$validation_set$i) < 1) {
      i <- 1
    } else {
      i <- max(agent$validation_set$i) + 1
    }
    
    steps <- length(columns) - 1
    i <- seq(from = i, to = i + steps)
    
    # Generate an ID based on `i`
    step_id <- formatC(i, width = 4, format = "d", flag = "0")
    return(step_id)
  }
  
  # Ensure that `step_id` is coerced to character
  step_id <- as.character(step_id)
  
  if (length(columns) == 1) {
    
    if (length(step_id) > 1) {
      warning(
        "Multiple `step_id` values provided for a single column:\n",
        "* Only the first `step_id` element will be used.",
        call. = FALSE
      )
    }
    
    return(step_id[1])
    
  } else if (length(columns) > 1) {
    
    if (length(step_id) == length(columns)) {
      
     if (anyDuplicated(step_id) != 0) {
       
       # Issue warning about duplicated `step_id` values
       warning(
         "Duplicate `step_id` values provided:\n",
         "* Only the first `step_id` element will be used.",
         call. = FALSE
       )
       
       step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
     }
    
      return(step_id)
      
    } else if (length(step_id) == 1) {
      
      # Generate multiple `step_id` values with single `step_id`
      step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
      
    } else if (length(step_id) != 1 && length(step_id) != length(columns)) {

      # Issue warning about length of `step_id`
      warning(
        "The number of `step_id` values is neither `1` nor the ",
        "number of `columns`:\n",
        "* Only the first `step_id` element will be used.",
        call. = FALSE
      )
      
      step_id <- generate_indexed_vals(step_id[1], seq_along(columns))
    }
  }
  
  step_id
}

check_step_id_duplicates <- function(step_id, agent) {
  
  if (any(step_id %in% agent$validation_set$step_id)) {

    error_at_index <- max(agent$validation_set$i)
    a_duplicate_step_id <- step_id[step_id %in% agent$validation_set$step_id][1]
    
    stop(
      "Just after step index `", error_at_index,
      "`, the following `step_id` has been ",
      "seen as used in a previous validation step:\n",
      "* \"", a_duplicate_step_id, "\"",
      call. = FALSE
    )
  }
}

check_is_a_table_object <- function(tbl) {

  if (!is_a_table_object(tbl)) {
    
    stop(
      "The object supplied is not a table object, valid tables are:\n",
      "* data frames, tibbles, or data.table objects\n",
      "* database tables (`tbl_dbi`)\n",
      "* `tbl_spark` objects (via the sparklyr package)",
      call. = FALSE
    )
  }
}

# This function verifies that some target table input is provided
# A portion of this is dedicated to checking the `read_fn` argument which
# is undergoing soft deprecation in `create_agent()` and `create_informant()`
check_table_input <- function(tbl, read_fn) {
  
  if (is.null(tbl) && is.null(read_fn)) {
    
    stop(
      "The `tbl` argument requires a table, this could either be:\n",
      " * An in-memory table, a database table, or a Spark table\n",
      " * A function or table-prep formula for getting a table during ",
      "interrogation",
      call. = FALSE
    )
  }
  
  if (!is.null(read_fn)) {
    
    warning(
      "Use `tbl` to specify a target table (`read_fn` is now undergoing ",
      "deprecation):\n",
      " * `tbl` can now accept a table-prep formula or a function to ",
      "get the target table at interrogation-time, and\n",
      " * A table can be supplied directly to `tbl` (as before)\n",
      call. = FALSE
    )
    
    if (!is.null(tbl)) {
      message(
        "The value supplied to `read_fn` has ignored since `tbl` ",
        "is also defined."
      )
    } else {
      tbl <- read_fn
    }
  }
  
  tbl
}

process_table_input <- function(tbl, tbl_name) {
  
  if (inherits(tbl, "function")) {
    
    read_fn <- tbl
    tbl <- NULL
    
  } else if (rlang::is_formula(tbl)) {
    
    read_fn <- tbl
    tbl <- NULL
    
    if (inherits(read_fn, "read_fn")) {
      
      if (inherits(read_fn, "with_tbl_name") && is.na(tbl_name)) {
        tbl_name <- read_fn %>% rlang::f_lhs() %>% as.character()
      }
    }
    
  } else {
    read_fn <- NULL
  }
  
  # Remove grouping for data frames or tibbles
  if (is_tbl_df(tbl) && dplyr::is_grouped_df(tbl)) {
    tbl <- dplyr::ungroup(tbl)
  }
  
  list(
    tbl = tbl,
    read_fn = read_fn,
    tbl_name = tbl_name
  )
}

generate_indexed_vals <- function(x, numbers, sep = ".") {
  paste0(x, sep, formatC(numbers, width = 4, format = "d", flag = "0"))
}

get_threshold_type <- function(threshold) {
  
  if (threshold >= 1) {
    threshold_type <- "absolute"
  } else if (threshold >= 0 && threshold < 1) {
    threshold_type <- "proportional"
  }
}

# nocov start

all_data_vars <- function(x, data_cols) {
  deparsed <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  reparsed <- parse(text = deparsed, keep.source = TRUE)
  x <- utils::getParseData(reparsed)
  if (is.null(x)) return(NA_character_)
  
  .data_vars <- pronoun_vars(x, ".data")
  .env_vars <- pronoun_vars(x, ".env")
  
  bare_syms <- x[
    x$token == "SYMBOL" &
      !x$text %in% c(".data", ".env") &
      !x$id %in% c(names(.data_vars), names(.env_vars)),
    c("id", "text")
  ]
  if (nrow(bare_syms) == 0) {
    all_cols <- unique(.data_vars)
  } else {
    unscoped_vars <- bare_syms$text
    names(unscoped_vars) <- bare_syms$id
    all_cols <- c(unscoped_vars, .data_vars)
    all_cols <- all_cols[order(as.integer(names(all_cols)))]
    all_cols <- unique(all_cols)
  }
  
  all_cols <- all_cols[all_cols %in% data_cols]
  
  if (length(all_cols) == 0) {
    NA_character_
  } else {
    sort(all_cols)
  }
  
}

pronoun_vars <- function(x, pronoun = c(".data", ".env")) {
  pronoun <- match.arg(pronoun)
  if (!any(x$text == pronoun)) return(character(0))
  conseq_pronoun <- rle(x$text == pronoun)
  x$dotdata <- rep(seq_along(conseq_pronoun$values), conseq_pronoun$lengths)
  x$dotdata <- ifelse(x$text == pronoun, x$dotdata + 1, x$dotdata)
  dotdata <- lapply(split(x, x$dotdata), function(g) {
    if (g$text[1] == pronoun && g$token[3] %in% c("'$'", "LBB")) {
      var <- g$text[4]
      names(var) <- g$id[4]
      if (g$token[4] == "STR_CONST") gsub('"', "", var) else var
    } else {
      character(0)
    }
  })
  allvars <- unlist(unname(dotdata))
  allvars
}

# nocov end

all_validations_fns_vec <- function() {
  
  c(
    "col_vals_lt",
    "col_vals_lte",
    "col_vals_equal",
    "col_vals_not_equal",
    "col_vals_gte",
    "col_vals_gt",
    "col_vals_between",
    "col_vals_not_between",
    "col_vals_in_set",
    "col_vals_not_in_set",
    "col_vals_make_set",
    "col_vals_make_subset",
    "col_vals_null",
    "col_vals_not_null",
    "col_vals_increasing",
    "col_vals_decreasing",
    "col_vals_regex",
    "col_vals_within_spec",
    "col_vals_expr",
    "rows_distinct",
    "rows_complete",
    "col_is_character",
    "col_is_numeric",
    "col_is_integer",
    "col_is_logical",
    "col_is_date",
    "col_is_posix",
    "col_is_factor",
    "col_exists",
    "col_schema_match",
    "row_count_match",
    "col_count_match",
    "tbl_match",
    "conjointly",
    "serially",
    "specially"
  )
}

row_based_validation_fns_vec <- function() {
  
  c(
    "col_vals_lt",
    "col_vals_lte",
    "col_vals_equal",
    "col_vals_not_equal",
    "col_vals_gte",
    "col_vals_gt",
    "col_vals_between",
    "col_vals_not_between",
    "col_vals_in_set",
    "col_vals_not_in_set",
    "col_vals_make_set",
    "col_vals_make_subset",
    "col_vals_null",
    "col_vals_not_null",
    "col_vals_increasing",
    "col_vals_decreasing",
    "col_vals_regex",
    "col_vals_within_spec",
    "col_vals_expr",
    "conjointly",
    "rows_distinct",
    "rows_complete"
  )
}

column_expansion_fns_vec <- function() {
  
  c(
    "col_vals_lt",
    "col_vals_lte",
    "col_vals_equal",
    "col_vals_not_equal",
    "col_vals_gte",
    "col_vals_gt",
    "col_vals_between",
    "col_vals_not_between",
    "col_vals_in_set",
    "col_vals_not_in_set",
    "col_vals_make_set",
    "col_vals_make_subset",
    "col_vals_null",
    "col_vals_not_null",
    "col_vals_increasing",
    "col_vals_decreasing",
    "col_vals_regex",
    "col_vals_within_spec",
    "col_is_character",
    "col_is_numeric",
    "col_is_integer",
    "col_is_logical",
    "col_is_date",
    "col_is_posix",
    "col_is_factor",
    "col_exists"
  )
}

get_tbl_dbi_src_info <- function(tbl) {
  utils::capture.output(tbl %>% unclass() %>% .$src)
}

get_tbl_dbi_src_details <- function(tbl) {
  tbl_src_info <- get_tbl_dbi_src_info(tbl)
  tbl_src_info[grepl("^src:", tbl_src_info)] %>% gsub("src:\\s*", "", .)
}

get_r_column_names_types <- function(tbl) {
  
  suppressWarnings(
    column_header <-
      tbl %>%
      utils::head(1) %>%
      dplyr::collect()
  )
  column_names_types <-
    vapply(column_header, function(x) class(x)[1], character(1))
  
  list(
    col_names = names(column_names_types),
    r_col_types = unname(unlist(column_names_types)),
    col_ptypes = utils::head(column_header, 0)
  )
}

get_tbl_information <- function(tbl) {
  
  if (is.data.frame(tbl)) {
    
    tbl_information <- get_tbl_information_df(tbl)
    
  } else if (is_tbl_spark(tbl)) {
    
    # nocov start
    
    tbl_information <- get_tbl_information_spark(tbl)
    
    # nocov end
    
  } else if (is_tbl_dbi(tbl)) {
    
    tbl_information <- get_tbl_information_dbi(tbl)
    
  } else if (is_arrow_object(tbl)) {
    
    # nocov start

    tbl_information <- get_tbl_information_arrow(tbl)
    
    # nocov end
    
  } else {
    
    # nocov start
    
    warning("Information on this table type cannot be obtained at present.",
            call. = FALSE)
    
    # nocov end
  } 
  
  tbl_information
}

get_tbl_information_df <- function(tbl) {
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  tbl_src <- if (tibble::is_tibble(tbl)) "tbl_df" else "data.frame"
  
  list(
    tbl_src = tbl_src,
    tbl_src_details = NA_character_,
    db_tbl_name = NA_character_,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = NA_character_,
    col_ptypes = r_column_names_types$col_ptypes
  )
}

# nocov start

get_tbl_information_spark <- function(tbl) {
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  tbl_schema <- sparklyr::sdf_schema(tbl)
  
  db_col_types <- 
    lapply(tbl_schema, `[[`, 2) %>%
    unlist() %>%
    unname() %>%
    tolower()
  
  list(
    tbl_src = "tbl_spark",
    tbl_src_details = "Spark",
    db_tbl_name = NA_character_,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = db_col_types,
    col_ptypes = r_column_names_types$col_ptypes
  )
}

# nocov end

get_tbl_information_dbi <- function(tbl) {

  tbl_connection <- tbl$src$con

  tbl_src_details <- tolower(get_tbl_dbi_src_details(tbl))
  
  if (grepl("sql server|sqlserver", tbl_src_details)) {
    
    # nocov start
    tbl_src <- "mssql"
    # nocov end
    
  } else if (grepl("duckdb", tbl_src_details)) {
    
    # nocov start
    tbl_src <- "duckdb"
    # nocov end
      
  } else if (grepl("bq_|bigquery", tbl_src_details)) {
      
    # nocov start
    tbl_src <- "bigquery"
    # nocov end
      
  } else {
    tbl_src <- gsub("^([a-z]*).*", "\\1", get_tbl_dbi_src_details(tbl))
  }
  
  db_tbl_name <- as.character(dbplyr::remote_name(tbl))
  
  r_column_names_types <- get_r_column_names_types(tbl)
  
  n_cols <- length(r_column_names_types$col_names)
  
  # nolint start
  
  if (tbl_src == "postgres") {
    
    # nocov start
    
    db_tbl_name_no_schema <- gsub(".*\\.", "", db_tbl_name)
    
    q_types <-
      as.character(
        glue::glue(
          "select column_name,data_type from \\
        information_schema.columns where \\
        table_name = '{db_tbl_name_no_schema}'"
        )
      )
    
    # nocov end
    
  } else if (tbl_src == "bigquery") {
    
    # nocov start
    
    # Obtain BigQuery attrs for project and dataset names
    # of the data table
    if (
      "pb_bq_project" %in% names(attributes(tbl)) &&
      "pb_bq_dataset" %in% names(attributes(tbl))
      ) {
      
      bq_project <- attr(tbl, "pb_bq_project", exact = TRUE)
      bq_dataset <- attr(tbl, "pb_bq_dataset", exact = TRUE)
      
      q_types <-
        as.character(
          glue::glue(
            "select column_name, data_type \\
        from `{bq_project}.{bq_dataset}.INFORMATION_SCHEMA.COLUMNS` \\
        where table_name = '{db_tbl_name}' \\
        order by ordinal_position"
          )
        )
      
    } else {
      q_types <- NULL
    }
    
    # nocov end
  
  } else {

    if (tbl_src == "mssql") {
      
      # nocov start
      
      q_types <-
        as.character(
          glue::glue(
            "SELECT TOP {n_cols} DATA_TYPE \\
          FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '{db_tbl_name}'"
          )
        )
      
      # nocov end
      
    } else { 
      
        q_types <-
        as.character(
          glue::glue(
            "SELECT DATA_TYPE FROM \\
          INFORMATION_SCHEMA.COLUMNS WHERE \\
          table_name = '{db_tbl_name}' LIMIT {n_cols}"
          )
        )
    }
  }
  
  # nolint end
  
  if (tbl_src == "postgres") {
    
    # nocov start
    
    db_col_types <- 
      DBI::dbGetQuery(tbl_connection, q_types) %>%
      dplyr::pull(data_type) %>%
      tolower()
    
    # nocov end
  }
  
  if (tbl_src %in% c("duckdb", "sqlite")) {
    
    db_col_types <-
      vapply(
        r_column_names_types$col_names,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          
          DBI::dbDataType(
            tbl_connection,
            tbl %>%
              dplyr::select(tidyselect::all_of(x)) %>%
              utils::head(1) %>%
              dplyr::collect() %>%
              dplyr::pull(x)
          )
        }
      ) %>%
      tolower()
  }
  
  if (!(tbl_src %in% c("duckdb", "sqlite", "postgres"))) {

    # nocov start
    
    db_col_types <- 
      DBI::dbGetQuery(tbl_connection, q_types) %>%
      dplyr::collect() %>%
      dplyr::pull(ifelse(tbl_src == "bigquery", "data_type", "DATA_TYPE")) %>%
      tolower()
    
    # nocov end
  }
  
  # nocov start
  
  if (!exists("db_col_types")) {
    db_col_types <- NA_character_
  }
  
  # nocov end

  list(
    tbl_src = tbl_src,
    tbl_src_details = get_tbl_dbi_src_details(tbl),
    db_tbl_name = db_tbl_name,
    col_names = r_column_names_types$col_names,
    r_col_types = r_column_names_types$r_col_types,
    db_col_types = db_col_types,
    col_ptypes = r_column_names_types$col_ptypes
  )
}

# nocov start

get_tbl_information_arrow <- function(tbl) {

  schema_cap <- utils::capture.output(tbl$schema)[-1][seq_len(ncol(tbl))]
  
  col_names <-
    vapply(
      schema_cap,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        unlist(strsplit(x, split = ": "))[1]
      }
    )
  
  db_col_types <-
    vapply(
      schema_cap,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        unlist(strsplit(x, split = ": "))[2]
      }
    )
  
  r_col_types <-
    vapply(
      dplyr::as_tibble(utils::head(tbl, 1)),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) class(x)[1]
    )
  
  list(
    tbl_src = "Arrow",
    tbl_src_details = class(tbl)[1],
    db_tbl_name = NA_character_,
    col_names = col_names,
    r_col_types = r_col_types,
    db_col_types = db_col_types,
    col_ptypes = dplyr::collect(utils::head(tbl, 0))
  )
}

# nocov end

pb_fmt_number <- function(
    x,
    decimals = 2,
    n_sigfig = NULL,
    drop_trailing_zeros = FALSE,
    drop_trailing_dec_mark = TRUE,
    use_seps = TRUE,
    scale_by = 1,
    suffixing = FALSE,
    pattern = "{x}",
    sep_mark = ",",
    dec_mark = ".",
    locale = NULL
) {
  
  if (is.null(x)) return(NULL)
  
  if (length(x) == 1 &&
      (!inherits(x, "numeric") && !inherits(x, "integer"))) {
    return(x)
  } 
  
  ((dplyr::tibble(a = x) %>%
      gt::gt() %>%
      gt::fmt_number(
        columns = "a",
        decimals = decimals,
        n_sigfig = n_sigfig,
        drop_trailing_zeros = drop_trailing_zeros,
        drop_trailing_dec_mark = drop_trailing_dec_mark,
        use_seps = use_seps,
        scale_by = scale_by,
        suffixing = suffixing,
        pattern = pattern,
        sep_mark = sep_mark,
        dec_mark = dec_mark,
        locale = locale
      ))$`_formats`[[1]][[1]][[1]])(x)
}

add_icon_img <- function(
    icon,
    height = 30,
    v_align = "middle"
) {

  file <- 
    system.file(
      "img", "function_icons", paste0(icon, ".png"),
      package = "pointblank"
    )
  
  image_raw <-
    readBin(
      con = file,
      what = "raw",
      n = file.info(file)$size
    )
  
  image_uri <- 
    paste0(
      "data:image/png;base64,",
      base64enc::base64encode(image_raw, 0)
    )
  
  img <- 
    htmltools::tags$img(
      src = image_uri,
      alt = icon,
      style = htmltools::css(
        height = htmltools::validateCssUnit(height),
        `vertical-align` = v_align
      )
    )
  
  gsub("\\s\\s+", " ", htmltools::HTML(as.character(img)))
}

add_icon_svg <- function(
    icon,
    height = 30,
    v_align = "middle"
) {
  
  file <- 
    system.file(
      "img", "function_icons", paste0(icon, ".svg"),
      package = "pointblank"
    )
  
  htmltools::tags$div(
    style = htmltools::css(
      margin = 0,
      padding = 0,
      display = "inline-block",
      height = "30px",
      `vertical-align` = "middle"
    ),
    htmltools::HTML(
      paste(readLines(con = file, warn = FALSE), collapse = "") %>%
        tidy_gsub("width=\"[0-9]*?px", paste0("width=\"", height, "px")) %>%
        tidy_gsub("height=\"[0-9]*?px", paste0("height=\"", height, "px"))
    )
  ) %>%
    as.character()
}

tidy_gsub <- function(
    x,
    pattern,
    replacement,
    fixed = FALSE
) {
  gsub(pattern, replacement, x, fixed = fixed)
}

capture_formula <- function(
    formula,
    separate = TRUE,
    remove_whitespace = TRUE,
    oneline = TRUE
) {
  
  # TODO: add option to use `htmltools::htmlEscape()`
  
  attributes(formula) <- NULL
  
  output <- utils::capture.output(formula)
  
  if (remove_whitespace) {
    output <- gsub("^\\s+", "", output)
  }
  
  if (oneline) {
    output <- paste(output, collapse = "")
  }
  
  if (separate) {
    if (grepl("^~", output)) {
      output <- c(NA_character_, output)
    } else {
      output <- strsplit(output, " ~ ") %>% unlist()
      output[2] <- paste("~", output[2])
    }
  }
  
  output
}

capture_function <- function(fn, escape = TRUE) {
  
  output <- utils::capture.output(fn)
  output <- paste(output[1:(length(output) - 1)], collapse = "\n")
  
  if (escape) {
    output <- htmltools::htmlEscape(output)  
  }
  
  output
}

pb_str_catalog <- function(
    item_vector,
    limit = 5,
    sep = ",",
    and_or = NULL,
    oxford = TRUE,
    as_code = TRUE,
    quot_str = NULL,
    na_rm = FALSE,
    lang = NULL
) {
  
  if (is.null(lang)) lang <- "en"
  
  if (na_rm) {
    item_vector <- item_vector[!is.na(item_vector)]
  }
  
  item_count <- length(item_vector)

  # If there is nothing in the `item_vector`, return
  # a text string with three hyphens
  if (item_count < 1) {
    return("---")
  }
  
  if (item_count > 2 && item_count > limit) {
    
    n_items <- item_count - limit
    
    more <- glue::glue(get_lsv("informant_report/snip_list_more")[[lang]])
    
    n_overlimit <- paste0("(", more, ")")
    
    item_vector <- item_vector[1:limit]
    
  } else {
    
    n_overlimit <- ""
  }

  if (is.null(quot_str)) {
    
    if (is.numeric(item_vector) || 
        is.logical(item_vector) ||
        inherits(item_vector, "Date") ||
        inherits(item_vector, "POSIXct")) {
      
      quot_str <- FALSE
    } else {
      quot_str <- TRUE
    }
  }

  surround <- c()
  
  if (quot_str) {
    surround <- "\""
  }
  
  if (as_code) {
    surround <- c(surround, "`")
  }

  if (is.null(and_or)) {
    and_or <- "and"
  }
  
  if (!(and_or %in% c("and", "or", ""))) {
    stop(
      "The value for `and_or` must be one of the following:\n",
      "* `\"and\"`, `\"or\"`, or an empty string",
      call. = FALSE
    )
  }
  
  if (and_or == "") {
    
    # Force the use of all possible commas where the conjunction
    # is not used (this is technically not using an Oxford comma but
    # this proceeds down the same codepath)
    oxford <- TRUE
    
  } else {
    
    and_or <- get_lsv(paste0("informant_report/snip_list_", and_or))[[lang]]
    
    # If a conjunction (the and/or types) is used in any language
    # other than English (where its use is definitely incorrect),
    # then force `oxford` to be FALSE (default is TRUE)
    if (lang != "en" && n_overlimit == "") {
      oxford <- FALSE
    }
  }

  surround_str_1 <- rev(surround) %>% paste(collapse = "")
  surround_str_2 <- surround %>% paste(collapse = "")
  
  cat_str <- paste0(surround_str_1, item_vector, surround_str_2)
  
  if (item_count == 1) {
    
    return(cat_str)
    
  } else if (item_count == 2) {
    
    return(paste0(cat_str[1], and_or, cat_str[2]))
    
  } else {
    
    separators <- rep(paste0(sep, " "), length(item_vector) - 1)
    
    if (!oxford) {
      separators[length(separators)] <- " "
    }

    if (n_overlimit == "") {
      separators[length(separators)] <- 
        paste0(
          separators[length(separators)],
          gsub("(^ | $)", "", and_or),
          " "
        )
    }
    
    separators[length(separators) + 1] <- ""
    
    if (length(cat_str) == 2) {
      
      cat_str <- 
        paste0(cat_str[1], ", ", cat_str[2])
      
    } else {
      
      cat_str <-
        paste0(cat_str, separators) %>%
        paste(collapse = "")
    }
    
    cat_str <- paste(cat_str, n_overlimit)
    
    cat_str <- gsub("\\s+$", "", cat_str)
    
    return(cat_str)
  }
}

pb_str_summary <- function(
    column,
    type
) {

  if (type == "5num") {
    
    color <- "dodgerblue"
    
    min_max <- pb_min_max_stats(column)
    
    minimum <- min_max$min
    q1 <- pb_quantile_stats(column, quantile = 0.25)
    median <- pb_quantile_stats(column, quantile = 0.5)
    q3 <- pb_quantile_stats(column, quantile = 0.75)
    maximum <- min_max$max
    
    summary_str <-
      htmltools::tags$span(
        class = "pb_label",
        style = htmltools::css(
          padding = "0 5px 0 5px",
          font_size = "smaller",
          border_color = "lightgray"
        ),
        htmltools::HTML(
          paste(
            generate_number(1, color, "Minimum"), minimum,
            generate_number(2, color, "Q1"), q1,
            generate_number(3, color, "Median"), median,
            generate_number(4, color, "Q3"), q3,
            generate_number(5, color, "Maximum"), maximum
          )
        )
      )
    
  } else if (type == "7num") {
    
    color <- "darkviolet"
    
    p2 <- pb_quantile_stats(column, quantile = 0.02)
    p9 <- pb_quantile_stats(column, quantile = 0.09)
    q1 <- pb_quantile_stats(column, quantile = 0.25)
    median <- pb_quantile_stats(column, quantile = 0.5)
    q3 <- pb_quantile_stats(column, quantile = 0.75)
    p91 <- pb_quantile_stats(column, quantile = 0.91)
    p98 <- pb_quantile_stats(column, quantile = 0.98)
    
    summary_str <-
      htmltools::tags$span(
        class = "pb_label",
        style = htmltools::css(
          padding = "0 5px 0 5px",
          font_size = "smaller",
          border_color = "lightgray"
        ),
        htmltools::HTML(
          paste(
            generate_number(1, color, "P2"), p2,
            generate_number(2, color, "P9"), p9,
            generate_number(3, color, "Q1"), q1,
            generate_number(4, color, "Median"), median,
            generate_number(5, color, "Q3"), q3,
            generate_number(6, color, "P91"), p91,
            generate_number(7, color, "P98"), p98
          )
        )
      )
    
  } else if (type == "bowley") {
    
    color <- "orangered"
    
    min_max <- pb_min_max_stats(column)
    
    minimum <- min_max$min
    p10 <- pb_quantile_stats(column, quantile = 0.10)
    q1 <- pb_quantile_stats(column, quantile = 0.25)
    median <- pb_quantile_stats(column, quantile = 0.5)
    q3 <- pb_quantile_stats(column, quantile = 0.75)
    p90 <- pb_quantile_stats(column, quantile = 0.90)
    maximum <- min_max$max
    
    summary_str <-
      htmltools::tags$span(
        class = "pb_label",
        style = htmltools::css(
          padding = "0 5px 0 5px",
          font_size = "smaller",
          border_color = "lightgray"
        ),
        htmltools::HTML(
          paste(
            generate_number(1, color, "Minimum"), minimum,
            generate_number(2, color, "P10"), p10,
            generate_number(3, color, "Q1"), q1,
            generate_number(4, color, "Median"), median,
            generate_number(5, color, "Q3"), q3,
            generate_number(6, color, "P90"), p90,
            generate_number(7, color, "Maximum"), maximum
          )
        )
      )
  }
  
  summary_str
}

generate_number <- function(
    number,
    color,
    title
) {
  
  number <- as.character(number)
  
  u_char <-
    switch(
      number,
      "1" = "&#10122;",
      "2" = "&#10123;",
      "3" = "&#10124;",
      "4" = "&#10125;",
      "5" = "&#10126;",
      "6" = "&#10127;",
      "7" = "&#10128;",
      "8" = "&#10129;",
      "9" = "&#10130;",
      "10" = "&#10131;"
    )

  as.character(
    htmltools::tags$span(
      style = paste0("color: ", color, "; cursor: default;"),
      title = title,
      htmltools::HTML(u_char)
    )
  )
}

pb_quantile_stats <- function(
    data_column,
    quantile
) {
  
  if (is_tbl_spark(data_column)) {
    
    # nocov start
    
    column_name <- colnames(data_column)
    
    quantile <- 
      sparklyr::sdf_quantile(
        data_column, column_name,
        probabilities = quantile
      ) %>% 
      unname() %>%
      round(2)
    
    return(quantile)
    
    # nocov end
    
  } else if (inherits(data_column, "data.frame")) {
    
    quantile <- 
      data_column %>%
      stats::quantile(probs = quantile, na.rm = TRUE) %>%
      unname() %>%
      round(2)
    
  } else if (is_tbl_dbi(data_column)) {
    
    data_column <- data_column %>% dplyr::filter(!is.na(1))
    
    n_rows <- 
      data_column %>%
      dplyr::count(name = "n") %>%
      dplyr::pull(n) %>%
      as.numeric()
    
    if (n_rows <= 5000) {
      
      data_column <- data_column %>% dplyr::collect()
      
      quantile <- 
        data_column %>%
        stats::quantile(probs = quantile, na.rm = TRUE) %>%
        unname() %>%
        round(2)
      
    } else {
      
      data_arranged <- 
        data_column %>%
        dplyr::rename(a = 1) %>%
        dplyr::filter(!is.na(a)) %>%
        dplyr::arrange(a) %>%
        utils::head(6E8)
      
      n_rows_data <-  
        data_arranged %>%
        dplyr::count(name = "n") %>%
        dplyr::pull(n) %>%
        as.numeric()
      
      quantile_row <- floor(quantile * n_rows_data)
      
      quantile <- 
        data_arranged %>%
        utils::head(quantile_row) %>%
        dplyr::arrange(desc(a)) %>%
        utils::head(1) %>%
        dplyr::pull(a) %>%
        as.numeric() %>%
        round(2)
    }
    
  } else {
    
    stop("This table type isn't yet supported", call. = FALSE)
  }
  
  quantile
}

pb_min_max_stats <- function(data_column) {
  
  data_column %>%
    dplyr::summarize_all(
      .funs = list(
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    dplyr::summarize_all(~ round(., 2)) %>%
    dplyr::mutate_all(.funs = as.numeric) %>%
    as.list()
}

cli_bullet_msg <- function(
    msg,
    bullet = cli::symbol$bullet,
    color = NULL,
    .envir = parent.frame()
) {
  
  msg <- glue::glue_collapse(msg, "\n")
  msg <- glue::glue(msg, .envir = .envir)
  
  if (!is.null(color)) {
    color_style <- cli::make_ansi_style(color)
    bullet <- color_style(bullet)
  }

  bullet <- paste0(bullet, " ")
  msg <- cli_ident(msg, bullet, "  ")
  rlang::inform(msg)
}

cli_ident <- function(
    x,
    initial = "  ",
    indent = initial
) {
  paste0(initial, gsub("\n", paste0("\n", indent), x))
}

print_time <- function(time_diff_s) {
  
  if (time_diff_s < 1) {
    return("")
  } else {
    return(
      paste0(
        " {.time_taken (",
        round(time_diff_s, 1) %>%
          formatC(format = "f", drop0trailing = FALSE, digits = 1),
        " s)}"
      )
    )
  }
}

gt_missing <- 
  if (packageVersion("gt") >= "0.6.0") {
    gt::sub_missing 
  } else {
    gt::fmt_missing
  }

pb_get_image_tag <- function(file, dir = "images") {
  
  repo_url <- "https://raw.githubusercontent.com/rstudio/pointblank/main"
  
  function_name <- paste0(gsub("man_(.*)_[1-9].png", "\\1", file), "()")
  example_code_idx <- gsub("man_.*?([1-9]).png", "\\1", file)
  
  ordinal_idx <-
    switch(
      example_code_idx,
      `1` = "first",
      `2` = "second",
      `3` = "third",
      `4` = "fourth",
      `5` = "fifth",
      `6` = "sixth",
      `7` = "seventh",
      `8` = "eighth",
      `9` = "ninth",
      "above"
    )
  
  alt_text <-
    paste0(
      "This image was generated from the ", ordinal_idx,
      " code example in the `", function_name, "` help file."
    )
  
  image_url <- file.path(repo_url, dir, file)
  
  paste0(
    "<img ",
    "src=\"", image_url, "\" ",
    "alt=\"", alt_text, "\" ",
    "style=\"width:100\\%;\">"
  )
}

deparse_expr <- function(expr, collapse = " ", ...) {
  if (rlang::is_scalar_atomic(expr)) {
    as.character(expr)
  } else {
    deparsed <- paste(deparse(expr, ...), collapse = collapse)
    paste("<expr>", deparsed)
  }
}
