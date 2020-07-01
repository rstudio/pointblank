is_ptblank_agent <- function(x) {
  inherits(x, "ptblank_agent")
}

is_a_table_object <- function(x) {
  inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))
}

get_tbl_object <- function(agent) {
  agent$tbl
}

has_agent_intel <- function(agent) {
  inherits(agent, "has_intel")
}

is_agent_empty <- function(agent) {
  if (!is_ptblank_agent(agent)) return(FALSE)
  if (nrow(agent$validation_set) == 0) TRUE else FALSE
}

interrogation_time <- function(agent) {
  if (has_agent_intel(agent)) agent$time else NA
}

number_of_validation_steps <- function(agent) {
  if (is_ptblank_agent(agent)) agent$validation_set %>% nrow() else NA
}

get_assertion_type_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "assertion_type"]]
}

get_column_as_sym_at_idx <- function(agent, idx) {
  rlang::sym(agent$validation_set[[idx, "column"]] %>% unlist() %>% gsub("'", "", .))
}

get_values_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "values"]] %>% unlist(recursive = FALSE)
}

get_column_na_pass_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "na_pass"]]
}

get_all_cols <- function(agent) {
  agent$col_names
}

resolve_expr_to_cols <- function(tbl, var_expr) {
  
  var_expr <- enquo(var_expr)
  
  if ((var_expr %>% rlang::get_expr() %>% as.character())[1] == "vars") {
    
    cols <- (var_expr %>% rlang::get_expr() %>% as.character())[-1]
    return(cols)
  }
  
  tidyselect::vars_select(.vars = colnames(tbl), {{ var_expr }}) %>% unname()
}

resolve_columns <- function(x, var_expr, preconditions) {
  
  # Return an empty character vector if the expr is NULL
  if (inherits(var_expr, "quosure") && var_expr %>% rlang::as_label() == "NULL") {
    return(character(0))
  } 
  
  # Get the column names
  if (is.null(preconditions)) {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      column <- resolve_expr_to_cols(tbl = x, var_expr = !!var_expr)
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
    
  } else {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      tbl <- apply_preconditions(tbl = x, preconditions = preconditions)
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      
      tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
  }
  
  column
}

normalize_step_id <- function(step_id, columns) {
  
  if (!is.null(step_id) && length(columns) > 1) {
    
    # Generate multiple `step_id` values with single `step_id`
    if (length(step_id) == 1) {
      step_id <- 
        paste0(
          step_id[1], ".",
          formatC(seq_along(columns), width = 4, format = "d", flag = "0")
        )
    } else if (length(step_id) != 1 && length(step_id) != length(columns)) {
      step_id <- 
        paste0(
          step_id[1], ".",
          formatC(seq_along(columns), width = 4, format = "d", flag = "0")
        )
      # TODO: issue warning about length of `step_id`
    } else if (anyDuplicated(step_id) != 0) {
      
      step_id <- 
        paste0(
          step_id[1], ".",
          formatC(seq_along(columns), width = 4, format = "d", flag = "0")
        )
      # TODO: issue warning about duplicated `step_id` values
    }
  }
  step_id
}

get_threshold_type <- function(threshold) {
  
  if (threshold >= 1) {
    threshold_type <- "absolute"
  } else if (threshold >= 0 && threshold < 1) {
    threshold_type <- "proportional"
  }
}

row_based_step_fns_vector <- function() {
  
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
    "col_vals_null",
    "col_vals_not_null",
    "col_vals_regex",
    "col_vals_expr",
    "conjointly",
    "rows_distinct"
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
    column_names_types <-
      tbl %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      vapply(
        FUN.VALUE = character(1),
        FUN = function(x) class(x)[1]
      )
  )
  
  list(
    col_names = names(column_names_types),
    r_col_types = unname(unlist(column_names_types))
  )
}

get_tbl_information <- function(tbl) {
  
  if (inherits(tbl, "data.frame")) {
    
    r_column_names_types <- get_r_column_names_types(tbl)
    
    tbl_src <- "data.frame"
    if (inherits(tbl, "tbl_df")) {
      tbl_src <- "tbl_df"
    } 
    
    return(
      list(
        tbl_src = tbl_src,
        tbl_src_details = NA_character_,
        db_tbl_name = NA_character_,
        col_names = r_column_names_types$col_names,
        r_col_types = r_column_names_types$r_col_types,
        db_col_types = NA_character_
      )
    )
    
  } else if (inherits(tbl, "tbl_dbi")) {
    
    tbl_src <- gsub("^([a-z]*).*", "\\1", get_tbl_dbi_src_details(tbl))
    
    r_column_names_types <- get_r_column_names_types(tbl)
    
    tbl_connection <- tbl %>% .$src %>% .$con
    
    db_tbl_name <- dbplyr::remote_name(tbl) %>% as.character()
    
    n_cols <- length(r_column_names_types$col_names)

    if (tbl_src != "postgres") {
      q_types <- 
        glue::glue(
          "SELECT DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '{db_tbl_name}' LIMIT {n_cols}"
        )
    } else {
      db_tbl_name_no_schema <- gsub('.*\\.', '', db_tbl_name)
      q_types <- 
        glue::glue(
          "select column_name,data_type from information_schema.columns where table_name = '{db_tbl_name_no_schema}'"
        )
    }
    
    if (tbl_src != "sqlite" & tbl_src != "postgres") {
      db_col_types <- 
        DBI::dbGetQuery(tbl_connection, q_types) %>%
        dplyr::collect() %>%
        dplyr::pull(DATA_TYPE) %>%
        tolower()
    }
    
    if (tbl_src == "sqlite") {
      db_col_types <-
        vapply(
          r_column_names_types$col_names,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE,
          FUN = function(x) {
            
            DBI::dbDataType(
              tbl_connection,
              tbl %>%
                dplyr::select(x) %>%
                utils::head(1) %>%
                dplyr::collect() %>%
                dplyr::pull(x)
            )
          }
        ) %>%
        tolower()
    }
    
    if (tbl_src == "postgres") {
      db_col_types <- 
        DBI::dbGetQuery(tbl_connection, q_types) %>%
        dplyr::pull(data_type) %>%
        tolower()
    }
    
    if (!exists("db_col_types")) {
      db_col_types <- NA_character_
    }
    
    return(
      list(
        tbl_src = tbl_src,
        tbl_src_details = get_tbl_dbi_src_details(tbl),
        db_tbl_name = db_tbl_name,
        col_names = r_column_names_types$col_names,
        r_col_types = r_column_names_types$r_col_types,
        db_col_types = db_col_types
      )
    )
    
  } else {
    warning("Information on this table type cannot be obtained at present.",
            call. = FALSE)
  } 
}

normalize_reporting_language <- function(reporting_lang) {
  
  if (is.null(reporting_lang)) return("en")
  
  if (!(tolower(reporting_lang) %in% reporting_languages)) {
    stop("The text ", reporting_lang, " doesn't correspond to a pointblank reporting language",
         call. = FALSE)
  }
  
  tolower(reporting_lang)
}

tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {
  gsub(pattern, replacement, x, fixed = fixed)
}
