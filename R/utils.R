is_ptblank_agent <- function(object) {
  inherits(object, "ptblank_agent")
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
  rlang::sym(agent$validation_set[[idx, "column"]] %>% gsub("'", "", .))
}

get_column_value_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "value"]]
}

get_column_set_values_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "set"]]
}

get_column_incl_na_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "incl_na"]]
}

get_column_regex_at_idx <- function(agent, idx) {
  agent$validation_set[[idx, "regex"]]
}

get_all_cols <- function(agent) {
  agent$col_names
}

evaluate_single <- function(x,
                            type,
                            column,
                            value = NULL,
                            set = NULL,
                            regex = NULL,
                            left = NULL,
                            right = NULL,
                            incl_na = NULL,
                            preconditions,
                            actions) {

  column <- column[1]
  x_ret <- x
  tbl <- x

  # Apply any preconditions
  if (!is.null(preconditions)) {
    
    tbl <- 
      preconditions %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
  }
  
  # Get the `column` number
  col_number <- ((tbl %>% colnames()) %in% column) %>% which()
  
  if (type == "col_vals_equal") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) == value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_not_equal") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) != value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_gt") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) > value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_gte") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) >= value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_lt") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) < value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_lte") {
    
    logicals <- (tbl %>% dplyr::pull(col_number)) <= value
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_between") {
    
    vals <- tbl %>% dplyr::pull(col_number)
    
    logicals <- vals >= left & vals <= right
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_not_between") {

    vals <- tbl %>% dplyr::pull(col_number)
    
    logicals <- vals < left | vals > right
    logicals[is.na(logicals)] <- incl_na
  }
  
  if (type == "col_vals_in_set") {

    logicals <- (tbl %>% dplyr::pull(col_number)) %in% set
  }
  
  if (type == "col_vals_not_in_set") {
    
    logicals <- !(tbl %>% dplyr::pull(col_number) %in% set)
  }
  
  if (type == "col_vals_regex") {

    vals <- tbl %>% dplyr::pull(col_number)
    na_vals <- is.na(vals)
    
    logicals <- grepl(pattern = regex, x = vals)
    logicals[na_vals] <- incl_na
  }
  
  if (type == "col_vals_not_null") {
    
    logicals <- !is.na(tbl %>% dplyr::pull(col_number))
  }
  
  if (type == "col_vals_null") {
    
    logicals <- is.na(tbl %>% dplyr::pull(col_number))
  }
  
  if (grepl("col_is_.*", type)) {
    
    # Get the column type
    column_type <-
      (tbl %>%
         dplyr::select({{ column }}) %>%
         utils::head(1) %>%
         dplyr::collect() %>%
         as.data.frame(stringsAsFactors = FALSE)
      )[1, 1] %>% 
      class()
    
    if (type == "col_is_numeric") {
      logicals <- ifelse(column_type[1] == "numeric", TRUE, FALSE)
    } else if (type == "col_is_integer") {
      logicals <- ifelse(column_type[1] == "integer", TRUE, FALSE)
    } else if (type == "col_is_character") {
      logicals <- ifelse(column_type[1] == "character", TRUE, FALSE)
    } else if (type == "col_is_logical") {
      logicals <- ifelse(column_type[1] == "logical", TRUE, FALSE)
    } else if (type == "col_is_factor") {
      logicals <- ifelse(column_type[1] == "factor", TRUE, FALSE)
    } else if (type == "col_is_posix") {
      logicals <- ifelse(column_type[1] == "POSIXct", TRUE, FALSE)
    } else if (type == "col_is_date") {
      logicals <- ifelse(column_type[1] == "Date", TRUE, FALSE)
    } else {
      logicals <- FALSE
    }
  }
  
  if (type == "col_exists") {
    
    column_names <-
      tbl %>%
      utils::head(1) %>%
      dplyr::as_tibble() %>%
      colnames()
    
    logicals <- ifelse(column %in% column_names, TRUE, FALSE)
  }
  
  logicals[which(is.na(logicals))] <- FALSE
  
  total_count <- length(logicals)
  true_count <- sum(logicals)
  false_count <- total_count - true_count
  false_fraction <- false_count / total_count
  
  if (!is.null(actions$notify_count)) {
    if (false_count >= actions$notify_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_count` threshold",
        " * `failing_count` ({false_count}) >= `notify_count` ({notify_count})",
        type = type,
        false_count = false_count,
        notify_count = actions$notify_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(actions$notify_fraction)) {
    if ((false_count/total_count) >= actions$notify_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `notify_fraction` ({notify_fraction})",
        type = type,
        false_fraction = false_fraction,
        notify_fraction = actions$notify_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(actions$stop_count)) {
    if (false_count >= actions$stop_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_count` threshold",
        " * `failing_count` ({false_count}) >= `stop_count` ({stop_count})",
        type = type,
        false_count = false_count,
        stop_count = actions$stop_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(actions$stop_fraction)) {
    if ((false_count/total_count) >= actions$stop_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `stop_fraction` ({stop_fraction})",
        type = type,
        false_fraction = false_fraction,
        stop_fraction = actions$stop_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(actions$warn_count)) {
    if (false_count >= actions$warn_count) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_count` threshold",
        " * `failing_count` ({false_count}) >= `warn_count` ({warn_count})",
        type = type,
        false_count = false_count,
        warn_count = actions$warn_count,
        .format = "{text}"
      )
      
    }
  } else if (!is.null(actions$warn_fraction)) {
    if ((false_count/total_count) >= actions$warn_fraction) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `warn_fraction` ({warn_fraction})",
        type = type,
        false_fraction = false_fraction,
        warn_fraction = actions$warn_fraction,
        .format = "{text}"
      )
    }
  }
  
  x_ret
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
      column <- column[1]
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
    
  } else {
    
    if (inherits(x, c("data.frame", "tbl_df", "tbl_dbi"))) {
      
      tbl <- x
      
      tbl <- 
        preconditions %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
      column <- column[1]
      
    } else if (inherits(x, ("ptblank_agent"))) {
      
      tbl <- get_tbl_object(agent = x)
      
      tbl <- 
        preconditions %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      column <- resolve_expr_to_cols(tbl = tbl, var_expr = !!var_expr)
    }
  }
  
  column
}

tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {
  
  gsub(pattern, replacement, x, fixed = fixed)
}
