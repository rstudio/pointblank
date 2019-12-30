#' Add properly formatted validation steps
#' 
#' @noRd
create_validation_step <- function(agent,
                                   assertion_type,
                                   column,
                                   value = NULL,
                                   set = NULL,
                                   regex = NULL,
                                   incl_na = NULL,
                                   preconditions = NULL,
                                   brief = NULL,
                                   warn_count = NULL,
                                   stop_count = NULL,
                                   notify_count = NULL,
                                   warn_fraction = NULL,
                                   stop_fraction = NULL,
                                   notify_fraction = NULL) {
  
  # Create a validation step as a single-row `tbl_df` object
  validation_step_df <-
    dplyr::tibble(
      assertion_type = assertion_type,
      column = list(column),
      value = ifelse(is.null(value), NA_real_, as.numeric(value)),
      set = ifelse(is.null(set), list(NULL), list(set)),
      regex = ifelse(is.null(regex), NA_character_, as.character(regex)),
      incl_na = ifelse(is.null(incl_na), as.logical(NA), as.logical(incl_na)),
      preconditions = ifelse(is.null(preconditions), list(NULL), list(preconditions)),
      brief = ifelse(is.null(brief), NA_character_, as.character(brief)),
      all_passed = as.logical(NA),
      n = NA_integer_,
      n_passed = NA_integer_,
      n_failed = NA_integer_,
      f_passed = NA_real_,
      f_failed = NA_real_,
      warn_count = ifelse(is.null(warn_count), NA_real_, as.numeric(warn_count)),
      stop_count = ifelse(is.null(stop_count), NA_real_, as.numeric(stop_count)),
      notify_count = ifelse(is.null(notify_count), NA_real_, as.numeric(notify_count)),
      warn_fraction = ifelse(is.null(warn_fraction), NA_real_, as.numeric(warn_fraction)),
      stop_fraction = ifelse(is.null(stop_fraction), NA_real_, as.numeric(stop_fraction)),
      notify_fraction = ifelse(is.null(notify_fraction), NA_real_, as.numeric(notify_fraction))
    )
  
  # Append `validation_step` to `validation_set`
  agent$validation_set <- 
    dplyr::bind_rows(agent$validation_set, validation_step_df)
  
  agent
}

apply_preconditions_to_tbl <- function(agent, idx, tbl) {

  preconditions <- agent$validation_set$preconditions[[idx]]
  
  if (!is.null(preconditions)) {
    
    tbl <- 
      preconditions %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
  }
  
  tbl
}

get_focal_tbl_object <- function(agent) {
  agent$focal_tbl
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

#' Get all column names from the table currently in focus
#' 
#' @noRd
get_all_cols <- function(agent) {
  
  # Get vector of all columns table currently in focus
  agent$focal_col_names
}

#' Does the agent have no validation steps available in the object?
#' 
#' @noRd
is_agent_empty <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    
    if (nrow(agent$validation_set) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Did the agent carry out an interrogation?
#' 
#' @noRd
did_agent_interrogate <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(ifelse(length(agent$validation_time) > 0, TRUE, FALSE))
  } else {
    return(NA)
  }
}

#' When did the agent carry out an interrogation?
#' 
#' @noRd
interrogation_time <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    if (did_agent_interrogate(agent)) {
      
      return(agent$validation_time)
      
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

#' How many validation steps are associated with the agent?
#' 
#' @noRd
number_of_validation_steps <- function(agent) {
  
  if (is_ptblank_agent(agent)) {
    return(agent$validation_set %>% nrow())
  } else {
    return(NA)
  }
}

#' How many validation steps are associated with the agent?
#' 
#' @noRd
create_autobrief <- function(agent,
                             assertion_type,
                             preconditions = NULL,
                             column = NULL,
                             value = NULL,
                             regex = NULL,
                             set = NULL,
                             left = NULL,
                             right = NULL) {
  
  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    if (assertion_type == "col_vals_gt") {
      operator <- ">"
    } else if (assertion_type == "col_vals_gte") {
      operator <- ">="
    } else if (assertion_type == "col_vals_lt") {
      operator <- "<"
    } else if (assertion_type == "col_vals_lte") {
      operator <- "<="
    } else if (assertion_type == "col_vals_equal") {
      operator <- "=="
    } else if (assertion_type == "col_vals_not_equal") {
      operator <- "!="
    } 

    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should be ", operator, " ", value
      )
  }
  
  
  if (assertion_type == "col_exists") {
    
    autobrief <- paste0("Expect that column `", column, "` exists")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <-
      ifelse(column %in% agent$focal_col_names, FALSE, TRUE)

    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_in_set", "not ", ""),
        "be part of set `", paste(set, collapse = ", "), "`"
      )
  }
  
  if (assertion_type %in%
      c("col_vals_between", "col_vals_not_between")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_between", "not ", ""),
        "be between `", left, "` and `", right, "`"
      )
  }
  
  if (assertion_type == "col_vals_regex") {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should match the regex expression `",
        regex, "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")),
        "values in `",
        column, "`",
        ifelse(is_column_computed, " (computed column) ", " "),
        "should ",
        ifelse(assertion_type == "col_vals_not_null", "not ", ""),
        "be NULL"
      )
  }
  
  if (grepl("col_is_.*", assertion_type)) {
    
    if (assertion_type %in% 
        c("col_is_numeric", "col_is_integer",
          "col_is_character", "col_is_logical",
          "col_is_factor")) {
      
      col_type <- gsub("col_is_", "", assertion_type)
    } else if (assertion_type == "col_is_posix") {
      col_type <- "POSIXct"
    } else if (assertion_type == "col_is_date") {
      col_type <- "Date"
    }
    
    autobrief <- 
      paste0("Expect that column `", column, "` is `", col_type, "`-based")
  }
  
  if (assertion_type == "rows_not_duplicated") {
    
    is_column_computed <- ifelse(column %in% agent$focal_col_names, FALSE, TRUE)
    
    autobrief <-
      paste0(
        "Expect that ",
        ifelse(
          !is.null(preconditions),
          paste0(
            "when the precondition ", "`",
            preconditions %>% rlang::f_rhs() %>% rlang::as_label(),
            "` is applied, "),
          paste0("")
        ),
        "rows from `", column, "` ", "have no duplicates"
      )
  }
  
  autobrief
}

ib_incl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= set[1] & {{ column }} <= set[2] ~ TRUE,
      {{ column }} < set[1] | {{ column }} > set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_excl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > set[1] & {{ column }} <= set[2] ~ TRUE,
      {{ column }} <= set[1] | {{ column }} > set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_incl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= set[1] & {{ column }} < set[2] ~ TRUE,
      {{ column }} < set[1] | {{ column }} >= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
ib_excl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > set[1] & {{ column }} < set[2] ~ TRUE,
      {{ column }} <= set[1] | {{ column }} >= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_incl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < set[1] | {{ column }} > set[2] ~ TRUE,
      {{ column }} >= set[1] & {{ column }} <= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_excl_incl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= set[1] | {{ column }} > set[2] ~ TRUE,
      {{ column }} > set[1] & {{ column }} <= set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_incl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < set[1] | {{ column }} >= set[2] ~ TRUE,
      {{ column }} >= set[1] & {{ column }} < set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}
nb_excl_excl <- function(table, column, set, incl_na) {
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= set[1] | {{ column }} >= set[2] ~ TRUE,
      {{ column }} > set[1] & {{ column }} < set[2] ~ FALSE,
      is.na({{ column }}) & incl_na ~ TRUE,
      is.na({{ column }}) & incl_na == FALSE ~ FALSE
    ))
}

#' Perform a single column validation that can issue warnings
#' 
#' @noRd
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
                            warn_count,
                            stop_count,
                            notify_count,
                            warn_fraction,
                            stop_fraction,
                            notify_fraction) {

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
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) == value
  }
  
  if (type == "col_vals_not_equal") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) != value
  }
  
  if (type == "col_vals_gt") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) > value
  }
  
  if (type == "col_vals_gte") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) >= value
  }
  
  if (type == "col_vals_lt") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) < value
  }
  
  if (type == "col_vals_lte") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) <= value
  }
  
  if (type == "col_vals_between") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals >= left &
      vals <= right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_not_between") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      vals < left |
      vals > right
    
    if (incl_na == TRUE) {
      logicals[which(is.na(logicals))] <- TRUE
    } else if (incl_na == FALSE) {
      logicals[which(is.na(logicals))] <- FALSE
    }
  }
  
  if (type == "col_vals_in_set") {
    
    logicals <- 
      tbl %>%
      dplyr::pull(col_number) %in% set
  }
  
  if (type == "col_vals_not_in_set") {
    
    logicals <- 
      !(tbl %>%
          dplyr::pull(col_number) %in% set)
  }
  
  if (type == "col_vals_regex") {
    
    vals <- 
      tbl %>%
      dplyr::pull(col_number)
    
    logicals <- 
      grepl(pattern = regex, x = vals)
  }
  
  if (type == "col_vals_not_null") {
    
    logicals <- 
      !is.na(tbl %>%
               dplyr::pull(col_number))
  }
  
  if (type == "col_vals_null") {
    
    logicals <- 
      is.na(tbl %>%
              dplyr::pull(col_number))
  }
  
  if (grepl("col_is_.*", type)) {
    
    # Get the column type
    column_type <-
      (tbl %>%
         dplyr::select(column) %>%
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
  
  if (!is.null(notify_count)) {
    if (false_count >= notify_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_count` threshold",
        " * `failing_count` ({false_count}) >= `notify_count` ({notify_count})",
        type = type,
        false_count = false_count,
        notify_count = notify_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(notify_fraction)) {
    if ((false_count/total_count) >= notify_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `notify_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `notify_fraction` ({notify_fraction})",
        type = type,
        false_fraction = false_fraction,
        notify_fraction = notify_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(stop_count)) {
    if (false_count >= stop_count) {
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_count` threshold",
        " * `failing_count` ({false_count}) >= `stop_count` ({stop_count})",
        type = type,
        false_count = false_count,
        stop_count = stop_count,
        .format = "{text}"
      )
    }
  } else if (!is.null(stop_fraction)) {
    if ((false_count/total_count) >= stop_fraction) {
      
      false_fraction <- round(false_fraction, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `stop_fraction` ({stop_fraction})",
        type = type,
        false_fraction = false_fraction,
        stop_fraction = stop_fraction,
        .format = "{text}"
      )
    }
  }
  
  if (!is.null(warn_count)) {
    if (false_count >= warn_count) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_count` threshold",
        " * `failing_count` ({false_count}) >= `warn_count` ({warn_count})",
        type = type,
        false_count = false_count,
        warn_count = warn_count,
        .format = "{text}"
      )
      
    }
  } else if (!is.null(warn_fraction)) {
    if ((false_count/total_count) >= warn_fraction) {
      
      messaging::emit_warning(
        "The validation (`{type}()`) meets or exceeds the `warn_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `warn_fraction` ({warn_fraction})",
        type = type,
        false_fraction = false_fraction,
        warn_fraction = warn_fraction,
        .format = "{text}"
      )
    }
  }
  
  x_ret
}
