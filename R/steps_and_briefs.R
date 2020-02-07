create_validation_step <- function(agent,
                                   assertion_type,
                                   column = NULL,
                                   values = NULL,
                                   na_pass = NULL,
                                   preconditions = NULL,
                                   actions = NULL,
                                   brief = NULL,
                                   active = NULL) {
  
  # Get the next step number (i)
  if (nrow(agent$validation_set) == 0) {
    i <- 1L
  } else {
    i <- max(agent$validation_set$i) + 1L
  }
  
  # Create a validation step as a single-row `tbl_df` object
  validation_step_df <-
    dplyr::tibble(
      i = i,
      assertion_type = assertion_type,
      column = ifelse(is.null(column), list(NULL), list(column)),
      values = ifelse(is.null(values), list(NULL), list(values)),
      na_pass = ifelse(is.null(na_pass), as.logical(NA), as.logical(na_pass)),
      preconditions = ifelse(is.null(preconditions), list(NULL), list(preconditions)),
      actions = ifelse(is.null(actions), list(NULL), list(actions)),
      brief = ifelse(is.null(brief), NA_character_, as.character(brief)),
      active = ifelse(is.null(active), NA, active),
      eval_error = NA,
      eval_warning = NA,
      capture_stack = list(NULL),
      all_passed = as.logical(NA),
      n = NA_integer_,
      n_passed = NA_integer_,
      n_failed = NA_integer_,
      f_passed = NA_real_,
      f_failed = NA_real_
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

create_autobrief <- function(agent,
                             assertion_type,
                             preconditions = NULL,
                             column = NULL,
                             values = NULL) {
  
  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
        "should be ", operator, " ", values
      )
  }
  
  
  if (assertion_type == "col_exists") {
    autobrief <- paste0("Expect that column `", column, "` exists")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
        "be part of set `", paste(values, collapse = ", "), "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_between", "col_vals_not_between")) {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
        "be between `", values[1], "` and `", values[2], "`"
      )
  }
  
  if (assertion_type == "col_vals_regex") {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
        values, "`"
      )
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
  
  if (assertion_type == "rows_distinct") {
    
    is_column_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
    
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
