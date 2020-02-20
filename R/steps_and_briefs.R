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
  
  precondition_text <- prep_precondition_text(preconditions)
  column_computed_text <- prep_column_computed_text(agent, column)
  column_text <- prep_column_text(column)
  values_text <- prep_values_text(values)
  
  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {
    
    operator <- 
      switch(
        assertion_type,
        "col_vals_gt" = ">",
        "col_vals_gte" = ">=",
        "col_vals_lt" = "<",
        "col_vals_lte" = "<=",
        "col_vals_equal" = "==",
        "col_vals_not_equal" = "!="
      )
    
    expectation_str <- 
      prep_compare_expectation_str(
        column_text,
        column_computed_text,
        operator,
        values_text
      )
    
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type == "col_exists") {
    
    autobrief <- 
      prep_col_exists_expectation_str(column_text) %>%
      as.character() %>%
      tidy_gsub("\\s{2,}", " ")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    expectation_str <- 
      prep_in_set_expectation_str(
        column_text,
        column_computed_text,
        values_text,
        not = grepl("not", assertion_type)
      )
    
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    expectation_str <- 
      prep_null_expectation_str(
        column_text,
        column_computed_text,
        not = grepl("not", assertion_type)
      )
    
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type %in% c("col_vals_between", "col_vals_not_between")) {
    
    value_1 <- strsplit(values_text, ", ") %>% unlist() %>% .[1]
    value_2 <- strsplit(values_text, ", ") %>% unlist() %>% .[2]
    
    expectation_str <- 
      prep_between_expectation_str(
        column_text,
        column_computed_text,
        value_1,
        value_2,
        not = grepl("not", assertion_type)
      )
    
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type == "col_vals_regex") {
    
    expectation_str <- 
      prep_regex_expectation_str(
        column_text,
        column_computed_text,
        values_text
      )
    
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (grepl("col_is_.*", assertion_type)) {
    
    if (assertion_type %in% 
        c("col_is_numeric", "col_is_integer", "col_is_character",
          "col_is_logical", "col_is_factor")) {
      col_type <- gsub("col_is_", "", assertion_type)
    } else if (assertion_type == "col_is_posix") {
      col_type <- "POSIXct"
    } else if (assertion_type == "col_is_date") {
      col_type <- "Date"
    }
    
    expectation_str <- prep_col_is_expectation_str(column_text, col_type)
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type == "rows_distinct") {

    expectation_str <- prep_row_distinct_expectation_str(column_text)
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  if (assertion_type == "conjointly") {

    values_text <- values_text %>% tidy_gsub("\"", "'")
    expectation_str <- prep_conjointly_expectation_str(values_text)
    autobrief <- finalize_autobrief(expectation_str, precondition_text)
  }
  
  autobrief
}

prep_precondition_text <- function(preconditions) {
  
  if (is.null(preconditions)) return("")
  
  precondition_label <- preconditions %>% rlang::f_rhs() %>% rlang::as_label()
  
  paste0("Precondition applied", ": `", precondition_label, "`.")
}

prep_column_computed_text <- function(agent, column) {
  
  if (is.null(column)) return("")
  
  column_is_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
  
  if (!column_is_computed) return("")
  
  parens <- c("(", ")")
  
  paste0(parens[1], "computed column", parens[2])
}

prep_column_text <- function(column) {
  paste0("`", column, "`")
}

prep_values_text <- function(values, limit = 3) {
  
  # Normalize the `columns` expression enclosed in `vars()`
  if (inherits(values, "quosures")) {
    
    values <- 
      vapply(
        values,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) as.character(rlang::get_expr(x))
      )
  }
  
  if (!is.null(limit) && length(values) > limit) {
    num_omitted <- length(values) - limit
    values_text <- paste0("`", values[seq_len(limit)], "`", collapse = ", ")
    additional_text <- glue::glue("(", "and {num_omitted} more", ")")
    values_text <- paste0(values_text, " ", additional_text)
  } else {
    values_text <- paste0("`", values, "`", collapse = ", ")
  }

  values_text
}

finalize_autobrief <- function(expectation_str, precondition_text) {
  
  glue::glue("{expectation_str} {precondition_text}") %>%
    as.character() %>%
    tidy_gsub("\\s{2,}", " ")
}

prep_compare_expectation_str <- function(column_text,
                                         column_computed_text,
                                         operator,
                                         values_text) {
  
  glue::glue(
    "Expect that values in {column_text} {column_computed_text} should be {operator} {values_text}."
  )
}

prep_in_set_expectation_str <- function(column_text,
                                        column_computed_text,
                                        values_text,
                                        not = FALSE) {
  
  if (!not) {
    glue::glue("Expect that values in {column_text} {column_computed_text} should be in the set of {values_text}.")
  } else {
    glue::glue("Expect that values in {column_text} {column_computed_text} should not be in the set of {values_text}.")
  }
}

prep_between_expectation_str <- function(column_text,
                                         column_computed_text,
                                         value_1,
                                         value_2,
                                         not = FALSE) {
  
  if (!not) {
    glue::glue("Expect that values in {column_text} {column_computed_text} should be between {value_1} and {value_2}.")
  } else {
    glue::glue("Expect that values in {column_text} {column_computed_text} should not be between {value_1} and {value_2}.")
  }
}

prep_null_expectation_str <- function(column_text,
                                      column_computed_text,
                                      not = FALSE) {
  
  if (!not) {
    glue::glue("Expect that all values in {column_text} {column_computed_text} should be NULL.")
  } else {
    glue::glue("Expect that all values in {column_text} {column_computed_text} should not be NULL.")
  }
}

prep_regex_expectation_str <- function(column_text,
                                       column_computed_text,
                                       values_text) {
  
  glue::glue(
    "Expect that values in {column_text} {column_computed_text} should match the regular expression: {values_text}."
  )
}

prep_col_exists_expectation_str <- function(column_text) {
  glue::glue("Expect that column {column_text} exists.")
}

prep_col_is_expectation_str <- function(column_text, col_type) {
  glue::glue("Expect that column {column_text} is of type: {col_type}.")
}

prep_row_distinct_expectation_str <- function(column_text) {

  if (column_text == "``") {
    glue::glue("Expect entirely distinct rows across all columns.")
  } else {
    glue::glue("Expect entirely distinct rows across {column_text}.")
  }
}

prep_conjointly_expectation_str <- function(values_text) {
  glue::glue("Expect conjoint 'pass' units across the following expressions: {values_text}.")
}
