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
  
  tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)
  
  tbl
}

apply_preconditions <- function(tbl, preconditions) {
  
  if (is.null(preconditions)) {
    return(tbl)
  }
  
  if (is.function(preconditions)) {
    
    tbl <- preconditions(tbl)
    
  } else if (rlang::is_formula(preconditions)) {
    
    # Take the RHS of `preconditions` and eval with `eval_tidy()`
    preconditions <- 
      preconditions %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
    
    if (inherits(preconditions, "fseq")) {
      
      tbl <- preconditions(tbl)
      
    } else {
      stop("If using formula syntax to define `preconditions`, the RHS ",
           "must resolve to a functional sequence.",
           call. = FALSE)
    }
    
  } else {
    stop("If providing `preconditions` it must either be as a function or a formula.",
         call. = FALSE)
  }
  
  tbl
}

create_autobrief <- function(agent,
                             assertion_type,
                             preconditions = NULL,
                             column = NULL,
                             values = NULL) {

  lang <- agent$reporting_lang
  precondition_text <- prep_precondition_text(preconditions, lang = lang)
  column_computed_text <- prep_column_computed_text(agent, column, lang = lang)
  values_text <- prep_values_text(values, lang = lang)
  column_text <- prep_column_text(column)
  
  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {
    
    operator <- prep_operator_text(fn_name = assertion_type)

    expectation_text <- 
      prep_compare_expectation_text(
        column_text,
        column_computed_text,
        operator,
        values_text,
        lang = lang
      )
    
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type == "col_exists") {
    
    autobrief <- 
      prep_col_exists_expectation_text(column_text, lang = lang) %>%
      as.character() %>%
      tidy_gsub("\\s{2,}", " ")
  }
  
  if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {
    
    expectation_text <- 
      prep_in_set_expectation_text(
        column_text,
        column_computed_text,
        values_text,
        not = grepl("not", assertion_type),
        lang = lang
      )
    
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type %in% c("col_vals_null", "col_vals_not_null")) {
    
    expectation_text <- 
      prep_null_expectation_text(
        column_text,
        column_computed_text,
        not = grepl("not", assertion_type),
        lang = lang
      )
    
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type %in% c("col_vals_between", "col_vals_not_between")) {
    
    value_1 <- strsplit(values_text, ", ") %>% unlist() %>% .[1]
    value_2 <- strsplit(values_text, ", ") %>% unlist() %>% .[2]
    
    expectation_text <- 
      prep_between_expectation_text(
        column_text,
        column_computed_text,
        value_1,
        value_2,
        not = grepl("not", assertion_type),
        lang = lang
      )
    
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type == "col_vals_regex") {
    
    expectation_text <- 
      prep_regex_expectation_text(
        column_text,
        column_computed_text,
        values_text,
        lang = lang
      )
    
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
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
    
    expectation_text <- prep_col_is_expectation_text(column_text, col_type, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type == "rows_distinct") {

    expectation_text <- prep_row_distinct_expectation_text(column_text, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type == "col_schema_match") {
    
    expectation_text <- prep_col_schema_match_expectation_text(lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  if (assertion_type == "conjointly") {

    values_text <- values_text %>% tidy_gsub("\"", "'")
    expectation_text <- prep_conjointly_expectation_text(values_text, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }
  
  autobrief
}

finalize_autobrief <- function(expectation_text, precondition_text) {
  
  glue::glue("{expectation_text} {precondition_text}") %>%
    as.character() %>%
    tidy_gsub("\\s{2,}", " ")
}

generate_autobriefs <- function(agent, columns, preconditions, values, assertion_type) {
  
  vapply(
    columns,
    USE.NAMES = FALSE,
    FUN.VALUE = character(1),
    FUN = function(x)
      create_autobrief(
        agent = agent,
        assertion_type = assertion_type,
        preconditions = preconditions,
        column = x,
        values = values
      )
  )
}

prep_operator_text <- function(fn_name) {
  
  switch(
    fn_name,
    "col_vals_gt" = ">",
    "col_vals_gte" = ">=",
    "col_vals_lt" = "<",
    "col_vals_lte" = "<=",
    "col_vals_equal" = "==",
    "col_vals_not_equal" = "!=",
    NA_character_
  )
}


prep_precondition_text <- function(preconditions, lang) {
  
  if (is.null(preconditions)) return("")

  if (rlang::is_formula(preconditions)) {
    precondition_label <- preconditions %>% rlang::f_rhs() %>% rlang::as_label()
  } else {
    precondition_label <- preconditions %>% rlang::as_label()
  }

  paste0(precondition_text[lang], ": `", precondition_label, "`.")
}

prep_column_computed_text <- function(agent, column, lang) {
  
  if (is.null(column)) return("")
  
  column_is_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)
  
  if (!column_is_computed) return("")
  
  parens <- c("(", ")")
  
  paste0(parens[1], column_computed_text[lang], parens[2])
}

prep_column_text <- function(column) {
  paste0("`", column, "`")
}

prep_values_text <- function(values, limit = 3, lang) {
  
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
    values_str <- paste0("`", values[seq_len(limit)], "`", collapse = ", ")
    additional_text <- glue::glue(glue::glue("(", values_text[lang], ")"))
    values_str <- paste0(values_str, " ", additional_text)
  } else {
    values_str <- paste0("`", values, "`", collapse = ", ")
  }

  values_str
}

prep_compare_expectation_text <- function(column_text,
                                          column_computed_text,
                                          operator,
                                          values_text,
                                          lang) {
  
  glue::glue(compare_expectation_text[lang])
}

prep_in_set_expectation_text <- function(column_text,
                                         column_computed_text,
                                         values_text,
                                         not = FALSE,
                                         lang) {
  
  if (!not) {
    glue::glue(in_set_expectation_text[lang])
  } else {
    glue::glue(not_in_set_expectation_text[lang])
  }
}

prep_between_expectation_text <- function(column_text,
                                          column_computed_text,
                                          value_1,
                                          value_2,
                                          not = FALSE,
                                          lang) {
  
  if (!not) {
    glue::glue(between_expectation_text[lang])
  } else {
    glue::glue(not_between_expectation_text[lang])
  }
}

prep_null_expectation_text <- function(column_text,
                                       column_computed_text,
                                       not = FALSE,
                                       lang) {
  
  if (!not) {
    glue::glue(null_expectation_text[lang])
  } else {
    glue::glue(not_null_expectation_text[lang])
  }
}

prep_regex_expectation_text <- function(column_text,
                                        column_computed_text,
                                        values_text,
                                        lang) {

  glue::glue(regex_expectation_text[lang])
}

prep_conjointly_expectation_text <- function(values_text, lang) {
  
  glue::glue(conjointly_expectation_text[lang])
}

prep_col_exists_expectation_text <- function(column_text, lang) {
  
  glue::glue(col_exists_expectation_text[lang])
}

prep_col_is_expectation_text <- function(column_text, col_type, lang) {
  
  glue::glue(col_is_expectation_text[lang])
}

prep_row_distinct_expectation_text <- function(column_text, lang) {

  if (column_text == "``") {
    glue::glue(all_row_distinct_expectation_text[lang])
  } else {
    glue::glue(across_row_distinct_expectation_text[lang])
  }
}

prep_col_schema_match_expectation_text <- function(lang) {
  
  glue::glue(col_schema_match_expectation_text[lang])
}
