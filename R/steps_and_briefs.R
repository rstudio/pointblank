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
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


create_validation_step <- function(
    agent,
    assertion_type,
    i_o = NULL,
    columns_expr = NULL,
    column = NULL,
    values = NULL,
    na_pass = NULL,
    preconditions = NULL,
    seg_expr = NULL,
    seg_col = NULL,
    seg_val = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = NULL,
    .call = rlang::caller_env(n = 2L)
) {

  # Get the next step number (i)
  i <- get_next_validation_set_row(agent)

  # Calculate the SHA1 hash for the validation step
  sha1 <- hash_validation_step(
    assertion_type = assertion_type,
    column = column,
    values = values,
    na_pass = na_pass,
    preconditions = preconditions,
    seg_col = seg_col,
    seg_val = seg_val
  )

  # Create a validation step as a single-row `tbl_df` object
  validation_step_df <-
    dplyr::tibble(
      i = i,
      i_o = ifelse(is.null(i_o), NA_integer_, as.integer(i_o)),
      step_id = step_id,
      sha1 = sha1,
      assertion_type = assertion_type,
      columns_expr = ifelse(
        is.null(columns_expr), NA_character_, as.character(columns_expr)
      ),
      column = ifelse(is.null(column), list(NULL), list(column)),
      values = ifelse(is.null(values), list(NULL), list(values)),
      na_pass = ifelse(is.null(na_pass), as.logical(NA), as.logical(na_pass)),
      preconditions = ifelse(
        is.null(preconditions), list(NULL), list(preconditions)
      ),
      seg_expr = ifelse(
        is.null(seg_expr), list(NULL), list(seg_expr)
      ),
      seg_col = ifelse(
        is.null(seg_col), NA_character_, as.character(seg_col)
      ),
      seg_val = ifelse(
        is.null(seg_val), NA_character_, as.character(seg_val)
      ),
      actions = ifelse(is.null(actions), list(NULL), list(actions)),
      label = ifelse(is.null(label), NA_character_, as.character(label)),
      brief = ifelse(is.null(brief), NA_character_, as.character(brief)),
      brief_cls = as.character(class(.env$brief)[1]),
      active = ifelse(is.null(active), list(NULL), list(active)),
      eval_active = NA,
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

  # glue-powered labels
  if (!is.na(validation_step_df$label)) {
    glue_mask <- rlang::new_environment(
      data = list(
        .step = assertion_type,
        .col = column,
        .seg_col = seg_col,
        .seg_val = seg_val
      ),
      parent = .call
    )
    label <- glue::glue_data(glue_mask, validation_step_df$label, .envir = NULL)
    validation_step_df$label <- as.character(label)
  }

  # Append `validation_step` to `validation_set`
  agent$validation_set <-
    dplyr::bind_rows(agent$validation_set, validation_step_df)

  agent
}

get_hash_version <- function() {
  "v0.12"
}

hash_validation_step <- function(assertion_type,
                                 column = NULL,
                                 values = NULL,
                                 na_pass = NULL,
                                 preconditions = NULL,
                                 seg_col = NULL,
                                 seg_val = NULL) {

  # pkg version that introduced the current hash implementation
  hash_version <- get_hash_version()

  values <- if (is.null(values) || is_a_table_object(values)) {
    NA_character_
  } else if (is.list(values)) {
    # Resolve `vars()` to scalar string
    toString(vapply(values, deparse_expr, character(1)))
  } else {
    deparse_expr(values)
  }

  preconditions <- if (inherits(preconditions, "fseq")) {
    # Spell out components of magrittr anonymous function
    magrittr_fn_seq <- environment(preconditions)[["_function_list"]]
    deparse_expr(magrittr_fn_seq)
  } else {
    deparse_expr(preconditions)
  }

  step_chr <- c(
    assertion_type = assertion_type,
    column = as.character(column %||% NA_character_),
    values = values,
    na_pass = as.character(na_pass %||% NA_character_),
    preconditions = preconditions,
    seg_col = as.character(seg_col %||% NA_character_),
    seg_val = as.character(seg_val %||% NA_character_)
  )

  # Maybe consider replacing with rlang::hash?
  step_hash <- digest::sha1(step_chr)

  paste(step_hash, hash_version, sep = "-")

}

apply_preconditions_to_tbl <- function(agent, idx, tbl) {

  preconditions <- agent$validation_set$preconditions[[idx]]

  tbl <- apply_preconditions(tbl = tbl, preconditions = preconditions)

  tbl
}

apply_segments_to_tbl <- function(agent, idx, tbl) {

  # Extract the `seg_col` and `seg_val` values for the validation step
  seg_col <- agent$validation_set$seg_col[[idx]]
  seg_val <- agent$validation_set$seg_val[[idx]]

  # If either of `seg_col` or `seg_val` is NA then return
  # the table unchanged
  if (is.na(seg_col) || is.na(seg_val)) {
    return(tbl)
  }

  # Generate a second set of 'preconditions' to filter the table
  seg_val <- gsub("'", "\\\\'", seg_val)
  preconditions <-
    stats::as.formula(
      glue::glue("~ . %>% dplyr::filter(`{seg_col}` == '{seg_val}')")
    )

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

      stop(
        "If using formula syntax to define `preconditions`, the RHS ",
        "must resolve to a functional sequence.",
        call. = FALSE
      )
    }

  } else {

    stop(
      "If providing `preconditions` it must either be as a function ",
      "or a formula.",
      call. = FALSE
    )
  }

  tbl
}

create_autobrief <- function(
    agent,
    assertion_type,
    preconditions = NULL,
    column = NULL,
    values = NULL
) {

  if (assertion_type == "serially") {

    # Develop a brief that explains how many tests are included
    # and what the final expectation is (if included)

    # Get number of test steps
    test_step_count <- values$total_test_calls

    # Get assertion type for final validation, if present
    has_final_validation <- values$has_final_validation

    if (has_final_validation) {
      assertion_type <- values$final_validation_type
    } else {
      autobrief <- ""
    }

    preconditions <- values$final_validation_preconditions
    column <- values$final_validation_column
    values <- values$final_validation_values

    finalize_serially_brief <- TRUE

  } else {

    finalize_serially_brief <- FALSE
  }

  lang <- agent$lang

  precondition_text <-
    prep_precondition_text(
      preconditions = preconditions,
      lang = lang
    )

  column_computed_text <-
    prep_column_computed_text(
      agent = agent,
      column = column,
      lang = lang
    )

  column_text <- prep_column_text(column = column)

  if (assertion_type %in%
      c("col_vals_gt", "col_vals_gte",
        "col_vals_lt", "col_vals_lte",
        "col_vals_equal", "col_vals_not_equal")) {

    values_text <- prep_values_text(values = values, lang = lang)

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

    values_text <- prep_values_text(values = values, lang = lang)

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

  if (assertion_type == "col_vals_make_set") {

    values_text <- prep_values_text(values = values, lang = lang)

    expectation_text <-
      prep_make_set_expectation_text(
        column_text,
        column_computed_text,
        values_text,
        lang = lang
      )

    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_vals_make_subset") {

    values_text <- prep_values_text(values = values, lang = lang)

    expectation_text <-
      prep_make_subset_expectation_text(
        column_text,
        column_computed_text,
        values_text,
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

    values_text <- prep_values_text(values = values, lang = lang)

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

  if (assertion_type == "col_vals_increasing") {

    expectation_text <-
      prep_increasing_expectation_text(
        column_text,
        column_computed_text,
        lang
      )

    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_vals_decreasing") {

    expectation_text <-
      prep_decreasing_expectation_text(
        column_text,
        column_computed_text,
        lang
      )

    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_vals_regex") {

    values_text <- prep_values_text(values = values, lang = lang)

    expectation_text <-
      prep_regex_expectation_text(
        column_text,
        column_computed_text,
        values_text,
        lang = lang
      )

    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_vals_within_spec") {

    values_text <- prep_values_text(values = values, lang = lang)

    expectation_text <-
      prep_within_spec_expectation_text(
        column_text,
        column_computed_text,
        values_text,
        lang = lang
      )

    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_vals_expr") {

    expectation_text <- prep_col_vals_expr_expectation_text(lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (grepl("col_is_.*", assertion_type)) {

    col_type <- prep_col_type(fn_name = assertion_type)
    expectation_text <-
      prep_col_is_expectation_text(column_text, col_type, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "rows_distinct") {

    expectation_text <-
      prep_row_distinct_expectation_text(column_text, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "rows_complete") {

    expectation_text <-
      prep_row_complete_expectation_text(column_text, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "col_schema_match") {

    expectation_text <- prep_col_schema_match_expectation_text(lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "row_count_match") {

    if (!is.numeric(values)) {

      expectation_text <- prep_row_count_match_expectation_text(lang = lang)
      autobrief <- finalize_autobrief(expectation_text, precondition_text)

    } else {

      values_text <- prep_values_text(values = values, lang = lang)

      expectation_text <-
        prep_row_count_match_n_expectation_text(values_text, lang = lang)
      autobrief <- finalize_autobrief(expectation_text, precondition_text)
    }
  }

  if (assertion_type == "col_count_match") {

    if (!is.numeric(values)) {

      expectation_text <- prep_col_count_match_expectation_text(lang = lang)
      autobrief <- finalize_autobrief(expectation_text, precondition_text)

    } else {

      values_text <- prep_values_text(values = values, lang = lang)

      expectation_text <-
        prep_col_count_match_n_expectation_text(values_text, lang = lang)
      autobrief <- finalize_autobrief(expectation_text, precondition_text)
    }
  }

  if (assertion_type == "conjointly") {

    values_text <-
      prep_values_text(values = values, lang = lang) %>%
      tidy_gsub("\"", "'")

    expectation_text <-
      prep_conjointly_expectation_text(values_text, lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "specially") {

    expectation_text <- prep_specially_expectation_text(lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (assertion_type == "tbl_match") {

    expectation_text <- prep_tbl_match_expectation_text(lang = lang)
    autobrief <- finalize_autobrief(expectation_text, precondition_text)
  }

  if (finalize_serially_brief) {

    serially_test_text <-
      get_serially_test_text(
        test_step_count = test_step_count,
        lang = lang
      )

    if (test_step_count == 1) {

      autobrief <- serially_test_text

    } else {

      autobrief <- paste(serially_test_text, autobrief)
    }
  }

  autobrief
}

finalize_autobrief <- function(
    expectation_text,
    precondition_text
) {

  glue::glue("{expectation_text} {precondition_text}") %>%
    as.character() %>%
    tidy_gsub("\\s{2,}", " ")
}

generate_autobriefs <- function(
    agent,
    columns,
    preconditions,
    values,
    assertion_type
) {

  vapply(
    columns,
    USE.NAMES = FALSE,
    FUN.VALUE = character(1),
    FUN = function(x) {
      create_autobrief(
        agent = agent,
        assertion_type = assertion_type,
        preconditions = preconditions,
        column = x,
        values = values
      )
    }
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

prep_col_type <- function(fn_name) {

  if (!grepl("col_is", fn_name)) {
    return(NA_character_)
  }

  if (grepl("col_is_(numeric|integer|character|logical|factor)", fn_name)) {
    col_type <- gsub("col_is_", "", fn_name)
  } else if (grepl("col_is_posix", fn_name)) {
    col_type <- "POSIXct"
  } else if (grepl("col_is_date", fn_name)) {
    col_type <- "Date"
  }

  col_type
}

prep_precondition_text <- function(
    preconditions,
    lang
) {

  if (is.null(preconditions)) return("")

  if (rlang::is_formula(preconditions)) {
    precondition_label <- preconditions %>% rlang::f_rhs() %>% rlang::as_label()
  } else {
    precondition_label <- preconditions %>% rlang::as_label()
  }

  paste0(
    get_lsv("autobriefs/precondition_text")[[lang]],
    ": `",
    precondition_label,
    "`."
  )
}

prep_column_computed_text <- function(
    agent,
    column,
    lang
) {

  if (is.null(column) || is.null(agent$col_names)) return("")

  column_is_computed <- ifelse(column %in% agent$col_names, FALSE, TRUE)

  if (!column_is_computed) return("")

  parens <- c("(", ")")

  paste0(
    parens[1],
    get_lsv("autobriefs/column_computed_text")[[lang]],
    parens[2]
  )
}

prep_column_text <- function(column) {
  paste0("`", column, "`")
}

prep_values_text <- function(
    values,
    limit = 3,
    lang
) {

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

    additional_text <-
      glue::glue(
        glue::glue(
          "(", get_lsv("autobriefs/values_text")[[lang]], ")"
        )
      )

    values_str <- paste0(values_str, " ", additional_text)

  } else {
    values_str <- paste0("`", values, "`", collapse = ", ")
  }

  values_str
}

# nolint start

prep_compare_expectation_text <- function(
    column_text,
    column_computed_text,
    operator,
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/compare_expectation_text")[[lang]])
}

prep_in_set_expectation_text <- function(
    column_text,
    column_computed_text,
    values_text,
    not = FALSE,
    lang
) {
  if (!not) {
    glue::glue(get_lsv("autobriefs/in_set_expectation_text")[[lang]])
  } else {
    glue::glue(get_lsv("autobriefs/not_in_set_expectation_text")[[lang]])
  }
}

prep_make_set_expectation_text <- function(
    column_text,
    column_computed_text,
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/make_set_expectation_text")[[lang]])
}

prep_make_subset_expectation_text <- function(
    column_text,
    column_computed_text,
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/make_subset_expectation_text")[[lang]])
}

prep_between_expectation_text <- function(
    column_text,
    column_computed_text,
    value_1,
    value_2,
    not = FALSE,
    lang
) {
  if (!not) {
    glue::glue(get_lsv("autobriefs/between_expectation_text")[[lang]])
  } else {
    glue::glue(get_lsv("autobriefs/not_between_expectation_text")[[lang]])
  }
}

prep_null_expectation_text <- function(
    column_text,
    column_computed_text,
    not = FALSE,
    lang
) {
  if (!not) {
    glue::glue(get_lsv("autobriefs/null_expectation_text")[[lang]])
  } else {
    glue::glue(get_lsv("autobriefs/not_null_expectation_text")[[lang]])
  }
}

prep_increasing_expectation_text <- function(
    column_text,
    column_computed_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/increasing_expectation_text")[[lang]])
}

prep_decreasing_expectation_text <- function(
    column_text,
    column_computed_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/decreasing_expectation_text")[[lang]])
}


prep_regex_expectation_text <- function(
    column_text,
    column_computed_text,
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/regex_expectation_text")[[lang]])
}

prep_within_spec_expectation_text <- function(
    column_text,
    column_computed_text,
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/within_spec_expectation_text")[[lang]])
}

prep_conjointly_expectation_text <- function(
    values_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/conjointly_expectation_text")[[lang]])
}

prep_specially_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/specially_expectation_text")[[lang]])
}

get_serially_test_text <- function(
    test_step_count,
    lang
) {
  if (test_step_count > 1) {
    as.character(
      glue::glue(get_lsv("autobriefs/serially_expectation_tests_text")[[lang]])
    )
  } else {
    get_lsv("autobriefs/serially_expectation_test_text")[[lang]]
  }
}

prep_col_exists_expectation_text <- function(
    column_text,
    lang
) {
  glue::glue(get_lsv("autobriefs/col_exists_expectation_text")[[lang]])
}

prep_col_is_expectation_text <- function(
    column_text,
    col_type,
    lang
) {
  glue::glue(get_lsv("autobriefs/col_is_expectation_text")[[lang]])
}

prep_row_distinct_expectation_text <- function(
    column_text,
    lang
) {
  if (column_text == "``") {
    glue::glue(
      get_lsv("autobriefs/all_row_distinct_expectation_text")[[lang]]
    )
  } else {
    glue::glue(
      get_lsv("autobriefs/across_row_distinct_expectation_text")[[lang]]
    )
  }
}

prep_row_complete_expectation_text <- function(
    column_text,
    lang
) {
  if (column_text == "``") {
    glue::glue(
      get_lsv("autobriefs/all_row_complete_expectation_text")[[lang]]
    )
  } else {
    glue::glue(
      get_lsv("autobriefs/across_row_complete_expectation_text")[[lang]]
    )
  }
}

prep_col_schema_match_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/col_schema_match_expectation_text")[[lang]])
}

prep_row_count_match_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/row_count_match_expectation_text")[[lang]])
}

prep_row_count_match_n_expectation_text <- function(values_text, lang) {
  glue::glue(get_lsv("autobriefs/row_count_match_n_expectation_text")[[lang]])
}

prep_col_count_match_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/col_count_match_expectation_text")[[lang]])
}

prep_col_count_match_n_expectation_text <- function(values_text, lang) {
  glue::glue(get_lsv("autobriefs/col_count_match_n_expectation_text")[[lang]])
}

prep_tbl_match_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/tbl_match_expectation_text")[[lang]])
}

prep_col_vals_expr_expectation_text <- function(lang) {
  glue::glue(get_lsv("autobriefs/col_vals_expr_expectation_text")[[lang]])
}

# nolint end

failure_message_gluestring <- function(
    fn_name,
    lang
) {

  if (!grepl("^expect", fn_name)) {
    fn_name <- paste0("expect_", fn_name)
  }

  # nolint start

  # TODO: Add all missing entries for failure text

  failure_text <-
    switch(
      fn_name,
      "expect_col_vals_gt" =,
      "expect_col_vals_gte" =,
      "expect_col_vals_lt" =,
      "expect_col_vals_lte" =,
      "expect_col_vals_equal" =,
      "expect_col_vals_not_equal" = get_lsv("autobriefs/compare_failure_text")[[lang]],
      "expect_col_vals_between" = get_lsv("autobriefs/between_failure_text")[[lang]],
      "expect_col_vals_not_between" = get_lsv("autobriefs/not_between_failure_text")[[lang]],
      "expect_col_vals_in_set" = get_lsv("autobriefs/in_set_failure_text")[[lang]],
      "expect_col_vals_not_in_set" = get_lsv("autobriefs/not_in_set_failure_text")[[lang]],
      "expect_col_vals_null" = get_lsv("autobriefs/null_failure_text")[[lang]],
      "expect_col_vals_not_null" = get_lsv("autobriefs/not_null_failure_text")[[lang]],
      "expect_col_vals_increasing" = get_lsv("autobriefs/increasing_failure_text")[[lang]],
      "expect_col_vals_decreasing" = get_lsv("autobriefs/decreasing_failure_text")[[lang]],
      "expect_col_vals_regex" = get_lsv("autobriefs/regex_failure_text")[[lang]],
      "expect_conjointly" = get_lsv("autobriefs/conjointly_failure_text")[[lang]],
      "expect_col_exists" = get_lsv("autobriefs/col_exists_failure_text")[[lang]],
      "expect_col_is_numeric" =,
      "expect_col_is_integer" =,
      "expect_col_is_character" =,
      "expect_col_is_logical" =,
      "expect_col_is_posix" =,
      "expect_col_is_date" =,
      "expect_col_is_factor" = get_lsv("autobriefs/col_is_failure_text")[[lang]],
      "expect_rows_distinct" = get_lsv("autobriefs/all_row_distinct_failure_text")[[lang]],
      "expect_rows_complete" = get_lsv("autobriefs/all_row_complete_failure_text")[[lang]],
      "expect_col_schema_match" = get_lsv("autobriefs/col_schema_match_failure_text")[[lang]],
      "expect_row_count_match" = get_lsv("autobriefs/row_count_match_failure_text")[[lang]],
      "expect_col_count_match" = get_lsv("autobriefs/col_count_match_failure_text")[[lang]],
      "expect_tbl_match" = get_lsv("autobriefs/tbl_match_failure_text")[[lang]]
    )

  # nolint end

  paste0(
    failure_text, "\n",
    "The `{fn_name}()` validation failed beyond the {threshold_type} ",
    "threshold level ({threshold}).\n",
    "* failure level ({failed_amount}) >= failure threshold ({threshold})"
  )
}
