#' Given an agent that is fully loaded with tasks, perform an interrogation
#'
#' The agent has all the information on what to do, so now all interrogations
#' can proceed efficiently, and, according to plan.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param extract_failed An option to collect rows that didn't pass a
#'   particular validation step. The default is `TRUE` and further options allow
#'   for fine control of how these rows are collected.
#' @param get_first_n If the option to collect non-passing rows is chosen, there
#'   is the option here to collect the first `n` rows here. Supply the number of
#'   rows to extract from the top of the non-passing rows table (the ordering of
#'   data from the original table is retained).
#' @param sample_n If the option to collect non-passing rows is chosen, this
#'   option allows for the sampling of `n` rows. Supply the number of rows to
#'   sample from the non-passing rows table. If `n` is greater than the number
#'   of non-passing rows, then all the rows will be returned.
#' @param sample_frac If the option to collect non-passing rows is chosen, this
#'   option allows for the sampling of a fraction of those rows. Provide a
#'   number in the range of `0` and `1`. The number of rows to return may be
#'   extremely large (and this is especially when querying remote databases),
#'   however, the `sample_limit` option will apply a hard limit to the returned
#'   rows.
#' @param sample_limit A value that limits the possible number of rows returned
#'   when sampling non-passing rows using the `sample_frac` option.
#'   
#' @return A `ptblank_agent` object.
#'   
#' @examples 
#' # Create a simple data frame with
#' # two columns of numerical values
#' df <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # Validate that values in column
#' # `a` from `df` are always > 5,
#' # using `interrogate()` completes
#' # the process
#' agent <-
#'   create_agent(tbl = df) %>%
#'   col_vals_gt(columns = vars(a), value = 5) %>%
#'   interrogate()
#'   
#' # Get a basic summary with
#' # `get_interrogation_summary()`
#' get_interrogation_summary(agent)[, 1:7]
#' 
#' @export
interrogate <- function(agent,
                        extract_failed = TRUE,
                        get_first_n = NULL,
                        sample_n = NULL,
                        sample_frac = NULL,
                        sample_limit = 5000) {

  # Add the starting time to the `agent` object
  agent$time <- Sys.time()
  
  for (i in seq_len(nrow(agent$validation_set))) {
    
    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
    
    # Get the table object for interrogation 
    table <- get_tbl_object(agent)
    
    # Use preconditions to modify the table
    table <- apply_preconditions_to_tbl(agent, idx = i, tbl = table)

    # Get the assertion type for this verification step
    assertion_type <- get_assertion_type_at_idx(agent, idx = i)
    
    # Perform table checking based on assertion type
    tbl_checked <-
      switch(
        assertion_type,
        "col_vals_gt" =,
        "col_vals_gte" =,
        "col_vals_lt" =,
        "col_vals_lte" =,
        "col_vals_equal" =,
        "col_vals_not_equal" = interrogate_comparison(agent, idx = i, table, assertion_type),
        "col_vals_between" =,
        "col_vals_not_between" = interrogate_between(agent, idx = i, table, assertion_type),
        "col_vals_in_set" =,
        "col_vals_not_in_set" = interrogate_set(agent, idx = i, table, assertion_type),
        "col_vals_null" = interrogate_null(agent, idx = i, table),
        "col_vals_not_null" = interrogate_not_null(agent, idx = i, table),
        "col_vals_regex" = interrogate_regex(agent, idx = i, table),
        "col_exists" = interrogate_col_exists(agent, idx = i, table),
        "col_is_numeric" =,
        "col_is_integer" =,
        "col_is_character" =,
        "col_is_logical" =,
        "col_is_posix" =,
        "col_is_date" =,
        "col_is_factor" = interrogate_col_type(agent, idx = i, table, assertion_type),
        "rows_not_duplicated" = interrogate_duplicated(agent, idx = i, table)
      )
    
    # Add in the necessary reporting data for the validation
    agent <-  add_reporting_data(agent, idx = i, tbl_checked = tbl_checked)

    # Add extracts of failed rows if `extract_failed` is TRUE
    agent <- 
      add_table_extract(
        agent = agent,
        idx = i,
        tbl_checked = tbl_checked,
        extract_failed = extract_failed,
        get_first_n = get_first_n,
        sample_n = sample_n,
        sample_frac = sample_frac,
        sample_limit = sample_limit
      )
    
    # Get the ending time for the validation step
    validation_end_time <- Sys.time()
    
    # Get the duration for the validation step    
    time_diff_s <- (validation_end_time - validation_start_time)[[1]] %>% round(4)
    
    # Add the timing information to the `agent` object
    agent$validation_set$time_processed[i] <- validation_start_time
    agent$validation_set$proc_duration_s[i] <- time_diff_s
  }
  
  class(agent) <- c("has_intel", "ptblank_agent")
  
  agent
}

interrogate_comparison <- function(agent, idx, table, assertion_type) {
  
  # Get the value for the expression
  value <- get_column_value_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- 
    get_column_as_sym_at_idx(agent = agent, idx = idx) %>%
    rlang::as_label()
  
  # Determine whether NAs should be allowed
  incl_na <- get_column_incl_na_at_idx(agent = agent, idx = idx)
  
  # Get operator values for all assertion types involving
  # simple operator comparisons
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
  
  # Construct a string-based expression for the validation
  expression <- paste(column, operator, value)
  
  # Perform rowwise validations for the column
  tbl_checked <-
    table %>%
    dplyr::mutate(pb_is_good_ = !!rlang::parse_expr(expression)) %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      is.na(pb_is_good_) ~ incl_na,
      TRUE ~ pb_is_good_
    ))
  
  tbl_checked
}

interrogate_between <- function(agent, idx, table, assertion_type) {
  
  # Get the set values for the expression
  set <- get_column_set_values_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  incl_na <- get_column_incl_na_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  incl_left <- names(set)[1] %>% as.logical()
  incl_right <- names(set)[2] %>% as.logical()
  incl_str <- paste(ifelse(names(set), "incl", "excl"), collapse = "_")
  
  if (assertion_type == "col_vals_between") {
    
    # Perform rowwise validations for the column
    tbl_checked <- 
      switch(
        incl_str,
        "incl_incl" = ib_incl_incl(table, {{column}}, set, incl_na),
        "excl_incl" = ib_excl_incl(table, {{column}}, set, incl_na),
        "incl_excl" = ib_incl_excl(table, {{column}}, set, incl_na),
        "excl_excl" = ib_excl_excl(table, {{column}}, set, incl_na)
      )
  }
  
  if (assertion_type == "col_vals_not_between") {
    
    # Perform rowwise validations for the column
    tbl_checked <- 
      switch(
        incl_str,
        "incl_incl" = nb_incl_incl(table, {{column}}, set, incl_na),
        "excl_incl" = nb_excl_incl(table, {{column}}, set, incl_na),
        "incl_excl" = nb_incl_excl(table, {{column}}, set, incl_na),
        "excl_excl" = nb_excl_excl(table, {{column}}, set, incl_na)
      )
  }
  
  tbl_checked
}

interrogate_set <- function(agent, idx, table, assertion_type) {
  
  # Get the set values for the expression
  set <- get_column_set_values_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  if (assertion_type == "col_vals_in_set") {
    
    # Perform rowwise validations for the column
    tbl_checked <-
      table %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        {{ column }} %in% set ~ TRUE,
        !({{ column }} %in% set) ~ FALSE
      ))
  }
  
  if (assertion_type == "col_vals_not_in_set") {
    
    # Perform rowwise validations for the column
    tbl_checked <-
      table %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        !({{ column }} %in% set) ~ TRUE,
        {{ column }} %in% set ~ FALSE
      ))
  }
  
  tbl_checked
}

interrogate_null <- function(agent, idx, table) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  tbl_checked <- 
    table %>%
    dplyr::mutate(pb_is_good_ = is.na({{ column }}))
  
  tbl_checked
}

interrogate_not_null <- function(agent, idx, table) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  tbl_checked <- 
    table %>%
    dplyr::mutate(pb_is_good_ = !is.na({{ column }}))
  
  tbl_checked
}

interrogate_regex <- function(agent, idx, table) {
  
  # Get the regex matching statement
  regex <- agent$validation_set$regex[idx]
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  incl_na <- get_column_incl_na_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  tbl_checked <- 
    table %>% 
    dplyr::mutate(pb_is_good_ = ifelse(!is.na({{ column }}), grepl(regex, {{ column }}), NA)) %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      is.na(pb_is_good_) ~ incl_na,
      TRUE ~ pb_is_good_
    ))
  
  tbl_checked
}

interrogate_col_exists <- function(agent, idx, table) {
  
  # Get the column names for the table
  column_names <- get_all_cols(agent = agent)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  tbl_checked <-
    dplyr::tibble(pb_is_good_ = as.character(column) %in% column_names)
  
  tbl_checked
}

interrogate_col_type <- function(agent, idx, table, assertion_type) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  column_class <-
    table %>%
    dplyr::select({{ column }}) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::as_tibble() %>%
    dplyr::pull({{ column }}) %>%
    class()
  
  validation_res <- 
    switch(
      column_class[1],
      "numeric" = ifelse(assertion_type == "col_is_numeric", TRUE, FALSE),
      "integer" = ifelse(assertion_type == "col_is_integer", TRUE, FALSE),
      "character" = ifelse(assertion_type == "col_is_character", TRUE, FALSE),
      "logical" = ifelse(assertion_type == "col_is_logical", TRUE, FALSE),
      "factor" = ifelse(assertion_type == "col_is_factor", TRUE, FALSE),
      "POSIXct" = ifelse(assertion_type == "col_is_posix", TRUE, FALSE),
      "Date" = ifelse(assertion_type == "col_is_date", TRUE, FALSE),
      FALSE
    )
  
  # Perform rowwise validations for the column
  tbl_checked <- dplyr::tibble(pb_is_good_ = validation_res)
  
  tbl_checked
}

interrogate_duplicated <- function(agent, idx, table) {

  # Determine if grouping columns are provided in the test
  # for distinct rows and parse the column names
  if (!is.na(agent$validation_set$column[idx] %>% unlist())) {
    
    column_names <- 
      get_column_as_sym_at_idx(agent = agent, idx = idx) %>%
      as.character()
    
    if (grepl("(,|&)", column_names)) {
      column_names <- 
        strsplit(split = "(, |,|&)", column_names) %>%
        unlist()
    }
    
  } else if (is.na(agent$validation_set$column[idx] %>% unlist())) {
    column_names <- get_all_cols(agent = agent)
  }
  
  col_syms <- rlang::syms(column_names)
  
  # Get total count of rows
  row_count <-
    table %>%
    dplyr::group_by() %>%
    dplyr::summarize(row_count = dplyr::n()) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(row_count)
  
  # Get the rows that are duplicate rows, if any
  duplicate_rows <- 
    table %>%
    dplyr::select({{ column_names }}) %>%
    dplyr::mutate(`__pb_index__` = seq(row_count)) %>%
    dplyr::group_by(!!!col_syms) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  duplicate_row_idx <-
    duplicate_rows %>%
    dplyr::pull(`__pb_index__`)
  
  # Determine whether the test for duplicated passed
  # (no duplicates) or failed (one or more duplicates)
  duplicate_count <-
    duplicate_rows %>%
    dplyr::group_by() %>%
    dplyr::summarize(row_count = dplyr::n()) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(row_count)
  
  validation_res <- ifelse(duplicate_count == 0, TRUE, FALSE)
  
  # Perform rowwise validations for the column
  tbl_checked <- 
    table %>%
    dplyr::mutate(pb_is_good_ = ifelse(
      dplyr::row_number() %in% duplicate_row_idx, FALSE, TRUE)
    )
  
  tbl_checked
}

add_reporting_data <- function(agent,
                               idx,
                               tbl_checked) {

  # Get total count of rows
  row_count <-
    tbl_checked %>%
    dplyr::group_by() %>%
    dplyr::summarize(row_count = dplyr::n()) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(row_count)
  
  # Get total count of TRUE rows
  n_passed <-
    tbl_checked %>%
    dplyr::filter(pb_is_good_ == TRUE) %>%
    dplyr::group_by() %>%
    dplyr::summarize(row_count = dplyr::n()) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(row_count)
  
  # Get total count of FALSE rows
  n_failed <-
    tbl_checked %>%
    dplyr::filter(pb_is_good_ == FALSE) %>%
    dplyr::group_by() %>%
    dplyr::summarize(row_count = dplyr::n()) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(row_count)
  
  agent$validation_set$n[idx] <- row_count
  agent$validation_set$n_passed[idx] <- n_passed
  agent$validation_set$n_failed[idx] <- n_failed
  agent$validation_set$f_passed[idx] <- round((n_passed / row_count), 5)
  agent$validation_set$f_failed[idx] <- round((n_failed / row_count), 5)
  
  if (n_failed > 0) {
    agent$validation_set$all_passed[idx] <- FALSE
  } else {
    agent$validation_set$all_passed[idx] <- TRUE
  }
  
  actions <-
    determine_action(
      validation_step = agent$validation_set[idx, ],
      false_count = n_failed,
      warn_count = agent$validation_set$warn_count[idx],
      stop_count = agent$validation_set$stop_count[idx],
      notify_count = agent$validation_set$notify_count[idx],
      warn_fraction = agent$validation_set$warn_fraction[idx],
      stop_fraction = agent$validation_set$stop_fraction[idx],
      notify_fraction = agent$validation_set$notify_fraction[idx]
    )
  
  agent$validation_set$notify[idx] <- actions$notify
  agent$validation_set$warn[idx] <- actions$warn
  
  agent
}

add_table_extract <- function(agent,
                              idx,
                              tbl_checked,
                              extract_failed,
                              get_first_n,
                              sample_n,
                              sample_frac,
                              sample_limit) {
  
  if (!extract_failed) {
    return(agent)
  }
  
  tbl_type <- tbl_checked %>% class()
  
  problem_rows <- 
    tbl_checked %>%
    dplyr::filter(pb_is_good_ == FALSE) %>%
    dplyr::select(-pb_is_good_)
  
  if (!is.null(get_first_n)) {
    
    problem_rows <-
      problem_rows %>%
      utils::head(get_first_n) %>%
      dplyr::as_tibble()
    
  } else if (all(!is.null(sample_n) & ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type))) {
    
    problem_rows <-
      dplyr::sample_n(
        tbl = problem_rows,
        size = sample_n,
        replace = FALSE) %>%
      dplyr::as_tibble()
    
  } else if (all(!is.null(sample_frac) & ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type))) {
    
    problem_rows <-
      dplyr::sample_frac(
        tbl = problem_rows,
        size = sample_frac,
        replace = FALSE) %>%
      dplyr::as_tibble() %>%
      utils::head(sample_limit)
    
  } else {
    
    problem_rows <-
      problem_rows %>%
      utils::head(5000) %>%
      dplyr::as_tibble()
  }
  
  # TODO: Make this a list object
  # Place the sample of problem rows in `agent$row_samples`
  
  if (nrow(problem_rows) > 0) {
    
    list_i <- list(problem_rows)
    list_i <- rlang::set_names(list_i, idx)
    
    agent$extracts <- c(agent$extracts, list_i)
  }
  
  agent
}

#' Determine the course of action for a given verification step
#' 
#' @noRd
determine_action <- function(validation_step,
                             false_count,
                             warn_count,
                             stop_count,
                             notify_count,
                             warn_fraction,
                             stop_fraction,
                             notify_fraction) {
  
  n <- validation_step$n[[1]]
  
  if (is.na(warn_count)) {
    warn <- FALSE
  } else {
    if (false_count >= warn_count) {
      warn <- TRUE
    } else {
      warn <- FALSE
    }
  }
  
  if (is.na(stop_count)) {
    stop <- FALSE
  } else {
    
    if (false_count >= stop_count) {
      
      type <- validation_step$assertion_type
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_count` threshold",
        " * `failing_count` ({false_count}) >= `stop_count` ({stop_count})",
        type = type,
        false_count = false_count,
        stop_count = stop_count,
        .format = "{text}"
      )
      
    } else {
      stop <- FALSE
    }
  }
  
  if (is.na(notify_count)) {
    notify <- FALSE
  } else {
    if (false_count >= notify_count) {
      notify <- TRUE
    } else {
      notify <- FALSE
    }
  }
  
  if (!is.na(warn_fraction)) {
    
    warn_count <- round(warn_fraction * n, 0)
    
    if (false_count >= warn_count) {
      warn <- TRUE
    } else {
      warn <- FALSE
    }
  }
  
  if (!is.na(stop_fraction)) {
    
    stop_count <- round(stop_fraction * n, 0)
    
    if (false_count >= stop_count) {
      
      type <- validation_step$assertion_type
      
      false_fraction <- round(false_count / n, 3)
      
      messaging::emit_error(
        "The validation (`{type}()`) meets or exceeds the `stop_fraction` threshold",
        " * `failing_fraction` ({false_fraction}) >= `stop_fraction` ({stop_fraction})",
        type = type,
        false_fraction = false_fraction,
        stop_fraction = stop_fraction,
        .format = "{text}"
      )
      
    } else {
      stop <- FALSE
    }
  }
  
  if (!is.na(notify_fraction)) {
    
    notify_count <- round(notify_fraction * n, 0)
    
    if (false_count >= notify_count) {
      notify <- TRUE
    } else {
      notify <- FALSE
    }
  }
  
  # Generate a tibble with action information
  dplyr::tibble(warn = warn, notify = notify)
}
