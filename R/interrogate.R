#' Given an agent that has a validation plan, perform an interrogation
#'
#' When the agent has all the information on what to do (i.e., a validation plan
#' which is a series of validation steps), the interrogation process can occur
#' according its plan. After that, the agent will have gathered intel, and we
#' can use functions like [get_agent_report()] and [all_passed()] to understand
#' how the interrogation went down.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param extract_failed An option to collect rows that didn't pass a particular
#'   validation step. The default is `TRUE` and further options allow for fine
#'   control of how these rows are collected.
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
#' # Create a simple table with two
#' # columns of numerical values
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' # Validate that values in column
#' # `a` from `tbl` are always > 5,
#' # using `interrogate()` carries out
#' # the validation plan and completes
#' # the whole process
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), 5) %>%
#'   interrogate()
#' 
#' @family Interrogate
#' @section Function ID:
#' 4-1
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
  
  if (agent$name == "::QUIET::" || !interactive()) {
    quiet <- TRUE
  } else {
    quiet <- FALSE
  }
  
  # Get the agent's validation step indices
  validation_steps <- unique(agent$validation_set$i)
  
  # Add start of interrogation console status
  create_cli_header(validation_steps, quiet)
  
  for (i in validation_steps) {

    # Skip the validation step if `active = FALSE`
    if (!agent$validation_set[[i, "active"]]) {
      cli::cli_alert_info("Step {.field {i}} is not set as {.field active}. Skipping.")
      next
    }
    
    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
    
    # Get the table object for interrogation 
    table <- get_tbl_object(agent)
    
    # Use the default `action_levels` list if it exists and
    # only if it isn't set for this validation step
    if (!is.null(agent$actions) && is.null(agent$validation_set$actions[i][[1]])) {
      agent$validation_set[[i, "actions"]] <- list(agent$actions)
    }
    
    # Use preconditions to modify the table
    table <- apply_preconditions_to_tbl(agent, idx = i, tbl = table)

    # Get the assertion type for this verification step
    assertion_type <- get_assertion_type_at_idx(agent, idx = i)

    if (assertion_type != "conjointly") {

      # Perform table checking based on assertion type
      tbl_checked <- check_table_with_assertion(agent, idx = i, table, assertion_type)
      
    } else if (assertion_type == "conjointly") {
      
      validation_formulas <- get_values_at_idx(agent = agent, idx = i)
      
      validation_n <- length(validation_formulas)

      # Create a double agent
      double_agent <- create_agent(tbl = agent$tbl)

      for (formula in validation_formulas) {

        double_agent <-
          eval(
            expr = parse(
              text =
                formula %>%
                rlang::f_rhs() %>%
                rlang::expr_deparse() %>%
                tidy_gsub("(.", "(double_agent", fixed = TRUE)
            ),
            envir = NULL
          )
      }

      tbl_checked <- table
      
      for (j in seq(nrow(double_agent$validation_set))) {
        
        # Get the assertion type for this verification step
        assertion_type <- get_assertion_type_at_idx(agent = double_agent, idx = j)
        
        new_col <- paste0("pb_is_good_", j)

        tbl_checked <- 
          check_table_with_assertion(
            agent = double_agent,
            idx = j,
            table = tbl_checked,
            assertion_type
          )
        
        tbl_checked <- tbl_checked$value
        
        tbl_checked <-
          tbl_checked %>%
          dplyr::rename(!!new_col := pb_is_good_)
      }
      
      columns_str_vec <- paste0("pb_is_good_", seq(j))
      columns_str_add <- paste0("pb_is_good_", seq(j), collapse = " + ")

      # Create function for validating step functions conjointly
      tbl_val_conjointly <- function(table,
                                     columns_str_add,
                                     columns_str_vec,
                                     validation_n) {
        
        tbl_checked %>%
          dplyr::mutate(pb_is_good_ = !!rlang::parse_expr(columns_str_add)) %>%
          dplyr::select(-dplyr::one_of(columns_str_vec)) %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            pb_is_good_ == validation_n ~ TRUE,
            TRUE ~ FALSE
          ))
      }
      
      # Perform rowwise validations for the column
      tbl_checked <-
        pointblank_try_catch(
          tbl_val_conjointly(
            table, columns_str_add, columns_str_vec, validation_n
          )
        )
    }

    # Add in the necessary reporting data for the validation
    agent <- add_reporting_data(agent, idx = i, tbl_checked = tbl_checked)
    
    # Perform any necessary actions if threshold levels are exceeded
    perform_action(agent, idx = i, type = "warn")
    perform_action(agent, idx = i, type = "notify")
    perform_action(agent, idx = i, type = "stop")

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
    
    create_post_step_cli_output(agent, i, time_diff_s, quiet)
  }
  
  class(agent) <- c("has_intel", "ptblank_agent")
  
  # nocov start
  
  # Generate gt-based reporting objects
  if (agent$embed_report) {
    
    gt_agent_report <- get_agent_report(agent)
    gt_agent_report_email <- get_agent_report(agent, size = "small")
    
    agent$reporting <-
      list(
        report_object = gt_agent_report,
        report_object_email = gt_agent_report_email
      )
  }
  
  # nocov end
  
  # Perform any necessary end actions
  perform_end_action(agent)
  
  # Add closing rule of interrogation console status
  create_cli_footer(quiet)
  
  agent
}

# nocov start

create_cli_header <- function(validation_steps, quiet) {
  
  if (quiet) return()
  
  if (length(validation_steps) < 1) {
    interrogation_progress_header <- 
      "Interrogation Started - there are no validation steps"
  } else if (length(validation_steps) == 1) {
    interrogation_progress_header <- 
      "Interrogation Started - there is a single validation step"
  } else {
    num_validation_steps <- max(validation_steps)
    interrogation_progress_header <- 
      "Interrogation Started - there are {num_validation_steps} steps"
  }
  
  cli::cli_h1(interrogation_progress_header)
}

create_cli_footer <- function(quiet) {
  
  if (quiet) return()
  
  interrogation_progress_footer <- "Interrogation Completed"
  
  cli::cli_h1(interrogation_progress_footer)
}

create_post_step_cli_output <- function(agent, i, time_diff_s, quiet) {
  
  if (quiet) return()
  
  interrogation_evaluation <- 
    agent$validation_set[i, ] %>%
    dplyr::select(eval_error, eval_warning) %>%
    dplyr::mutate(condition = dplyr::case_when(
      !eval_error & !eval_warning ~ "OK",
      eval_error & eval_warning ~ "{.yellow WARNING} + {.red ERROR}",
      eval_error ~ "{.red ERROR}",
      eval_warning ~ "{.yellow WARNING}"
    )) %>%
    dplyr::pull(condition)
  
  validation_condition <-
    agent$validation_set[i, ] %>%
    dplyr::select(warn, stop) %>%
    dplyr::mutate(condition = dplyr::case_when(
      is.na(warn) & is.na(stop) ~ "NONE",
      !is.na(stop) && stop ~ "STOP",
      !is.na(warn) && warn ~ "WARN",
      TRUE ~ "NONE"
    )) %>% 
    dplyr::pull(condition)
  
  notify_condition <-
    agent$validation_set[i, ] %>%
    dplyr::select(notify) %>%
    dplyr::mutate(condition = dplyr::case_when(
      !is.na(notify) && notify ~ "NOTIFY",
      TRUE ~ "NONE"
    )) %>% 
    dplyr::pull(condition)
  
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
  
  cli::cli_div(
    theme = list(
      span.green = list(color = "green"),
      span.red = list(color = "red"),
      span.yellow = list(color = "yellow"),
      span.blue = list(color = "blue"),
      span.time_taken = list(color = "magenta")
    )
  )
  if (interrogation_evaluation != "OK") {
    cli::cli_alert_info(
      c("Step {.field {i}}: an evaluation issue requires attention ",
        "(", interrogation_evaluation, ").",
        print_time(time_diff_s)
      )
    )
  } else if (validation_condition == "NONE" & notify_condition == "NONE") {
    cli::cli_alert_success(
      c("Step {.field {i}}: {.green OK}.", print_time(time_diff_s))
    )
  } else if (validation_condition != "NONE" & notify_condition == "NONE") {
    if (validation_condition == "STOP") {
      cli::cli_alert_danger(
        c("Step {.field {i}}: {.red STOP} condition met.", print_time(time_diff_s))
      )
    } else {
      cli::cli_alert_warning(
        c("Step {.field {i}}: {.yellow WARNING} condition met.", print_time(time_diff_s))
      )
    }
  } else if (validation_condition != "NONE" & notify_condition != "NONE") {
    if (validation_condition == "STOP") {
      cli::cli_alert_danger(
        c("Step {.field {i}}: {.red STOP} and {.blue NOTIFY} conditions met.", print_time(time_diff_s))
      )
    } else {
      cli::cli_alert_warning(
        c("Step {.field {i}}: {.yellow WARNING} and {.blue NOTIFY} conditions met.", print_time(time_diff_s))
      )
    }
  } else if (validation_condition == "NONE" & notify_condition != "NONE") {
    cli::cli_alert_warning(
      c("Step {.field {i}}: {.blue NOTIFY} condition met.", print_time(time_diff_s))
    )
  }
  cli::cli_end()
}

# nocov end

check_table_with_assertion <- function(agent, idx, table, assertion_type) {
  
    switch(
      assertion_type,
      "col_vals_gt" =,
      "col_vals_gte" =,
      "col_vals_lt" =,
      "col_vals_lte" =,
      "col_vals_equal" =,
      "col_vals_not_equal" = interrogate_comparison(agent, idx, table, assertion_type),
      "col_vals_between" =,
      "col_vals_not_between" = interrogate_between(agent, idx, table, assertion_type),
      "col_vals_in_set" =,
      "col_vals_not_in_set" = interrogate_set(agent, idx, table, assertion_type),
      "col_vals_null" = interrogate_null(agent, idx, table),
      "col_vals_not_null" = interrogate_not_null(agent, idx, table),
      "col_vals_regex" = interrogate_regex(agent, idx, table),
      "col_exists" = interrogate_col_exists(agent, idx, table),
      "col_is_numeric" =,
      "col_is_integer" =,
      "col_is_character" =,
      "col_is_logical" =,
      "col_is_posix" =,
      "col_is_date" =,
      "col_is_factor" = interrogate_col_type(agent, idx, table, assertion_type),
      "rows_distinct" = interrogate_distinct(agent, idx, table),
      "col_schema_match" = interrogate_col_schema_match(agent, idx, table)
    )
}

interrogate_comparison <- function(agent, idx, table, assertion_type) {

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
  
  # Get the value for the expression
  value <- get_values_at_idx(agent = agent, idx = idx)

  # Normalize a column in `vars()` to a `name` object
  if (inherits(value, "list")) {
    value <- value[1][[1]] %>% rlang::get_expr()
  }
  
  # Obtain the target column as a label
  column <- 
    get_column_as_sym_at_idx(agent = agent, idx = idx) %>%
    rlang::as_label()
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_comparison(table, column, operator, value, na_pass))
}
# Function for validating comparison step functions
tbl_val_comparison <- function(table, column, operator, value, na_pass) {
  
  column_validity_checks_column_value(table = table, column = {{ column }}, value = {{ value }})
  
  # Construct a string-based expression for the validation
  expression <- paste(column, operator, value)
  
  table %>%
    dplyr::mutate(pb_is_good_ = !!rlang::parse_expr(expression)) %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      is.na(pb_is_good_) ~ na_pass,
      TRUE ~ pb_is_good_
    ))
}

interrogate_between <- function(agent, idx, table, assertion_type) {

  # Get the set values for the expression
  set <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  left <- set[1]
  right <- set[2]
  
  # Normalize `left` and `right` to `name` objects
  # (if they are given as columns in `vars()`)
  if (inherits(left, "list")) {
    left <- left[[1]] %>% rlang::get_expr()
  } else {
    left <- unname(left)
  }
  if (inherits(right, "list")) {
    right <- right[[1]] %>% rlang::get_expr()
  } else {
    right <- unname(right)
  }

  incl_str <- paste(ifelse(names(set) %>% as.logical(), "incl", "excl"), collapse = "_")

  if (assertion_type == "col_vals_between") {
    
    # Perform rowwise validations for the column
    tbl_evaled <- 
      switch(
        incl_str,
        "incl_incl" = 
          pointblank_try_catch(tbl_val_ib_incl_incl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "excl_incl" = 
          pointblank_try_catch(tbl_val_ib_excl_incl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "incl_excl" = 
          pointblank_try_catch(tbl_val_ib_incl_excl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "excl_excl" = 
          pointblank_try_catch(tbl_val_ib_excl_excl(table, {{ column }}, {{ left }}, {{ right }}, na_pass))
      )
  }
  
  if (assertion_type == "col_vals_not_between") {
    
    # Perform rowwise validations for the column
    tbl_evaled <- 
      switch(
        incl_str,
        "incl_incl" = 
          pointblank_try_catch(tbl_val_nb_incl_incl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "excl_incl" = 
          pointblank_try_catch(tbl_val_nb_excl_incl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "incl_excl" = 
          pointblank_try_catch(tbl_val_nb_incl_excl(table, {{ column }}, {{ left }}, {{ right }}, na_pass)),
        "excl_excl" = 
          pointblank_try_catch(tbl_val_nb_excl_excl(table, {{ column }}, {{ left }}, {{ right }}, na_pass))
      )
  }
  
  tbl_evaled
}
tbl_val_ib_incl_incl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= {{ left }} & {{ column }} <= {{ right }} ~ TRUE,
      {{ column }} < {{ left }} | {{ column }} > {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_ib_excl_incl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > {{ left }} & {{ column }} <= {{ right }} ~ TRUE,
      {{ column }} <= {{ left }} | {{ column }} > {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_ib_incl_excl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} >= {{ left }} & {{ column }} < {{ right }} ~ TRUE,
      {{ column }} < {{ left }} | {{ column }} >= {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_ib_excl_excl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} > {{ left }} & {{ column }} < {{ right }} ~ TRUE,
      {{ column }} <= {{ left }} | {{ column }} >= {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_nb_incl_incl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < {{ left }} | {{ column }} > {{ right }} ~ TRUE,
      {{ column }} >= {{ left }} & {{ column }} <= {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_nb_excl_incl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= {{ left }} | {{ column }} > {{ right }} ~ TRUE,
      {{ column }} > {{ left }} & {{ column }} <= {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_nb_incl_excl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} < {{ left }} | {{ column }} >= {{ right }} ~ TRUE,
      {{ column }} >= {{ left }} & {{ column }} < {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}
tbl_val_nb_excl_excl <- function(table, column, left, right, na_pass) {
  
  column_validity_checks_ib_nb(table = table, column = {{ column }}, left = {{ left }}, right = {{ right }})
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      {{ column }} <= {{ left }} | {{ column }} >= {{ right }} ~ TRUE,
      {{ column }} > {{ left }} & {{ column }} < {{ right }} ~ FALSE,
      is.na({{ column }}) & na_pass ~ TRUE,
      is.na({{ column }}) & na_pass == FALSE ~ FALSE
    ))
}

interrogate_set <- function(agent, idx, table, assertion_type) {

  # Get the set values for the expression
  set <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine if an NA value is part of the set
  na_pass <- any(is.na(set))
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  if (assertion_type == "col_vals_in_set") {
    
    # Create function for validating the `col_vals_in_set()` step function
    tbl_val_in_set <- function(table, column, na_pass) {
      
      column_validity_checks_column(table = table, column = {{ column }})
      
      table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          {{ column }} %in% set ~ TRUE,
          !({{ column }} %in% set) ~ FALSE
        )) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
    }
    
    # Perform rowwise validations for the column
    tbl_evaled <- pointblank_try_catch(tbl_val_in_set(table, {{ column }}, na_pass))
  }
  
  if (assertion_type == "col_vals_not_in_set") {
    
    # Create function for validating the `col_vals_not_in_set()` step function
    tbl_val_not_in_set <- function(table, column, na_pass) {
      
      column_validity_checks_column(table = table, column = {{ column }})
      
      table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          !({{ column }} %in% set) ~ TRUE,
          {{ column }} %in% set ~ FALSE
        )) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ !na_pass,
          TRUE ~ pb_is_good_
        ))
    }
  
    # Perform rowwise validations for the column
    tbl_evaled <- pointblank_try_catch(tbl_val_not_in_set(table, {{ column }}, na_pass))
  }
  
  tbl_evaled
}

interrogate_regex <- function(agent, idx, table) {
  
  # Get the regex matching statement
  regex <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_regex()` step function
  tbl_val_regex <- function(table, column, regex, na_pass) {
    
    column_validity_checks_column(table = table, column = {{ column }})
    
    table %>% 
      dplyr::mutate(pb_is_good_ = ifelse(!is.na({{ column }}), grepl(regex, {{ column }}), NA)) %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        is.na(pb_is_good_) ~ na_pass,
        TRUE ~ pb_is_good_
      ))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_regex(table, {{ column }}, regex, na_pass))
}

interrogate_null <- function(agent, idx, table) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_null()` step function
  tbl_val_null <- function(table, column) {
    
    column_validity_checks_column(table = table, column = {{ column }})
    
    table %>% dplyr::mutate(pb_is_good_ = is.na({{ column }}))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_null(table, {{ column }}))
}

interrogate_not_null <- function(agent, idx, table) {

  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_null()` step function
  tbl_val_not_null <- function(table, column) {
    
    column_validity_checks_column(table = table, column = {{ column }})
    
    table %>% dplyr::mutate(pb_is_good_ = !is.na({{ column }}))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_not_null(table, {{ column }}))
}

interrogate_col_exists <- function(agent, idx, table) {

  # Get the column names for the table
  column_names <- get_all_cols(agent = agent)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_exists()` step function
  tbl_col_exists <- function(table, column, column_names) {
    dplyr::tibble(pb_is_good_ = as.character(column) %in% column_names)
  }
  
  # Perform the validation of the column
  pointblank_try_catch(tbl_col_exists(table, {{ column }}, column_names))
}

interrogate_col_type <- function(agent, idx, table, assertion_type) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_is_*()` step functions
  tbl_col_is <- function(table, column, assertion_type) {
    
    column_validity_checks_column(table = table, column = {{ column }})
    
    column_class <-
      table %>%
      dplyr::select({{ column }}) %>%
      utils::head(1) %>%
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
    
    dplyr::tibble(pb_is_good_ = validation_res)
  }
  
  # Perform the validation of the column
  pointblank_try_catch(tbl_col_is(table, {{ column }}, assertion_type))
}

interrogate_distinct <- function(agent, idx, table) {

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
  
  # Create function for validating the `rows_distinct()` step function
  tbl_rows_distinct <- function(table, column_names, col_syms) {
    
    table %>%
      dplyr::select({{ column_names }}) %>%
      dplyr::group_by(!!!col_syms) %>%
      dplyr::mutate(`pb_is_good_` = ifelse(dplyr::n() == 1, TRUE, FALSE)) %>%
      dplyr::ungroup()
  }
  
  # Create another variation of `tbl_rows_distinct_1()` that works for MySQL
  tbl_rows_distinct_mysql <- function(table, column_names, col_syms) {

    unduplicated <- 
      table %>%
      dplyr::select({{ column_names }}) %>%
      dplyr::group_by(!!!col_syms) %>%
      dplyr::summarize(`pb_is_good_` = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(`pb_is_good_` = ifelse(`pb_is_good_` == 1, TRUE, FALSE)) %>%
      dplyr::filter(`pb_is_good_` == TRUE)

    table %>%
      dplyr::select({{ column_names }}) %>%
      dplyr::left_join(unduplicated, by = column_names) %>%
      dplyr::mutate(`pb_is_good_` = ifelse(is.na(`pb_is_good_`), FALSE, TRUE))
  }
  
  # Perform the validation of the table
  if (agent$tbl_src == "mysql") {
    pointblank_try_catch(tbl_rows_distinct_mysql(table, {{ column_names }}, col_syms))
  } else {
    pointblank_try_catch(tbl_rows_distinct(table, {{ column_names }}, col_syms))
  }
}

interrogate_col_schema_match <- function(agent, idx, table) {

  # Get the reference `col_schema` object
  table_schema_y <- agent$validation_set$values[[idx]]
  
  # Get the `table` `col_schema` object
  if (inherits(table, "tbl_dbi")) {
    
    if (inherits(table_schema_y, "sql_type")) {
      
      if (all(!is.na(agent$db_col_types))) {
        table_schema_x <- create_col_schema_from_names_types(agent$col_names, agent$db_col_types)
        class(table_schema_x) <- c("sql_type", "col_schema")
      } else {
        table_schema_x <- col_schema(.tbl = table, .db_col_types = "sql")
      }
      
    } else if (inherits(table_schema_y, "r_type")) {
      
      if (all(!is.na(agent$col_types))) {
        table_schema_x <- create_col_schema_from_names_types(agent$col_names, agent$col_types)
        class(table_schema_x) <- c("r_type", "col_schema")
      } else {
        table_schema_x <- col_schema(.tbl = table, .db_col_types = "r")
      }
    }
    
  } else {
    table_schema_x <- col_schema(.tbl = table)
  }
  
  # Create function for validating the `col_schema_match()` step function
  tbl_col_schema_match <- function(table, table_schema_x, table_schema_y) {
    
    if (identical(table_schema_x, table_schema_y)) {
      dplyr::tibble(pb_is_good_ = TRUE)
    } else {
      dplyr::tibble(pb_is_good_ = FALSE)
    }
  }

  # Perform the validation of the table 
  pointblank_try_catch(tbl_col_schema_match(table, table_schema_x, table_schema_y))
}

# Validity checks for the column and value 
column_validity_checks_column_value <- function(table, column, value) {
  
  table_colnames <- colnames(table)
  if (!(as.character(column) %in% table_colnames)) {
    stop("The value for `column` doesn't correspond to a column name.")
  }
  if (inherits(value, "name")) {
    if (!(as.character(value) %in% table_colnames)) {
      stop("The column supplied as the `value` doesn't correspond to a column name.")
    }
  }
}

# Validity check for the column
column_validity_checks_column <- function(table, column) {
  
  table_colnames <- colnames(table)
  if (!(as.character(column) %in% table_colnames)) {
    stop("The value for `column` doesn't correspond to a column name.")
  }
}

# Validity checks for `tbl_val_ib_*()` functions
column_validity_checks_ib_nb <- function(table, column, left, right) {
  
  table_colnames <- colnames(table)
  
  if (!(as.character(column) %in% table_colnames)) {
    stop("The value for `column` doesn't correspond to a column name.")
  }
  if (inherits(left, "name")) {
    if (!(as.character(left) %in% table_colnames)) {
      stop("The column supplied as the `left` value doesn't correspond to a column name.")
    }
  }
  if (inherits(right, "name")) {
    if (!(as.character(right) %in% table_colnames)) {
      stop("The column supplied as the `right` value doesn't correspond to a column name.")
    }
  }
}

pointblank_try_catch <- function(expr) {
  
  warn <- err <- NULL
  
  value <- 
    withCallingHandlers(
      tryCatch(expr, error = function(e) {
        err <<- e
        NULL
      }), warning = function(w) {
        warn <<- w
        invokeRestart("muffleWarning")
      })
  
  eval_list <- 
    list(value = value, warning = warn, error = err)

  class(eval_list) <- "table_eval"
  eval_list
}

add_reporting_data <- function(agent, idx, tbl_checked) {

  if (!inherits(tbl_checked, "table_eval")) {
    stop("The validated table must be of class `table_eval`.")
  }

  has_warnings <- !is.null(tbl_checked$warning)
  has_error <- !is.null(tbl_checked$error)

  capture_stack <- tbl_checked[c("warning", "error")]
  
  agent$validation_set$eval_warning[idx] <- has_warnings
  agent$validation_set$eval_error[idx] <- has_error
  agent$validation_set$capture_stack[[idx]] <- capture_stack
  
  if (is.null(tbl_checked$value)) {
    return(agent)
  }

  # Store the `tbl_checked$value` tbl
  agent$validation_set$tbl_checked[[idx]] <- list(tbl_checked$value)

  tbl_checked <- tbl_checked$value

  # Get total count of rows
  row_count <- 
    tbl_checked %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  # Get total count of TRUE rows
  n_passed <-
    tbl_checked %>%
    dplyr::filter(pb_is_good_ == TRUE) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  # Get total count of FALSE rows
  n_failed <-
    tbl_checked %>%
    dplyr::filter(pb_is_good_ == FALSE) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.numeric()
  
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
    
  determine_action(agent, idx, false_count = n_failed)
}

perform_action <- function(agent, idx, type) {

  actions <- agent$validation_set[[idx, "actions"]] %>% unlist(recursive = FALSE)
  
  .warn <- agent$validation_set[[idx, "warn"]]
  .notify <- agent$validation_set[[idx, "notify"]]
  .stop <- agent$validation_set[[idx, "stop"]]
  
  .name <- agent$name
  .time <- agent$time
  .tbl <- agent$tbl
  .tbl_name <- agent$tbl_name
  .tbl_src <- agent$tbl_src
  .tbl_src_details <- agent$tbl_src_details
  .col_names <- agent$col_names
  .col_types <- agent$col_types
  
  .i <- idx
  .type <- agent$validation_set[[idx, "assertion_type"]]
  .column <- agent$validation_set[[idx, "column"]] %>% unlist()
  .values <- agent$validation_set[[idx, "values"]] %>% unlist()
  .brief <- agent$validation_set[[idx, "brief"]]
  
  .eval_error <- agent$validation_set[[idx, "eval_error"]]
  .eval_warning <- agent$validation_set[[idx, "eval_warning"]]
  .capture_stack <- agent$validation_set[[idx, "capture_stack"]]
  
  .n <- agent$validation_set[[idx, "n"]]
  .n_passed <- agent$validation_set[[idx, "n_passed"]]
  .n_failed <- agent$validation_set[[idx, "n_failed"]]
  .f_passed <- agent$validation_set[[idx, "f_passed"]]
  .f_failed <- agent$validation_set[[idx, "f_failed"]]
  
  # Have the local vars packaged in a list to make creating
  # custom functions more convenient
  x <-
    list(
      warn = .warn,
      notify = .notify,
      stop = .stop,
      name = .name,
      time = .time,
      tbl = .tbl,
      tbl_name = .tbl_name,
      tbl_src = .tbl_src,
      tbl_src_details = .tbl_src_details,
      col_names = .col_names,
      col_types = .col_types,
      i = .i,
      type = .type,
      column = .column,
      values = .values,
      brief = .brief,
      eval_error = .eval_error,
      eval_warning = .eval_warning,
      capture_stack = .capture_stack,
      n = .n,
      n_passed = .n_passed,
      n_failed = .n_failed,
      f_passed = .f_passed,
      f_failed = .f_failed
    )

  if (type == "warn") {
    if (!is.na(.warn) && .warn) {
      if ("warn" %in% names(actions$fns) && !is.null(actions$fns$warn)) {
        actions$fns$warn %>% rlang::f_rhs() %>% rlang::eval_tidy()
      }
    }
  } else if (type == "notify") {
    if (!is.na(.notify) && .notify) {
      if ("notify" %in% names(actions$fns) && !is.null(actions$fns$notify)) {
        actions$fns$notify %>% rlang::f_rhs() %>% rlang::eval_tidy()
      }
    }
  } else if (type == "stop") {
    if (!is.na(.stop) && .stop) {
      if ("stop" %in% names(actions$fns) && !is.null(actions$fns$stop)) {
        actions$fns$stop %>% rlang::f_rhs() %>% rlang::eval_tidy()
      }
    }
  }
  
  return(NULL)
}

perform_end_action <- function(agent) {

  actions <- agent$end_fns %>% unlist()

  .warn <- agent$validation_set$warn
  .notify <- agent$validation_set$notify
  .stop <- agent$validation_set$stop
  
  .name <- agent$name
  .time <- agent$time
  .tbl <- agent$tbl
  .tbl_name <- agent$tbl_name
  .tbl_src <- agent$tbl_src
  .tbl_src_details <- agent$tbl_src_details
  .col_names <- agent$col_names
  .col_types <- agent$col_types
  
  .i <- agent$validation_set$i
  .type <- agent$validation_set$assertion_type
  .column <- agent$validation_set$column
  .values <- agent$validation_set$values
  .brief <- agent$validation_set$brief
  
  .eval_error <- agent$validation_set$eval_error
  .eval_warning <- agent$validation_set$eval_warning
  .capture_stack <- agent$validation_set$capture_stack
  
  .n <- agent$validation_set$n
  .n_passed <- agent$validation_set$n_passed
  .n_failed <- agent$validation_set$n_failed
  .f_passed <- agent$validation_set$f_passed
  .f_failed <- agent$validation_set$f_failed
  
  .validation_set <- agent$validation_set
  
  .report_object <- agent$reporting$report_object
  .report_object_email <- agent$reporting$report_object_email
  
  if (!is.null(.report_object)) {
    .report_html <- gt::as_raw_html(.report_object, inline_css = FALSE)
  } else {
    .report_html <- NULL
  }
  
  if (!is.null(.report_object_email)) {
    .report_html_email <- gt::as_raw_html(.report_object_email, inline_css = TRUE)
  } else {
    .report_html_email <- NULL
  }

  # Have the local vars packaged in a list to make creating
  # custom functions more convenient
  x <-
    list(
      warn = .warn,
      notify = .notify,
      stop = .stop,
      name = .name,
      time = .time,
      tbl = .tbl,
      tbl_name = .tbl_name,
      tbl_src = .tbl_src,
      tbl_src_details = .tbl_src_details,
      col_names = .col_names,
      col_types = .col_types,
      i = .i,
      type = .type,
      column = .column,
      values = .values,
      brief = .brief,
      eval_error = .eval_error,
      eval_warning = .eval_warning,
      capture_stack = .capture_stack,
      n = .n,
      n_passed = .n_passed,
      n_failed = .n_failed,
      f_passed = .f_passed,
      f_failed = .f_failed,
      validation_set = .validation_set,
      report_object = .report_object,
      report_html = .report_html,
      report_html_email = .report_html_email
    )

  lapply(actions, function(y) {
    y %>% rlang::f_rhs() %>% rlang::eval_tidy()
  })
  
  return(NULL)
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

  if (is.null(tbl_checked$value)) {
    return(agent)
  }
  
  tbl_checked <- tbl_checked$value
  
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
    
  } else if (all(!is.null(sample_n) & 
                 ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type))) {
    
    problem_rows <-
      dplyr::sample_n(
        tbl = problem_rows,
        size = sample_n,
        replace = FALSE) %>%
      dplyr::as_tibble()
    
  } else if (all(!is.null(sample_frac) & 
                 ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type))) {
    
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
  
  # Place the sample of problem rows in `agent$extracts`
  if (nrow(problem_rows) > 0) {
    
    list_i <- list(problem_rows)
    list_i <- rlang::set_names(list_i, idx)
    
    agent$extracts <- c(agent$extracts, list_i)
  }
  
  agent
}

determine_action <- function(agent, idx, false_count) {

  al <- agent$validation_set[[idx, "actions"]] %>% unlist(recursive = FALSE)
  n <- agent$validation_set[[idx, "n"]]

  warn <- stop <- notify <- FALSE
  
  if (is.null(al$warn_count) && is.null(al$warn_fraction)) {
    warn <- NA
  }
  if (is.null(al$stop_count) && is.null(al$stop_fraction)) {
    stop <- NA
  }
  if (is.null(al$notify_count) && is.null(al$notify_fraction)) {
    notify <- NA
  }

  if (!is.na(warn)) {
    if (is.null(al$warn_count)) {
      warn <- FALSE
    } else if (false_count >= al$warn_count) {
      warn <- TRUE
    }
  }
  
  if (!is.na(stop)) {
    if (is.null(al$stop_count)) {
      stop <- FALSE
    } else if (false_count >= al$stop_count) {
      stop <- TRUE
    }
  }
  
  if (!is.na(notify)) {
    if (is.null(al$notify_count)) {
      notify <- FALSE
    } else if (false_count >= al$notify_count) {
      notify <- TRUE
    }
  }
  
  if (!is.na(warn)) {
    if (!is.null(al$warn_fraction)) {
      warn_count <- round(al$warn_fraction * n, 0)
      if (false_count > 0 && false_count >= warn_count) warn <- TRUE
    }
  }
  
  if (!is.na(stop)) {
    if (!is.null(al$stop_fraction)) {
      stop_count <- round(al$stop_fraction * n, 0)
      if (false_count > 0 && false_count >= stop_count) stop <- TRUE
    }
  }
  
  if (!is.na(notify)) {
    if (!is.null(al$notify_fraction)) {
      notify_count <- round(al$notify_fraction * n, 0)
      if (false_count > 0 && false_count >= notify_count) notify <- TRUE
    }
  }
  
  agent$validation_set[[idx, "warn"]] <- warn
  agent$validation_set[[idx, "notify"]] <- notify
  agent$validation_set[[idx, "stop"]] <- stop
  
  agent
}
