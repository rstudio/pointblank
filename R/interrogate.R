#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Given an agent that has a validation plan, perform an interrogation
#'
#' @description 
#' When the agent has all the information on what to do (i.e., a validation plan
#' which is a series of validation steps), the interrogation process can occur
#' according its plan. After that, the agent will have gathered intel, and we
#' can use functions like [get_agent_report()] and [all_passed()] to understand
#' how the interrogation went down.
#'
#' @param agent An agent object of class `ptblank_agent` that is created with
#'   [create_agent()].
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
#' @section Demos:
#'  
#' Create a simple table with two columns of numerical values.
#' 
#' ```{r}
#' tbl <-
#'   dplyr::tibble(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3)
#'   )
#' 
#' tbl
#' ```
#' 
#' Validate that values in column `a` from `tbl` are always less than `5`. Using
#' `interrogate()` carries out the validation plan and completes the whole
#' process.
#' 
#' ```{r}
#' agent <-
#'   create_agent(
#'     tbl = tbl,
#'     label = "`interrogate()` example"
#'   ) %>%
#'   col_vals_gt(vars(a), value = 5) %>%
#'   interrogate()
#' ```
#' 
#' \if{html}{
#' 
#' We can print the resulting object to see the information report.
#' 
#' \out{
#' `r pb_get_image_tag(file = "man_interrogate_1.png")`
#' }
#' }
#' 
#' @family Interrogate and Report
#' @section Function ID:
#' 6-1
#' 
#' @export
interrogate <- function(
    agent,
    extract_failed = TRUE,
    get_first_n = NULL,
    sample_n = NULL,
    sample_frac = NULL,
    sample_limit = 5000
) {
  
  #
  # INITIAL PROCESSING OF AGENT
  #
  
  # Add the starting time to the `agent` object
  agent$time_start <- Sys.time()
  
  # Stop function if `agent$tbl` and `agent$read_fn` are both NULL
  if (is.null(agent$tbl) && is.null(agent$read_fn)) {
    
    stop(
      "We can't `interrogate()` because the agent doesn't have a data table ",
      "or a function to obtain one:\n",
      "* Use the `set_tbl()` function to specify a table",
      call. = FALSE
    )
  }

  # Materialization of table given that there is a table-prep formula
  # available in the agent object
  if (is.null(agent$tbl) && !is.null(agent$read_fn)) {
    
    if (inherits(agent$read_fn, "function")) {
      agent$tbl <- rlang::exec(agent$read_fn)
    } else if (rlang::is_formula(agent$read_fn)) {
      agent$tbl <- agent$read_fn %>% rlang::f_rhs() %>% rlang::eval_tidy()
      
      if (inherits(agent$tbl, "read_fn")) {
        
        if (inherits(agent$tbl, "with_tbl_name")) {
          agent$tbl_name <- agent$tbl %>% rlang::f_lhs() %>% as.character()
        }
        
        agent$tbl <- materialize_table(agent$tbl)
      }
      
    } else {

      # TODO: create a better `stop()` message
      stop(
        "The `read_fn` object must be a function or an R formula.\n",
        "* A function can be made with `function()` {<table reading code>}.\n",
        "* An R formula can also be used, with the expression on the RHS.",
        call. = FALSE
      )
    }
    
    # Obtain basic information on the table and
    # set the relevant list elements
    tbl_information <- get_tbl_information(tbl = agent$tbl)

    agent$db_tbl_name <- tbl_information$db_tbl_name
    agent$tbl_src <- tbl_information$tbl_src
    agent$tbl_src_details <- tbl_information$tbl_src_details
    agent$col_names <- tbl_information$col_names
    agent$col_types <- tbl_information$r_col_types
    agent$db_col_types <- tbl_information$db_col_types

    agent$extracts <- list()
  }

  # Quieting of an agent's remarks either when the agent has the
  # special label `"::QUIET::"` or the session is non-interactive
  if (agent$label == "::QUIET::" || !interactive()) {
    quiet <- TRUE
  } else {
    quiet <- FALSE
  }
  
  # TODO: Handle possible expansion of table through evaluation
  # of all `seg_expr` values
  
  
  # Get the agent's validation step indices
  validation_steps <- seq_len(nrow(agent$validation_set))
  
  # Signal the start of interrogation in the console
  create_cli_header_a(
    validation_steps = validation_steps,
    quiet = quiet
  )
  
  #
  # PROCESSING OF VALIDATION STEPS AS INDIVIDUAL INTERROGATIONS
  #
  
  for (i in validation_steps) {
    
    # Get the table object for interrogation 
    table <- get_tbl_object(agent = agent)
    
    # Evaluate any expression in `agent$validation_set$active`
    if (rlang::is_formula(agent$validation_set[[i, "active"]][[1]])) {
      
      is_active <- 
        agent$validation_set[[i, "active"]][[1]] %>%
        rlang::f_rhs() %>%
        rlang::eval_tidy()
      
      agent$validation_set[[i, "eval_active"]] <- is_active(table)
      rm(is_active)
      
    } else {
      
      agent$validation_set[[i, "eval_active"]] <- 
        agent$validation_set[[i, "active"]][[1]]
    }
    
    # Set the validation step as `active = FALSE` if there is a
    # `seg_expr` declared but not resolved `seg_col`
    if (!is.null(agent$validation_set$seg_expr[[i]]) &&
        is.na(agent$validation_set$seg_col[i])) {
      agent$validation_set[[i, "eval_active"]] <- FALSE
    }
    
    # Set the validation step as `active = FALSE` if there is a
    # no column available as a result of a select expression
    if (!is.null(agent$validation_set$column[[i]]) &&
        is.na(agent$validation_set$column[[i]]) &&
        agent$validation_set$assertion_type[[i]] %in%
        column_expansion_fns_vec()) {
      agent$validation_set[[i, "eval_active"]] <- FALSE
    }

    # Skip the validation step if `active = FALSE`
    if (!agent$validation_set[[i, "eval_active"]]) {
      
      if (!quiet) {
        cli::cli_alert_info(
          "Step {.field {i}} is not set as {.field active}. Skipping."
        )
      }
      
      next
    }
    
    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
    
    # Use preconditions to modify the table
    table <- apply_preconditions_to_tbl(agent = agent, idx = i, tbl = table)
    
    # Use segmentation directives to constrain the table
    table <- apply_segments_to_tbl(agent = agent, idx = i, tbl = table)
    
    # Get the assertion type for this verification step
    assertion_type <- get_assertion_type_at_idx(agent = agent, idx = i)

    if (!(assertion_type %in% c("conjointly", "serially"))) {

      # Perform table checking based on assertion type
      tbl_checked <- 
        check_table_with_assertion(
          agent = agent,
          idx = i,
          table = table,
          assertion_type = assertion_type
        )
      
    } else if (assertion_type == "conjointly") {
      
      validation_formulas <- get_values_at_idx(agent = agent, idx = i)
      validation_n <- length(validation_formulas)
      
      validation_fns <- 
        validation_formulas %>% 
        lapply(rlang::f_rhs) %>%
        vapply(
          FUN.VALUE = character(1),
          USE.NAMES = FALSE,
          FUN = function(x) {
            as.character(x)[1]
          }
        )
      
      any_1_unit <- any(grepl("col_is|col_exists", validation_fns))
      any_n_unit <- any(grepl("col_vals", validation_fns))

      if (any_1_unit && any_n_unit) {

        col_is_idx <- which(grepl("col_is|col_exists", validation_fns))
        col_vals_idx <- which(grepl("col_vals", validation_fns))
        
        validation_formulas <- 
          c(
            validation_formulas[col_vals_idx], 
            validation_formulas[col_is_idx]
          )
      }

      # Create a double agent
      double_agent <- create_agent(tbl = table)

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
        assertion_type <- 
          get_assertion_type_at_idx(
            agent = double_agent,
            idx = j
          )
        
        new_col <- paste0("pb_is_good_", j)

        tbl_check_t <- 
          check_table_with_assertion(
            agent = double_agent,
            idx = j,
            table = tbl_checked,
            assertion_type
          )
        
        tbl_check_t <- tbl_check_t$value
        
       if (grepl("col_vals", assertion_type)) {
         
         tbl_checked <- tbl_check_t
         
       } else {

         tbl_checked <-
           tbl_checked %>%
           dplyr::mutate(
             pb_is_good_ = {{ tbl_check_t }} %>%
               utils::head(1) %>%
               dplyr::pull(pb_is_good_)
           )
       }

        tbl_checked <-
          tbl_checked %>%
          dplyr::rename(!!new_col := pb_is_good_)
      }
      
      columns_str_vec <- paste0("pb_is_good_", seq(j))
      columns_str_add <- paste0("pb_is_good_", seq(j), collapse = " + ")

      # Create function for validating step functions conjointly
      tbl_val_conjointly <- function(columns_str_add,
                                     columns_str_vec,
                                     validation_n) {
       
        # TODO: Require check to ensure that the validation functions used
        # are entirely of the combined set of `col_vals_*()`, `col_is_*()`,
        # and `col_exists()`
        
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
            columns_str_add = columns_str_add,
            columns_str_vec = columns_str_vec,
            validation_n = validation_n
          )
        )
      
    } else if (assertion_type == "serially") {
      
      validation_formulas <- get_values_at_idx(agent = agent, idx = i)
      validation_n <- length(validation_formulas)
      
      assertion_types <-
        vapply(
          validation_formulas,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE,
          FUN = function(x) {
            x %>%
              rlang::f_rhs() %>%
              as.character() %>%
              .[[1]]
          }
        )
      
      # Set initial value of `failed_testing`
      failed_testing <- FALSE
      
      # Initialize the `serially_validation_set` tibble; this
      # will be populated by all validations using `serially()` tests
      serially_validation_set <- dplyr::tibble()
      
      has_final_validation <-
        assertion_types[length(assertion_types)] %in% all_validations_fns_vec()
      
      # Get the total number of `test_*()` calls supplied
      test_call_n <- 
        if (has_final_validation) validation_n - 1 else validation_n
      
      #
      # Determine the total number of test steps
      #
      
      # Create a `double_agent` that will be used just for determining
      # the number of test steps
      double_agent <- create_agent(tbl = table, label = "::QUIET::")
      
      for (k in seq_len(test_call_n)) {
        
        double_agent <-
          eval(
            expr = parse(
              text =
                validation_formulas[[k]] %>%
                rlang::f_rhs() %>%
                rlang::expr_deparse() %>%
                tidy_gsub("(.", "(double_agent", fixed = TRUE) %>%
                tidy_gsub("^test_", "") %>%
                tidy_gsub("threshold\\s+?=\\s.*$", ")") %>%
                tidy_gsub(",\\s+?\\)$", ")")
                
            ),
            envir = NULL
          )
      }
      
      test_step_n <- nrow(double_agent$validation_set)
        
      # Perform tests as validation steps in sequence
      for (k in seq_len(test_call_n)) {
        
        # Create a double agent
        double_agent <- create_agent(tbl = table, label = "::QUIET::")
        
        deparsed_call <- 
          validation_formulas[[k]] %>%
          rlang::f_rhs() %>%
          rlang::expr_deparse() %>%
          paste(collapse = " ")
        
        if (grepl("threshold", deparsed_call)) {
          
          threshold_value <-
            validation_formulas[[k]] %>%
            rlang::f_rhs() %>%
            rlang::expr_deparse() %>%
            tidy_gsub(".*?(threshold\\s+?=\\s+[0-9\\.]+?).+?", "\\1") %>%
            tidy_gsub("threshold\\s+?=\\s+?", "") %>%
            as.numeric()
          
          double_agent <-
            eval(
              expr = parse(
                text =
                  validation_formulas[[k]] %>%
                  rlang::f_rhs() %>%
                  rlang::expr_deparse() %>%
                  tidy_gsub("(.", "(double_agent", fixed = TRUE) %>%
                  tidy_gsub("^test_", "") %>%
                  tidy_gsub(
                    "threshold\\s+?=\\s+?[0-9\\.]+?",
                    paste0(
                      "actions = action_levels(stop_at = ",
                      threshold_value, ")"
                    )
                  )
              ),
              envir = NULL
            )
          
        } else {
          
          threshold_value <- 1
          
          double_agent <-
            eval(
              expr = parse(
                text =
                  validation_formulas[[k]] %>%
                  rlang::f_rhs() %>%
                  rlang::expr_deparse() %>%
                  tidy_gsub("(.", "(double_agent", fixed = TRUE) %>%
                  tidy_gsub("^test_", "") %>%
                  tidy_gsub(
                    "\\)$",
                    paste0(
                      ", actions = action_levels(stop_at = ",
                      threshold_value, "))"
                    )
                  )
              ),
              envir = NULL
            )
        }
        
        double_agent <- double_agent %>% interrogate()
        
        serially_validation_set <-
          dplyr::bind_rows(
            serially_validation_set,
            double_agent$validation_set %>%
              dplyr::select(
                -c(step_id, sha1, -warn, -notify, -tbl_checked,
                   interrogation_notes
                )
              ) %>%
              dplyr::mutate(i_o = .env$k)
          )
        
        stop_vec <- double_agent$validation_set$stop
        
        if (!all(is.na(stop_vec)) && any(stop_vec)) {
          
          # Get the first instance of a STOP
          stop_idx <- min(which(stop_vec))
          
          # Get the `tbl_checked` object at the `stop_idx` index
          tbl_check <-
            double_agent$validation_set$tbl_checked[[stop_idx]][[1]]
          
          # Get the assertion type for this verification step
          assertion_type <- 
            get_assertion_type_at_idx(
              agent = double_agent,
              idx = stop_idx
            )
          
          tbl_checked <- 
            check_table_with_assertion(
              agent = double_agent,
              idx = stop_idx,
              table = table,
              assertion_type
            )
          
          failed_testing <- TRUE
          
          break
        }
        
        tbl_checked <- pointblank_try_catch(dplyr::tibble(`pb_is_good_` = TRUE))
      }
      
      if (!failed_testing && has_final_validation) {
        
        double_agent <- create_agent(tbl = table, label = "::QUIET::")
        
        double_agent <-
          eval(
            expr = parse(
              text =
                validation_formulas[[validation_n]] %>%
                rlang::f_rhs() %>%
                rlang::expr_deparse() %>%
                tidy_gsub("(.", "(double_agent", fixed = TRUE)
            ),
            envir = NULL
          )
        
        double_agent <- double_agent %>% interrogate()
        
        serially_validation_set <-
          dplyr::bind_rows(
            serially_validation_set,
            double_agent$validation_set %>%
              dplyr::select(
                -c(step_id, sha1, -warn, -notify, -tbl_checked,
                   interrogation_notes
                )
              ) %>%
              dplyr::mutate(i_o = .env$k)
          )
        
        # Get the assertion type for this verification step
        assertion_type <- 
          get_assertion_type_at_idx(
            agent = double_agent,
            idx = 1
          )
        
        tbl_checked <- 
          check_table_with_assertion(
            agent = double_agent,
            idx = 1,
            table = table,
            assertion_type = assertion_type
          )
      }
      
      # Renumber `i` in the `serially()` validation set so that
      # it is an ascending integer sequence
      serially_validation_set <-
        serially_validation_set %>%
        dplyr::mutate(i = seq_len(nrow(.)))
      
      # Add interrogation notes
      agent$validation_set[[i, "interrogation_notes"]] <-
        list(
          list(
            validation = "serially",
            total_test_calls = test_call_n,
            total_test_steps = test_step_n,
            failed_testing = failed_testing,
            has_final_validation = has_final_validation,
            testing_validation_set = serially_validation_set
          )
        )
    }

    # Add in the necessary reporting data for the validation
    agent <- 
      add_reporting_data(
        agent = agent,
        idx = i,
        tbl_checked = tbl_checked
      )
    
    # Perform any necessary actions if threshold levels are exceeded
    perform_action(agent = agent, idx = i, type = "warn")
    perform_action(agent = agent, idx = i, type = "notify")
    perform_action(agent = agent, idx = i, type = "stop")

    # Add extracts of failed rows if validation function operates on
    # values in rows and `extract_failed` is TRUE
    if (assertion_type %in% row_based_validation_fns_vec()) {
      
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
    }
    
    # Get the ending time for the validation step
    validation_end_time <- Sys.time()
    
    # Get the time duration for the validation step (in seconds)    
    time_diff_s <- 
      get_time_duration(
        start_time = validation_start_time,
        end_time = validation_end_time
      )

    # Add the timing information to the `agent` object
    agent$validation_set$time_processed[i] <- validation_start_time
    agent$validation_set$proc_duration_s[i] <- time_diff_s
    
    create_post_step_cli_output_a(
      agent = agent,
      i = i,
      time_diff_s = time_diff_s,
      quiet = quiet
    )
  }
  
  #
  # POST-INTERROGATION PHASE
  #
  
  # Bestowing of the class `"has_intel"` to the agent, given that
  # all validation steps have been carried out
  class(agent) <- c("has_intel", "ptblank_agent")
  
  # Add the ending time to the `agent` object
  agent$time_end <- Sys.time()
  
  # nocov start
  
  # Generate gt-based reporting objects
  if (agent$embed_report) {
    
    gt_agent_report <- get_agent_report(agent = agent)
    gt_agent_report_email <- get_agent_report(agent = agent, size = "small")
    
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
  create_cli_footer_a(quiet)
  
  # Update the ending time to the `agent` object
  agent$time_end <- Sys.time()
  
  agent
}

# nocov start

get_time_duration <- function(start_time,
                              end_time,
                              units = "secs",
                              round = 4) {
  
  round(
    as.numeric(difftime(time1 = end_time, time2 = start_time, units = units)),
    digits = round
  )
}

create_cli_header_a <- function(validation_steps,
                                quiet) {
  
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

create_cli_footer_a <- function(quiet) {
  
  if (quiet) return()
  
  interrogation_progress_footer <- "Interrogation Completed"
  
  cli::cli_h1(interrogation_progress_footer)
}

create_post_step_cli_output_a <- function(
    agent,
    i,
    time_diff_s,
    quiet
) {
  
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
      c(
        "Step {.field {i}}: an evaluation issue requires attention ",
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
        c(
          "Step {.field {i}}: {.red STOP} condition met.",
          print_time(time_diff_s)
        )
      )
    } else {
      cli::cli_alert_warning(
        c(
          "Step {.field {i}}: {.yellow WARNING} condition met.",
          print_time(time_diff_s)
        )
      )
    }
  } else if (validation_condition != "NONE" & notify_condition != "NONE") {
    if (validation_condition == "STOP") {
      cli::cli_alert_danger(
        c(
          "Step {.field {i}}: {.red STOP} and ",
          "{.blue NOTIFY} conditions met.",
          print_time(time_diff_s)
        )
      )
    } else {
      cli::cli_alert_warning(
        c(
          "Step {.field {i}}: {.yellow WARNING} and ",
          "{.blue NOTIFY} conditions met.",
          print_time(time_diff_s)
        )
      )
    }
  } else if (validation_condition == "NONE" & notify_condition != "NONE") {
    cli::cli_alert_warning(
      c(
        "Step {.field {i}}: {.blue NOTIFY} condition met.",
        print_time(time_diff_s)
      )
    )
  }
  cli::cli_end()
}

# nocov end

check_table_with_assertion <- function(
    agent,
    idx,
    table,
    assertion_type
) {
  
  # nolint start
  
    switch(
      assertion_type,
      "col_vals_gt" =,
      "col_vals_gte" =,
      "col_vals_lt" =,
      "col_vals_lte" =,
      "col_vals_equal" =,
      "col_vals_not_equal" = interrogate_comparison(
        agent = agent,
        idx = idx,
        table = table,
        assertion_type = assertion_type
      ),
      "col_vals_between" =,
      "col_vals_not_between" = interrogate_between(
        agent = agent,
        idx = idx,
        table = table,
        assertion_type = assertion_type
      ),
      "col_vals_in_set" =,
      "col_vals_make_set" =,
      "col_vals_make_subset" =,
      "col_vals_not_in_set" = interrogate_set(
        agent = agent,
        idx = idx,
        table = table,
        assertion_type = assertion_type
      ),
      "col_vals_null" = interrogate_null(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_vals_not_null" = interrogate_not_null(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_vals_increasing" =,
      "col_vals_decreasing" = interrogate_direction(
        agent = agent,
        idx = idx,
        table = table,
        assertion_type = assertion_type
      ),
      "col_vals_regex" = interrogate_regex(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_vals_within_spec" = interrogate_within_spec(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_vals_expr" = interrogate_expr(
        agent = agent,
        idx = idx,
        table = table
      ),
      "specially" = interrogate_specially(
        agent = agent,
        idx = idx,
        x = table
      ),
      "col_exists" = interrogate_col_exists(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_is_numeric" =,
      "col_is_integer" =,
      "col_is_character" =,
      "col_is_logical" =,
      "col_is_posix" =,
      "col_is_date" =,
      "col_is_factor" = interrogate_col_type(
        agent = agent,
        idx = idx,
        table = table,
        assertion_type = assertion_type
      ),
      "rows_distinct" = interrogate_distinct(
        agent = agent,
        idx = idx,
        table = table
      ),
      "rows_complete" = interrogate_complete(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_schema_match" = interrogate_col_schema_match(
        agent = agent,
        idx = idx,
        table = table
      ),
      "row_count_match" = interrogate_row_count_match(
        agent = agent,
        idx = idx,
        table = table
      ),
      "col_count_match" = interrogate_col_count_match(
        agent = agent,
        idx = idx,
        table = table
      ),
      "tbl_match" = interrogate_tbl_match(
        agent = agent,
        idx = idx,
        table = table
      )
    )
  
  # nolint end
}

interrogate_comparison <- function(
    agent,
    idx,
    table,
    assertion_type
) {

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
  } else {
    if (is.character(value)) {
      value <- paste0("'", value, "'")
    }
  }
  
  # Obtain the target column as a label
  column <- 
    get_column_as_sym_at_idx(agent = agent, idx = idx) %>%
    rlang::as_label()
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Perform rowwise validations for the column
  pointblank_try_catch(
    tbl_val_comparison(
      table = table,
      column = column,
      operator = operator,
      value = value,
      na_pass = na_pass
    )
  )
}

# Function for validating comparison step functions
tbl_val_comparison <- function(
    table,
    column,
    operator,
    value,
    na_pass
) {
  
  # Ensure that the input `table` is actually a table object
  tbl_validity_check(table = table)
  
  # Ensure that the value provided is valid 
  column_validity_checks_column_value(
    table = table,
    column = {{ column }},
    value = {{ value }}
  )
  
  # Construct a string-based expression for the validation
  expression <- paste(column, operator, value)
  
  if (is_tbl_mssql(table)) {
    
    table %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        !!rlang::parse_expr(expression) ~ 1,
        TRUE ~ 0
      ))
    
  } else {
    
    table %>%
      dplyr::mutate(pb_is_good_ = !!rlang::parse_expr(expression)) %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        is.na(pb_is_good_) ~ na_pass,
        TRUE ~ pb_is_good_
      ))
  }
}

interrogate_between <- function(
    agent,
    idx,
    table,
    assertion_type
) {
  
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
  
  inclusive <- as.logical(names(set))

  tbl_evaled <- 
    pointblank_try_catch(
      tbl_vals_between(
        table = table,
        column = {{ column }},
        left = {{ left }},
        right = {{ right }},
        inclusive = {{ inclusive }},
        na_pass = {{ na_pass }},
        assertion_type = {{ assertion_type }}
      )
    )
  
  tbl_evaled
}

tbl_vals_between <- function(
    table,
    column,
    left,
    right,
    inclusive,
    na_pass,
    assertion_type
) {
  
  # Ensure that the input `table` is actually a table object
  tbl_validity_check(table = table)
  
  column_validity_checks_ib_nb(
    table = table,
    column = {{ column }},
    left = {{ left }},
    right = {{ right }}
  )
  
  true <- if (is_tbl_mssql(table)) 1 else TRUE
  false <- if (is_tbl_mssql(table)) 0 else FALSE
  na_pass_bool <- if (na_pass) true else false
  
  #
  # Statement with appropriate operators for function type
  # and boundary inclusion options
  # 
  
  if (assertion_type == "col_vals_between") {
    
    # 1. ib_incl_incl
    if (identical(inclusive, c(TRUE, TRUE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `>=`({{ column }}, {{ left }}) &
            `<=`({{ column }}, {{ right }}) ~ {{ true }},
          `<`({{ column }}, {{ left }}) |
            `>`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
    
    # 2. ib_excl_incl
    if (identical(inclusive, c(FALSE, TRUE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `>`({{ column }}, {{ left }}) &
            `<=`({{ column }}, {{ right }}) ~ {{ true }},
          `<=`({{ column }}, {{ left }}) |
            `>`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }

    # 3. ib_incl_excl
    if (identical(inclusive, c(TRUE, FALSE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `>=`({{ column }}, {{ left }}) &
            `<`({{ column }}, {{ right }}) ~ {{ true }},
          `<`({{ column }}, {{ left }}) |
            `>=`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
    
    # 4. ib_excl_excl
    if (identical(inclusive, c(FALSE, FALSE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `>`({{ column }}, {{ left }}) &
            `<`({{ column }}, {{ right }}) ~ {{ true }},
          `<=`({{ column }}, {{ left }}) |
            `>=`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
    
  } else {
    
    # 5. nb_incl_incl
    if (identical(inclusive, c(TRUE, TRUE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `<`({{ column }}, {{ left }}) |
            `>`({{ column }}, {{ right }}) ~ {{ true }},
          `>=`({{ column }}, {{ left }}) &
            `<=`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }

    # 6. nb_excl_incl
    if (identical(inclusive, c(FALSE, TRUE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `<=`({{ column }}, {{ left }}) |
            `>`({{ column }}, {{ right }}) ~ {{ true }},
          `>`({{ column }}, {{ left }}) &
            `<=`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
    
    # 7. nb_incl_excl
    if (identical(inclusive, c(TRUE, FALSE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `<`({{ column }}, {{ left }}) |
            `>=`({{ column }}, {{ right }}) ~ {{ true }},
          `>=`({{ column }}, {{ left }}) &
            `<`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
    
    # 8. nb_excl_excl
    if (identical(inclusive, c(FALSE, FALSE))) {
      
      table <- 
        table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          `<=`({{ column }}, {{ left }}) |
            `>=`({{ column }}, {{ right }}) ~ {{ true }},
          `>`({{ column }}, {{ left }}) &
            `<`({{ column }}, {{ right }}) ~ {{ false }}
        ))
    }
  }
  
  table %>%
    dplyr::mutate(pb_is_good_ = dplyr::case_when(
      is.na({{ column }}) ~ na_pass_bool,
      TRUE ~ pb_is_good_
    ))
}

interrogate_set <- function(
    agent,
    idx,
    table,
    assertion_type
) {

  # Get the set values for the expression
  set <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine if an NA value is part of the set
  na_pass <- any(is.na(set))
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  if (assertion_type == "col_vals_in_set") {
    
    # Create function for validating the `col_vals_in_set()` step
    tbl_val_in_set <- function(table,
                               column,
                               na_pass) {
      
      # Ensure that the input `table` is actually a table object
      tbl_validity_check(table = table)
      
      # Ensure that the `column` provided is valid
      column_validity_checks_column(table = table, column = {{ column }})
      
      true <- if (is_tbl_mssql(table)) 1 else TRUE
      false <- if (is_tbl_mssql(table)) 0 else FALSE
      na_pass_bool <- if (na_pass) true else false
      
      table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          {{ column }} %in% set ~ {{ true }},
          !({{ column }} %in% set) ~ {{ false }}
        )) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na({{ column }}) ~ na_pass_bool,
          TRUE ~ pb_is_good_
        ))
    }
    
    # Perform rowwise validations for the column
    tbl_evaled <- 
      pointblank_try_catch(
        tbl_val_in_set(
          table = table,
          column = {{ column }},
          na_pass = na_pass
        )
      )
  }
  
  if (assertion_type == "col_vals_make_set") {
    
    # Create function for validating the `col_vals_make_set()` step
    tbl_vals_make_set <- function(table,
                                  column,
                                  na_pass) {
      
      # Ensure that the input `table` is actually a table object
      tbl_validity_check(table = table)
      
      # Ensure that the `column` provided is valid
      column_validity_checks_column(table = table, column = {{ column }})
      
      # Define function to get distinct values from a column in the
      # order of first appearance
      table_col_distinct_values <-
        table %>%
        dplyr::select({{ column }}) %>%
        dplyr::distinct({{ column }}) %>%
        dplyr::collect() %>%
        dplyr::pull({{ column }})

      if (na_pass) {
        # Remove any NA values from the vector
        table_col_distinct_values <-
          table_col_distinct_values[!is.na(table_col_distinct_values)]
        
        # Remove any NA values from the set
        set <- set[!is.na(set)]
      }
      
      extra_variables <- 
        base::setdiff(table_col_distinct_values, set)
      
      table_col_distinct_set <-
        base::intersect(table_col_distinct_values, set)

      dplyr::bind_rows(
        dplyr::tibble(set_element = as.character(set)) %>%
          dplyr::left_join(
            dplyr::tibble(
              col_element = as.character(table_col_distinct_set),
              pb_is_good_ = TRUE
            ),
            by = c("set_element" = "col_element")
          ) %>%
          dplyr::mutate(
            pb_is_good_ = ifelse(is.na(pb_is_good_), FALSE, pb_is_good_)
          ),
        dplyr::tibble(
          set_element = "::outside_values::",
          pb_is_good_ = NA
        ) %>%
          dplyr::mutate(pb_is_good_ = length(extra_variables) == 0)
      ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
    }
    
    # Perform rowwise validations for the column
    tbl_evaled <- 
      pointblank_try_catch(
        tbl_vals_make_set(
          table = table,
          column = {{ column }},
          na_pass = na_pass
        )
      )
  }
  
  if (assertion_type == "col_vals_make_subset") {
    
    # Create function for validating the `col_vals_make_subset()` step
    tbl_vals_make_subset <- function(table,
                                     column,
                                     na_pass) {
      
      # Ensure that the input `table` is actually a table object
      tbl_validity_check(table = table)
      
      # Ensure that the `column` provided is valid
      column_validity_checks_column(table = table, column = {{ column }})
      
      # Define function to get distinct values from a column in the
      # order of first appearance
      table_col_distinct_values <-
        table %>%
        dplyr::select({{ column }}) %>%
        dplyr::distinct({{ column }}) %>%
        dplyr::collect() %>%
        dplyr::pull({{ column }})
      
      if (na_pass) {
        
        # Remove any NA values from the vector
        table_col_distinct_values <-
          table_col_distinct_values[!is.na(table_col_distinct_values)]
        
        # Remove any NA values from the set
        set <- set[!is.na(set)]
      }
      
      table_col_distinct_set <-
        base::intersect(table_col_distinct_values, set)
      
      dplyr::tibble(set_element = as.character(set)) %>%
        dplyr::left_join(
          dplyr::tibble(
            col_element = as.character(table_col_distinct_set),
            pb_is_good_ = TRUE
          ),
          by = c("set_element" = "col_element")
        ) %>%
        dplyr::mutate(
          pb_is_good_ = ifelse(is.na(pb_is_good_), FALSE, pb_is_good_)
        ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
    }
    
    # Perform rowwise validations for the column
    tbl_evaled <- 
      pointblank_try_catch(
        tbl_vals_make_subset(
          table = table,
          column = {{ column }},
          na_pass = na_pass
        )
      )
  }
  
  if (assertion_type == "col_vals_not_in_set") {
    
    # Create function for validating the `col_vals_not_in_set()` step function
    tbl_val_not_in_set <- function(table,
                                   column,
                                   na_pass) {
      
      # Ensure that the input `table` is actually a table object
      tbl_validity_check(table = table)
      
      # Ensure that the `column` provided is valid
      column_validity_checks_column(table = table, column = {{ column }})
      
      true <- if (is_tbl_mssql(table)) 1 else TRUE
      false <- if (is_tbl_mssql(table)) 0 else FALSE
      na_pass_bool <- if (na_pass) false else true
      
      table %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          !({{ column }} %in% set) ~ {{ true }},
          {{ column }} %in% set ~ {{ false }}
        )) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na({{ column }}) ~ na_pass_bool,
          TRUE ~ pb_is_good_
        ))
    }
  
    # Perform rowwise validations for the column
    tbl_evaled <- 
      pointblank_try_catch(
        tbl_val_not_in_set(
          table = table,
          column = {{ column }},
          na_pass = na_pass
        )
      )
  }
  
  tbl_evaled
}

interrogate_direction <- function(
    agent,
    idx,
    table,
    assertion_type
) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Get the values for `allow_stationary` and either of
  # the tolerance values
  stat_tol <- get_values_at_idx(agent = agent, idx = idx)

  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  if (assertion_type == "col_vals_increasing") {
    direction <- "increasing"
  } else {
    direction <- "decreasing"
  }

  # Create function for validating any `col_vals_increasing()` and
  # `col_vals_decreasing()` steps
  tbl_val_direction <- function(table,
                                column,
                                na_pass,
                                direction) {
    
    # Exit if the table is from the `mssql` source 
    if (is_tbl_mssql(table)) {
      
      stop(
        "Direction-based validations (`col_vals_increasing()`/
        `col_vals_decreasing()`) are currently not supported on Microsoft ",
        "SQL Server database tables.",
        call. = FALSE
      )
    }
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
    column_validity_checks_column(table = table, column = {{ column }})

    tbl <- 
      table %>%
      dplyr::mutate(
        pb_lagged_difference_ = {{ column }} - dplyr::lag({{ column }}))
    
    if (stat_tol[1] == 0) {
      
      if (direction == "increasing") {
        
        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            pb_lagged_difference_ > 0 ~ TRUE,
            pb_lagged_difference_ <= 0 ~ FALSE,
            is.na({{ column }}) & !na_pass ~ FALSE
          ))
        
      } else {
        
        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            pb_lagged_difference_ < 0 ~ TRUE,
            pb_lagged_difference_ >= 0 ~ FALSE,
            is.na({{ column }}) & !na_pass ~ FALSE
          ))
      }
    }
    
    if (stat_tol[1] == 1) {

      if (direction == "increasing") {
        
        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            pb_lagged_difference_ >= 0 ~ TRUE,
            pb_lagged_difference_ < 0 ~ FALSE,
            is.na({{ column }}) & !na_pass ~ FALSE
          ))
        
      } else {
        
        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            pb_lagged_difference_ <= 0 ~ TRUE,
            pb_lagged_difference_ > 0 ~ FALSE,
            is.na({{ column }}) & !na_pass ~ FALSE
          ))
      }
    }
    
    # If a tolerance is set to some non-zero value, then accept
    # differential values greater than or equal to that tolerance value
    if (stat_tol[2] != 0) {
      
      if (direction == "increasing") {
        
        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = ifelse(
            !is.na(pb_lagged_difference_) & 
              pb_lagged_difference_ >= (-abs(stat_tol[2])), TRUE, pb_is_good_
          ))
        
      } else {

        tbl <-
          tbl %>%
          dplyr::mutate(pb_is_good_ = ifelse(
            !is.na(pb_lagged_difference_) & 
              pb_lagged_difference_ <= abs(stat_tol[2]), TRUE, pb_is_good_
          ))
      }
    }
    
    tbl <-
      tbl %>%
      dplyr::mutate(pb_is_good_ = ifelse(
        is.na(pb_lagged_difference_) & is.na(pb_is_good_), TRUE, pb_is_good_
      )) %>%
      dplyr::select(-pb_lagged_difference_)
  }
  
  # Perform rowwise validations for the column
  tbl_evaled <- 
    pointblank_try_catch(
      tbl_val_direction(
        table = table,
        column = {{ column }},
        na_pass = na_pass,
        direction = direction
      )
    )
}

interrogate_regex <- function(
    agent,
    idx,
    table
) {
  
  # Get the regex matching statement
  regex <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  tbl_type <- agent$tbl_src
  
  # Create function for validating the `col_vals_regex()` step function
  tbl_val_regex <- function(table,
                            tbl_type,
                            column,
                            regex,
                            na_pass) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
    column_validity_checks_column(table = table, column = {{ column }})
    
    # nocov start
    
    if (tbl_type == "sqlite") {
      
      stop(
        "Regex-based validations are currently not supported on SQLite ",
        "database tables.",
        call. = FALSE
      )
    }
    
    if (tbl_type == "mssql") {
      
      stop(
        "Regex-based validations are currently not supported on Microsoft ",
        "SQL Server database tables.",
        call. = FALSE
      )
    }
    
    if (tbl_type == "tbl_spark") {
      
      tbl <- 
        table %>%
        dplyr::mutate(
          pb_is_good_ = ifelse(
            !is.na({{ column }}), RLIKE({{ column }}, regex), NA)
        ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
      
    } else if (tbl_type == "mysql") {

      tbl <- 
        table %>%
        dplyr::mutate(pb_is_good_ = ifelse(
          !is.na({{ column }}), {{ column }} %REGEXP% regex, NA)
        ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
      
    } else if (tbl_type == "duckdb") {

      tbl <- 
        table %>%
        dplyr::mutate(pb_is_good_ = ifelse(
          !is.na({{ column }}), regexp_matches({{ column }}, regex), NA)
        ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
      
    } else {
      
      # This works for postgres and local tables; untested so far in other DBs
      tbl <- 
        table %>% 
        dplyr::mutate(pb_is_good_ = ifelse(
          !is.na({{ column }}), grepl(regex, {{ column }}), NA)
        ) %>%
        dplyr::mutate(pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
    }
    
    # nocov end
    
    tbl
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(
    tbl_val_regex(
      table = table,
      tbl_type = tbl_type,
      column = {{ column }},
      regex = regex,
      na_pass = na_pass
    )
  )
}

interrogate_within_spec <- function(
    agent,
    idx,
    table
) {
  
  # Get the specification text
  spec <- get_values_at_idx(agent = agent, idx = idx)
  
  # Determine whether NAs should be allowed
  na_pass <- get_column_na_pass_at_idx(agent = agent, idx = idx)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  tbl_type <- agent$tbl_src
  
  # Create function for validating the `col_vals_within_spec()` step function
  tbl_val_within_spec <- function(table,
                                  tbl_type,
                                  column,
                                  spec,
                                  na_pass) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
    column_validity_checks_column(table = table, column = {{ column }})
    
    # nocov start
    
    if (tbl_type == "sqlite") {
      
      stop(
        "Specification-based validations are currently not supported on ",
        "SQLite database tables.",
        call. = FALSE
      )
    }
    
    if (inherits(table, "tbl_dbi") || inherits(table, "tbl_spark")) {
      
      # Not possible: `"isbn"`, `"creditcard"`, and `"phone"`
      if (spec %in% c("isbn", "creditcard", "phone")) {
        
        stop(
          "Validations with the `\"", spec, "\"` specification are currently ",
          "not supported on `tbl_dbi` or `tbl_spark` tables.",
          call. = FALSE
        )
      }
      
      if (grepl("iban", spec)) {
        country <- toupper(gsub("(iban\\[|\\])", "", spec))
        spec <- "iban"
      } else if (grepl("postal", spec)) {
        country <- toupper(gsub("(postal\\[|\\])", "", spec))
        spec <- "postal"
      }
      
      # Perform regex-based specification checks
      if (grepl("iban", spec) || grepl("postal", spec) ||
          spec %in% c(
            "swift", "email", "url",
            "ipv4", "ipv6", "mac"
          )
      ) {
        
        regex <-
          switch(
            spec,
            iban = regex_iban(country = country),
            postal = regex_postal_code(country = country),
            swift = regex_swift_bic(),
            email = regex_email(),
            url = regex_url(),
            ipv4 = regex_ipv4_address(),
            ipv6 = regex_ipv6_address(),
            mac = regex_mac()
          )
        
        if (tbl_type == "tbl_spark") {
          
          tbl <- 
            table %>%
            dplyr::mutate(
              pb_is_good_ = ifelse(
                !is.na({{ column }}), RLIKE({{ column }}, regex), NA)
            ) %>%
            dplyr::mutate(pb_is_good_ = dplyr::case_when(
              is.na(pb_is_good_) ~ na_pass,
              TRUE ~ pb_is_good_
            ))
          
        } else if (tbl_type == "mysql") {
          
          tbl <- 
            table %>%
            dplyr::mutate(pb_is_good_ = ifelse(
              !is.na({{ column }}), {{ column }} %REGEXP% regex, NA)
            ) %>%
            dplyr::mutate(pb_is_good_ = dplyr::case_when(
              is.na(pb_is_good_) ~ na_pass,
              TRUE ~ pb_is_good_
            ))
          
        } else if (tbl_type == "duckdb") {
          
          tbl <- 
            table %>%
            dplyr::mutate(pb_is_good_ = ifelse(
              !is.na({{ column }}), regexp_matches({{ column }}, regex), NA)
            ) %>%
            dplyr::mutate(pb_is_good_ = dplyr::case_when(
              is.na(pb_is_good_) ~ na_pass,
              TRUE ~ pb_is_good_
            ))
          
        } else {
          
          # This works for postgres and local tables;
          # untested so far in other DBs
          tbl <- 
            table %>% 
            dplyr::mutate(pb_is_good_ = ifelse(
              !is.na({{ column }}), grepl(regex, {{ column }}), NA)
            ) %>%
            dplyr::mutate(pb_is_good_ = dplyr::case_when(
              is.na(pb_is_good_) ~ na_pass,
              TRUE ~ pb_is_good_
            ))
        }
      }
      
      # VIN
      
      if (spec == "vin") {
        
        tbl <-
          check_vin_db(table, column = {{ column }}) %>%
          dplyr::mutate(pb_is_good_ = dplyr::case_when(
            is.na(pb_is_good_) ~ na_pass,
            TRUE ~ pb_is_good_
          ))
      }
      
    } else {
      
      # This is for local tables
      
      if (grepl("iban", spec)) {
        country <- toupper(gsub("(iban\\[|\\])", "", spec))
        fn <- check_iban
      } else if (grepl("postal", spec)) {
        country <- toupper(gsub("(postal\\[|\\])", "", spec))
        fn <- check_postal_code
      } else {
        country <- NULL
        fn <-
          switch(
            spec,
            phone = check_phone,
            creditcard = check_credit_card,
            vin = check_vin,
            isbn = check_isbn,
            swift = check_swift_bic,
            email = check_email,
            url = check_url,
            ipv4 = check_ipv4_address,
            ipv6 = check_ipv6_address,
            mac = check_mac
          )
      }
      
      if (!is.null(country)) {
        
        tbl <-
          dplyr::mutate(table, pb_is_good_ = ifelse(
            !is.na({{ column }}), fn({{ column }}, country = country), NA
          ))
        
      } else {
        
        tbl <-
          dplyr::mutate(table, pb_is_good_ = ifelse(
            !is.na({{ column }}), fn({{ column }}), NA
          ))
        
      }
      
      tbl <- 
        dplyr::mutate(tbl, pb_is_good_ = dplyr::case_when(
          is.na(pb_is_good_) ~ na_pass,
          TRUE ~ pb_is_good_
        ))
    }
    
    # nocov end
    
    tbl
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(
    tbl_val_within_spec(
      table = table,
      tbl_type = tbl_type,
      column = {{ column }},
      spec = spec,
      na_pass = na_pass
    )
  )
}

interrogate_expr <- function(
    agent,
    idx,
    table
) {
  
  # Get the expression
  expr <- get_values_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_expr()` step function
  tbl_val_expr <- function(table,
                           expr) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    expr <- expr[[1]]

    table %>% 
      dplyr::mutate(pb_is_good_ = !!expr) %>%
      dplyr::filter(!is.na(pb_is_good_))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_expr(table = table, expr = expr))
}

interrogate_specially <- function(
    agent,
    idx,
    x
) {
  
  # Get the user-defined function
  fn <- get_values_at_idx(agent = agent, idx = idx)[[1]]
  
  # Create function for validating the `col_vals_expr()` step function
  val_with_fn <- function(x, fn) {
    
    if (!is.function(fn)) {
      stop("The value provided for `fn` is not a function.", call. = FALSE)
    }
    
    res <- fn(x)
    
    if (is.logical(res)) {
      
      tbl <- dplyr::tibble(`pb_is_good_` = res)
      
    } else if (is_a_table_object(res)) {
      
      n_cols_res <- get_table_total_columns(res)
      
      res_tbl_vec <- dplyr::pull(dplyr::collect(res[, n_cols_res]))
      
      if (!is.logical(res_tbl_vec)) {
        
        stop(
          "If the provided function for `specially()` yields a table, the ",
          "final column must be logical.",
          call. = FALSE
        )
      }
      
      tbl <- dplyr::tibble(`pb_is_good_` = res_tbl_vec)
      
    } else {
      
      stop(
        "The function used in `specially()` must return the following:\n",
        "* a logical vector, or\n",
        "* a table where the final column is logical",
        call. = FALSE
      )
    }
    
    tbl
  }
  
  # Perform validation with the function on x
  pointblank_try_catch(val_with_fn(x = x, fn = fn))
}

interrogate_null <- function(
    agent,
    idx,
    table
) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_null()` step function
  tbl_val_null <- function(table,
                           column) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
    column_validity_checks_column(table = table, column = {{ column }})
    
    true <- if (is_tbl_mssql(table)) 1 else TRUE
    false <- if (is_tbl_mssql(table)) 0 else FALSE
    
    table %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        is.na({{ column }}) ~ {{ true }},
        TRUE ~ {{ false }}
      ))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_null(table = table, column = {{ column }}))
}

interrogate_not_null <- function(
    agent,
    idx,
    table
) {

  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_vals_null()` step function
  tbl_val_not_null <- function(table,
                               column) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
    column_validity_checks_column(table = table, column = {{ column }})
    
    true <- if (is_tbl_mssql(table)) 1 else TRUE
    false <- if (is_tbl_mssql(table)) 0 else FALSE
    
    table %>%
      dplyr::mutate(pb_is_good_ = dplyr::case_when(
        is.na({{ column }}) ~ {{ false }},
        TRUE ~ {{ true }}
      ))
  }
  
  # Perform rowwise validations for the column
  pointblank_try_catch(tbl_val_not_null(table = table, column = {{ column }}))
}

interrogate_col_exists <- function(
    agent,
    idx,
    table
) {

  # Get the column names for the table
  column_names <- get_all_cols(agent = agent)
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_exists()` step function
  tbl_col_exists <- function(table,
                             column,
                             column_names) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    dplyr::tibble(pb_is_good_ = as.character(column) %in% column_names)
  }
  
  # Perform the validation of the column
  pointblank_try_catch(
    tbl_col_exists(
      table = table,
      column = {{ column }},
      column_names = column_names
    )
  )
}

interrogate_col_type <- function(
    agent,
    idx,
    table,
    assertion_type
) {
  
  # Obtain the target column as a symbol
  column <- get_column_as_sym_at_idx(agent = agent, idx = idx)
  
  # Create function for validating the `col_is_*()` step functions
  tbl_col_is <- function(table,
                         column,
                         assertion_type) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # Ensure that the `column` provided is valid
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
        "ordered" = ifelse(assertion_type == "col_is_factor", TRUE, FALSE),
        "factor" = ifelse(assertion_type == "col_is_factor", TRUE, FALSE),
        "POSIXct" = ifelse(assertion_type == "col_is_posix", TRUE, FALSE),
        "Date" = ifelse(assertion_type == "col_is_date", TRUE, FALSE),
        FALSE
      )
    
    dplyr::tibble(pb_is_good_ = validation_res)
  }
  
  # Perform the validation of the column
  pointblank_try_catch(
    tbl_col_is(
      table = table,
      column = {{ column }},
      assertion_type = assertion_type
    )
  )
}

interrogate_distinct <- function(
    agent,
    idx,
    table
) {
  
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
  tbl_rows_distinct <- function(table,
                                column_names,
                                col_syms) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    table %>%
      dplyr::select({{ column_names }}) %>%
      dplyr::group_by(!!!col_syms) %>%
      dplyr::mutate(`pb_is_good_` = ifelse(dplyr::n() == 1, TRUE, FALSE)) %>%
      dplyr::ungroup()
  }
  
  # nocov start
  
  # Create another variation of `tbl_rows_distinct()` that works for MySQL
  tbl_rows_distinct_mysql <- function(table,
                                      column_names,
                                      col_syms) {

    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
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
    pointblank_try_catch(
      tbl_rows_distinct_mysql(
        table = table,
        column_names = {{ column_names }},
        col_syms = col_syms
      )
    )
  } else {
    pointblank_try_catch(
      tbl_rows_distinct(
        table = table,
        column_names = {{ column_names }},
        col_syms = col_syms
      )
    )
  }
  
  # nocov end
}

interrogate_complete <- function(
    agent,
    idx,
    table
) {
  
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
  
  # Create function for validating the `rows_complete()` step function
  tbl_rows_complete <- function(table,
                                column_names,
                                col_syms) {
    
    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    if (is_tbl_dbi(table) || is_tbl_spark(table)) {
      
      col_expr <- 
        rlang::parse_expr(
          paste0("!is.na(", column_names, ")", collapse = " && ")
        )
      
      table_check <- 
        table %>%
        dplyr::select({{ column_names }}) %>%
        dplyr::mutate(pb_is_good_ = col_expr)
      
    } else {
      
      table_check <- 
        table %>%
        dplyr::select({{ column_names }}) %>%
        dplyr::mutate(pb_is_good_ = stats::complete.cases(.))
    }
    
    table_check
  }
  
  # Perform the validation of the table
  pointblank_try_catch(
    tbl_rows_complete(
      table = table,
      column_names = {{ column_names }},
      col_syms = col_syms
    )
  )
}

interrogate_col_schema_match <- function(
    agent,
    idx,
    table
) {

  # Get the reference `col_schema` object (this is user-supplied)
  table_schema_y <- agent$validation_set$values[[idx]]
  
  # nocov start
  
  # Get the `table` `col_schema` object (this is constructed from the table)
  if (inherits(table, "tbl_dbi") || inherits(table, "tbl_spark")) {
    
    if (inherits(table_schema_y, "sql_type")) {
      if (all(!is.na(agent$db_col_types))) {
        
        table_schema_x <-
          col_schema_from_names_types(
            names = agent$col_names,
            types = agent$db_col_types
          )
        
        class(table_schema_x) <- c("sql_type", "col_schema")
      } else {
        table_schema_x <- col_schema(.tbl = table, .db_col_types = "sql")
      }
      
    } else if (inherits(table_schema_y, "r_type")) {
      
      table_schema_x <- create_col_schema_from_df(tbl = table)
      class(table_schema_x) <- c("r_type", "col_schema")
    }
    
  # nocov end
    
  } else {
    table_schema_x <- col_schema(.tbl = table)
  }
  
  # Create function for validating the `col_schema_match()` step function
  tbl_col_schema_match <- function(table,
                                   table_schema_x,
                                   table_schema_y) {

    # Ensure that the input `table` is actually a table object
    tbl_validity_check(table = table)
    
    # nolint start
    
    # Extract options from `table_schema_y`
    complete <- table_schema_y$`__complete__`
    in_order <- table_schema_y$`__in_order__`
    is_exact <- table_schema_y$`__is_exact__`

    table_schema_y$`__complete__` <- NULL
    table_schema_y$`__in_order__` <- NULL
    table_schema_y$`__is_exact__` <- NULL
    
    # nolint end
    
    class(table_schema_y) <- class(table_schema_x)
    
    if (complete && length(table_schema_y) < length(table_schema_x)) {
      return(dplyr::tibble(pb_is_good_ = FALSE))
    }

    if (!in_order) {
      
      table_schema_x <- 
        structure(
          table_schema_x[order(names(table_schema_x))],
          class = class(table_schema_x)
        )
      
      table_schema_y <- 
        structure(
          table_schema_y[order(names(table_schema_y))],
          class = class(table_schema_y)
        )
    }
    
    # If there is no requirement for completeness in the user-defined
    # schema, use only the intersecting names across the schemas in the
    # reference schema
    if (!complete) {
      
      table_schema_x <-
        structure(
          table_schema_x[
            base::intersect(names(table_schema_x), names(table_schema_y))
          ],
          class = class(table_schema_x)
        )
    }
    
    if (!is_exact) {
      
      # Check that matching between the reference schema (x) and
      # the user-defined schema (y) occurs with looser matching
      # of types for each column
      
      unit_results <- c()
      
      for (i in seq_along(length(table_schema_y))) {
        
        unit_results <-
          c(
            unit_results,
            names(table_schema_y[i]) == names(table_schema_x[i])
          )
        
        unit_results <-
          c(
            unit_results,
            table_schema_y[[i]] %in% table_schema_x[[i]]
          )
      }

      dplyr::tibble(pb_is_good_ = all(unit_results))
      
    } else {
    
      # Check for exact matching between the reference schema and
      # the user-defined schema
      if (identical(table_schema_x, table_schema_y)) {
        dplyr::tibble(pb_is_good_ = TRUE)
      } else {
        dplyr::tibble(pb_is_good_ = FALSE)
      }
    }
  }

  # Perform the validation of the table 
  pointblank_try_catch(
    tbl_col_schema_match(
      table = table,
      table_schema_x = table_schema_x,
      table_schema_y = table_schema_y
    )
  )
}

interrogate_row_count_match <- function(
    agent,
    idx,
    table
) {
  
  # Create function for validating the `row_count_match()` step function
  tbl_row_count_match <- function(table) {
    
    tbl_validity_check(table = table)
    
    count <- agent$validation_set$values[[idx]]
    
    if (!is.numeric(count)) {
      
      # Get the comparison table (this is user-supplied)
      tbl_compare <- materialize_table(tbl = agent$validation_set$values[[idx]])
      
      # TODO: improve failure message for check of `tbl_compare`
      tbl_validity_check(table = tbl_compare)
      
      count <- get_table_total_rows(tbl_compare)
    } 
    
    # Check for exact matching in row counts between the two tables
    if (get_table_total_rows(table) == count) {
      dplyr::tibble(pb_is_good_ = TRUE)
    } else {
      dplyr::tibble(pb_is_good_ = FALSE)
    }
  }
  
  # Perform the validation of the table 
  pointblank_try_catch(
    tbl_row_count_match(table = table)
  )
}

interrogate_col_count_match <- function(
    agent,
    idx,
    table
) {
  
  # Create function for validating the `col_count_match()` step function
  tbl_col_count_match <- function(table) {
    
    tbl_validity_check(table = table)
    
    count <- agent$validation_set$values[[idx]]
    
    if (!is.numeric(count)) {
      
      # Get the comparison table (this is user-supplied)
      tbl_compare <- materialize_table(tbl = agent$validation_set$values[[idx]])
      
      # TODO: improve failure message for check of `tbl_compare`
      tbl_validity_check(table = tbl_compare)
      
      count <- get_table_total_columns(tbl_compare)
    } 
    
    # Check for exact matching in row counts between the two tables
    if (get_table_total_columns(table) == count) {
      dplyr::tibble(pb_is_good_ = TRUE)
    } else {
      dplyr::tibble(pb_is_good_ = FALSE)
    }
  }
  
  # Perform the validation of the table 
  pointblank_try_catch(
    tbl_col_count_match(table = table)
  )
}

interrogate_tbl_match <- function(
    agent,
    idx,
    table
) {
  
  # Get the comparison table (this is user-supplied)
  tbl_compare <- materialize_table(tbl = agent$validation_set$values[[idx]])
  
  # Create function for validating the `tbl_match()` step function
  tbl_match <- function(table,
                        tbl_compare) {
    
    # Ensure that the input `table` and `tbl_compare` objects
    # are actually table objects
    # TODO: improve failure message to specify which table isn't valid
    tbl_validity_check(table = table)
    tbl_validity_check(table = tbl_compare)
    
    # Exit if either table is from the `mssql` source 
    if (is_tbl_mssql(table) || is_tbl_mssql(tbl_compare)) {
      
      stop(
        "The `table_match()` validation is currently not supported ",
        "on Microsoft SQL Server database tables.",
        call. = FALSE
      )
    }
    
    # Ensure that both tables are `ungroup()`ed first
    table <- dplyr::ungroup(table)
    tbl_compare <- dplyr::ungroup(tbl_compare)
    
    #
    # Stage 1: Check that the column schemas match for both tables
    #
    
    col_schema_matching <-
      test_col_schema_match(
        object = table,
        schema = col_schema(.tbl = tbl_compare, .db_col_types = "r")
      )

    if (!col_schema_matching) {
      return(dplyr::tibble(pb_is_good_ = FALSE))
    }

    #
    # Stage 2: Check for exact matching in row counts between the two tables
    #
    
    row_count_matching <-
      get_table_total_rows(table) == get_table_total_rows(tbl_compare)
    
    if (!row_count_matching) {
      return(dplyr::tibble(pb_is_good_ = FALSE))
    }
    
    #
    # Stage 3: Check for exact data by cell across matched columns
    #          between the two tables
    #
    
    # TODO: handle edge case where both tables have zero rows
    
    column_count <- get_table_total_columns(table)
    row_count <- get_table_total_rows(table)
    
    column_all_matched <- c()
    
    for (i in seq_len(column_count)) {
      
      col_pair_match <- 
        dplyr::bind_cols(
          dplyr::collect(dplyr::rename(dplyr::select(table, i), a = 1)),
          dplyr::collect(dplyr::rename(dplyr::select(tbl_compare, i), b = 1))
        ) %>%
        dplyr::mutate(pb_is_good_ = identical(a, b)) %>%
        dplyr::pull(pb_is_good_) %>%
        all()
      
      column_all_matched <- c(column_all_matched, col_pair_match)
    }
    
    dplyr::tibble(pb_is_good_ = all(column_all_matched))
  }
  
  # Perform the validation of the table 
  pointblank_try_catch(
    tbl_match(
      table = table,
      tbl_compare = tbl_compare
    )
  )
}

# Validity check for the table
tbl_validity_check <- function(table) {
  
  if (!is_a_table_object(table)) {
    stop(
      "The 'table' in this validation step is not really a table object.",
      call. = FALSE
    )  
  }  
}

# nolint start

# Validity checks for the column and value 
column_validity_checks_column_value <- function(
    table,
    column,
    value
) {
  
  table_colnames <- colnames(table)
  
  if (!(as.character(column) %in% table_colnames)) {
    
    stop(
      "The value for `column` doesn't correspond to a column name.",
      call. = FALSE
    )
  }
  
  if (inherits(value, "name")) {
    
    if (!(as.character(value) %in% table_colnames)) {
      
      stop(
        "The column supplied as the `value` doesn't correspond ",
        "to a column name.",
        call. = FALSE
      )
    }
  }
}

# nolint end

# Validity check for the column
column_validity_checks_column <- function(
    table,
    column
) {
  
  table_colnames <- colnames(table)
  
  if (!(as.character(column) %in% table_colnames)) {
    
    stop(
      "The value for `column` doesn't correspond to a column name.",
      call. = FALSE
    )
  }
}

# Validity checks for `tbl_val_ib_*()` functions
column_validity_checks_ib_nb <- function(
    table,
    column,
    left,
    right
) {
  
  table_colnames <- colnames(table)
  
  if (!(as.character(column) %in% table_colnames)) {
    
    stop(
      "The value for `column` doesn't correspond to a column name.",
      call. = FALSE
    )
  }
  
  if (inherits(left, "name")) {
    
    if (!(as.character(left) %in% table_colnames)) {
      
      stop(
        "The column supplied as the `left` value doesn't correspond ",
        "to a column name.",
        call. = FALSE
      )
    }
  }
  
  if (inherits(right, "name")) {
    
    if (!(as.character(right) %in% table_colnames)) {
      
      stop(
        "The column supplied as the `right` value doesn't correspond ",
        "to a column name.",
        call. = FALSE
      )
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
  
  eval_list <- list(value = value, warning = warn, error = err)

  class(eval_list) <- "table_eval"
  eval_list
}

add_reporting_data <- function(
    agent,
    idx,
    tbl_checked
) {
  
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
  
  #
  # Get total count of TRUE rows
  #
  
  if (is_tbl_mssql(tbl_checked)) {
    
    # nocov start
    
    n_passed <-
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == 1) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::pull(n) %>%
      as.numeric()
    
    # nocov end
    
  } else {
    
    n_passed <-
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == TRUE) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::pull(n) %>%
      as.numeric()
  }
  
  #
  # Get total count of FALSE rows
  #
  
  if (is_tbl_mssql(tbl_checked)) {
    
    # nocov start
    
    n_failed <-
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == 0) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::pull(n) %>%
      as.numeric()
    
    # nocov end
    
  } else {
    
    n_failed <-
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == FALSE) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::pull(n) %>%
      as.numeric()
  }
  
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
    
  determine_action(agent = agent, idx = idx, false_count = n_failed)
}

perform_action <- function(
    agent,
    idx,
    type
) {

  actions <- 
    agent$validation_set[[idx, "actions"]] %>%
    unlist(recursive = FALSE)
  
  .warn <- agent$validation_set[[idx, "warn"]]
  .notify <- agent$validation_set[[idx, "notify"]]
  .stop <- agent$validation_set[[idx, "stop"]]
  
  .agent_label <- agent$label
  .time_start <- agent$time_start
  .time_end <- agent$time_end
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
  .actions <- agent$validation_set[[idx, "actions"]] %>% unlist()
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
      agent_label = .agent_label,
      time_start = .time_start,
      time_end = .time_end,
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
      actions = .actions,
      brief = .brief,
      eval_error = .eval_error,
      eval_warning = .eval_warning,
      capture_stack = .capture_stack,
      n = .n,
      n_passed = .n_passed,
      n_failed = .n_failed,
      f_passed = .f_passed,
      f_failed = .f_failed,
      this_type = NA_character_
    )

  if (type == "warn") {
    x$this_type <- "warn"
    if (!is.na(.warn) && .warn) {
      if ("warn" %in% names(actions$fns) && !is.null(actions$fns$warn)) {
        actions$fns$warn %>% rlang::f_rhs() %>% rlang::eval_tidy()
      }
    }
  } else if (type == "notify") {
    x$this_type <- "notify"
    if (!is.na(.notify) && .notify) {
      if ("notify" %in% names(actions$fns) && !is.null(actions$fns$notify)) {
        actions$fns$notify %>% rlang::f_rhs() %>% rlang::eval_tidy()
      }
    }
  } else if (type == "stop") {
    x$this_type <- "stop"
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
  
  .agent_label <- agent$label
  .time_start <- agent$time_start
  .time_end <- agent$time_end
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
  .actions <- agent$validation_set$actions
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
  .report_object_small <- agent$reporting$report_object_email
  
  if (!is.null(.report_object)) {
    .report_html <- 
      gt::as_raw_html(.report_object, inline_css = FALSE)
  } else {
    .report_html <- NULL
  }
  
  if (!is.null(.report_object_small)) {
    .report_html_small <- 
      gt::as_raw_html(.report_object_small, inline_css = TRUE)
  } else {
    .report_html_small <- NULL
  }

  # Have the local vars packaged in a list to make creating
  # custom functions more convenient
  x <-
    list(
      warn = .warn,
      notify = .notify,
      stop = .stop,
      agent_label = .agent_label,
      time_start = .time_start,
      time_end = .time_end,
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
      actions = .actions,
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
      report_html_small = .report_html_small
    )

  lapply(actions, FUN = function(y) {
    y %>% rlang::f_rhs() %>% rlang::eval_tidy()
  })
  
  return(NULL)
}

add_table_extract <- function(
    agent,
    idx,
    tbl_checked,
    extract_failed,
    get_first_n,
    sample_n,
    sample_frac,
    sample_limit
) {

  if (!extract_failed) {
    return(agent)
  }

  if (is.null(tbl_checked$value)) {
    return(agent)
  }

  tbl_checked <- tbl_checked$value
  
  tbl_type <- tbl_checked %>% class()
  
  if (is_tbl_mssql(tbl_checked)) {
    
    # nocov start
    
    problem_rows <- 
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == 0) %>%
      dplyr::select(-pb_is_good_)
    
    # nocov end
    
  } else {
    
    problem_rows <- 
      tbl_checked %>%
      dplyr::filter(pb_is_good_ == FALSE) %>%
      dplyr::select(-pb_is_good_)
  }
  
  if (!is.null(get_first_n)) {
    
    problem_rows <-
      problem_rows %>%
      utils::head(get_first_n) %>%
      dplyr::as_tibble()
    
  } else if (
    all(
      !is.null(sample_n) & 
      ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type)
    )
  ) {
    
    problem_rows <-
      dplyr::sample_n(
        tbl = problem_rows,
        size = sample_n,
        replace = FALSE) %>%
      dplyr::as_tibble()
    
  } else if (
    all(
      !is.null(sample_frac) & 
      ("data.frame" %in% tbl_type || "tbl_df" %in% tbl_type)
    )
  ) {
    
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

determine_action <- function(
    agent,
    idx,
    false_count
) {

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
