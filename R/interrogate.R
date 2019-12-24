#' Given an agent that is fully loaded with tasks, perform an interrogation
#'
#' The agent has all the information on what to do, so now all interrogations
#' can proceed efficiently, and, according to plan.
#'
#' @param agent An agent object of class `ptblank_agent`.
#' @param get_problem_rows An option to collect rows that didn't pass a
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
#' @examples 
#' # Create 2 simple data frames
#' # with 2 columns of numerical
#' # values in each
#' df_1 <-
#'   data.frame(
#'     a = c(5, 7, 6, 5, 8, 7),
#'     b = c(7, 1, 0, 0, 0, 3))
#'     
#' df_2 <-
#'   data.frame(
#'     c = c(8, 8, 8, 6, 1, 3),
#'     d = c(9, 8, 7, 2, 3, 3))
#' 
#' # Validate that values in column
#' # `a` from `df_1` are always >= 5,
#' # and also validate that, in `df_2`,
#' # values in `c` are always == 8
#' # when values in `d` are >= 5  
#' agent <-
#'   create_agent() %>%
#'   focus_on(tbl_name = "df_1") %>%
#'   col_vals_gte(
#'     column = a,
#'     value = 5) %>%
#'   focus_on(tbl_name = "df_2") %>%
#'   col_vals_equal(
#'     column = c,
#'     value = 8,
#'     preconditions = d >= 5) %>%
#'   interrogate()
#'   
#' # Get a basic summary with
#' # `get_interrogation_summary()`
#' get_interrogation_summary(agent)[, 1:7]
#' 
#' @return A \pkg{pointblank} agent object.
#' @import rlang
#' @export
interrogate <- function(agent,
                        get_problem_rows = TRUE,
                        get_first_n = NULL,
                        sample_n = NULL,
                        sample_frac = NULL,
                        sample_limit = 5000) {

  # Add the starting time to the `agent` object
  agent$validation_time <- Sys.time()
  
  for (i in seq(nrow(agent$validation_set))) {

    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
  
    # Get the table object for interrogation  
    table <- get_tbl_object(agent = agent, idx = i)
    
    # Use preconditions to modify the table
    table <- apply_preconditions_to_tbl(agent = agent, idx = i, tbl = table)
    
    # Get the assertion type for this verification step
    assertion_type <- get_assertion_type_at_idx(agent = agent, idx = i)
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on betweenness checking

    if (assertion_type %in% c("col_vals_between", "col_vals_not_between")) {

      # Get the set values for the expression
      set <- get_column_set_values_at_idx(agent = agent, idx = i)
      
      # Determine whether NAs should be allowed
      incl_na <- get_column_incl_na_at_idx(agent = agent, idx = i)

      # Obtain the target column as a symbol
      column <- get_column_as_sym_at_idx(agent = agent, idx = i)
      
      incl_left <- names(set)[1] %>% as.logical()
      incl_right <- names(set)[2] %>% as.logical()
      
      if (assertion_type == "col_vals_between") {
        
        # Perform rowwise validations for the column
        tbl_checked <- ib_incl_incl(table = table, column = {{column}}, set = set, incl_na = incl_na)
      }
      
      if (assertion_type == "col_vals_not_between") {
        
        # Perform rowwise validations for the column
        tbl_checked <- nb_incl_incl(table = table, column = {{column}}, set = set, incl_na = incl_na)
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on set membership
    
    if (assertion_type %in% c("col_vals_in_set", "col_vals_not_in_set")) {

      # Get the set values for the expression
      set <- get_column_set_values_at_idx(agent = agent, idx = i)
      
      # Obtain the target column as a symbol
      column <- get_column_as_sym_at_idx(agent = agent, idx = i)
      
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
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on regex matching
    
    if (assertion_type == "col_vals_regex") {
      
      # Get the regex matching statement
      regex <- agent$validation_set$regex[i]
      
      # Obtain the target column as a symbol
      column <- get_column_as_sym_at_idx(agent = agent, idx = i)
      
      # Perform rowwise validations for the column
      tbl_checked <- 
        table %>% dplyr::mutate(pb_is_good_ = grepl(regex, {{ column }}))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on the presence
    # of NULL values
    
    if (assertion_type == "col_vals_null") {
      
      # Obtain the target column as a symbol
      column <- get_column_as_sym_at_idx(agent = agent, idx = i)
      
      # Perform rowwise validations for the column
      tbl_checked <- 
        table %>% dplyr::mutate(pb_is_good_ = is.na({{ column }}))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on the absence
    # of NULL values
    
    if (assertion_type == "col_vals_not_null") {
      
      # Obtain the target column as a symbol
      column <- get_column_as_sym_at_idx(agent = agent, idx = i)
      
      # Perform rowwise validations for the column
      tbl_checked <- 
        table %>% dplyr::mutate(pb_is_good_ = !is.na({{ column }}))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # check the table structure
    
    if (assertion_type == "cols_exist") {
      
      # Get the column names for the table
      column_names <-
        table %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::as_tibble() %>%
        colnames()
      
      tbl_checked <-
        ifelse(
          agent$validation_set$column[i] %in% column_names,
          TRUE, FALSE)
      
      agent$validation_set$n[i] <- 1
      
      agent$validation_set$n_passed[i] <- 
        agent$validation_set$f_passed[i] <-
        ifelse(tbl_checked, 1, 0)
      
      agent$validation_set$n_failed[i] <-
        agent$validation_set$f_failed[i] <-
        ifelse(tbl_checked, 0, 1)
      
      agent$validation_set$all_passed[i] <-
        ifelse(tbl_checked, TRUE, FALSE)
      
      if (tbl_checked) {
        n_failed <- false_count <- 0
      } else {
        n_failed <- false_count <- 1  
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that rely on
    # comparison operators
    
    if (assertion_type %in%
        c("col_vals_gt", "col_vals_gte",
          "col_vals_lt", "col_vals_lte",
          "col_vals_equal", "col_vals_not_equal")) {
      
      # Get operator values for all assertion types involving
      # simple operator comparisons
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
      
      # Get the final tbl_checked on the table and the query
      tbl_checked <- 
        table %>%
        dplyr::mutate_(.dots = stats::setNames(
          paste0(
            agent$validation_set$column[i],
            operator,
            agent$validation_set$value[i]),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Determine the `false_count` for all validations
    # that validate individual rows in one or more table columns
    
    if (grepl("col_vals.*", assertion_type)) {
      
      # Get total count of rows
      row_count <-
        tbl_checked %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = dplyr::n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      # Get total count of TRUE rows
      n_passed <-
        tbl_checked %>%
        dplyr::filter(pb_is_good_ == TRUE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = dplyr::n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      # Get total count of FALSE rows
      n_failed <-
        tbl_checked %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = dplyr::n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 5)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 5)
      
      # Get count of rows where `pb_is_good_ == FALSE`
      false_count <-
        tbl_checked %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(pb_is_not_good_ = dplyr::n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()
      
      if (false_count > 0) {
        
        # State that `all_passed` is FALSE
        agent$validation_set$all_passed[i] <- FALSE
        
        # Collect problem rows if requested
        
        if (isTRUE(get_problem_rows)) {
          
          problem_rows <- 
            tbl_checked %>%
            dplyr::filter(pb_is_good_ == FALSE) %>%
            dplyr::select(-pb_is_good_)
          
          if (!is.null(get_first_n)) {
            
            problem_rows <-
              problem_rows %>%
              utils::head(get_first_n) %>%
              dplyr::as_tibble()
            
          } else if (!is.null(sample_n) &
                     !(agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL"))) {
            
            problem_rows <-
              dplyr::sample_n(
                tbl = problem_rows,
                size = sample_n,
                replace = FALSE) %>%
              dplyr::as_tibble()
            
          } else if (!is.null(sample_frac) &
                     !(agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL"))) {
            
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
          
          problem_rows <-
            problem_rows %>%
            dplyr::mutate(pb_step_ = i) %>%
            dplyr::select(pb_step_, dplyr::everything())
        }

        # Place the sample of problem rows in `agent$row_samples`
        agent$row_samples <- problem_rows
        
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables on expected column types
    
    if (grepl("col_is_.*", assertion_type)) {
      
      if (inherits(table, "data.frame")) {
        
        column_type <-
          (table %>%
             dplyr::select_(agent$validation_set$column[i]) %>%
             dplyr::filter(dplyr::row_number() == 1) %>%
             dplyr::collect() %>%
             as.data.frame(stringsAsFactors = FALSE))[1, 1] %>% 
          class()
        
        agent$validation_set$n[i] <- 1
        
        if (assertion_type == "col_is_numeric") {
          passed <- ifelse(column_type[1] == "numeric", TRUE, FALSE)
        } else if (assertion_type == "col_is_integer") {
          passed <- ifelse(column_type[1] == "integer", TRUE, FALSE)
        } else if (assertion_type == "col_is_character") {
          passed <- ifelse(column_type[1] == "character", TRUE, FALSE)
        } else if (assertion_type == "col_is_logical") {
          passed <- ifelse(column_type[1] == "logical", TRUE, FALSE)
        } else if (assertion_type == "col_is_factor") {
          passed <- ifelse(column_type[1] == "factor", TRUE, FALSE)
        } else if (assertion_type == "col_is_posix") {
          passed <- ifelse(column_type[1] == "POSIXct", TRUE, FALSE)
        } else if (assertion_type == "col_is_date") {
          passed <- ifelse(column_type[1] == "Date", TRUE, FALSE)
        } else {
          passed <- FALSE
        }
        
        if (passed == TRUE) {
          agent$validation_set$n_passed[i] <- 
            agent$validation_set$f_passed[i] <- 1
          agent$validation_set$n_failed[i] <- 
            agent$validation_set$f_failed[i] <- 0
          false_count <- 0
        } else {
          agent$validation_set$n_passed[i] <- 
            agent$validation_set$f_passed[i] <- 0
          agent$validation_set$n_failed[i] <- 
            agent$validation_set$f_failed[i] <- 1
          false_count <- 1
        }
        
        if (false_count > 0) {
          agent$validation_set$all_passed[i] <- FALSE
        } else if (false_count == 0) {
          agent$validation_set$all_passed[i] <- TRUE
        }
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables on expectation of non-duplicated rows
    
    if (assertion_type == "rows_not_duplicated") {
      
      # Determine if grouping columns are provided in the test
      # for distinct rows and parse the column names
      
      if (!is.na(agent$validation_set$column[i])) {
        if (grepl("(,|&)", agent$validation_set$column[i])) {
          columns <-
            stringr::str_trim(
              stringr::str_split(
                agent$validation_set$column[i],
                pattern = "(,|&)") %>%
                purrr::flatten_chr())
        } else {
          columns <- agent$validation_set$column[i]
        }
      } else if (is.na(agent$validation_set$column[i])) {
        columns <-
          table %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::as_tibble() %>%
          names()
      }
      
      # Get total count of rows
      row_count <-
        table %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = dplyr::n()) %>%
        dplyr::as_tibble() %>%
        purrr::flatten_dbl()

      # Get the rows that are duplicate rows, if any
      duplicate_rows <- 
        table %>%
        dplyr::select(dplyr::one_of(columns)) %>%
        dplyr::group_by_(.dots = columns) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::ungroup()
      
      # Determine whether the test for duplicated passed
      # (no duplicates) or failed (one or more duplicates)
      passed <-
        ifelse(
          duplicate_rows %>%
            dplyr::group_by() %>%
            dplyr::summarize(row_count = dplyr::n()) %>%
            dplyr::as_tibble() %>%
            purrr::flatten_dbl() == 0, TRUE, FALSE)
      
      if (passed == TRUE) {
        n_passed <- row_count
        n_failed <- false_count <- 0
      } else if (passed == FALSE) {
        n_failed <- false_count <-
          duplicate_rows %>%
          dplyr::group_by() %>%
          dplyr::summarize(row_count = dplyr::n()) %>%
          dplyr::as_tibble() %>%
          purrr::flatten_dbl()
        
        n_passed <- row_count - n_failed
      }
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 5)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 5)
      
      if (false_count > 0) {
        agent$validation_set$all_passed[i] <- FALSE
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    # ---------------------------------------------------------------
    # Determine the course of action for each validation check
    actions <-
      determine_action(
        validation_step = agent$validation_set[i, ],
        false_count = false_count,
        warn_count = agent$validation_set$warn_count[i],
        stop_count = agent$validation_set$stop_count[i],
        notify_count = agent$validation_set$notify_count[i],
        warn_fraction = agent$validation_set$warn_fraction[i],
        stop_fraction = agent$validation_set$stop_fraction[i],
        notify_fraction = agent$validation_set$notify_fraction[i]
      )
    
    agent$validation_set$notify[i] <- actions$notify
    agent$validation_set$warn[i] <- actions$warn
    
    # Get the ending time for the validation step
    validation_end_time <- Sys.time()
    
    # Get the duration for the validation step    
    time_diff_s <- (validation_end_time - validation_start_time)[[1]] %>% round(4)
    
    # Add the timing information to the `agent` object
    agent$validation_set$time_processed[i] <- validation_start_time
    agent$validation_set$proc_duration_s[i] <- time_diff_s
  }
  
  # Disconnect any open PostgreSQL connections --------------------
  #disconnect_postgres()
  
  # Notification Step - email ---------------------------------------------
  if (length(agent$email$email_creds_file_path) > 0 &
      length(agent$email$email_notification_recipients) > 0 & 
      agent$email$email_notifications_active == TRUE &
      nrow(agent$validation_set) > 0 &
      any(agent$validation_set$notify == TRUE)) {
    
    # TODO: perform notification via notification method
  }
  
  # Notification Step - Slack ---------------------------------------------
  if (length(agent$slack$slack_webhook_url) > 0 & 
      agent$slack$slack_notifications_active == TRUE &
      nrow(agent$validation_set) > 0 &
      any(agent$validation_set$notify == TRUE)) {
    
    # TODO: perform notification via notification method
    notify_count <- 
      agent$validation_set %>%
      dplyr::select(notify) %>%
      dplyr::filter(notify == TRUE) %>%
      nrow()
    
    warning_count <- 
      agent$validation_set %>%
      dplyr::select(warn) %>%
      dplyr::filter(warn == TRUE) %>%
      nrow()
    
    # If a value for `slack_footer_timestamp` is not
    # provided, use the system time as the timestamp
    if (is.null(slack_footer_timestamp)) {
      slack_footer_timestamp <- Sys.time() %>% as.integer()
    }
    
    if (notify_count > 0) {
      
      notify_text <-
        ifelse(
          notify_count == 1,
          glue::glue("There is {notify_count} validation step that resulted in significant failures."),
          glue::glue("There are {notify_count} validation steps that resulted in significant failures."))
    }
    
    if (warning_count > 0) {
      
      warning_text <-
        ifelse(
          warning_count == 1,
          glue::glue("There is {warning_count} validation step that issued a warning."),
          glue::glue("There are {warning_count} validation steps that issued warnings."))
    }
    
    if (notify_count > 0 & warning_count > 0) {
      notification_text <- paste0(
        gsub("\\.", "", notify_text),
        " and ",
        gsub("There", "there", warning_text))
    }
    
    if (notify_count > 1 & warning_count == 0) {
      notification_text <- notify_text
    }
    
    slack_footer_timestamp <- Sys.time() %>% as.integer()
    
    httr::POST(
      url = agent$slack_webhook_url,
      encode = "form",
      httr::add_headers(
        `Content-Type` = "application/x-www-form-urlencoded",
        Accept = "*/*"),
      body = utils::URLencode(
        glue::glue(
          "payload={{
                   \"channel\": \"{agent$slack_channel}\",
                   \"username\": \"{agent$slack_username}\",
                   \"attachments\": [
                   {{
                   \"fallback\": \"{agent$slack_title}\",
                   \"color\": \"danger\",
                   \"author_name\": \"{agent$slack_author_name}\",
                   \"title\": \"{agent$slack_title}\",
                   \"title_link\": \"{agent$slack_report_url}\",
                   \"text\": \"{notification_text}\",
                   \"thumb_url\": \"{agent$slack_footer_thumb_url}\",
                   \"footer\": \"{agent$slack_footer_text}\",
                   \"ts\": {slack_footer_timestamp}
                   }}
                   ]
                   }}")))
  }
  
  agent
}
