#' Given an agent that is fully loaded with
#' tasks, perform an interrogation
#' @description The agent has all the
#' information on what to do, so now all
#' interrogations can proceed efficiently,
#' and, according to plan.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @return an agent object.
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr group_by group_by_ mutate_ filter filter_ select select_ collect ungroup summarize row_number n
#' @importFrom tidyr nest_
#' @importFrom stringr str_split
#' @importFrom purrr flatten_chr
#' @importFrom readr read_csv read_tsv
#' @importFrom stats setNames
#' @importFrom utils head
#' @export interrogate

interrogate <- function(agent) {
  
  # Get the starting time for the interrogation
  interrogation_start_time <- Sys.time()
  
  # Add the starting time to the `agent` object
  agent$validation_time <- interrogation_start_time
  
  # Create bindings for variables
  pb_is_good_ <- set <- NULL
  
  # Get number of rows in `validation_set`
  n_validations <- nrow(agent$validation_set)
  
  for (i in 1:n_validations) {
    
    # Get the starting time for the validation step
    validation_start_time <- Sys.time()
    
    if (agent$validation_set$db_type[i] == "local_df") {
      
      # Create `table` object as the direct reference to a
      # local `data.frame` or `tbl_df` object
      table <- get(agent$validation_set$tbl_name[i])
      
    } else if (agent$validation_set$db_type[i] == "local_file") {
      
      file_path <- agent$validation_set$file_path[i]
      col_types <- agent$validation_set$col_types[i]
      
      # Infer the file type from the extension
      file_extension <- 
        (agent$validation_set$file_path[i] %>% 
           basename() %>% 
           str_split(pattern = "\\.") %>% 
           unlist())[2] %>% 
        tolower()
      
      if (is.na(col_types)) {
        if (file_extension == "csv") {
          table <- 
            suppressMessages(
              readr::read_csv(file_path))
        } else if (file_extension == "tsv") {
          table <- 
            suppressMessages(
              readr::read_tsv(file_path))
        }
      }
      
      if (!is.na(col_types)){
        if (file_extension == "csv") {
          table <- 
            suppressMessages(
              readr::read_csv(file_path,
                              col_types = col_types))
        } else if (file_extension == "tsv") {
          table <- 
            suppressMessages(
              readr::read_tsv(file_path,
                              col_types = col_types))
        }
      }
      
    } else if (agent$validation_set$db_type[i] %in% c("PostgreSQL", "MySQL")) {
      
      # Create `table` object as an SQL entry point for a remote table
      table <- 
        set_entry_point(
          table = agent$validation_set$tbl_name[i],
          db_type = agent$validation_set$db_type[i],
          creds_file = agent$validation_set$db_cred_file_path[i],
          initial_sql = agent$validation_set$init_sql[i])
    }
    
    # Use preconditions to modify the table
    if (!is.na(agent$preconditions[[i, 1]])) {

      # Get the preconditions as a character vector
      preconditions <-
        agent$preconditions[[i, 1]] %>%
        strsplit(";") %>%
        unlist() %>%
        trimws()

      if (!is.null(preconditions)) {
        for (j in 1:length(preconditions)) {

          # Apply the preconditions to filter the table
          # before any validation occurs
          table <-
            table %>%
            dplyr::filter_(preconditions[j])
        }
      }
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that rely on
    # comparison operators
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_gt", "col_vals_gte",
          "col_vals_lt", "col_vals_lte",
          "col_vals_equal", "col_vals_not_equal")) {
      
      # Get operator values for all assertion types involving
      # simple operator comparisons
      if (agent$validation_set$assertion_type[i] == "col_vals_gt") {
        operator <- ">"
      } else if (agent$validation_set$assertion_type[i] == "col_vals_gte") {
        operator <- ">="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_lt") {
        operator <- "<"
      } else if (agent$validation_set$assertion_type[i] == "col_vals_lte") {
        operator <- "<="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_equal") {
        operator <- "=="
      } else if (agent$validation_set$assertion_type[i] == "col_vals_not_equal") {
        operator <- "!="
      } 
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0(
            agent$validation_set$column[i],
            operator,
            agent$validation_set$value[i]),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on betweenness checking
    
    if (agent$validation_set$assertion_type[i] == "col_vals_between") {
      
      # Get the `left` and `right` bounding values
      left <- agent$validation_set$set[[i]][[1]][[1]][1]
      right <- agent$validation_set$set[[i]][[1]][[1]][2]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0(
            agent$validation_set$column[i],
            " >= ", left, " & ",
            agent$validation_set$column[i],
            " <= ", right),
          "pb_is_good_"))
    }
    
    if (agent$validation_set$assertion_type[i] == "col_vals_not_between") {
      
      # Get the `left` and `right` bounding values
      left <- agent$validation_set$set[[i]][[1]][[1]][1]
      right <- agent$validation_set$set[[i]][[1]][[1]][2]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("!(",
                 agent$validation_set$column[i],
                 " >= ", left, " & ",
                 agent$validation_set$column[i],
                 " <= ", right, ")"),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # rely on set membership
    
    if (agent$validation_set$assertion_type[i] == "col_vals_in_set") {
      
      # Get the set values for the expression
      set <- agent$validation_set$set[[i]][[1]][[1]]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0(
            agent$validation_set$column[i],
            " %in% c(",
            paste(paste0("'", set) %>% paste0("'"),
                  collapse = ", "), ")"),
          "pb_is_good_"))
    }
    
    if (agent$validation_set$assertion_type[i] == "col_vals_not_in_set") {
      
      # Get the set values for the expression
      set <- agent$validation_set$set[[i]][[1]][[1]]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("!(",
                 agent$validation_set$column[i],
                 " %in% c(",
                 paste(paste0("'", set) %>% paste0("'"),
                       collapse = ", "), "))"),
          "pb_is_good_"))
    }
    
    if (agent$validation_set$assertion_type[i] == "col_vals_regex") {
      
      # Get the regex matching statement
      regex <- agent$validation_set$regex[i]
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("grepl(\"",
                 agent$validation_set$regex[i],
                 "\", ", agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    if (agent$validation_set$assertion_type[i] == "col_vals_null") {
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("is.na(",
                 agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    if (agent$validation_set$assertion_type[i] == "col_vals_not_null") {
      
      # Get the final judgment on the table and the query
      judgment <- 
        table %>%
        dplyr::mutate_(.dots = setNames(
          paste0("!is.na(",
                 agent$validation_set$column[i],
                 ")"),
          "pb_is_good_"))
    }
    
    # ---------------------------------------------------------------
    # Judge tables based on assertion types that
    # check the table structure
    
    
    if (agent$validation_set$assertion_type[i] == "col_exists") {
      
      # Get the column names for the table
      column_names <-
        table %>%
        dplyr::filter(row_number() == 1) %>%
        tibble::as_tibble() %>%
        colnames()
      
      judgment <-
        ifelse(
          agent$validation_set$column[i] %in% column_names,
          TRUE, FALSE)
      
      agent$validation_set$n[i] <- 1
      
      agent$validation_set$n_passed[i] <- 
        agent$validation_set$f_passed[i] <-
        ifelse(judgment, 1, 0)
      
      agent$validation_set$n_failed[i] <-
        agent$validation_set$f_failed[i] <-
        ifelse(judgment, 0, 1)
      
      agent$validation_set$all_passed[i] <-
        ifelse(judgment, TRUE, FALSE)
      
      if (judgment) {
        n_failed <- false_count <- 0
      } else {
        n_failed <- false_count <- 1  
      }
    }
    
    # ---------------------------------------------------------------
    # Determine the `false_count` for all validations that examine
    # individual rows in one or more table columns
    
    if (grepl("col_vals.*", agent$validation_set$assertion_type[i])) {
      
      # Get total count of rows
      row_count <-
        judgment %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        tibble::as_tibble() %>%
        .$row_count
      
      # Get total count of TRUE rows
      n_passed <-
        judgment %>%
        dplyr::filter(pb_is_good_ == TRUE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        tibble::as_tibble() %>%
        .$row_count
      
      # Get total count of FALSE rows
      n_failed <-
        judgment %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        tibble::as_tibble() %>%
        .$row_count
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 3)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 3)
      
      # Get count of rows where `pb_is_good_ == FALSE`
      false_count <-
        judgment %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::group_by() %>%
        dplyr::summarize(pb_is_not_good_ = n()) %>%
        tibble::as_tibble() %>%
        .$pb_is_not_good_
      
      if (false_count > 0) {
        
        # State that `all_passed` is FALSE
        agent$validation_set$all_passed[i] <- FALSE
        
        # Collect up to 5 problem rows 
        problem_rows <- 
          judgment %>%
          dplyr::filter(pb_is_good_ == FALSE) %>%
          dplyr::select(-pb_is_good_) %>%
          head(5) %>%
          tibble::as_tibble()
        
        # Place the sample of problem rows in
        # the `agent$validation_set` tbl_df
        # as a nested tbl_df
        agent$validation_set$row_sample[i] <- 
          problem_rows %>%
          tidyr::nest_(
            key_col = "data",
            nest_cols = names(.))
        
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    if (grepl("col_is_.*", agent$validation_set$assertion_type[i])) {
      
      if (inherits(table, "data.frame")) {
        
        column_type <-
          table %>%
          dplyr::select_(agent$validation_set$column[i]) %>%
          dplyr::filter(row_number() == 1) %>%
          dplyr::collect() %>%
          as.data.frame(stringsAsFactors = FALSE) %>% 
          .[1, 1] %>% 
          class()
        
        agent$validation_set$n[i] <- 1
        
        if (agent$validation_set$assertion_type[i] == "col_is_numeric") {
          passed <- ifelse(column_type[1] == "numeric", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_integer") {
          passed <- ifelse(column_type[1] == "integer", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_character") {
          passed <- ifelse(column_type[1] == "character", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_logical") {
          passed <- ifelse(column_type[1] == "logical", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_factor") {
          passed <- ifelse(column_type[1] == "factor", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_posix") {
          passed <- ifelse(column_type[1] == "POSIXct", TRUE, FALSE)
        } else if (agent$validation_set$assertion_type[i] == "col_is_date") {
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
    
    if (agent$validation_set$assertion_type[i] == "rows_not_duplicated") {
      
      # Determine if grouping columns are provided in the test
      # for distinct rows and parse the column names
      
      if (!is.na(agent$validation_set$column[i])) {
        if (grepl("(,|&)", agent$validation_set$column[i])) {
          columns <-
            stringr::str_split(agent$validation_set$column[i], pattern = "(,|&)") %>%
            purrr::flatten_chr() %>%
            trimws()
        } else {
          columns <- agent$validation_set$column[i]
        }
      } else if (is.na(agent$validation_set$column[i])) {
        columns <-
          table %>%
          dplyr::filter(row_number() == 1) %>%
          tibble::as_tibble() %>%
          names()
      }
      
      # Get total count of rows
      row_count <-
        table %>%
        dplyr::group_by() %>%
        dplyr::summarize(row_count = n()) %>%
        tibble::as_tibble() %>%
        .$row_count
      
      # Get the rows that are duplicate rows, if any
      duplicate_rows <- 
        table %>%
        dplyr::select_(paste0("c(", paste(columns, collapse = ", ")) %>% paste0(")")) %>%
        dplyr::group_by_(.dots = columns) %>%
        dplyr::filter(n() > 1) %>%
        dplyr::ungroup()
      
      # Determine whether the test for duplicated passed
      # (no duplicates) or failed (one or more duplicates)
      passed <-
        ifelse(
          duplicate_rows %>%
            dplyr::group_by() %>%
            dplyr::summarize(row_count = n()) %>%
            tibble::as_tibble() %>%
            .$row_count == 0, TRUE, FALSE)
      
      if (passed == TRUE) {
        n_passed <- row_count
        n_failed <- false_count <- 0
      } else if (passed == FALSE) {
        n_failed <- false_count <-
          duplicate_rows %>%
          dplyr::group_by() %>%
          dplyr::summarize(row_count = n()) %>%
          tibble::as_tibble() %>%
          .$row_count
        
        n_passed <- row_count - n_failed
      }
      
      agent$validation_set$n[i] <- row_count
      agent$validation_set$n_passed[i] <- n_passed
      agent$validation_set$n_failed[i] <- n_failed
      agent$validation_set$f_passed[i] <- round((n_passed / row_count), 3)
      agent$validation_set$f_failed[i] <- round((n_failed / row_count), 3)
      
      if (false_count > 0) {
        agent$validation_set$all_passed[i] <- FALSE
      } else if (false_count == 0) {
        agent$validation_set$all_passed[i] <- TRUE
      }
    }
    
    # ---------------------------------------------------------------
    # Determine the course of action
    # for each validation check
    
    actions <-
      determine_action(
        n = agent$validation_set$n[i],
        false_count = false_count,
        warn_count = agent$validation_set$warn_count[i],
        notify_count = agent$validation_set$notify_count[i],
        warn_fraction = agent$validation_set$warn_fraction[i],
        notify_fraction = agent$validation_set$notify_fraction[i])
    
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
  disconnect_postgres()
  
  # Notification Step ---------------------------------------------
  if (length(agent$email_creds_file_path) > 0 &
      length(agent$notification_recipients) > 0 & 
      agent$notification_emails_active == TRUE &
      nrow(agent$validation_set) > 0 &
      any(agent$validation_set$notify == TRUE)) {
    
    pb_notify(
      agent = agent,
      recipients = agent$notification_recipients,
      creds_file = agent$email_creds_file_path)
  }
  
  return(agent)
}
