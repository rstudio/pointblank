#' Given an agent that is fully loaded with
#' tasks, perform an interrogation
#' @description The agent has all the
#' information on what to do, so now all
#' interrogations can proceed efficiently,
#' and, according to plan.
#' @return an agent object.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_ filter select
#' @importFrom tidyr nest_
#' @export interrogate

interrogate <- function(agent) {
  
  # Get number of rows in `validation_set`
  n_validations <- nrow(agent$validation_set)
  
  for (i in 1:n_validations) {
    
    if (agent$validation_set$db_type[i] == "local") {
      
      # Create `table` object as the direct reference to a
      # local `data.frame` or `tbl_df` object
      table <- get(agent$validation_set$tbl_name[i])
      
    } else if (agent$validation_set$db_type[i] == "PostgreSQL") {
      
      # Create `table` object as an SQL entry point for a remote table
      table <- 
        set_entry_point(
          table = agent$validation_set$tbl_name[i],
          db_type = agent$validation_set$db_type[i],
          creds_file = agent$validation_set$db_cred_file_path[i])
    }
    
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
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_in_set")) {
      
      # Get the set values for the expression
      if (agent$validation_set$assertion_type[i] == "col_vals_in_set") {
        set <- agent$validation_set$set[[i]][[1]][[1]]
      }
      
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
    
    if (agent$validation_set$assertion_type[i] %in%
        c("col_vals_regex")) {
      
      # Get the regex matching statement
      if (agent$validation_set$assertion_type[i] == "col_vals_regex") {
        regex <- agent$validation_set$regex[i]
      }
      
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
    
    # Get total count of rows
    n <-
      judgment %>%
      dplyr::group_by() %>%
      dplyr::summarize(row_count = n()) %>%
      tibble::as_tibble() %>%
      .$row_count
    
    # Get total count of TRUE rows
    n_passed <-
      judgment %>%
      filter(pb_is_good_ == TRUE) %>%
      dplyr::group_by() %>%
      dplyr::summarize(row_count = n()) %>%
      tibble::as_tibble() %>%
      .$row_count
    
    # Get total count of FALSE rows
    n_failed <-
      judgment %>%
      filter(pb_is_good_ == FALSE) %>%
      dplyr::group_by() %>%
      dplyr::summarize(row_count = n()) %>%
      tibble::as_tibble() %>%
      .$row_count
    
    agent$validation_set$n[i] <- n
    agent$validation_set$n_passed[i] <- n_passed
    agent$validation_set$n_failed[i] <- n_failed
    agent$validation_set$f_passed[i] <- round((n_passed / n), 3)
    agent$validation_set$f_failed[i] <- round((n_failed / n), 3)
    
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
    
    #
    # Determine the course of action
    # for each validation check
    #
    
    actions <-
      determine_action(
        false_count = false_count,
        report_count = agent$validation_set$report_count[i],
        warn_count = agent$validation_set$warn_count[i],
        notify_count = agent$validation_set$notify_count[i])
    
    agent$validation_set$report[i] <- actions$report
    agent$validation_set$notify[i] <- actions$notify
    agent$validation_set$warn[i] <- actions$warn
  }
  
  # Disconnect any open PostgreSQL connections
  disconnect_postgres()
  
  return(agent)
}
