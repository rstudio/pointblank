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
      
      table <- get(agent$validation_set$tbl_name[i])
      
    } else if (agent$validation_set$db_type[i] == "PostgreSQL") {
      
      table <- 
        set_entry_point(
          table = agent$validation_set$tbl_name[i],
          db_type = agent$validation_set$db_type[i],
          credentials_file = agent$validation_set$db_cred_file_path[i])
    }
    
    if (agent$validation_set$assertion_type[i] == "verify_col_gt") {
      operator <- ">"
    }
    
    judgment <- 
      table %>%
      dplyr::mutate_(.dots = setNames(
        paste0(
          agent$validation_set$column[i],
          operator,
          agent$validation_set$value[i]),
        "pb_is_good_"))
    
    # Get count of rows where `pb_is_good_ == FALSE`
    false_count <-
      judgment %>%
      filter(pb_is_good_ == FALSE) %>%
      group_by() %>%
      summarize(pb_is_not_good_ = n()) %>%
      as_tibble() %>%
      .$pb_is_not_good_
    
    if (false_count > 0) {
      
      agent$validation_set$passed <- FALSE
      
      problem_rows <- 
        judgment %>%
        dplyr::filter(pb_is_good_ == FALSE) %>%
        dplyr::select(-pb_is_good_) %>%
        head(5) %>%
        tibble::as_tibble()
      
      agent$validation_set$row_sample[i] <- 
        problem_rows %>%
        tidyr::nest_(key_col = "data", nest_cols = names(.))
      
      agent$validation_set$row_sample[i][[1]][[1]] 
    } else {
      agent$validation_set$passed[i] <- TRUE
    }
    
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
  
  return(agent)
}
