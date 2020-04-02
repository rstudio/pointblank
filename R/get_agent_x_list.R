#' Get the agent's x-list
#' 
#' The agent's x-list is a record of information that the agent possesses at any
#' given time. The x-list will contain the most information after an
#' interrogation has taken place. The x-list can be constrained to a particular
#' validation step (by supplying the step number to the `i` argument), or, we
#' can get the information for all validation steps by leaving `i` unspecified.
#' The x-list is indeed an R `list` object that contains a veritable cornucopia
#' of information.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' @param i The validation step number, which is assigned to each validation
#'   step in the order of definition. If `NULL` (the default), the x list will
#'   be of the post-interrogation form (complete information). If a valid step
#'   number is provided then x-list will have partial information, that is, data
#'   constrained to to interrogation result of the named step.
#' 
#' @return A `list` object.
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 5-2
#' 
#' @export
get_agent_x_list <- function(agent,
                             i = NULL) {
  
  if (!is.null(i)) {
    
    .warn <- agent$validation_set[[i, "warn"]]
    .notify <- agent$validation_set[[i, "notify"]]
    .stop <- agent$validation_set[[i, "stop"]]
    
    .name <- agent$name
    .time <- agent$time
    .reporting_lang <- agent$reporting_lang
    .tbl <- agent$tbl
    .tbl_name <- agent$tbl_name
    .tbl_src <- agent$tbl_src
    .tbl_src_details <- agent$tbl_src_details
    .col_names <- agent$col_names
    .col_types <- agent$col_types
    
    .i <- i
    .type <- agent$validation_set[[i, "assertion_type"]]
    .columns <- agent$validation_set[[i, "column"]] %>% unlist()
    .values <- agent$validation_set[[i, "values"]] %>% unlist()
    .briefs <- agent$validation_set[[i, "brief"]]
    
    .eval_error <- agent$validation_set[[i, "eval_error"]]
    .eval_warning <- agent$validation_set[[i, "eval_warning"]]
    .capture_stack <- agent$validation_set[[i, "capture_stack"]]
    
    .n <- agent$validation_set[[i, "n"]]
    .n_passed <- agent$validation_set[[i, "n_passed"]]
    .n_failed <- agent$validation_set[[i, "n_failed"]]
    .f_passed <- agent$validation_set[[i, "f_passed"]]
    .f_failed <- agent$validation_set[[i, "f_failed"]]
    
    x <-
      list(
        time = .time,
        name = .name,
        tbl_name = .tbl_name,
        tbl_src = .tbl_src,
        tbl_src_details = .tbl_src_details,
        tbl = .tbl,
        col_names = .col_names,
        col_types = .col_types,
        i = .i,
        type = .type,
        columns = .columns,
        values = .values,
        briefs = .briefs,
        eval_error = .eval_error,
        eval_warning = .eval_warning,
        capture_stack = .capture_stack,
        n = .n,
        n_passed = .n_passed,
        n_failed = .n_failed,
        f_passed = .f_passed,
        f_failed = .f_failed,
        warn = .warn,
        stop = .stop,
        notify = .notify,
        reporting_lang = .reporting_lang
      )
    
    class(x) <- c("x_list_i", "x_list")
  }
  
  if (is.null(i)) {
    
    .warn <- agent$validation_set$warn
    .notify <- agent$validation_set$notify
    .stop <- agent$validation_set$stop
    
    .name <- agent$name
    .time <- agent$time
    .reporting_lang <- agent$reporting_lang
    .tbl <- agent$tbl
    .tbl_name <- agent$tbl_name
    .tbl_src <- agent$tbl_src
    .tbl_src_details <- agent$tbl_src_details
    .col_names <- agent$col_names
    .col_types <- agent$col_types
    
    .i <- agent$validation_set$i
    .type <- agent$validation_set$assertion_type
    .columns <- agent$validation_set$column
    .values <- agent$validation_set$values
    .briefs <- agent$validation_set$brief
    
    .eval_error <- agent$validation_set$eval_error
    .eval_warning <- agent$validation_set$eval_warning
    .capture_stack <- agent$validation_set$capture_stack
    
    .n <- agent$validation_set$n
    .n_passed <- agent$validation_set$n_passed
    .n_failed <- agent$validation_set$n_failed
    .f_passed <- agent$validation_set$f_passed
    .f_failed <- agent$validation_set$f_failed
    
    .validation_set <- agent$validation_set
    
    .report_object <- agent %>% get_agent_report()
    .report_object_small <- agent %>% get_agent_report(size = "small")
    
    if (!is.null(.report_object)) {
      .report_html <- gt::as_raw_html(.report_object, inline_css = FALSE)
    } else {
      .report_html <- NULL
    }
    
    if (!is.null(.report_object_small)) {
      .report_html_small <- gt::as_raw_html(.report_object_small, inline_css = TRUE)
    } else {
      .report_html_small <- NULL
    }
    
    if (length(.time) != 0) {
      
      x <-
        list(
          time = .time,
          report_html_small = .report_html_small
        )
      
      .email_object <- 
        blastula::compose_email(
          header = NULL,
          body = glue::glue(stock_msg_body()) %>% blastula::md(),
          footer = glue::glue(stock_msg_footer()) %>% blastula::md(),
        )
      
    } else {
      .email_object <- NULL
    }
    
    x <-
      list(
        time = .time,
        name = .name,
        tbl_name = .tbl_name,
        tbl_src = .tbl_src,
        tbl_src_details = .tbl_src_details,
        tbl = .tbl,
        col_names = .col_names,
        col_types = .col_types,
        i = .i,
        type = .type,
        columns = .columns,
        values = .values,
        briefs = .briefs,
        eval_error = .eval_error,
        eval_warning = .eval_warning,
        capture_stack = .capture_stack,
        n = .n,
        n_passed = .n_passed,
        n_failed = .n_failed,
        f_passed = .f_passed,
        f_failed = .f_failed,
        warn = .warn,
        stop = .stop,
        notify = .notify,
        reporting_lang = .reporting_lang,
        validation_set = .validation_set,
        report_object = .report_object,
        email_object = .email_object,
        report_html = .report_html,
        report_html_small = .report_html_small
      )
    
    class(x) <- c("x_list_n", "x_list")
  }
  
  x
}
