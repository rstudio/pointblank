#' Print the agent information to the console
#'
#' This function will allow the agent to print a useful report.
#' 
#' @param x An agent object of class `ptblank_agent`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_agent <- function(x, ...) {
  
  # nocov start 
  
  print(get_agent_report(x))
  
  # nocov end 
}


#' Print the step-focused x-list to the console
#'
#' This function will nicely print a step-focused x-list object to the console.
#' 
#' @param x An x-list object of class `x_list_i`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.x_list_i <- function(x, ...) {
  
  # nocov start
  
  cli::cli_div(
    theme = list(
      span.cyan = list(color = "cyan"),
      span.red = list(color = "red"),
      span.blue = list(color = "blue"),
      span.green = list(color = "green"),
      span.yellow = list(color = "yellow"),
      span.orange = list(color = "orange")
    )
  )
  
  length_rows <- length(x$warn)
  
  cli::cli_rule(left = "The x-list for `{x$name}`", right = "STEP {x$i}")
  cli::cli_text("{.cyan $time} ({.red POSIXct [1]})")
  cli::cli_text("{.cyan $name $tbl_name $tbl_src} ({.red chr [1]})")
  cli::cli_text("{.cyan $tbl} ({.blue {class(x$tbl)}})")
  cli::cli_text("{.cyan $tbl_src_details} ({.red chr [{length(x$tbl_src_details)}]})")
  cli::cli_text("{.cyan $col_names $col_types} ({.red chr [{length(x$col_names)}]})")
  cli::cli_text("{.cyan $i $type $column $values $brief} ({.green mixed [{length(x$i)}]})")
  cli::cli_text("{.cyan $eval_error $eval_warning} ({.yellow lgl [{length(x$i)}]})")
  cli::cli_text("{.cyan $capture_stack} ({.orange list [{length(x$capture_stack)}]})")
  cli::cli_text("{.cyan $i $type $column $values $brief} ({.green mixed [{length(x$i)}]})")
  cli::cli_text("{.cyan $n $n_passed $n_failed $f_passed $f_failed} ({.green num [{length_rows}]})")
  cli::cli_text("{.cyan $warn $notify $stop} ({.yellow lgl [{length_rows}]})")
  cli::cli_rule()

  # nocov end 
}

#' Print the complete x-list to the console
#'
#' This function will nicely print a complete x-list object to the console.
#' 
#' @param x An x-list object of class `x_list_n`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.x_list_n <- function(x, ...) {
  
  # nocov start
  
  cli::cli_div(
    theme = list(
      span.cyan = list(color = "cyan"),
      span.red = list(color = "red"),
      span.blue = list(color = "blue"),
      span.green = list(color = "green"),
      span.yellow = list(color = "yellow"),
      span.orange = list(color = "orange"),
      span.pink = list(color = "pink"),
      span.brown = list(color = "brown")
    )
  )

  length_rows <- length(x$warn)
  validation_set_rows <- nrow(x$validation_set)
  validation_set_cols <- ncol(x$validation_set)
  
  cli::cli_rule(left = "The x-list for `{x$name}`", right = "COMPLETE")
  cli::cli_text("{.cyan $time} ({.red POSIXct [1]})")
  cli::cli_text("{.cyan $name $tbl_name $tbl_src} ({.red chr [1]})")
  cli::cli_text("{.cyan $tbl} ({.blue {class(x$tbl)}})")
  cli::cli_text("{.cyan $tbl_src_details} ({.red chr [{length(x$tbl_src_details)}]})")
  cli::cli_text("{.cyan $col_names $col_types} ({.red chr [{length(x$col_names)}]})")
  cli::cli_text("{.cyan $i $type $column $values $brief} ({.green mixed [{length(x$i)}]})")
  cli::cli_text("{.cyan $eval_error $eval_warning} ({.yellow lgl [{length(x$i)}]})")
  cli::cli_text("{.cyan $capture_stack} ({.orange list [{length(x$capture_stack)}]})")
  cli::cli_text("{.cyan $i $type $column $values $brief} ({.green mixed [{length(x$i)}]})")
  cli::cli_text("{.cyan $n $n_passed $n_failed $f_passed $f_failed} ({.green num [{length_rows}]})")
  cli::cli_text("{.cyan $warn $notify $stop} ({.yellow lgl [{length_rows}]})")
  cli::cli_text("{.cyan $validation_set} ({.blue tbl_df [{validation_set_rows}, {validation_set_cols}]})")
  cli::cli_text("{.cyan $report_object} ({.pink gt_tbl})")
  cli::cli_text("{.cyan $report_html $report_html_email} ({.brown html})")
  cli::cli_rule()
    
  # nocov end 
}
