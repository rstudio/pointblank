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


# nolint start

#' Print the agent information to the console
#'
#' This function will allow the agent to print a useful report.
#' 
#' @param x An agent object of class `ptblank_agent`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_agent <- function(x, view = interactive(), ...) {
  
  # nocov start 
  
  print(get_agent_report(x), view = view, ...)
  
  # nocov end 
}

#' Knit print the agent information table
#'
#' This facilitates printing of the agent report table within a knitr code
#' chunk.
#'
#' @param x An object of class `ptblank_agent`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_agent <- function(x, ...) {
  
  # nocov start 

  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(get_agent_report(x), ...)
  
  # nocov end 
}


#' Print the table information report
#'
#' This function will allow the table information to be nicely printed.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_informant <- function(x, view = interactive(), ...) {
  
  # nocov start 
  
  print(get_informant_report(x), view = view, ...)
  
  # nocov end 
}

#' Knit print the table information report
#'
#' This facilitates printing of the table information report within a knitr code
#' chunk.
#'
#' @param x An object of class `ptblank_informant`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_informant <- function(x, ...) {
  
  # nocov start 
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(get_informant_report(x), ...)
  
  # nocov end 
}


#' Print a single-step x-list to the console
#'
#' This function will print an x-list object, for a single step, to the console.
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

  cli::cli_rule(
    left = "The x-list for `{x$name}`",
    right = "STEP {x$i}"
  )
  cli::cli_text(
    "{.cyan $time_start $time_end} ({.red POSIXct [{length(x$time_start)}]})"
  )
  cli::cli_text(
    "{.cyan $label $tbl_name $tbl_src $tbl_src_details} ({.red chr [1]})"
  )
  cli::cli_text(
    "{.cyan $tbl} ({.blue {class(x$tbl)}})"
  )
  cli::cli_text(
    "{.cyan $col_names $col_types} ({.red chr [{length(x$col_names)}]})"
  )
  cli::cli_text(
    "{.cyan $i $type $columns $values $label $briefs} ",
    "({.green mixed [{length(x$i)}]})"
  )
  cli::cli_text(
    "{.cyan $eval_error $eval_warning} ({.yellow lgl [{length(x$i)}]})"
  )
  cli::cli_text(
    "{.cyan $capture_stack} ({.orange list [{length(x$capture_stack)}]})"
  )
  cli::cli_text(
    "{.cyan $n $n_passed $n_failed $f_passed $f_failed} ",
    "({.green num [{length_rows}]})"
  )
  cli::cli_text(
    "{.cyan $warn $stop $notify} ({.yellow lgl [{length_rows}]})"
  )
  cli::cli_text(
    "{.cyan $lang} ({.red chr [1]})"
  )
  cli::cli_rule(
    right = ifelse(length(x$time) == 0, "NO INTERROGATION PERFORMED", "")
  )

  # nocov end 
}

#' Print an x-list comprising all validation steps to the console
#'
#' This function will print a x-list object, with all validation steps included,
#' to the console.
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
  
  cli::cli_rule(
    left = "The x-list for `{x$name}`",
    right = "ALL STEPS"
  )
  cli::cli_text(
    "{.cyan $time_start $time_end} ({.red POSIXct [{length(x$time_start)}]})"
  )
  cli::cli_text(
    "{.cyan $label $tbl_name $tbl_src $tbl_src_details} ({.red chr [1]})"
  )
  cli::cli_text(
    "{.cyan $tbl} ({.blue {class(x$tbl)}})"
  )
  cli::cli_text(
    "{.cyan $col_names $col_types} ({.red chr [{length(x$col_names)}]})"
  )
  cli::cli_text(
    "{.cyan $i $type $columns $values $label $briefs} ",
    "({.green mixed [{length(x$i)}]})"
  )
  cli::cli_text(
    "{.cyan $eval_error $eval_warning} ({.yellow lgl [{length(x$i)}]})"
  )
  cli::cli_text(
    "{.cyan $capture_stack} ({.orange list [{length(x$capture_stack)}]})"
  )
  cli::cli_text(
    "{.cyan $n $n_passed $n_failed $f_passed $f_failed} ",
    "({.green num [{length_rows}]})"
  )
  cli::cli_text(
    "{.cyan $warn $stop $notify} ({.yellow lgl [{length_rows}]})"
  )
  cli::cli_text(
    "{.cyan $validation_set} ",
    "({.blue tbl_df [{validation_set_rows}, {validation_set_cols}]})"
  )
  cli::cli_text(
    "{.cyan $lang} ({.red chr [1]})"
  )
  cli::cli_text(
    "{.cyan $report_object} ({.pink gt_tbl})"
  )
  cli::cli_text(
    "{.cyan $email_object} ({.pink blastula_message})"
  )
  cli::cli_text(
    "{.cyan $report_html $report_html_small} ({.red chr [1]})"
  )
  cli::cli_rule(
    right = ifelse(length(x$time) == 0, "NO INTERROGATION PERFORMED", "")
  )
    
  # nocov end 
}

#' Print the `action_levels` object
#'
#' This function will allow the `action_levels` to be nicely printed.
#' 
#' @param x An object of class `action_levels`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.action_levels <- function(x, ...) {
  
  has_warn_fns <- if (!is.null(x$fns$warn)) TRUE else FALSE
  has_stop_fns <- if (!is.null(x$fns$stop)) TRUE else FALSE
  has_notify_fns <- if (!is.null(x$fns$notify)) TRUE else FALSE
  
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
  
  cli::cli_rule(
    left = "The `action_levels` settings",
  )
  
  if (!is.null(x$warn_fraction)) {
    cli::cli_text(
      "{.yellow WARN} failure threshold of {x$warn_fraction} of all test units."
    )
  }
  if (!is.null(x$warn_count)) {
    cli::cli_text(
      "{.yellow WARN} failure threshold of ",
      "{pb_fmt_number(x$warn_count, decimals = 0)} test units."
    )
  }
  if (has_warn_fns) {
    if (is.null(x$warn_fraction) && is.null(x$warn_count)) {
      cli::cli_alert_warning(
        "{.yellow WARN} fns provided without a failure threshold."
      )
      cli::cli_alert_info(
        "Set the {.yellow WARN} threshold using the `warn_at` argument."
      )
      cli::cli_text()
    } else {
      cli::cli_text(
        "{.yellow \\fns\\} {paste(as.character(x$fns$warn), collapse = ' ')}"
      )
    }
  }
  
  if (!is.null(x$stop_fraction)) {
    cli::cli_text(
      "{.red STOP} failure threshold of {x$stop_fraction} of all test units."
    )
  }
  if (!is.null(x$stop_count)) {
    cli::cli_text(
      "{.red STOP} failure threshold of ",
      "{pb_fmt_number(x$stop_count, decimals = 0)} test units."
    )
  }
  if (has_stop_fns) {
    if (is.null(x$stop_fraction) && is.null(x$stop_count)) {
      cli::cli_alert_warning(
        "{.red STOP} fns provided without a failure threshold."
      )
      cli::cli_alert_info(
        "Set the {.red STOP} threshold using the `stop_at` argument."
      )
      cli::cli_text()
    } else {
      cli::cli_text(
        "{.red \\fns\\} {paste(as.character(x$fns$stop), collapse = ' ')}"
      )
    }
  }
  
  if (!is.null(x$notify_fraction)) {
    cli::cli_text(
      "{.blue NOTIFY} failure threshold of {x$notify_fraction} of all test units."
    )
  }
  if (!is.null(x$notify_count)) {
    cli::cli_text(
      "{.blue NOTIFY} failure threshold of ",
      "{pb_fmt_number(x$notify_count, decimals = 0)} test units."
    )
  }
  if (has_notify_fns) {
    if (is.null(x$notify_fraction) && is.null(x$notify_count)) {
      cli::cli_alert_warning(
        "{.blue NOTIFY} fns provided without a failure threshold."
      )
      cli::cli_alert_info(
        "Set the {.blue NOTIFY} threshold using the `notify_at` argument."
      )
      cli::cli_text()
    } else {
      cli::cli_text(
        "{.blue \\fns\\} {paste(as.character(x$fns$notify), collapse = ' ')}"
      )
    }
  }
  
  cli::cli_rule()
  
  # nocov end 
}

# nolint end
