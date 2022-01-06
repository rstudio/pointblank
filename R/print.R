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


# nocov start
# nolint start

#
# ptblank_agent
#

#' Print the `ptblank_agent` object
#'
#' This function will allow the agent object to print a useful HTML-based
#' report.
#' 
#' @param x An object of class `ptblank_agent`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_agent <- function(x, view = interactive(), ...) {

  print(get_agent_report(x), view = view, ...)
}

#' Knit print the `ptblank_agent` object
#'
#' This facilitates printing of the `ptblank_agent` object within a knitr code
#' chunk.
#'
#' @param x An object of class `ptblank_agent`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_agent <- function(x, ...) {
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(get_agent_report(x), ...)
}

#
# ptblank_informant
#

#' Print the `ptblank_informant` object
#'
#' This function will allow the informant object to print a useful HTML-based
#' report.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_informant <- function(x, view = interactive(), ...) {
  
  print(get_informant_report(x), view = view, ...)
}

#' Knit print the `ptblank_informant` object
#'
#' This facilitates printing of the `ptblank_informant` object within a knitr
#' code chunk.
#'
#' @param x An object of class `ptblank_informant`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_informant <- function(x, ...) {
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(get_informant_report(x), ...)
}

#
# ptblank_multiagent
#

#' Print the `ptblank_multiagent` object
#'
#' This function will allow the multiagent object to print a useful HTML-based
#' report.
#' 
#' @param x An object of class `ptblank_multiagent`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_multiagent <- function(x, view = interactive(), ...) {
  
  print(get_multiagent_report(x), view = view, ...)
}

#' Knit print the `ptblank_multiagent` object
#'
#' This facilitates printing of the `ptblank_multiagent` within a knitr code
#' chunk.
#'
#' @param x An object of class `ptblank_multiagent`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_multiagent <- function(x, ...) {
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(get_multiagent_report(x), ...)
}

#
# ptblank_multiagent_report.long
#

#' Print the `ptblank_multiagent_report.long` object
#'
#' This function will print the `ptblank_multiagent_report.long` object, which
#' is an HTML-based report.
#' 
#' @param x An object of class `ptblank_multiagent_report.long`.
#' @param view The value for `print()`s `browse` argument.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.ptblank_multiagent_report.long <- function(x, view = interactive(), ...) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  print(x, browse = view, ...)
}

#' Knit print the `ptblank_multiagent_report.long` object
#'
#' This facilitates printing of the `ptblank_multiagent_report.long` within a
#' knitr code chunk.
#'
#' @param x An object of class `ptblank_multiagent_report.long`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_multiagent_report.long <- function(x, ...) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(x, ...)
}

#
# ptblank_tbl_scan
#

#' Print the `ptblank_tbl_scan` object
#'
#' This function will print the `ptblank_tbl_scan` object, which is an
#' HTML-based report.
#'
#' @param x An object of class `ptblank_tbl_scan`.
#' @param ... Any additional parameters.
#' @param view The value for `print()`s `browse` argument.
#'
#' @keywords internal
#'
#' @export
print.ptblank_tbl_scan <- function(x, ..., view = interactive()) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  print(x, browse = view, ...)
}

#' Knit print the `ptblank_tbl_scan` object
#'
#' This facilitates printing of the `ptblank_tbl_scan` within a knitr code
#' chunk.
#'
#' @param x An object of class `ptblank_tbl_scan`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.ptblank_tbl_scan <- function(x, ...) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(x, ...)
}

#
# x_list_i
#

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
    left = "The x-list for `{x$tbl_name}`",
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
    right = ifelse(length(x$time_start) == 0, "NO INTERROGATION PERFORMED", "")
  )
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
    left = "The x-list for `{x$tbl_name}`",
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
    right = ifelse(length(x$time_start) == 0, "NO INTERROGATION PERFORMED", "")
  )
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

  has_warn_fns <- !is.null(x$fns$warn)
  has_stop_fns <- !is.null(x$fns$stop)
  has_notify_fns <- !is.null(x$fns$notify)
  
  cli::cli_div(
    theme = list(
      span.yellow = list(color = "yellow"),
      span.red = list(color = "red"),
      span.blue = list(color = "blue")
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
}

#' Print the `tbl_store` object
#'
#' This function will allow the `tbl_store` to be nicely printed.
#' 
#' @param x An object of class `tbl_store`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.tbl_store <- function(x, ...) {
  
  tbl_names <- names(x)
  
  n_tbls <- length(tbl_names)
  
  has_given_name <- 
    vapply(
      x,
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      FUN = function(x) inherits(x, "with_tbl_name")
    )
  
  tbl_formulas <-
    vapply(
      x,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) capture_formula(x)[2]
    )

  cli::cli_div(
    theme = list(
      span.yellow = list(color = "yellow"),
      span.red = list(color = "red"),
      span.blue = list(color = "blue")
    )
  )
  
  cli::cli_rule(
    left = "The `table_store` table-prep formulas",
    right = paste0("n = ", n_tbls)
  )
  
  for (i in seq_len(n_tbls)) {
    cli::cli_text(
      paste0(
        "{.yellow {i}} {.blue {tbl_names[i]}}",
        "{.red {ifelse(has_given_name[i], '', '*')}} // {tbl_formulas[i]}"
      )
    )
  }
  
  cli::cli_rule()
}

#' Print the a table-prep formula
#'
#' This function will allow a table-prep formula to be nicely printed.
#' 
#' @param x An object of class `read_fn`.
#' @param ... Any additional parameters.
#' 
#' @keywords internal
#' @export
print.read_fn <- function(x, ...) {
  
  tbl_name <- capture_formula(x)[1]
  tbl_formula <- capture_formula(x)[2]
  
  has_given_name <- inherits(x, "with_tbl_name")
  
  cli::cli_div(
    theme = list(
      span.red = list(color = "red"),
      span.blue = list(color = "blue")
    )
  )
  
  cli::cli_text(
    paste0(
      "{.blue {ifelse(is.na(tbl_name), '', tbl_name)}}",
      "{.red {ifelse(has_given_name, '', '*')}} // {tbl_formula}"
    )
  )
}

# nolint end
# nocov end
