#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


#' Read a **pointblank** YAML file to create an *agent* object
#'
#' @description
#'
#' With `yaml_read_agent()` we can read a **pointblank** YAML file that
#' describes a validation plan to be carried out by an *agent* (typically
#' generated by the [yaml_write()] function. What's returned is a new *agent*
#' with that validation plan, ready to interrogate the target table at will
#' (using the table-prep formula that is set with the `tbl` argument of
#' [create_agent()]). The agent can be given more validation steps if needed
#' before using [interrogate()] or taking part in any other agent ops (e.g.,
#' writing to disk with outputs intact via [x_write_disk()] or again to
#' **pointblank** YAML with [yaml_write()]).
#'
#' To get a picture of how `yaml_read_agent()` is interpreting the validation
#' plan specified in the **pointblank** YAML, we can use the
#' [yaml_agent_show_exprs()] function. That function shows us (in the console)
#' the **pointblank** expressions for generating the described validation plan.
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The name of the YAML file that contains fields related to an *agent*.
#'
#' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to the YAML file (combined with `filename`).
#'
#' @return A `ptblank_agent` object.
#'
#' @section Examples:
#'
#' There's a YAML file available in the **pointblank** package that's also
#' called `"agent-small_table.yml"`. The path for it can be accessed through
#' `system.file()`:
#'
#' ```r
#' yml_file_path <-
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ```
#'
#' The YAML file can be read as an agent with a pre-existing validation plan by
#' using the `yaml_read_agent()` function.
#'
#' ```r
#' agent <- yaml_read_agent(filename = yml_file_path)
#'
#' agent
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_1.png")`
#' }
#' }
#'
#' This particular agent is using `~ tbl_source("small_table", "tbl_store.yml")`
#' to source the table-prep from a YAML file that holds a table store (can be
#' seen using `yaml_agent_string(agent = agent)`). Let's put that file in the
#' working directory (the **pointblank** package has the corresponding YAML
#' file):
#'
#' ```r
#' yml_tbl_store_path <-
#'   system.file(
#'     "yaml", "tbl_store.yml",
#'     package = "pointblank"
#'   )
#'
#' file.copy(from = yml_tbl_store_path, to = ".")
#' ```
#'
#' As can be seen from the validation report, no interrogation was yet
#' performed. Saving an agent to YAML will remove any traces of interrogation
#' data and serve as a plan for a new interrogation on the same target table. We
#' can either follow this up with with [interrogate()] and get an agent with
#' intel, or, we can interrogate directly from the YAML file with
#' [yaml_agent_interrogate()]:
#'
#' ```r
#' agent <- yaml_agent_interrogate(filename = yml_file_path)
#'
#' agent
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_2.png")`
#' }
#' }
#'
#' @family pointblank YAML
#' @section Function ID:
#' 11-2
#'
#' @export
yaml_read_agent <- function(
    filename,
    path = NULL
) {

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  initial_wd <- fs::path_abs(fs::path_wd())
  wd_path <- fs::as_fs_path(dirname(filename))

  if (!fs::dir_exists(wd_path)) {
    stop(
      "The `path` provided (", as.character(wd_path), ") does not exist.",
      call. = FALSE
    )
  }

  if (initial_wd != wd_path) {
    setwd(as.character(wd_path))
    on.exit(setwd(as.character(initial_wd)))
  }

  file_to_read <- basename(filename)

  expr_from_agent_yaml(path = file_to_read, interrogate = FALSE) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}

#' Get an *agent* from **pointblank** YAML and `interrogate()`
#'
#' @description
#'
#' The `yaml_agent_interrogate()` function operates much like the
#' [yaml_read_agent()] function (reading a **pointblank** YAML file and
#' generating an *agent* with a validation plan in place). The key difference is
#' that this function takes things a step further and interrogates the target
#' table (defined by table-prep formula that is required in the YAML file). The
#' additional auto-invocation of [interrogate()] uses the default options of
#' that function. As with [yaml_read_agent()] the agent is returned except, this
#' time, it has intel from the interrogation.
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The name of the YAML file that contains fields related to an *agent*.
#'
#' @param path #' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to the YAML file (combined with `filename`).
#'
#' @return A `ptblank_agent` object.
#'
#' @section Examples:
#'
#' There's a YAML file available in the **pointblank** package that's also
#' called `"agent-small_table.yml"`. The path for it can be accessed through
#' `system.file()`:
#'
#' ```r
#' yml_file_path <-
#'   system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ```
#'
#' The YAML file can be read as an agent with a pre-existing validation plan by
#' using the [yaml_read_agent()] function.
#'
#' ```r
#' agent <- yaml_read_agent(filename = yml_file_path)
#'
#' agent
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_1.png")`
#' }
#' }
#'
#' This particular agent is using `~ tbl_source("small_table", "tbl_store.yml")`
#' to source the table-prep from a YAML file that holds a table store (can be
#' seen using `yaml_agent_string(agent = agent)`). Let's put that file in the
#' working directory (the **pointblank** package has the corresponding YAML
#' file):
#'
#' ```r
#' yml_tbl_store_path <-
#'   system.file(
#'     "yaml", "tbl_store.yml",
#'     package = "pointblank"
#'   )
#'
#' file.copy(from = yml_tbl_store_path, to = ".")
#' ```
#'
#' As can be seen from the validation report, no interrogation was yet
#' performed. Saving an agent to YAML will remove any traces of interrogation
#' data and serve as a plan for a new interrogation on the same target table. We
#' can either follow this up with with [interrogate()] and get an agent with
#' intel, or, we can interrogate directly from the YAML file with
#' `yaml_agent_interrogate()`:
#'
#' ```r
#' agent <- yaml_agent_interrogate(filename = yml_file_path)
#'
#' agent
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_write_2.png")`
#' }
#' }
#'
#' @family pointblank YAML
#' @section Function ID:
#' 11-4
#'
#' @export
yaml_agent_interrogate <- function(
    filename,
    path = NULL
) {

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  expr_from_agent_yaml(path = filename, interrogate = TRUE) %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
}

#' Display validation expressions using **pointblank** YAML
#'
#' @description
#'
#' The `yaml_agent_show_exprs()` function follows the specifications of a
#' **pointblank** YAML file to generate and show the **pointblank** expressions
#' for generating the described validation plan. The expressions are shown in
#' the console, providing an opportunity to copy the statements and extend as
#' needed. A **pointblank** YAML file can itself be generated by using the
#' [yaml_write()] function with a pre-existing *agent*, or, it can be carefully
#' written by hand.
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The name of the YAML file that contains fields related to an *agent*.
#'
#' @param path #' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to the YAML file (combined with `filename`).
#'
#' @section Examples:
#'
#' Let's create a validation plan for the data quality analysis of the
#' `small_table` dataset. We need an agent and its table-prep formula enables
#' retrieval of the target table.
#'
#' ```r
#' agent <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "A simple example with the `small_table`.",
#'     actions = action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'       notify_at = 0.35
#'     )
#'   ) %>%
#'   col_exists(columns = c(date, date_time)) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(columns = d, value = 100) %>%
#'   col_vals_lte(columns = c, value = 5)
#' ```
#'
#' The agent can be written to a **pointblank** YAML file with [yaml_write()].
#'
#' ```r
#' yaml_write(
#'   agent = agent,
#'   filename = "agent-small_table.yml"
#' )
#' ```
#'
#' At a later time, the YAML file can be read into a new agent with the
#' [yaml_read_agent()] function.
#'
#' ```r
#' agent <- yaml_read_agent(filename = "agent-small_table.yml")
#'
#' agent
#' ```
#'
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_yaml_agent_show_exprs_1.png")`
#' }
#' }
#'
#' To get a sense of which expressions are being used to generate the new agent,
#' we can use `yaml_agent_show_exprs()`.
#'
#' ```r
#' yaml_agent_show_exprs(filename = "agent-small_table.yml")
#' ```
#'
#' ```
#' create_agent(
#'   tbl = ~small_table,
#'   actions = action_levels(
#'     warn_at = 0.1,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   ),
#'   tbl_name = "small_table",
#'   label = "A simple example with the `small_table`."
#' ) %>%
#'   col_exists(
#'     columns = c(date, date_time)
#'   ) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(
#'     columns = d,
#'     value = 100
#'   ) %>%
#'   col_vals_lte(
#'     columns = c,
#'     value = 5
#'   )
#' ```
#'
#' @family pointblank YAML
#' @section Function ID:
#' 11-6
#'
#' @export
yaml_agent_show_exprs <- function(
    filename,
    path = NULL
) {

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  message(expr_from_agent_yaml(path = filename, interrogate = FALSE))
}

expr_from_agent_yaml <- function(path, interrogate = FALSE) {

  # Read the YAML file with `yaml::read_yaml()`
  y <- yaml::read_yaml(file = path)

  # Backcompatibility with YAML files that have the deprecated `read_fn` key
  if ("read_fn" %in% names(y)) {

    read_fn_idx <- which(names(y) == "read_fn")
    names(y)[read_fn_idx] <- "tbl"
  }

  # Get the `table_name`, `tbl`, `label`, and `active`
  # fields from the YAML file and create argument strings
  tbl <- paste0("  tbl = ", y$tbl)
  label <- paste0("  label = \"", y$label, "\"")

  # Create argument strings for the `actions` and
  # `end_fns` arguments (which could be NULL)
  actions <- make_action_levels_str(y$actions)
  end_fns <- make_end_fns_str(y$end_fns)

  # The `embed_report` and `lang` values are
  # taken from the YAML (if they exist) and transformed
  # to argument strings
  if (!is.null(y$embed_report) && y$embed_report) {
    embed_report <- paste0("  embed_report = ", y$embed_report)
  } else {
    embed_report <- NULL
  }
  if (!is.null(y$lang) && y$lang != "en") {
    lang <- paste0("  lang = \"", y$lang, "\"")
  } else {
    lang <- NULL
  }

  if (!is.null(y$locale) && y$locale != "en") {
    locale <- paste0("  locale = \"", y$locale, "\"")
  } else {
    locale <- NULL
  }

  if (!is.null(y$tbl_name) && !is.na(y$tbl_name)) {
    tbl_name <- paste0("  tbl_name = \"", y$tbl_name, "\"")
  } else {
    tbl_name <- NULL
  }

  # Generate all of the validation steps that make up
  # the agent's validation plan
  validation_steps <- make_validation_steps(y$steps)

  # Generate the expression string
  expr_str <-
    paste0(
      "create_agent(\n",
      paste(
        c(
          tbl, actions, end_fns,
          tbl_name, label, embed_report,
          lang, locale
        ),
        collapse = ",\n"
      ),
      "\n) ",
      validation_steps
    )

  # Add the `interrogate()` statement if needed (this is
  # for the `yaml_agent_interrogate()` function)
  if (interrogate) {
    expr_str <- paste0(expr_str, "%>%\ninterrogate()")
  }

  expr_str
}

make_action_levels_str <- function(al) {

  if (is.null(al)) {
    return(NULL)
  }

  top_args <- c()

  if (!is.null(al$warn_fraction) || !is.null(al$warn_count)) {
    top_args <-
      c(top_args, paste0("warn_at = ", c(al$warn_fraction, al$warn_count)))
  }
  if (!is.null(al$stop_fraction) || !is.null(al$stop_count)) {
    top_args <-
      c(top_args, paste0("stop_at = ", c(al$stop_fraction, al$stop_count)))
  }
  if (!is.null(al$notify_fraction) || !is.null(al$notify_count)) {
    top_args <-
      c(
        top_args,
        paste0("notify_at = ", c(al$notify_fraction, al$notify_count))
      )
  }

  fns_args <- c()

  if (!is.null(al$fns$warn)) {
    fns_args <- c(fns_args, paste0("warn = ", al$fns$warn))
  }
  if (!is.null(al$fns$stop)) {
    fns_args <- c(fns_args, paste0("stop = ", al$fns$stop))
  }
  if (!is.null(al$fns$notify)) {
    fns_args <- c(fns_args, paste0("notify = ", al$fns$notify))
  }

  if (length(fns_args) > 0) {
    fns_args <-
      paste0("    fns = list(\n", paste0("      ", fns_args), "\n    )")
  }

  paste0(
    "  actions = action_levels(\n",
    paste0(
      c(paste0("    ", top_args, collapse = ",\n"), fns_args),
      collapse = ",\n"
    ),
    "\n  )"
  )
}

make_end_fns_str <- function(end_fns) {

  if (is.null(end_fns)) {
    return(NULL)
  }

  paste0(
    "  end_fns = list(\n",
      c(paste0("    ", end_fns, collapse = ",\n")),
    "\n  )"
  )
}

make_validation_steps <- function(steps) {

  if (length(steps) == 0) return("")

  tidyselect_regex <-
    paste0(
      "^(",
      paste(c("vars", "c", tidyselect_helpers()), collapse = "|"),
      ")\\(.*?\\)$"
    )

  # nolint start

  str_exprs <-
    lapply(
      seq_along(steps),
      FUN = function(x) {

        step_i <- steps[[x]]
        step_fn <- names(step_i)

        args <-
          vapply(
            seq_along(step_i[[1]]),
            FUN.VALUE = character(1),
            FUN = function(x) {

              arg_name <- names(step_i[[1]][x])
              val <- step_i[[1]][[x]]

              others <- c("preconditions", "segments", "expr", "schema")

              if (step_fn %in% c("row_count_match", "col_count_match")) {

                if (arg_name == "count") {
                  return(paste("  count =", val, collapse = ",\n"))
                }
              }

              if (arg_name == "fns") {
                return(paste("  ", val, collapse = ",\n"))
              }

              if (arg_name == "tbl_compare") {
                return(paste("  tbl =", gsub("\n", " ", val), collapse = ",\n"))
              }

              if (arg_name == "inclusive") {
                if (all(val)) {
                  return("")
                } else {
                  return(
                    paste0(
                      "  inclusive = c(",
                      paste(as.character(val), collapse = ", "),
                      ")"
                    )
                  )
                }
              }

              # Return empty string if seeing default values
              if (arg_name == "active") {

                if (is.logical(val)) {
                  if (val) {
                    return("")
                  } else {
                    return(paste("  active = FALSE"))
                  }
                } else {
                  return(paste("  active =", val[1]))
                }
              }

              if (arg_name == "preconditions" && is.null(val)) {
                return("")
              }

              if (arg_name == "na_pass" && !val) {
                return("")
              }

              if (arg_name == "inclusive" && all(val)) {
                return("")
              }

              if (is.null(val[1])) {

                val <- "NULL"

              } else if (!is.null(val[1]) && is.logical(val[1])) {

                val <- as.character(val)

              } else if (!is.null(val[1]) && arg_name == "actions") {

                return(make_action_levels_str(val))

              } else if (!is.null(val[1]) && arg_name == "schema") {

                vals <-
                  vapply(
                    val,
                    FUN.VALUE = character(1),
                    USE.NAMES = FALSE,
                    FUN = function(x) {
                      if (length(x) > 1) {
                        val <-
                          paste0(
                            "c(", paste0(
                              paste0("\"", as.character(x), "\""),
                              collapse = ", "),
                            ")"
                          )
                      } else {
                        val <-
                          paste0("\"", as.character(x), "\"")
                      }
                      val
                    }
                  )

                val <-
                  paste0("  schema = col_schema(\n",
                    paste("   ", names(val), "=", vals, collapse = ",\n"),
                    "\n  )"
                  )

                return(val)

              } else if (!is.null(val[1]) &&
                         is.character(val[1]) &&
                         !grepl(tidyselect_regex, val[1]) &&
                         !(arg_name %in% others)) {

                val <-
                  vapply(
                    val,
                    FUN.VALUE = character(1),
                    USE.NAMES = FALSE,
                    FUN = function(x) {
                      if (is.na(x)) {
                        return(x)
                      } else {
                        paste0("\"", x, "\"")
                      }
                    }
                  )

                #val <- paste0("\"", val, "\"")
              }

              if (length(val) > 1) {
                val <-
                  paste0("c(", paste(as.character(val), collapse = ", "), ")")
              } else {
                val <- as.character(val)
              }

              paste(" ", arg_name[1], "=", val[1])
            }
          )

        args <- args[args != ""]

        args %>%
          paste(collapse = ",\n") %>%
          paste0("%>%\n", step_fn, "(\n", ., "\n)")
      }
    ) %>%
    unlist() %>%
    paste(collapse = " ") %>%
    paste0(., " ")

  # nolint end

  str_exprs <-
    gsub(
      "rows_distinct(\n  columns = NULL\n)", "rows_distinct()",
      str_exprs,
      fixed = TRUE
    )

  str_exprs <-
    gsub(
      "rows_complete(\n  columns = NULL\n)", "rows_complete()",
      str_exprs,
      fixed = TRUE
    )

  str_exprs
}

agent_get_exprs <- function(agent, expanded) {

  temp_dir <- tempdir()

  yaml_name <- paste0(gt::random_id(), ".yaml")

  yaml_write(
    agent = agent,
    filename = yaml_name,
    path = temp_dir,
    expanded = expanded,
    quiet = TRUE
  )

  expr_from_agent_yaml(path = file.path(temp_dir, yaml_name))
}
