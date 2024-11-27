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
#  Copyright (c) 2017-2024 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


#' Write an *agent*, *informant*, *multiagent*, or table scan to disk
#'
#' @description
#'
#' Writing an *agent*, *informant*, *multiagent*, or even a table scan to disk
#' with `x_write_disk()` can be useful for keeping data validation intel or
#' table information close at hand for later retrieval (with [x_read_disk()]).
#' By default, any data table that the *agent* or *informant* may have held
#' before being committed to disk will be expunged (not applicable to any table
#' scan since they never hold a table object). This behavior can be changed by
#' setting `keep_tbl` to `TRUE` but this only works in the case where the table
#' is not of the `tbl_dbi` or the `tbl_spark` class.
#'
#' @details
#'
#' It is recommended to set up a table-prep formula so that the *agent* and
#' *informant* can access refreshed data after being read from disk through
#' [x_read_disk()]. This can be done initially with the `tbl` argument of
#' [create_agent()]/[create_informant()] by passing in a table-prep formula or a
#' function that can obtain the target table when invoked. Alternatively, we can
#' use the [set_tbl()] with a similarly crafted `tbl` expression to ensure that
#' an *agent* or *informant* can retrieve a table at a later time.
#'
#' @param x *One of several types of objects*
#'
#'   `<object>` // **required**
#'
#'   An *agent* object of class `ptblank_agent`, an *informant* of class
#'   `ptblank_informant`, or an table scan of class `ptblank_tbl_scan`.
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The filename to create on disk for the `agent`, `informant`, or table scan.
#'
#' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to which the file should be saved (this is automatically
#'   combined with `filename`).
#'
#' @param keep_tbl *Keep data table inside object*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   An option to keep a data table that is associated with the
#'   *agent* or *informant* (which is the case when the *agent*, for example, is
#'   created using `create_agent(tbl = <data table, ...)`). The default is
#'   `FALSE` where the data table is removed before writing to disk. For
#'   database tables of the class `tbl_dbi` and for Spark DataFrames
#'   (`tbl_spark`) the table is always removed (even if `keep_tbl` is set to
#'   `TRUE`).
#'
#' @param keep_extracts *Keep data extracts inside object*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   An option to keep any collected extract data for failing rows. Only applies
#'   to *agent* objects. By default, this is `FALSE` (i.e., extract data is
#'   removed).
#'
#' @param quiet *Inform (or not) upon file writing*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   Should the function *not* inform when the file is written?
#'
#' @return Invisibly returns `TRUE` if the file has been written.
#'
#' @section Examples:
#'
#' ## A: Writing an `agent` to disk
#'
#' Let's go through the process of (1) developing an agent with a validation
#' plan (to be used for the data quality analysis of the [`small_table`]
#' dataset), (2) interrogating the agent with the [interrogate()] function, and
#' (3) writing the agent and all its intel to a file.
#'
#' Creating an `action_levels` object is a common workflow step when creating a
#' pointblank agent. We designate failure thresholds to the `warn`, `stop`, and
#' `notify` states using [action_levels()].
#'
#' ```r
#' al <-
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' ```
#'
#' Now, let's create a pointblank `agent` object and give it the `al` object
#' (which serves as a default for all validation steps which can be overridden).
#' The data will be referenced in the `tbl` argument with a leading `~`.
#'
#' ```r
#' agent <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`",
#'     actions = al
#'   )
#' ```
#'
#' Then, as with any `agent` object, we can add steps to the validation plan by
#' using as many validation functions as we want. After that, use
#' [interrogate()].
#'
#' ```r
#' agent <-
#'   agent %>%
#'   col_exists(columns = c(date, date_time)) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(columns = d, value = 100) %>%
#'   col_vals_lte(columns = c, value = 5) %>%
#'   interrogate()
#' ```
#'
#' The `agent` can be written to a file with the `x_write_disk()` function.
#'
#' ```r
#' x_write_disk(
#'   agent,
#'   filename = "agent-small_table.rds"
#' )
#' ```
#'
#' We can read the file back as an agent with the [x_read_disk()] function and
#' we'll get all of the intel along with the restored agent.
#'
#' If you're consistently writing agent reports when periodically checking data,
#' we could make use of the [affix_date()] or [affix_datetime()] depending on
#' the granularity you need. Here's an example that writes the file with the
#' format: `"<filename>-YYYY-mm-dd_HH-MM-SS.rds"`.
#'
#' ```r
#' x_write_disk(
#'   agent,
#'   filename = affix_datetime(
#'     "agent-small_table.rds"
#'   )
#' )
#' ```
#'
#' ## B: Writing an `informant` to disk
#'
#' Let's go through the process of (1) creating an informant object that
#' minimally describes the [`small_table`] dataset, (2) ensuring that data is
#' captured from the target table using the [incorporate()] function, and (3)
#' writing the informant to a file.
#'
#' Create a pointblank `informant` object with [create_informant()] and the
#' [`small_table`] dataset. Use [incorporate()] so that info snippets are
#' integrated into the text.
#'
#' ```r
#' informant <-
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "high_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "low_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = a,
#'     info = "From {low_a} to {high_a}."
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values."
#'   ) %>%
#'   info_columns(
#'     columns = date,
#'     info = "The date part of `date_time`."
#'   ) %>%
#'   incorporate()
#' ```
#'
#' The `informant` can be written to a file with `x_write_disk()`. Let's do this
#' with [affix_date()] so that the filename has a datestamp.
#'
#' ```r
#' x_write_disk(
#'   informant,
#'   filename = affix_date(
#'     "informant-small_table.rds"
#'   )
#' )
#' ```
#'
#' We can read the file back into a new informant object (in the same state as
#' when it was saved) by using [x_read_disk()].
#'
#' ## C: Writing a multiagent to disk
#'
#' Let's create one more pointblank agent object, provide it with some
#' validation steps, and [interrogate()].
#'
#' ```r
#' agent_b <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`x_write_disk()`",
#'     actions = al
#'   ) %>%
#'   col_vals_gt(
#'     columns = b,
#'     value = g,
#'     na_pass = TRUE,
#'     label = "b > g"
#'   ) %>%
#'   col_is_character(
#'     columns = c(b, f),
#'     label = "Verifying character-type columns"
#'   ) %>%
#'   interrogate()
#' ```
#'
#' Now we can combine the earlier `agent` object with the newer `agent_b` to
#' create a `multiagent`.
#'
#' ```r
#' multiagent <- create_multiagent(agent, agent_b)
#' ```
#'
#' The `multiagent` can be written to a file with the `x_write_disk()` function.
#'
#' ```r
#' x_write_disk(
#'   multiagent,
#'   filename = "multiagent-small_table.rds"
#' )
#' ```
#'
#' We can read the file back as a multiagent with the [x_read_disk()] function
#' and we'll get all of the constituent agents and their associated intel back
#' as well.
#'
#' ## D: Writing a table scan to disk
#'
#' We can get a report that describes all of the data in the `storms` dataset.
#'
#' ```r
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' ```
#'
#' The table scan object can be written to a file with `x_write_disk()`.
#'
#' ```r
#' x_write_disk(
#'   tbl_scan,
#'   filename = "tbl_scan-storms.rds"
#' )
#' ```
#'
#' @family Object Ops
#' @section Function ID:
#' 9-1
#'
#' @export
x_write_disk <- function(
    x,
    filename,
    path = NULL,
    keep_tbl = FALSE,
    keep_extracts = FALSE,
    quiet = FALSE
) {

  if (
    !any(
      inherits(x, "ptblank_agent") |
      inherits(x, "ptblank_informant") |
      inherits(x, "ptblank_multiagent") |
      inherits(x, "ptblank_tbl_scan")
      )
    ) {
    stop(
      "The object provided isn't one of the four types that can be saved:\n",
      "* the `agent` (`ptblank_agent`)\n",
      "* the `informant()` (`ptblank_informant`)\n",
      "* the `multiagent()` (`ptblank_multiagent`)\n",
      "* a table scan (`ptblank_tbl_scan`)",
      call. = FALSE
    )
  }

  if (inherits(x, "ptblank_agent")) {

    x$validation_set$tbl_checked <- NULL

    x$validation_set <-
      x$validation_set %>%
      dplyr::mutate(tbl_checked = list(NULL))

    if (keep_tbl) {

      if (inherits(x$tbl, "tbl_dbi") || inherits(x$tbl, "tbl_spark")) {

        warning(
          "A table of class `tbl_dbi` or `tbl_spark` cannot be ",
          "kept with the object.",
          call. = FALSE
        )

        x <- remove_tbl(x)
      }

    } else if (!keep_tbl) {

      x <- remove_tbl(x)
    }

    if (!keep_extracts) {
      x$extracts <- list()
    }

    object_type <- "agent"

  } else if (inherits(x, "ptblank_informant")) {

    object_type <- "informant"

  } else if (inherits(x, "ptblank_multiagent")) {

    object_type <- "multiagent"

  } else {

    object_type <- "table scan"
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  filename <- as.character(fs::path_norm(fs::path_expand(filename)))

  # Write the object to disk
  saveRDS(x, file = filename)

  # Generate cli message w.r.t. written RDS file
  if (!quiet) {
    cli_bullet_msg(
      msg = "The {object_type} has been written as `{filename}`",
      bullet = cli::symbol$tick,
      color = "green"
    )
  }

  invisible(TRUE)
}

#' Read an *agent*, *informant*, *multiagent*, or table scan from disk
#'
#' @description
#'
#' An *agent*, *informant*, *multiagent*, or table scan that has been written to
#' disk (with [x_write_disk()]) can be read back into memory with the
#' `x_read_disk()` function. For an *agent* or an *informant* object that has
#' been generated in this way, it may not have a data table associated with it
#' (depending on whether the `keep_tbl` option was `TRUE` or `FALSE` when
#' writing to disk) but it should still be able to produce reporting (by
#' printing the *agent* or *informant* to the console or using
#' [get_agent_report()]/[get_informant_report()]). An *agent* will return an
#' x-list with [get_agent_x_list()] and yield any available data extracts with
#' [get_data_extracts()]. Furthermore, all of an *agent*'s validation steps will
#' still be present (along with results from the last interrogation).
#'
#' @details
#'
#' Should a written-to-disk *agent* or *informant* possess a table-prep formula
#' or a specific in-memory tablewe could use the [interrogate()] or
#' [incorporate()] function again. For a *data quality reporting* workflow, it
#' is useful to [interrogate()] target tables that evolve over time. While the
#' same validation steps will be used, more can be added before calling
#' [interrogate()]. For an *information management* workflow with an *informant*
#' object, using [incorporate()] will update aspects of the reporting such as
#' table dimensions, and info snippets/text will be regenerated.
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The name of a file that was previously written by [x_write_disk()].
#'
#' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to the file (combined with `filename`).
#'
#' @param quiet *Inform (or not) upon file writing*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   Should the function *not* inform when the file is written?
#'
#' @return Either a `ptblank_agent`, `ptblank_informant`, or a
#'   `ptblank_tbl_scan` object.
#'
#' @section Examples:
#'
#' ## A: Reading an agent from disk
#'
#' The process of developing an agent and writing it to disk with the
#' [x_write_disk()] function is explained in that function's documentation.
#' Suppose we have such a written file that's named `"agent-small_table.rds"`,
#' we could read that to a new agent object with `x_read_disk()`.
#'
#' ```r
#' agent <- x_read_disk("agent-small_table.rds")
#' ```
#'
#' ## B: Reading an informant from disk
#'
#' If there is an informant written to disk via [x_write_disk()] and it's named
#' `"informant-small_table.rds"`. We could read that to a new informant object
#' with `x_read_disk()`.
#'
#' ```r
#' informant <- x_read_disk("informant-small_table.rds")
#' ```
#'
#' ## C: Reading a multiagent from disk
#'
#' The process of creating a multiagent and writing it to disk with the
#' [x_write_disk()] function is shown in that function's documentation. Should
#' we have such a written file called `"multiagent-small_table.rds"`, we could
#' read that to a new multiagent object with `x_read_disk()`.
#'
#' ```r
#' multiagent <- x_read_disk("multiagent-small_table.rds")
#' ```
#'
#' ## D: Reading a table scan from disk
#'
#' If there is a table scan written to disk via [x_write_disk()] and it's named
#' `"tbl_scan-storms.rds"`, we could read it back into R with `x_read_disk()`.
#'
#' ```r
#' tbl_scan <- x_read_disk("tbl_scan-storms.rds")
#' ```
#'
#' @family Object Ops
#' @section Function ID:
#' 9-2
#'
#' @export
x_read_disk <- function(
    filename,
    path = NULL,
    quiet = FALSE
) {

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  filename <- as.character(fs::path_norm(fs::path_expand(filename)))

  x <- readRDS(filename)

  if (quiet) {
    return(x)
  }

  if (inherits(x, "ptblank_agent")) {
    object_type <- "agent"
  } else if (inherits(x, "ptblank_informant")) {
    object_type <- "informant"
  } else if (inherits(x, "ptblank_multiagent")) {
    object_type <- "multiagent"
  } else if (inherits(x, "ptblank_tbl_scan")) {
    object_type <- "table scan"
  } else {
    object_type <- "object"
  }

  # Generate cli message w.r.t. read in RDS file
  if (!quiet) {
    cli_bullet_msg(
      msg = "The {object_type} has been read from `{filename}`",
      bullet = cli::symbol$tick,
      color = "green"
    )
  }

  x
}

#' Export an *agent*, *informant*, *multiagent*, or table scan to HTML
#'
#' @description
#'
#' The *agent*, *informant*, *multiagent*, and the table scan object can be
#' easily written as HTML with `export_report()`. Furthermore, any report
#' objects from the *agent*, *informant*, and *multiagent* (generated using
#' [get_agent_report()], [get_informant_report()], and
#' [get_multiagent_report()]) can be provided here for HTML export. Each HTML
#' document written to disk is self-contained and easily viewable in a web
#' browser.
#'
#' @param x *One of several types of objects*
#'
#'   `<object>` // **required**
#'
#'   An *agent* object of class `ptblank_agent`, an *informant* of class
#'   `ptblank_informant`, a *multiagent* of class `ptblank_multiagent`, a table
#'   scan of class `ptblank_tbl_scan`, or, customized reporting objects
#'   (`ptblank_agent_report`, `ptblank_informant_report`,
#'   `ptblank_multiagent_report.wide`, `ptblank_multiagent_report.long`).
#'
#' @param filename *File name*
#'
#'   `scalar<character>` // **required**
#'
#'   The filename to create on disk for the HTML export of the object provided.
#'   It's recommended that the extension `".html"` is included.
#'
#' @param path *File path*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional path to which the file should be saved (this is automatically
#'   combined with `filename`).
#'
#' @param quiet *Inform (or not) upon file writing*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   Should the function *not* inform when the file is written?
#'
#' @return Invisibly returns `TRUE` if the file has been written.
#'
#' @section Examples:
#'
#' ## A: Writing an agent report as HTML
#'
#' Let's go through the process of (1) developing an agent with a validation
#' plan (to be used for the data quality analysis of the [`small_table`]
#' dataset), (2) interrogating the agent with the [interrogate()] function, and
#' (3) writing the agent and all its intel to a file.
#'
#' Creating an `action_levels` object is a common workflow step when creating a
#' pointblank agent. We designate failure thresholds to the `warn`, `stop`, and
#' `notify` states using [action_levels()].
#'
#' ```r
#' al <-
#'   action_levels(
#'     warn_at = 0.10,
#'     stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' ```
#'
#' Now create a pointblank `agent` object and give it the `al` object (which
#' serves as a default for all validation steps which can be overridden). The
#' data will be referenced in the `tbl` argument with a leading `~`.
#'
#' ```r
#' agent <-
#'   create_agent(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`export_report()`",
#'     actions = al
#'   )
#' ```
#'
#' As with any agent object, we can add steps to the validation plan by using as
#' many validation functions as we want. Then, we [interrogate()].
#'
#' ```r
#' agent <-
#'   agent %>%
#'   col_exists(columns = c(date, date_time)) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   col_vals_gt(columns = d, value = 100) %>%
#'   col_vals_lte(columns = c, value = 5) %>%
#'   interrogate()
#' ```
#'
#' The agent report can be written to an HTML file with `export_report()`.
#'
#' ```r
#' export_report(
#'   agent,
#'   filename = "agent-small_table.html"
#' )
#' ```
#'
#' If you're consistently writing agent reports when periodically checking data,
#' we could make use of `affix_date()` or `affix_datetime()` depending on the
#' granularity you need. Here's an example that writes the file with the format:
#' `"<filename>-YYYY-mm-dd_HH-MM-SS.html"`.
#'
#' ```r
#' export_report(
#'   agent,
#'   filename = affix_datetime(
#'     "agent-small_table.html"
#'   )
#' )
#' ```
#'
#' ## B: Writing an informant report as HTML
#'
#' Let's go through the process of (1) creating an informant object that
#' minimally describes the [`small_table`] dataset, (2) ensuring that data is
#' captured from the target table using the [incorporate()] function, and (3)
#' writing the informant report to HTML.
#'
#' Create a pointblank `informant` object with [create_informant()] and the
#' [`small_table`] dataset. Use [incorporate()] so that info snippets are
#' integrated into the text.
#'
#' ```r
#' informant <-
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "`export_report()`"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "high_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "low_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = a,
#'     info = "From {low_a} to {high_a}."
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values."
#'   ) %>%
#'   info_columns(
#'     columns = date,
#'     info = "The date part of `date_time`."
#'   ) %>%
#'   incorporate()
#' ```
#'
#' The informant report can be written to an HTML file with `export_report()`.
#' Let's do this with [affix_date()] so the filename has a datestamp.
#'
#' ```r
#' export_report(
#'   informant,
#'   filename = affix_date(
#'     "informant-small_table.html"
#'   )
#' )
#' ```
#'
#' ## C: Writing a table scan as HTML
#'
#' We can get a report that describes all of the data in the `storms` dataset.
#'
#' ```r
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' ```
#'
#' The table scan object can be written to an HTML file with `export_report()`.
#'
#' ```r
#' export_report(
#'   tbl_scan,
#'   filename = "tbl_scan-storms.html"
#' )
#' ```
#'
#' @family Object Ops
#' @section Function ID:
#' 9-3
#'
#' @export
export_report <- function(
    x,
    filename,
    path = NULL,
    quiet = FALSE
) {

  if (
    !any(
      inherits(x, "ptblank_agent") |
      inherits(x, "ptblank_informant") |
      inherits(x, "ptblank_multiagent") |
      inherits(x, "ptblank_tbl_scan") |
      inherits(x, "ptblank_agent_report") |
      inherits(x, "ptblank_informant_report") |
      inherits(x, "ptblank_multiagent_report.wide") |
      inherits(x, "ptblank_multiagent_report.long")
    )
  ) {
    stop(
      "The object provided isn't one of the four types that can be saved:\n",
      "* the `agent` (`ptblank_agent`)\n",
      "* the `informant()` (`ptblank_informant`)\n",
      "* the `multiagent()` (`ptblank_multiagent`)\n",
      "* a table scan (`ptblank_tbl_scan`)",
      call. = FALSE
    )
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  filename <- as.character(fs::path_norm(fs::path_expand(filename)))

  if (inherits(x, "ptblank_agent")) {

    object_type <- "agent"

    x %>%
      get_agent_report() %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_informant")) {

    object_type <- "informant"

    x %>%
      get_informant_report() %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_tbl_scan")) {

    object_type <- "table scan"

    x %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_agent_report")) {

    object_type <- "agent report"

    x %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_informant_report")) {

    object_type <- "informant report"

    x %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_multiagent_report.wide")) {

    object_type <- "multiagent report (wide)"

    x %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)

  } else if (inherits(x, "ptblank_multiagent_report.long")) {

    object_type <- "multiagent report (long)"

    x %>%
      htmltools::as.tags() %>%
      htmltools::save_html(file = filename)
  }

  # Generate cli message w.r.t. written HTML file
  if (!quiet) {
    cli_bullet_msg(
      msg = "The {object_type} has been written as `{filename}`",
      bullet = cli::symbol$tick,
      color = "green"
    )
  }

  invisible(TRUE)
}


#' Set a data table to an *agent* or an *informant*
#'
#' @description
#'
#' Setting a data table to an *agent* or an *informant* with `set_tbl()`
#' replaces any associated table (a data frame, a tibble, objects of class
#' `tbl_dbi` or `tbl_spark`).
#'
#' @param x *A pointblank agent or informant object*
#'
#'   `obj:<ptblank_agent|ptblank_informant>` // **required**
#'
#'   An *agent* object of class `ptblank_agent`, or, an *informant* of class
#'   `ptblank_informant`.
#'
#' @param tbl *Table or expression for reading in one*
#'
#'   `obj:<tbl_*>|<tbl reading expression>` // **required**
#'
#'   The input table for the *agent* or the *informant*. This can be a data
#'   frame, a tibble, a `tbl_dbi` object, or a `tbl_spark` object.
#'   Alternatively, an expression can be supplied to serve as instructions on
#'   how to retrieve the target table at interrogation- or incorporation-time.
#'   There are two ways to specify an association to a target table: (1) as a
#'   table-prep formula, which is a right-hand side (RHS) formula expression
#'   (e.g., `~ { <tbl reading code>}`), or (2) as a function (e.g.,
#'   `function() { <tbl reading code>}`).
#'
#' @param tbl_name *A table name*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   A optional name to assign to the new input table object. If no value is
#'   provided, a name will be generated based on whatever information is
#'   available.
#'
#' @param label *An optional label for reporting*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional label for the validation plan or information report. If no
#'   value is provided then any existing label will be retained.
#'
#' @section Examples:
#'
#' Set proportional failure thresholds to the `warn`, `stop`, and `notify`
#' states using [action_levels()].
#'
#' ```r
#' al <-
#'   action_levels(
#'       warn_at = 0.10,
#'       stop_at = 0.25,
#'     notify_at = 0.35
#'   )
#' ```
#'
#' Create an agent that has `small_table` set as the target table via `tbl`.
#' Apply the actions, add some validation steps and then interrogate the data.
#'
#' ```r
#' agent_1 <-
#'   create_agent(
#'     tbl = small_table,
#'     tbl_name = "small_table",
#'     label = "An example.",
#'     actions = al
#'   ) %>%
#'   col_exists(columns = c(date, date_time)) %>%
#'   col_vals_regex(
#'     columns = b,
#'     regex = "[0-9]-[a-z]{3}-[0-9]{3}"
#'   ) %>%
#'   rows_distinct() %>%
#'   interrogate()
#' ```
#'
#' Replace the agent's association to `small_table` with a mutated version of it
#' (one that removes duplicate rows). Then, interrogate the new target table.
#'
#' ```r
#' agent_2 <-
#'   agent_1 %>%
#'   set_tbl(
#'     tbl = small_table %>% dplyr::distinct()
#'   ) %>%
#'   interrogate()
#' ```
#'
#' @family Object Ops
#' @section Function ID:
#' 9-4
#'
#' @export
set_tbl <- function(
    x,
    tbl,
    tbl_name = NULL,
    label = NULL
) {

  tbl_list <- process_table_input(tbl = tbl, tbl_name = tbl_name)

  if (is_ptblank_informant(x)) {

    x$tbl <- tbl_list$tbl
    x$read_fn <- tbl_list$read_fn

    if (is.null(tbl_name)) {
      if (is.null(x$tbl_name)) {
        x$tbl_name <- tbl_list$tbl_name
        x$metadata$table$name <- tbl_list$tbl_name
      }
    } else {
      x$tbl_name <- tbl_name
      x$metadata$table$name <- tbl_name
    }

    if (!is.null(label)) {
      x$info_label <- label
    }

    tbl_material <- materialize_table(tbl = tbl)

    # Obtain basic information on the table and
    # set the relevant list elements
    tbl_info <- get_tbl_information(tbl = tbl_material)

    # nolint start

    table.type <- tbl_info$tbl_src
    table.columns <- get_table_total_columns(data = tbl_material)

    if (inherits(tbl_material, "ArrowObject")) {
      table.rows <- nrow(tbl_material)
    } else {
      table.rows <- get_table_total_rows(data = tbl_material)
    }

    x$metadata$table$`_columns` <- table.columns
    x$metadata$table$`_rows` <- table.rows
    x$metadata$table$`_type` <- table.type

    # nolint end

    # Use incorporate to force an revision of metadata
    x <- incorporate(informant = x)
  }

  if (is_ptblank_agent(x)) {

    x$tbl <- tbl_list$tbl
    x$read_fn <- tbl_list$read_fn

    if (!is.null(tbl_name)) {
      x$tbl_name <- tbl_name
    }

    if (!is.null(label)) {
      x$label <- label
    }

    # Obtain basic information on the table and
    # set the relevant list elements
    tbl_info <- get_tbl_information(tbl = materialize_table(tbl = tbl))

    x$db_tbl_name <- tbl_info$db_tbl_name
    x$tbl_src <- tbl_info$tbl_src
    x$tbl_src_details <- tbl_info$tbl_src_details
    x$col_names <- tbl_info$col_names
    x$col_types <- tbl_info$r_col_types
    x$db_col_types <- tbl_info$db_col_types

    # Remove any data extracts
    x$extracts <- list()
  }

  invisible(x)
}

# Remove a data table associated with an *agent* or an *informant*
remove_tbl <- function(x) {

  x$tbl <- NULL
  x$read_fn <- NULL
  invisible(x)
}
