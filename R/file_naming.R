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


#' Put the current date into a file name
#'
#' @description
#'
#' This function helps to affix the current date to a filename. This is useful
#' when writing *agent* and/or *informant* objects to disk as part of a
#' continuous process. The date can be in terms of UTC time or the local system
#' time. The date can be affixed either to the end of the filename (before the
#' file extension) or at the beginning with a customizable delimiter.
#'
#' The [x_write_disk()], [yaml_write()] functions allow for the writing of
#' **pointblank** objects to disk. Furthermore the [log4r_step()] function has
#' the `append_to` argument that accepts filenames, and, it's reasonable that a
#' series of log files could be differentiated by a date component in the naming
#' scheme. The modification of the filename string takes effect immediately but
#' not at the time of writing a file to disk. In most cases, especially when
#' using `affix_date()` with the aforementioned file-writing functions, the file
#' timestamps should approximate the time components affixed to the filenames.
#'
#' @param filename The filename to modify.
#'
#' @param position Where to place the formatted date. This could either be at
#'   the `"end"` of the filename (the default) or at the `"start"`.
#'
#' @param format A [base::strptime()] format string for formatting the date. By
#'   default, this is `"%Y-%m-%d"` which expresses the date according to the ISO
#'   8601 standard (as `YYYY-MM-DD`). Refer to the documentation on
#'   [base::strptime()] for conversion specifications if planning to use a
#'   different format string.
#'
#' @param delimiter The delimiter characters to use for separating the date
#'   string from the original file name.
#'
#' @param utc_time An option for whether to use the current UTC time to
#'   establish the date (the default, with `TRUE`), or, use the system's local
#'   time (`FALSE`).
#'
#' @return A character vector.
#'
#' @section Examples:
#'
#' ## The basics of creating a filename with the current date
#'
#' Taking the generic `"pb_file"` name for a file, we add the current date to it
#' as a suffix.
#'
#' ```r
#' affix_date(filename = "pb_file")
#' ```
#' ```
#' ## [1] "pb_file_2022-04-01"
#' ```
#'
#' File extensions won't get in the way:
#'
#' ```r
#' affix_date(filename = "pb_file.rds")
#' ```
#' ```
#' ## [1] "pb_file_2022-04-01.rds"
#' ```
#'
#' The date can be used as a prefix.
#'
#' ```r
#' affix_date(
#'   filename = "pb_file",
#'   position = "start"
#' )
#' ```
#' ```
#' ## [1] "2022-04-01_pb_file"
#' ```
#'
#' The date pattern can be changed and so can the delimiter.
#'
#' ```r
#' affix_date(
#'   filename = "pb_file.yml",
#'   format = "%Y%m%d",
#'   delimiter = "-"
#' )
#' ```
#' ```
#' ## [1] "pb_file-20220401.yml"
#' ```
#'
#' ## Using a date-based filename in a **pointblank** workflow
#'
#' We can use a file-naming convention involving dates when writing output files
#' immediately after interrogating. This is just one example (any workflow
#' involving a `filename` argument is applicable). It's really advantageous to
#' use date-based filenames when interrogating directly from YAML in a scheduled
#' process.
#'
#' ```r
#' yaml_agent_interrogate(
#'   filename = system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ) %>%
#'   x_write_disk(
#'     filename = affix_date(
#'       filename = "small_table_agent.rds",
#'       delimiter = "-"
#'     ),
#'     keep_tbl = TRUE,
#'     keep_extracts = TRUE
#'   )
#' ```
#'
#' In the above, we used the written-to-disk agent (The
#' `"agent-small_table.yml"` YAML file) for an interrogation via
#' [yaml_agent_interrogate()]. Then, the results were written to disk as an RDS
#' file. In the `filename` argument of [x_write_disk()], the [affix_date()]
#' function was used to ensure that a daily run would produce a file whose name
#' indicates the day of execution.
#'
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-3
#'
#' @seealso The [affix_datetime()] function provides the same features except it
#'   produces a datetime string by default.
#'
#' @export
affix_date <- function(
    filename,
    position = c("end", "start"),
    format = "%Y-%m-%d",
    delimiter = "_",
    utc_time = TRUE
) {

  position <- match.arg(position)

  affix_time_to_filename(
    filename = filename,
    position = position,
    format = format,
    delimiter = delimiter,
    utc_time = utc_time,
    add_tz = FALSE
  )
}

#' Put the current datetime into a file name
#'
#' @description
#'
#' This function helps to affix the current datetime to a filename. This is
#' useful when writing *agent* and/or *informant* objects to disk as part of a
#' continuous process. The datetime string can be based on the current UTC time
#' or the local system time. The datetime can be affixed either to the end of
#' the filename (before the file extension) or at the beginning with a
#' customizable delimiter. Optionally, the time zone information can be
#' included. If the datetime is based on the local system time, the user system
#' time zone is shown with the format `<time>(+/-)hhmm`. If using UTC time, then
#' the `<time>Z` format is adopted.
#'
#' The [x_write_disk()], [yaml_write()] functions allow for the writing of
#' **pointblank** objects to disk. The modification of the filename string takes
#' effect immediately but not at the time of writing a file to disk. In most
#' cases, especially when using `affix_datetime()` with the aforementioned
#' file-writing functions, the file timestamps should approximate the time
#' components affixed to the filenames.
#'
#' @inheritParams affix_date
#'
#' @param position Where to place the formatted datetime. This could either be
#'   at the `"end"` of the filename (the default) or at the `"start"`.
#'
#' @param format A [base::strptime()] format string for formatting the
#'   datetime. By default, this is `"%Y-%m-%dT%H:%M:%S"` which expresses the
#'   date according to the ISO 8601 standard. For example, if the current
#'   datetime is `2020-12-04 13:11:23`, the formatted string would become
#'   `"2020-12-04T13:11:23"`. Refer to the documentation on [base::strptime()]
#'   for conversion specifications if planning to use a different format string.
#'
#' @param delimiter The delimiter characters to use for separating the datetime
#'   string from the original file name.
#'
#' @param utc_time An option for whether to use the current UTC time to
#'   establish the datetime (the default, with `TRUE`), or, use the system's
#'   local time (`FALSE`).
#'
#' @param add_tz Should the time zone (as an offset from UTC) be provided? If
#'   `TRUE` then the UTC offset will be either provided as `<time>Z` (if
#'   `utc_time = TRUE`) or `<time>(+/-)hhmm`. By default, this is `FALSE`.
#'
#' @return A character vector.
#'
#' @section Examples:
#'
#' ## The basics of creating a filename with the current date and time
#'
#' Taking the generic `"pb_file"` name for a file, we add the current datetime
#' to it as a suffix.
#'
#' ```r
#' affix_datetime(filename = "pb_file")
#' ```
#' ```
#' ## [1] "pb_file_2022-04-01_00-32-53"
#' ```
#'
#' File extensions won't get in the way:
#'
#' ```r
#' affix_datetime(filename = "pb_file.rds")
#' ```
#' ```
#' ## [1] "pb_file_2022-04-01_00-32-53.rds"
#' ```
#'
#' The datetime can be used as a prefix.
#'
#' ```r
#' affix_datetime(
#'   filename = "pb_file",
#'   position = "start"
#' )
#' ```
#' ```
#' ## [1] "2022-04-01_00-32-53_pb_file"
#' ```
#'
#' The datetime pattern can be changed and so can the delimiter.
#'
#' ```r
#' affix_datetime(
#'   filename = "pb_file.yml",
#'   format = "%Y%m%d_%H%M%S",
#'   delimiter = "-"
#' )
#' ```
#' ```
#' ## [1] "pb_file-20220401_003253.yml"
#' ```
#'
#' Time zone information can be included. By default, all datetimes are given in
#' the UTC time zone.
#'
#' ```r
#' affix_datetime(
#'   filename = "pb_file.yml",
#'   add_tz = TRUE
#' )
#' ```
#' ```
#' ## [1] "pb_file_2022-04-01_00-32-53Z.yml"
#' ```
#'
#' We can use the system's local time zone with `utc_time = FALSE`.
#'
#' ```r
#' affix_datetime(
#'   filename = "pb_file.yml",
#'   utc_time = FALSE,
#'   add_tz = TRUE
#' )
#' ```
#' ```
#' ## [1] "pb_file_2022-03-31_20-32-53-0400.yml"
#' ```
#'
#' ## Using a datetime-based filename in a **pointblank** workflow
#'
#' We can use a file-naming convention involving datetimes when writing output
#' files immediately after interrogating. This is just one example (any workflow
#' involving a `filename` argument is applicable). It's really advantageous to
#' use datetime-based filenames when interrogating directly from YAML in a
#' scheduled process, especially if multiple validation runs per day are being
#' executed on the same target table.
#'
#' ```r
#' yaml_agent_interrogate(
#'   filename = system.file(
#'     "yaml", "agent-small_table.yml",
#'     package = "pointblank"
#'   )
#' ) %>%
#'   x_write_disk(
#'     filename = affix_datetime(
#'       filename = "small_table_agent.rds",
#'       delimiter = "-"
#'     ),
#'     keep_tbl = TRUE,
#'     keep_extracts = TRUE
#'   )
#' ```
#'
#' In the above, we used the written-to-disk agent (The
#' `"agent-small_table.yml"` YAML file) for an interrogation via
#' [yaml_agent_interrogate()]. Then, the results were written to disk as an RDS
#' file. In the `filename` argument of [x_write_disk()], the [affix_datetime()]
#' function was used to ensure that frequent runs would produce files whose
#' names indicate the day and time of execution.
#'
#' @family Utility and Helper Functions
#' @section Function ID:
#' 13-4
#'
#' @seealso The [affix_date()] function provides the same features except it
#'   produces a date string by default.
#'
#' @export
affix_datetime <- function(
    filename,
    position = c("end", "start"),
    format = "%Y-%m-%d_%H-%M-%S",
    delimiter = "_",
    utc_time = TRUE,
    add_tz = FALSE
) {

  position <- match.arg(position)

  affix_time_to_filename(
    filename = filename,
    position = position,
    format = format,
    delimiter = delimiter,
    utc_time = utc_time,
    add_tz = add_tz
  )
}

affix_time_to_filename <- function(
    filename,
    position,
    format,
    delimiter,
    utc_time,
    add_tz
) {

  curr_time <- Sys.time()

  if (utc_time) {
    attr(curr_time, "tzone") <- "UTC"
  }

  curr_time <- format(curr_time, ifelse(add_tz, paste0(format, "%z"), format))
  curr_time <- gsub("\\+0000", "Z", curr_time)

  if (position == "end") {

    filename_rev <-
      paste0(
        tools::file_path_sans_ext(filename),
        delimiter,
        curr_time,
        ifelse(tools::file_ext(filename) != "", ".", ""),
        tools::file_ext(filename)
      )

  } else {

    filename_rev <-
      paste0(
        curr_time,
        delimiter,
        filename
      )
  }

  filename_rev
}
