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


#' Given an *informant* object, update and incorporate table snippets
#' 
#' @description 
#' When the *informant* object has a number of snippets available (by using
#' [info_snippet()]) and the strings to use them (by using the `info_*()`
#' functions and `{<snippet_name>}` in the text elements), the process of
#' incorporating aspects of the table into the info text can occur by
#' using the `incorporate()` function. After that, the information will be fully
#' updated (getting the current state of table dimensions, re-rendering the
#' info text, etc.) and we can print the *informant* object or use the
#' [get_informant_report()] function to see the information report.
#' 
#' @param informant An informant object of class `ptblank_informant`.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @section Demos:
#' 
#' Take the `small_table` and assign it to `changing_table` (we'll modify it
#' later):
#' 
#' ```{r}
#' changing_table <- small_table
#' 
#' changing_table
#' ```
#' 
#' Use [create_informant()] to generate an informant object with
#' `changing_table` given to the `tbl` argument with a leading `~` (ensures that
#' the table will be fetched each time it is needed, instead of being statically
#' stored in the object). We'll add two snippets with [info_snippet()], add
#' information with the [info_columns()] and [info_section()] functions and then
#' use `incorporate()` to work the snippets into the info text.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ changing_table,
#'     tbl_name = "changing_table",
#'     label = "`informant()` example"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "col_count",
#'     fn = ~ . %>% ncol()
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. ((SIMPLE))"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. ((CALC))"
#'   ) %>%
#'   info_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   incorporate()
#' ```
#' We can print the resulting object to see the information report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' 
#' \out{
#' `r pb_get_image_tag(file = "man_incorporate_1.png")`
#' }
#' }
#' 
#' Let's modify `test_table` to give it more rows and an extra column.
#' 
#' ```r
#' changing_table <- 
#'   dplyr::bind_rows(changing_table, changing_table) %>%
#'   dplyr::mutate(h = a + c)
#' ```
#' 
#' Using `incorporate()` will cause the snippets to be reprocessed and
#' accordingly the content of the report will be updated to keep up with the
#' current state of the `changing_table`.
#' 
#' ```r
#' informant <- informant %>% incorporate()
#' ```
#' 
#' When printed again, we'll also see that the row and column counts in the
#' header have been updated to reflect the new dimensions of the target table.
#' Furthermore, the info text in the `ROWS` section has updated text
#' (`"There are 26 rows available."`).
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' 
#' \out{
#' `r pb_get_image_tag(file = "man_incorporate_2.png")`
#' }
#' }
#' 
#' @family Incorporate and Report
#' @section Function ID:
#' 7-1
#' 
#' @export
incorporate <- function(informant) {

  # Obtain the informant's snippets
  meta_snippets <- informant$meta_snippets
  
  # Quieting of an informant's remarks either when the
  # session is non-interactive
  if (!interactive()) {
    quiet <- TRUE
  } else {
    quiet <- FALSE
  }
  
  # Signal the start of incorporation in the console
  create_cli_header_i(
    snippets_to_process = meta_snippets,
    quiet = quiet
  )
  
  # Get the starting time for the gathering of info
  info_gather_start_time <- Sys.time()
  
  # Get the target table for this informant object
  # TODO: extend the materialize table function to use an agent or informant
  tbl <- informant$tbl
  tbl_name <- informant$tbl_name
  read_fn <- informant$read_fn
  
  # Extract the informant's `lang` and `locale` values
  lang <- informant$lang
  locale <- informant$locale
  
  # Prefer reading a table from a `read_fn` if it's available
  # TODO: Verify that the table is a table object
  # and provide an error if it isn't
  if (!is.null(read_fn)) {
    
    if (inherits(read_fn, "function")) {
      
      tbl <- rlang::exec(read_fn)
      
    } else if (rlang::is_formula(read_fn)) {
      
      tbl <- 
        read_fn %>% 
        rlang::f_rhs() %>% 
        rlang::eval_tidy(env = caller_env(n = 1))
      
      if (inherits(tbl, "read_fn")) {
        
        if (inherits(tbl, "with_tbl_name") && is.na(tbl_name)) {
          tbl_name <- tbl %>% rlang::f_lhs() %>% as.character()
        }
        
        tbl <-
          tbl %>%
          rlang::f_rhs() %>%
          rlang::eval_tidy(env = caller_env(n = 1))
      }
      
    } else {
      
      # TODO: Improve the `stop()` message here
      stop(
        "The `read_fn` object must be a function or an R formula.\n",
        "* A function can be made with `function()` {<table reading code>}.\n",
        "* An R formula can also be used, with the expression on the RHS.",
        call. = FALSE
      )
    }
  }
  
  # Update the following property values without user intervention
  #  - _columns
  #  - _rows
  #  - _type
  
  x <- create_agent(tbl = tbl)
  
  table.type <- x$tbl_src
  column_names <- x$col_names
  column_types_r <- x$col_types
  
  table.columns <- length(column_names)
  table.rows <- dplyr::count(tbl, name = "n") %>% dplyr::pull(n)
  
  # TODO: Sync column names, determining which are newly seen
  # and those that are no longer seen
  
  # TODO: Sync column types
  
  # Get the ending time for the gathering of info
  info_gather_end_time <- Sys.time()
  
  # Get the time duration for the processing of snippets (in seconds)    
  time_diff_s <- 
    get_time_duration(
      start_time = info_gather_start_time,
      end_time = info_gather_end_time
    )
  
  if (!quiet) {
    cli::cli_alert_success(
      c("Information gathered.", print_time(time_diff_s))
    )
  }
  
  #
  # Incorporate snippets
  #
  
  # Get the starting time for the processing of snippets
  snippets_start_time <- Sys.time()
  
  for (i in seq_along(meta_snippets)) {

    snippet_fn <- 
      informant$meta_snippets[[i]] %>%
      rlang::f_rhs()
    
    snippet_f_rhs_str <-
      informant$meta_snippets[[i]] %>%
      rlang::f_rhs() %>%
      as.character()

    if (any(grepl("pb_str_catalog", snippet_f_rhs_str)) &&
        any(grepl("lang = NULL", snippet_f_rhs_str)) &&
        lang != "en") {

      # We are inside this conditional because the snippet involves
      # the use of `pb_str_catalog()` and it requires a resetting
      # of the `lang` value (from `NULL` to the informant `lang`)
      
      select_call_idx <-
        which(grepl("select", snippet_f_rhs_str))
      
      pb_str_catalog_call_idx <-
        which(grepl("pb_str_catalog", snippet_f_rhs_str))
      
      snippet_f_rhs_str[pb_str_catalog_call_idx] <-
        gsub(
          "lang = NULL", paste0("lang = \"", lang, "\""),
          snippet_f_rhs_str[pb_str_catalog_call_idx]
        )
      
      # Put the snippet back together as a formula and
      # get only the RHS
      snippet_fn <-
        paste0(
          "~",
          snippet_f_rhs_str[select_call_idx],
          " %>% ",
          snippet_f_rhs_str[pb_str_catalog_call_idx]
        ) %>%
        stats::as.formula() %>%
        rlang::f_rhs()
    }
    
    snippet_fn <- snippet_fn %>% rlang::eval_tidy()
    
    if (inherits(snippet_fn, "fseq")) {
      
      snippet <- snippet_fn(tbl)
      
      # The following stmts always assume that numeric
      # values should be formatted with the default options
      # of `pb_fmt_number()` in the informant's locale
      if (is.numeric(snippet)) {
        
        if (is.integer(snippet)) {
          
          snippet <- 
            snippet %>%
            pb_fmt_number(locale = locale, decimals = 0)
          
        } else {
          
          snippet <- 
            snippet %>%
            pb_fmt_number(locale = locale)
        }
      }
      
      assign(x = names(informant$meta_snippets[i]), value = snippet)
    }
  }
  
  # Get the ending time for the processing of snippets
  snippets_end_time <- Sys.time()
  
  # Get the time duration for the processing of snippets (in seconds)    
  time_diff_s <- 
    get_time_duration(
      start_time = snippets_start_time,
      end_time = snippets_end_time
    )
  
  if (length(meta_snippets) > 0) {
    if (!quiet) {
      cli::cli_alert_success(
        c("Snippets processed.", print_time(time_diff_s))
      )
    }
  }
  
  # Get the starting time for the information building
  info_build_start_time <- Sys.time()
  
  metadata_meta_label <- 
    glue_safely(
      informant$metadata[["info_label"]],
      .otherwise = "~SNIPPET MISSING~"
    )
  
  metadata_table <-
    lapply(informant$metadata[["table"]], function(x) {
      glue_safely(x, .otherwise = "~SNIPPET MISSING~")
    })
  
  metadata_columns <- 
    lapply(informant$metadata[["columns"]], lapply, function(x) {
      glue_safely(x, .otherwise = "~SNIPPET MISSING~")
    })
  
  extra_sections <- 
    base::setdiff(
      names(informant$metadata),
      c("info_label", "table", "columns")
    )
  
  metadata_extra <- informant$metadata[extra_sections]
  
  for (i in seq_along(extra_sections)) {
    for (j in seq_along(metadata_extra[[i]])) {
      
      metadata_extra[[i]][[j]] <-
        lapply(metadata_extra[[i]][[j]], function(x) {
          glue_safely(x, .otherwise = "(SNIPPET MISSING)")
        })
    }
  }
  
  metadata_rev <-
    c(
      list(info_label = metadata_meta_label),
      list(table = metadata_table),
      list(columns = metadata_columns),
      metadata_extra,
      list(updated = Sys.time())
    )
  
  # nolint start
  metadata_rev$table$`_columns` <- as.character(table.columns)
  metadata_rev$table$`_rows` <- as.character(table.rows)
  metadata_rev$table$`_type` <- table.type
  # nolint end
  
  informant$metadata_rev <- metadata_rev
  
  # Get the ending time for the information building
  info_build_end_time <- Sys.time()
  
  # Get the time duration for the processing of snippets (in seconds)    
  time_diff_s <- 
    get_time_duration(
      start_time = info_build_start_time,
      end_time = info_build_end_time
    )
  
  if (!quiet) {
    cli::cli_alert_success(
      c("Information built.", print_time(time_diff_s))
    )
  }
  
  create_cli_footer_i(quiet = quiet)
  
  informant
}

create_cli_header_i <- function(
    snippets_to_process,
    quiet
) {
  
  if (quiet) return()
  
  if (length(snippets_to_process) < 1) {
    incorporation_progress_header <- 
      "Incorporation Started"
  } else if (length(snippets_to_process) == 1) {
    incorporation_progress_header <- 
      "Incorporation Started - there is a single snippet to process"
  } else {
    num_snippets <- length(snippets_to_process)
    incorporation_progress_header <- 
      "Incorporation Started - there are {num_snippets} snippets to process"
  }
  
  cli::cli_h1(incorporation_progress_header)
}

create_cli_footer_i <- function(quiet) {
  
  if (quiet) return()
  
  interrogation_progress_footer <- "Incorporation Completed"
  
  cli::cli_h1(interrogation_progress_footer)
}
