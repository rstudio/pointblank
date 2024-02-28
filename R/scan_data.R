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


# nocov start

#' Thoroughly scan a table to better understand it
#'
#' @description
#' 
#' Generate an HTML report that scours the input table data. Before calling up
#' an *agent* to validate the data, it's a good idea to understand the data with
#' some level of precision. Make this the initial step of a well-balanced 
#' *data quality reporting* workflow. The reporting output contains several
#' sections to make everything more digestible, and these are:
#' 
#' \describe{
#' \item{Overview}{Table dimensions, duplicate row counts, column types, and
#' reproducibility information}
#' \item{Variables}{A summary for each table variable and further statistics and
#' summaries depending on the variable type}
#' \item{Interactions}{A matrix plot that shows interactions between variables}
#' \item{Correlations}{A set of correlation matrix plots for numerical
#' variables}
#' \item{Missing Values}{A summary figure that shows the degree of missingness
#' across variables}
#' \item{Sample}{A table that provides the head and tail rows of the dataset}
#' }
#' 
#' The resulting object can be printed to make it viewable in the RStudio
#' Viewer. It's also a `"shiny.tag.list"` object and so can be integrated in R
#' Markdown HTML output or in Shiny applications. If you need the output HTML,
#' it's to export that to a file with the [export_report()] function.
#' 
#' @param tbl *A data table*
#' 
#'   `obj:<tbl_*>` // **required**
#' 
#'   The input table. This can be a data frame, tibble, a `tbl_dbi` object, or a
#'   `tbl_spark` object.
#'   
#' @param sections *Sections to include*
#' 
#'   `scalar<character>` // *default:* `"OVICMS"`
#' 
#'   The sections to include in the finalized `Table Scan` report. A string with
#'   key characters representing section names is required here. The default
#'   string is `"OVICMS"` wherein each letter stands for the following sections
#'   in their default order: `"O"`: `"overview"`; `"V"`: `"variables"`; `"I"`:
#'   `"interactions"`; `"C"`: `"correlations"`; `"M"`: `"missing"`; and `"S"`:
#'   `"sample"`. This string can be comprised of less characters and the order
#'   can be changed to suit the desired layout of the report. For `tbl_dbi` and
#'   `tbl_spark` objects supplied to `tbl`, the `"interactions"` and
#'   `"correlations"` sections are currently excluded.
#'   
#' @param navbar *Include navigation in HTML report*
#' 
#'   `scalar<logical>` // *default:* `TRUE`
#' 
#'   Should there be a navigation bar anchored to the top of the report page?
#'   
#' @param width *Width option for HTML report*
#' 
#'   `scalar<integer>` // *default:* `NULL` (`optional`)
#' 
#'   An optional fixed width (in pixels) for the HTML report. By default, no
#'   fixed width is applied.
#'   
#' @param lang *Reporting language*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   The language to use for label text in the report. By default, `NULL` will
#'   create English (`"en"`) text. Other options include French (`"fr"`), German
#'   (`"de"`), Italian (`"it"`), Spanish (`"es"`), Portuguese (`"pt"`), Turkish
#'   (`"tr"`), Chinese (`"zh"`),  Russian (`"ru"`), Polish (`"pl"`), Danish
#'   (`"da"`), Swedish (`"sv"`), and Dutch (`"nl"`).
#'   
#' @param locale *Locale for value formatting within reports*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   An optional locale ID to use for formatting values in the report according
#'   the locale's rules. Examples include `"en_US"` for English (United States)
#'   and `"fr_FR"` for French (France); more simply, this can be a language
#'   identifier without a country designation, like `"es"` for Spanish (Spain,
#'   same as `"es_ES"`).
#' 
#' @return A `ptblank_tbl_scan` object.
#' 
#' @section Examples:
#' 
#' Get an HTML document that describes all of the data in the `dplyr::storms`
#' dataset.
#' 
#' ```r
#' tbl_scan <- scan_data(tbl = dplyr::storms)
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_scan_data_1.png")`
#' }
#' }
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-1
#' 
#' @export
scan_data <- function(
    tbl,
    sections = "OVICMS",
    navbar = TRUE,
    width = NULL,
    lang = NULL,
    locale = NULL
) {

  # If the document is undergoing knitting then adjust some options
  if (isTRUE(getOption("knitr.in.progress"))) {
    
    navbar <- FALSE
    
    if (is.null(width)) {
      width <- "720px"
    }
  }
  
  if (!is.null(width) && is.numeric(width)) {
    width <- gt::px(width[1])
  }
  
  if (interactive()) {
    
    cli::cli_div(
      theme = list(
        span.time_taken = list(color = "magenta", "font-weight" = "normal")
      )
    )
  }
  
  # Stop function if the length of the `sections` vector is not 1
  if (length(sections) != 1) {
    
    stop(
      "The length of the `section` vector must be 1.",
      call. = FALSE
    )
  }
  
  # Stop function if the `sections` vector is not of the `character` type
  if (!is.character(sections)) {
    
    stop(
      "The `section` vector must be of the `character` class.",
      call. = FALSE
    )
  }
  
  # Stop function if the length of `sections` string is not at least 1
  if (nchar(sections) < 1) {
    
    stop(
      "At least one `section` is required.",
      call. = FALSE
    )
  }
  
  # Stop function if there are unrecognized sections in `sections`
  if (!all(unique(unlist(strsplit(toupper(sections), ""))) %in% 
           c("O", "V", "I", "C", "M", "S"))) {
    stop(
      "All key characters provided in `sections` must be valid:\n",
      "* Allowed values are \"O\", \"V\", \"I\", \"C\", \"M\", and \"S\".",
      call. = FALSE
    )
  }
  
  sections_abbrev <- unique(unlist(strsplit(toupper(sections), "")))
  
  # Transform the `sections` string to a vector of section names
  sections <- 
    vapply(
      unique(unlist(strsplit(toupper(sections), ""))),
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        switch(
          x,
          O = "overview",
          V = "variables",
          I = "interactions",
          C = "correlations",
          M = "missing",
          S = "sample"
        )
      }
    )
  
  # Limit components if a `tbl_dbi` object is supplied as the `tbl`
  if (inherits(tbl, "tbl_dbi") || inherits(tbl, "tbl_spark")) {
    sections <- setdiff(sections, c("interactions", "correlations"))
  }
  
  if (any(c("interactions", "correlations") %in% sections)) {
    
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      
      stop(
        "The `interactions` and `correlations` sections require ", 
        "the ggplot2 package:\n",
        "* It can be installed with `install.packages(\"ggplot2\")`.",
        call. = FALSE
      )
    }
    
    if (!requireNamespace("ggforce", quietly = TRUE)) {
      
      stop(
        "The `interactions` and `correlations` sections require ", 
        "the ggforce package:\n",
        "* It can be installed with `install.packages(\"ggforce\")`.",
        call. = FALSE
      )
    }
  }
  
  # Normalize the reporting language identifier and stop if necessary
  lang <- normalize_reporting_language(lang)
  
  # Set the `locale` to the `lang` value if `locale` isn't set
  if (is.null(locale)) locale <- lang
  
  # Attempt to get the table name through `match.call()` and `deparse()`
  tbl_name <- deparse(match.call()$tbl)
  
  # In the case where the table is piped in a `"."` is the
  # result; since it's unknown, we treat it as NA
  if (length(tbl_name) == 1 && tbl_name == ".") {
    tbl_name <- NA_character_
  } else if (length(tbl_name) > 1) {
    tbl_name <- NA_character_
  }
  
  # Get the starting time for the table scan
  scan_start_time <- Sys.time()
  
  if (interactive()) {
    
    cli::cli_h1(
      paste0(
        "Data Scan started. Processing ",
        length(unique(sections_abbrev)), " ",
        "section",
        ifelse(length(unique(sections_abbrev)) > 1, "s", ""),
        "."
      )
    )
  }
  
  table_scan <- 
    build_table_scan_page(
      data = tbl,
      tbl_name = tbl_name,
      sections = sections,
      navbar = navbar,
      lang = lang,
      locale = locale,
      width = width
    )
  
  # Get the ending time for the table scan
  scan_end_time <- Sys.time()
  
  # Get the time duration for the table scan    
  time_diff_s <- 
    get_time_duration(
      start_time = scan_start_time,
      end_time = scan_end_time
    )
  
  if (interactive()) {
    
    cli::cli_h1(
      paste0(
        "Data Scan finished. ", print_time(time_diff_s)
      )
    )
  }
  
  table_scan
}

#
# Generate section components such as tables and plots
#

probe_overview_stats <- function(
    data,
    lang,
    locale
) {
  
  n_cols <- get_table_total_columns(data = data)
  n_rows <- get_table_total_rows(data = data)
  
  n_rows_distinct <- get_table_total_distinct_rows(data = data)
  
  duplicate_rows <- n_rows - n_rows_distinct
  
  na_cells <- get_table_total_missing_values(data = data)

  tbl_info <- get_tbl_information(tbl = data)
  
  tbl_src <- tbl_info$tbl_src
  r_col_types <- tbl_info$r_col_types
  
  data_overview_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_columns")[[lang]],      
        get_lsv("table_scan/tbl_lab_rows")[[lang]],         
        get_lsv("table_scan/tbl_lab_NAs")[[lang]],          
        get_lsv("table_scan/tbl_lab_duplicate_rows")[[lang]]
      ),
      value = c(
        n_cols, n_rows, na_cells, duplicate_rows
      ),
      pct = NA_real_
    ) %>%
    dplyr::mutate(pct = dplyr::case_when(
      dplyr::row_number() == 3 ~ na_cells / (n_cols * n_rows),
      dplyr::row_number() == 4 ~ duplicate_rows / n_rows,
      TRUE ~ NA_real_
    ))
  
  r_col_types_tbl <- 
    dplyr::tibble(r_col_types = r_col_types) %>%
    dplyr::group_by(r_col_types) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    utils::head(6E8)
  
  data_overview_gt <-
    gt::gt(data_overview_tbl) %>%
    gt::fmt_markdown(columns = "label") %>%
    gt::fmt_number(columns = "value", decimals = 0, locale = locale) %>%
    gt::fmt_percent(columns = "pct", decimals = 2, locale = locale) %>%
    gt::cols_merge(columns = c("value", "pct"), pattern = "{1} ({2})") %>%
    gt::cols_align(align = "right", columns = "value") %>%
    gt::text_transform(
      locations = gt::cells_body(columns = "value", rows = 1:2),
      fn = function(x) {
        gsub(" (NA)", "", x, fixed = TRUE)
      }
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = "value", rows = 3:4),
      fn = function(x) {
        gsub("^0 \\(.*", "0", x)
      }
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  r_col_types_gt <-
    gt::gt(r_col_types_tbl) %>%
    gt::fmt_number(columns = "count", decimals = 0, locale = locale) %>%
    gt::cols_align(align = "right", columns = "count") %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )

  reproducibility_gt <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_scan_build_time")[[lang]],
        get_lsv("table_scan/tbl_lab_pointblank_version")[[lang]],
        get_lsv("table_scan/tbl_lab_r_version")[[lang]],
        get_lsv("table_scan/tbl_lab_system_os")[[lang]]
      ),
      value = c(
        paste0("`", strftime(Sys.time()), "`"),
        paste0("`", as.character(utils::packageVersion("pointblank")), "`"),
        paste0(
          R.version$version.string,
          "<br><span style=\"font-size: smaller;\"><em>",
          R.version$nickname, "</em></span>") %>%
          gsub("-", "&ndash;", .),
        paste0("`", R.version$platform, "`")
      )
    ) %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::everything()) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  list(
    data_overview_gt = data_overview_gt,
    r_col_types_gt = r_col_types_gt,
    reproducibility_gt = reproducibility_gt
  )
}

probe_columns <- function(
    data,
    lang,
    locale
) {
  
  n_rows <- get_table_total_rows(data = data)
  
  col_names <- get_table_column_names(data = data)
  
  col_labels <- get_tbl_df_column_labels(data = data)
  
  tbl_info <- get_tbl_information(tbl = data)
  
  col_types <- gsub("integer64", "integer", tbl_info$r_col_types, fixed = TRUE)
  
  column_descriptions <- 
    lapply(
      seq_along(col_names),
      FUN = function(x) {
        
        col_name <- col_names[x]
        col_type <- col_types[x]
        col_label <- col_labels[x]
        
        switch(
          col_type,
          character = probe_columns_character(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          Date = probe_columns_date(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          factor = probe_columns_factor(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          integer = probe_columns_integer(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          logical = probe_columns_logical(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          numeric = probe_columns_numeric(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          POSIXct = probe_columns_posix(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows,
            lang = lang,
            locale = locale
          ),
          probe_columns_other(
            data = data,
            column = col_name,
            col_label = col_label,
            n_rows = n_rows
          )
        )
      }
    )
  
  column_descriptions
}

get_column_description_gt <- function(
    data_column,
    n_rows,
    lang,
    locale
) {

  distinct_count <- get_table_column_distinct_rows(data_column = data_column)
  
  na_cells <- get_table_column_na_values(data_column = data_column)

  inf_cells <- get_table_column_inf_values(data_column = data_column)
  
  column_description_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_distinct")[[lang]],
        get_lsv("table_scan/tbl_lab_NAs")[[lang]],
        "`Inf`/`-Inf`"
      ),
      value = c(distinct_count, na_cells, inf_cells)
    )
  
  column_description_gt <-
    gt::gt(column_description_tbl) %>%
    gt::fmt_markdown(columns = "label") %>%
    gt::fmt_number(columns = "value", decimals = 0, locale = locale) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_description_gt
}

get_numeric_stats_gt <- function(
    data_column,
    lang,
    locale
) {
  
  summary_stats <- get_table_column_summary(data_column = data_column)

  column_stats_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_mean")[[lang]],   
        get_lsv("table_scan/tbl_lab_minimum")[[lang]],
        get_lsv("table_scan/tbl_lab_maximum")[[lang]]
      ),
      value = c(
        summary_stats$mean,
        summary_stats$min,
        summary_stats$max
      )
    )

  column_stats_gt <-
    gt::gt(column_stats_tbl) %>%
    gt::fmt_markdown(columns = "label") %>%
    gt::fmt_number(
      columns = "value",
      decimals = 2,
      drop_trailing_zeros = TRUE,
      locale = locale
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_stats_gt
}

get_quantile_stats_gt <- function(
    data_column,
    lang,
    locale
) {
  
  if (inherits(data_column, "tbl_dbi")) {
    
    data_column <-  dplyr::filter(data_column, !is.na(1))
    
    n_rows <- get_table_total_rows(data = data_column)
    
    if (n_rows <= 1000) {
      data_column <- dplyr::collect(data_column)
      quantile_stats <- get_df_column_qtile_stats(data_column = data_column)
    } else {
      quantile_stats <- get_dbi_column_qtile_stats(data_column = data_column)
    }
  } else {
    quantile_stats <- get_df_column_qtile_stats(data_column = data_column)
  }
  
  quantile_stats_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_minimum")[[lang]],
        get_lsv("table_scan/tbl_lab_5_percentile")[[lang]],
        "Q1",
        get_lsv("table_scan/tbl_lab_median")[[lang]],
        "Q3",
        get_lsv("table_scan/tbl_lab_95_percentile")[[lang]],
        get_lsv("table_scan/tbl_lab_maximum")[[lang]],
        get_lsv("table_scan/tbl_lab_range")[[lang]],
        "IQR"
      ),
      value = c(
        quantile_stats$min,
        quantile_stats$p05,
        quantile_stats$q_1,
        quantile_stats$med,
        quantile_stats$q_3,
        quantile_stats$p95,
        quantile_stats$max,
        quantile_stats$range,
        quantile_stats$iqr
      )
    )
    
  quantile_stats_gt <-
    gt::gt(quantile_stats_tbl) %>%
    gt::fmt_number(
      columns = "value",
      decimals = 2,
      locale = locale
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  quantile_stats_gt
}

get_df_column_quantile_stats <- function(data_column) {
  
  if (inherits(data_column, "tbl_spark")) {
    quantile_stats <- get_spark_column_qtile_stats(data_column = data_column)
  } else {
    quantile_stats <- get_df_column_qtile_stats(data_column = data_column)
  }
  
  quantile_stats
}

get_descriptive_stats_gt <- function(
    data_column,
    lang,
    locale
) {
  
  if (inherits(data_column, "tbl_dbi") ||
      inherits(data_column, "tbl_spark")) {
    
    data_column <- dplyr::filter(data_column, !is.na(1))
    
    mean <- get_dbi_column_mean(data_column = data_column)

    variance <- 
      get_dbi_column_variance(
        data_column = data_column,
        mean_value = mean
      )
    
    sd <- variance^0.5
    cv <- sd / mean
    
    descriptive_stats <- 
      dplyr::tibble(mean = mean, variance = variance, sd = sd, cv = cv)
    descriptive_stats <- 
      dplyr::summarize_all(descriptive_stats, ~ round(., 2))
    descriptive_stats <- as.list(descriptive_stats)
    
  } else {
    
    # Create simple function to obtain the coefficient of variation
    cv <- function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    
    descriptive_stats <-
      dplyr::summarize_all(
        data_column,
        .funs = list(
          mean = ~ mean(., na.rm = TRUE),
          variance = ~ stats::var(., na.rm = TRUE),
          sd = ~ stats::sd(., na.rm = TRUE),
          cv = ~ cv(.)
        )
      )
    descriptive_stats <- 
      dplyr::summarize_all(descriptive_stats, ~ round(., 2))
    descriptive_stats <- as.list(descriptive_stats)
  }

  descriptive_stats_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_mean")[[lang]],
        get_lsv("table_scan/tbl_lab_variance")[[lang]],
        get_lsv("table_scan/tbl_lab_standard_deviation")[[lang]],
        get_lsv("table_scan/tbl_lab_cov")[[lang]]
      ),
      value = c(
        descriptive_stats$mean,
        descriptive_stats$variance,
        descriptive_stats$sd,
        descriptive_stats$cv
      )
    )
  
  gt::gt(descriptive_stats_tbl) %>%
    gt::fmt_number(
      columns = "value",
      decimals = 2,
      drop_trailing_zeros = FALSE,
      locale = locale
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_common_values_gt <- function(
    data_column,
    lang,
    locale
) {
  
  n_rows <- get_table_total_rows(data = data_column)

  common_values_tbl <- dplyr::group_by_at(data_column, 1)
  common_values_tbl <- dplyr::count(common_values_tbl)
  common_values_tbl <- dplyr::arrange(common_values_tbl, dplyr::desc(n))
  common_values_tbl <- utils::head(common_values_tbl, 6E8)
  common_values_tbl <- dplyr::ungroup(common_values_tbl)
  
  n_rows_common_values_tbl <- 
    dplyr::pull(dplyr::count(common_values_tbl, name = "n", wt = n), n)
  
  if (n_rows_common_values_tbl > 10) {
    
    top_ten_rows <- utils::head(common_values_tbl, 10)
    
    other_values_tbl <-
      dplyr::anti_join(
        common_values_tbl,
        top_ten_rows,
        by = colnames(common_values_tbl)
      )
    other_values_tbl <- dplyr::arrange(other_values_tbl, dplyr::desc(n))
    other_values_tbl <- utils::head(other_values_tbl, 6E8)
    
    other_values_distinct <- 
      dplyr::pull(dplyr::count(other_values_tbl, name = "n", wt = n), n)
    
    other_values_n <- dplyr::select(other_values_tbl, n)
    other_values_n <- dplyr::group_by(other_values_n)
    other_values_n <- 
      dplyr::summarize(other_values_n, sum = sum(n, na.rm = TRUE))
    other_values_n <- dplyr::ungroup(other_values_n)
    other_values_n <- dplyr::pull(other_values_n, sum)

    common_values_tbl <-
      dplyr::bind_rows(
        top_ten_rows %>% 
          utils::head(9) %>%
          dplyr::collect() %>%
          dplyr::mutate(
            n = as.numeric(n),
            frequency = n / n_rows
          ) %>%
          dplyr::rename(value = 1) %>%
          dplyr::mutate(value = as.character(value)),
        dplyr::tibble(
          value = paste0(
            "**",
            get_lsv("table_scan/other_values_text")[[lang]],
            "** (", other_values_distinct, ")"
          ),
          n = other_values_n,
          frequency = other_values_n / n_rows
        )
      )
    
    n_rows_summary_tbl <- nrow(common_values_tbl)
    
    common_values_gt <-
      common_values_tbl %>%
      gt::gt() %>%
      gt::cols_label(
        value = get_lsv("table_scan/tbl_lab_value")[[lang]],
        n = get_lsv("table_scan/tbl_lab_count")[[lang]],
        frequency = get_lsv("table_scan/tbl_lab_frequency")[[lang]],
      ) %>%
      gt_missing(columns = "value", missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = "value"),
        fn = function(x) {
          ifelse(
            x == "**NA**",
            "<code>NA</code>",
            x
          )
        }
      ) %>%
      gt::fmt_percent(
        columns = "frequency",
        decimals = 1,
        locale = locale
      ) %>%
      gt::fmt_markdown(
        columns = "value",
        rows = n_rows_summary_tbl
      ) %>%
      gt::tab_options(
        table.border.top.style = "none",
        table.width = "100%"
      )
    
  } else {
    
    common_values_gt <-
      dplyr::collect(common_values_tbl) %>%
      dplyr::mutate(frequency = n / n_rows) %>%
      dplyr::rename(value = 1) %>%
      dplyr::mutate(value = as.character(value)) %>%
      gt::gt() %>%
      gt::cols_label(
        value = get_lsv("table_scan/tbl_lab_value")[[lang]],
        n = get_lsv("table_scan/tbl_lab_count")[[lang]],
        frequency = get_lsv("table_scan/tbl_lab_frequency")[[lang]],
      ) %>%
      gt_missing(columns = "value", missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = "value"),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::fmt_percent(
        columns = "frequency",
        decimals = 1,
        locale = locale
      ) %>%
      gt::fmt_markdown(columns = "value") %>%
      gt::tab_options(
        table.border.top.style = "none",
        table.width = "100%"
      )
  }
  
  common_values_gt
}

get_head_tail_slices <- function(data_column) {
  
  head_tail_slices <- probe_sample(data = data_column)
  
  head_tail_slices$probe_sample
}

get_top_bottom_slice <- function(
    data_column,
    lang,
    locale
) {
  
  n_rows <- get_table_total_rows(data = data_column)

  data_column_freq <-
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count() %>%
    dplyr::ungroup()
  
  name_1 <- rlang::sym(get_lsv("table_scan/tbl_lab_value")[[lang]])
  name_2 <- rlang::sym(get_lsv("table_scan/tbl_lab_count")[[lang]])
  
  data_column_freq <- 
    dplyr::select(data_column_freq, !!name_1 := 1, !!name_2 := 2)
  
  data_column_top_n <- dplyr::arrange(data_column_freq, dplyr::desc(!!name_2))
  data_column_top_n <- utils::head(data_column_top_n, 10)
  data_column_top_n <- dplyr::collect(data_column_top_n)
  
  data_column_top_n[, 3] <- data_column_top_n[, 2, drop = TRUE] / n_rows
  
  colnames(data_column_top_n)[3] <- 
    get_lsv("table_scan/tbl_lab_frequency")[[lang]]
  
  data_column_bottom_n <- dplyr::arrange(data_column_freq, !!name_2)
  data_column_bottom_n <- utils::head(data_column_bottom_n, 10)
  data_column_bottom_n <- dplyr::collect(data_column_bottom_n)
  
  data_column_bottom_n[, 3] <- 
    data_column_bottom_n[, 2, drop = TRUE] / n_rows
  
  colnames(data_column_bottom_n)[3] <- 
    get_lsv("table_scan/tbl_lab_frequency")[[lang]]
  
  top_slice <-
    get_table_slice_gt(
      data_column = data_column_top_n,
      locale = locale
    )
  
  bottom_slice <-
    get_table_slice_gt(
      data_column = data_column_bottom_n,
      locale = locale
    )
  
  list(
    top_slice = top_slice,
    bottom_slice = bottom_slice
  )
}

get_character_nchar_stats_gt <- function(
    data_column,
    lang,
    locale
) {
  
  character_nchar_stats <- 
    data_column %>%
    dplyr::mutate_all(.funs = nchar) %>%
    dplyr::rename(nchar = 1) %>%
    dplyr::summarize_all(
      .funs = list(
        mean = ~ mean(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate_all(.funs = as.numeric) %>%
    as.list()
  
  dplyr::tribble(
    ~label,                                         ~value,
    get_lsv("table_scan/tbl_lab_mean")[[lang]],     character_nchar_stats$mean,
    get_lsv("table_scan/tbl_lab_minimum")[[lang]],  character_nchar_stats$min,
    get_lsv("table_scan/tbl_lab_maximum")[[lang]],  character_nchar_stats$max
  ) %>%
    gt::gt() %>%
    gt::fmt_number(
      columns = "value",
      decimals = 1,
      locale = locale
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_character_nchar_plot <- function(
    data_column,
    lang,
    locale
) {
  
  suppressWarnings(
    plot_histogram <- 
      get_table_column_histogram(
        data_column = data_column,
        lang = lang,
        locale = locale
      )
  )

  # Save PNG file to disk
  ggplot2::ggsave(
    filename = "temp_histogram_ggplot.png",
    plot = plot_histogram,
    device = "png",
    dpi = 300,
    width = 5,
    height = 3
  )
  
  # Wait longer for file to be written on async file systems
  Sys.sleep(0.25)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(
          filename = "temp_histogram_ggplot.png",
          height = "500px"
        ) %>%
          gsub("height:500px", "width: 100%", .)
      )
    )
  
  file.remove("temp_histogram_ggplot.png")
  
  image_html
}

probe_columns_numeric <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- dplyr::select(data, tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  column_numeric_stats_gt <-
    get_numeric_stats_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  column_quantile_stats_gt <-
    get_quantile_stats_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  column_descriptive_stats_gt <-
    get_descriptive_stats_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  column_common_values_gt <- 
    get_common_values_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  top_bottom_slices_gt <-
    get_top_bottom_slice(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "numeric",
    column_description_gt = column_description_gt,
    column_stats_gt = column_numeric_stats_gt,
    column_quantile_gt = column_quantile_stats_gt,
    column_descriptive_gt = column_descriptive_stats_gt,
    column_common_gt = column_common_values_gt,
    column_top_slice_gt = top_bottom_slices_gt$top_slice,
    column_bottom_slice_gt = top_bottom_slices_gt$bottom_slice
  )
}

probe_columns_integer <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  probe_columns_integer_list <- 
    probe_columns_numeric(
      data = data,
      column = column,
      col_label = col_label,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  probe_columns_integer_list$column_type <- "integer"
  
  probe_columns_integer_list
}

probe_columns_character <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  column_common_values_gt <- 
    get_common_values_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  column_nchar_stats_gt <-
    get_character_nchar_stats_gt(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  column_nchar_plot <- 
    get_character_nchar_plot(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "character",
    column_description_gt = column_description_gt,
    column_common_gt = column_common_values_gt,
    column_nchar_gt = column_nchar_stats_gt,
    column_nchar_plot = column_nchar_plot
  )
}

probe_columns_logical <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "logical",
    column_description_gt = column_description_gt
  )
}

probe_columns_factor <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "factor",
    column_description_gt = column_description_gt
  )
}

probe_columns_date <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "date",
    column_description_gt = column_description_gt
  )
}

probe_columns_posix <- function(
    data,
    column,
    col_label,
    n_rows,
    lang,
    locale
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = "datetime",
    column_description_gt = column_description_gt
  )
}

probe_columns_other <- function(
    data,
    column,
    col_label,
    n_rows
) {
  
  data_column <- data %>% dplyr::select(tidyselect::any_of(column))
  
  column_classes <- paste(class(data_column), collapse = ", ")
  
  list(
    column_name = column,
    column_label = col_label,
    column_type = column_classes
  )
}

probe_interactions <- function(data) {
  
  category_cutoff <- 5

  tbl_info <- get_tbl_information(tbl = data)
  col_names <- tbl_info$col_names
  col_types <- tbl_info$r_col_types
  columns_char <- col_names[col_types == "character"]
  columns_numeric <- col_names[col_types %in% c("integer", "numeric")]
  col_names <- c(columns_char, columns_numeric)
  
  columns_char_distinct_count <- 
    vapply(
      columns_char, FUN.VALUE = integer(1), USE.NAMES = FALSE,
      FUN = function(x) {
        data %>%
          dplyr::select(dplyr::one_of(x)) %>%
          dplyr::distinct() %>%
          dplyr::count() %>%
          dplyr::pull(n)
      }
    )
  
  # Remove the character-based columns from the vector of
  # `col_names` if there are too many categories
  col_names <- 
    col_names %>% 
    base::setdiff(columns_char[columns_char_distinct_count > category_cutoff])
  
  # Create a ggplot2 plot matrix with the data
  plot_matrix <-
    data %>%
    dplyr::select(dplyr::one_of(col_names)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .panel_x, y = .panel_y)) + 
    ggplot2::geom_point(alpha = 0.50, shape = 16, size = 1) + 
    ggforce::geom_autodensity() +
    ggplot2::geom_density2d() +
    ggforce::facet_matrix(
      rows = ggplot2::vars(gt::everything()), layer.diag = 2, layer.upper = 3, 
      grid.y.diag = FALSE) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 8
      ),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  
  # Save PNG file to disk
  ggplot2::ggsave(
    filename = "temp_matrix_ggplot.png",
    plot = plot_matrix,
    device = "png",
    dpi = 300,
    width = length(col_names),
    height = length(col_names)
  )
  
  # Wait longer for file to be written on async file systems
  Sys.sleep(0.25)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(
          filename = "temp_matrix_ggplot.png",
          height = "500px"
        ) %>%
          gsub("height:500px", "width: 100%", .)
      )
    )
  
  file.remove("temp_matrix_ggplot.png")
  
  list(
    probe_interactions = image_html
  )
}

probe_correlations <- function(data) {
  
  tbl_info <- get_tbl_information(tbl = data)
  
  col_names <- tbl_info$col_names
  col_types <- tbl_info$r_col_types
  
  columns_numeric <- col_names[col_types %in% c("integer", "numeric")]
  
  if (length(columns_numeric) < 3) {
    
    return(
      list(
        probe_corr_pearson = NULL,
        probe_corr_kendall = NULL,
        probe_corr_spearman = NULL
      )
    )
  }
  
  data_corr <- dplyr::select(data, dplyr::one_of(columns_numeric))
  
  corr_pearson <- 
    stats::cor(data_corr, method = "pearson", use = "pairwise.complete.obs")
  
  corr_kendall <- 
    stats::cor(data_corr, method = "kendall", use = "pairwise.complete.obs")
  
  corr_spearman <- 
    stats::cor(data_corr, method = "spearman", use = "pairwise.complete.obs")
  
  labels_vec <- seq_along(columns_numeric)
  names(labels_vec) <- columns_numeric
  
  pearson_plot <- get_corr_plot(mat = corr_pearson, labels_vec = labels_vec)
  kendall_plot <- get_corr_plot(mat = corr_kendall, labels_vec = labels_vec)
  spearman_plot <- get_corr_plot(mat = corr_spearman, labels_vec = labels_vec)
  
  list(
    probe_corr_pearson  = pearson_plot,
    probe_corr_kendall  = kendall_plot, 
    probe_corr_spearman = spearman_plot
  )
}

get_corr_plot <- function(
    mat,
    labels_vec
) {
  
  corr_df <- 
    as.data.frame(as.table(mat)) %>%
    dplyr::mutate(Freq = ifelse(Var1 == Var2, NA_real_, Freq)) %>%
    dplyr::mutate(Var1 = factor(Var1, levels = names(labels_vec))) %>%
    dplyr::mutate(Var2 = factor(Var2, levels = rev(names(labels_vec))))
  
  plot_missing <- 
    corr_df %>%
    ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2, fill = Freq)) +
    ggplot2::geom_tile(color = "white", linejoin = "bevel") +
    ggplot2::scale_fill_gradientn(
      colours = c("blue", "white", "red"),
      na.value = "gray30",
      limits = c(-1, 1)
    ) +
    ggplot2::labs(x = "", y = "") + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1, size = 10
      ),
      axis.text.y = ggplot2::element_text(size = 10),
      panel.grid = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.5, 1.03),
      plot.margin = ggplot2::unit(c(1, 0.5, 0, 0), "cm"),
      legend.key.width = ggplot2::unit(2.0, "cm"),
      legend.key.height = ggplot2::unit(3.0, "mm")
    )
  
  temp_filename <- paste0("temp_correlation_ggplot-", gt::random_id(), ".png")
  
  # Save PNG file to disk
  ggplot2::ggsave(
    filename = temp_filename,
    plot = plot_missing,
    device = "png",
    dpi = 300,
    width = length(labels_vec),
    height = length(labels_vec)
  )
  
  # Wait longer for file to be written on async file systems
  Sys.sleep(0.25)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(filename = temp_filename, height = "500px") %>%
          gsub("height:500px", "width: 100%", .)
      )
    )
  
  file.remove(temp_filename)
  
  image_html
}

probe_missing <- function(data) {
  
  n_rows <- get_table_total_rows(data = data)
  col_names <- get_table_column_names(data = data)

  if (n_rows <= 1000) {
    data <- dplyr::collect(data)
  }
  
  if (inherits(data, "tbl_dbi")) {
    frequency_tbl <- get_tbl_dbi_missing_tbl(data = data)
  } else if (inherits(data, "tbl_spark")) {
    frequency_tbl <- get_tbl_spark_missing_tbl(data = data)
  } else {
    frequency_tbl <- get_tbl_df_missing_tbl(data = data)
  }
  
  missing_by_column_tbl <- get_missing_by_column_tbl(data = data)
  
  missing_value_plot <- 
    get_missing_value_plot(
      data = data,
      frequency_tbl = frequency_tbl,
      missing_by_column_tbl = missing_by_column_tbl
    )
  
  # Save PNG file to disk
  ggplot2::ggsave(
    filename = "temp_missing_ggplot.png",
    plot = missing_value_plot,
    device = "png",
    dpi = 300,
    width = length(col_names) * 0.8,
    height = 6
  )
  
  # Wait longer for file to be written on async file systems
  Sys.sleep(0.25)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(
          filename = "temp_missing_ggplot.png",
          height = "500px"
        ) %>%
          gsub("height:500px", "width: 100%", .)
      )
    )
  
  file.remove("temp_missing_ggplot.png")
  
  list(probe_missing = image_html)
}

probe_sample <- function(data) {

  probe_sample <-
    data %>%
    gt::gt_preview(top_n = 5, bottom_n = 5) %>%
    gt_missing(columns = gt::everything(), missing_text = "**NA**") %>%
    gt::text_transform(
      locations = gt::cells_body(columns = gt::everything()),
      fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
    ) %>%
    gt::tab_options(table.width = "100%") %>% 
    gt::tab_style(
      style = gt::cell_text(font = "monospace"),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_body()
      )
    ) %>% 
    gt::text_transform(
      locations = gt::cells_body(), 
      fn = function(x) {
        ifelse(
          x == "<code>NA</code>",
          "<code style=\"color:#3A70FB\">NA</code>",
          x
        )
      }
    )
  
  list(probe_sample = probe_sample)
}

bootstrap_lib <- function() {
  
  htmltools::htmlDependency(
    name = "bootstrap",
    version = "3.3.2",
    package = "pointblank",
    src = file.path("lib", "bootstrap"),
    script = "js/bootstrap.min.js",
    stylesheet = "css/bootstrap.min.css",
    meta = list(viewport = "width=device-width, initial-scale=1")
  )
}

build_table_scan_page <- function(
    data,
    tbl_name,
    sections,
    navbar,
    lang,
    locale,
    width = NULL
) {
  
  if (navbar) {
    navbar <- navbar(sections = sections, lang = lang)
  } else {
    navbar <- NULL
  }
  
  probe_list <-
    sections %>%
    lapply(
      FUN = function(x) {
        switch(
          x,
          overview = probe_overview_stats_assemble(
            data = data,
            tbl_name = tbl_name,
            lang = lang,
            locale = locale
          ),
          variables = probe_columns_assemble(
            data = data,
            lang = lang,
            locale = locale
          ),
          interactions = probe_interactions_assemble(
            data = data,
            lang = lang
          ),
          correlations = probe_correlations_assemble(
            data = data,
            lang = lang
          ),
          missing = probe_missing_assemble(
            data = data,
            lang = lang
          ),
          sample = probe_sample_assemble(
            data = data,
            lang = lang
          )
        )
      } 
    )
  
  bootstrap_lib <- 
    readr::read_file(
      file = system.file(
        "lib", "bootstrap", "css", "bootstrap.min.css",
        package = "pointblank"
      )
    )
  
  jquery_lib <- 
    readr::read_file(
      file = system.file(
        "lib", "jquery", "jquery-1.12.4.min.js",
        package = "pointblank"
      )
    )
  
  extra_js <- 
    readr::read_file(
      file = system.file(
        "javascript", "toggle_anchor.js",
        package = "pointblank"
      )
    )
  
  extra_css <-
    readr::read_file(
      file = system.file(
        "css", "extra.css",
        package = "pointblank"
      )
    )
  
  table_scan <- 
    htmltools::tagList(
      htmltools::HTML("<!doctype html>"),
      htmltools::tags$html(
        lang = lang,
        htmltools::HTML(
          "<head>\n",
          "   <style>code {color: #333 !important; ",
          "   background: none !important; padding: 0 !important; }</style>",
          paste0(
            "   <meta name=\"viewport\" content=\"width=device-width, ",
            "initial-scale=1, shrink-to-fit=no\">\n"
          ),
          "   <style>\n",
          "     ", bootstrap_lib, "\n",
          "   </style>\n",
          "   <style>\n",
          "     ", extra_css, "\n",
          "   </style>\n",
          " </head>"
        ),
        htmltools::tags$body(
          htmltools::tags$a(class = "anchor-pos", id = "top"),
          navbar,
          htmltools::tags$div(
            class = "content",
            htmltools::tags$div(
              class = "container",
              style = if (!is.null(width)) {
                paste0("width: ", width, ";") 
              } else {
                NULL
              },
              probe_list
            )
          ),
          htmltools::tags$footer(
            htmltools::tags$div(
              class = "container-fluid",
              htmltools::tags$div(
                class = "row center-block footer-text",
                htmltools::tags$p(
                  class = "text-muted text-center",
                  htmltools::HTML(
                    get_lsv("table_scan/footer_text_fragment")[[lang]]
                  )
                )
              )
            )
          ),
          htmltools::HTML(
            "<script>\n",
            jquery_lib, "\n",
            "</script>"
          ),
          htmltools::tags$script(
            "<script>\n",
            extra_js, "\n",
            "</script>"
          )
        )
      )
    )
  
  class(table_scan) <- c("ptblank_tbl_scan", class(table_scan))
  
  table_scan
}

probe_overview_stats_assemble <- function(
    data,
    tbl_name,
    lang,
    locale
) {
  
  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
  
  if (is.na(tbl_name)) {
    header <- 
      get_lsv("table_scan/nav_overview_ts")[[lang]]
  } else {
    header <- 
      glue::glue(get_lsv("table_scan/section_title_overview_of_ts")[[lang]])
  }
  
  row_header <- row_header(id = "overview", header = htmltools::HTML(header))
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.overview Starting assembly of 'Overview' section...}"
    )
  }
  
  overview_stats <- 
    probe_overview_stats(data = data, lang = lang, locale = locale)
  
  overview_stats_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        htmltools::tags$div(
          class = "row spacing",
          htmltools::tags$ul(
            class = "nav nav-pills",
            role = "tablist",
            nav_pill_li(
              label = get_lsv("table_scan/nav_overview_ts")[[lang]],
              id = "overview-dataset_overview",
              active = TRUE
            ),
            nav_pill_li(
              label = get_lsv(text = c(
                "table_scan",
                "button_label_overview_reproducibility_ts"
              ))[[lang]],
              id = "overview-reproducibility",
              active = FALSE
            )
          ),
          htmltools::tags$div(
            class = "tab-content",
            style = "padding-top: 10px;",
            tab_panel(
              id = "overview-dataset_overview",
              active = TRUE,
              panel_component_list = list(
                panel_component(
                  size = 6,
                  title = get_lsv(text = c(
                    "table_scan",
                    "subsection_title_overview_table_overview"
                  ))[[lang]],
                  content = overview_stats$data_overview_gt
                ),
                panel_component(
                  size = 6,
                  title = get_lsv(text = c(
                    "table_scan",
                    "subsection_title_overview_column_types"
                  ))[[lang]],
                  content = overview_stats$r_col_types_gt
                )
              )
            ),
            tab_panel(
              id = "overview-reproducibility",
              active = FALSE,
              panel_component_list = list(
                panel_component(
                  size = 12,
                  title = get_lsv(text = c(
                    "table_scan",
                    "subsection_title_overview_reproducibility_information"
                  ))[[lang]],
                  content = overview_stats$reproducibility_gt
                )
              )
            )
          )
        )
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.overview ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  overview_stats_tags
}

probe_columns_assemble <- function(
    data,
    lang,
    locale
) {
  
  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )

  header <- get_lsv("table_scan/nav_variables_ts")[[lang]]
  
  row_header <- row_header(id = "variables", header = header)
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.variables Starting assembly of 'Variables' section...}"
    )
  }
  
  columns_data <- probe_columns(data = data, lang = lang, locale = locale)
  
  columns_tag_lists <- 
    lapply(
      columns_data, 
      function(x) {
        
        id_val <- gt::random_id()
        
        if (x$column_type %in% c("numeric", "integer")) {
          
          htmltools::tagList(
            htmltools::tags$div(
              class = "row spacing",
              htmltools::tags$a(
                class = "anchor-pos anchor-pos-variable",
                id = paste0("pp_var_", id_val),
                htmltools::tags$div(
                  class = "variable",
                  htmltools::tags$div(
                    class = "col-sm-3",
                    htmltools::tags$p(
                      class = "h4",
                      style = htmltools::css(
                        `margin-bottom` = "0",
                        `margin-top` = "-2px"
                      ),
                      title = x$column_name,
                      htmltools::tags$a(
                        href = paste0("#pp_var_", id_val),
                        x$column_name,
                        htmltools::tags$br(),
                        if (!is.na(x$column_label)) {
                          htmltools::tagList(
                            htmltools::tags$p(
                              x$column_label,
                              style = htmltools::css(
                                `font-size` = "small",
                                color = "gray",
                                `margin-bottom` = "0"
                              )
                            )
                          )
                        },
                        htmltools::tags$small(
                          style = htmltools::css(
                            `margin-top` = "0"
                          ),
                          htmltools::tags$code(x$column_type)
                        )
                      )
                    )
                  ),
                  htmltools::tags$div(
                    class = "col-sm-5",
                    x$column_description_gt
                  ),
                  htmltools::tags$div(
                    class = "col-sm-4",
                    x$column_stats_gt
                  ),
                  htmltools::tags$div(
                    class = "col-sm-12 text-left",
                    htmltools::tags$button(
                      class = "btn btn-default btn-sm",
                      style = "margin-top: 5px;",
                      `data-toggle` = "collapse",
                      `data-target` = paste0(
                        "#bottom-", id_val, ", #minifreqtable", id_val
                      ),
                      `aria-expanded` = "true",
                      `aria-controls` = "collapseExample",
                      get_lsv("table_scan/btn_toggle_details")[[lang]]
                    )
                  ),
                  htmltools::tags$div(
                    id = paste0("bottom-", id_val),
                    class = "collapse",
                    `aria-expanded` = "false",
                    style = "height: 5px;",
                    htmltools::tags$div(
                      class = "row spacing",
                      htmltools::tags$ul(
                        class = "nav nav-tabs",
                        role = "tablist",
                        htmltools::tags$li(
                          role = "presentation",
                          class = "active",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0(
                              "#", id_val, "bottom-",
                              id_val, "statistics"
                            ),
                            `aria-controls` = paste0(
                              id_val, "bottom-", id_val, "statistics"
                            ),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv(text = c(
                              "table_scan",
                              "tab_label_variables_statistics_ts"
                            ))[[lang]]
                          )
                        ),
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0(
                              "#", id_val, "bottom-",
                              id_val, "common_values"
                            ),
                            `aria-controls` = paste0(
                              id_val, "bottom-",
                              id_val, "common_values"
                            ),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv(text = c(
                              "table_scan",
                              "tab_label_variables_common_values_ts"
                            ))[[lang]]
                          )
                        ),
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0(
                              "#", id_val, "bottom-",
                              id_val, "max_min_slices"
                            ),
                            `aria-controls` = paste0(
                              id_val, "bottom-",
                              id_val, "max_min_slices"
                            ),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv(text = c(
                              "table_scan",
                              "tab_label_variables_max_min_slices_ts"
                            ))[[lang]]
                          )
                        )
                      ),
                      htmltools::tags$div(
                        class = "tab-content",
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12 active",
                          id = paste0(
                            id_val, "bottom-",
                            id_val, "statistics"
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_quantile_statistics"
                              ))[[lang]]
                            ),
                            x$column_quantile_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                paste0(
                                  "subsection_title_variables_",
                                  "descriptive_statistics"
                                )
                              ))[[lang]]
                            ),
                            x$column_descriptive_gt
                          )
                        ),
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(
                            id_val, "bottom-",
                            id_val, "common_values"
                          ),
                          htmltools::tags$div(
                            class = "col-sm-12",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_common_values"
                              ))[[lang]]
                            ),
                            x$column_common_gt
                          )
                        ),
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(
                            id_val, "bottom-",
                            id_val, "max_min_slices"
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_maximum_values"
                              ))[[lang]]
                            ),
                            x$column_top_slice_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_minimum_values"
                              ))[[lang]]
                            ),
                            x$column_bottom_slice_gt
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          
        } else if (x$column_type == "character") {
          
          htmltools::tagList(
            htmltools::tags$div(
              class = "row spacing",
              htmltools::tags$a(
                class = "anchor-pos anchor-pos-variable",
                id = paste0("pp_var_", id_val),
                htmltools::tags$div(
                  class = "variable",
                  htmltools::tags$div(
                    class = "col-sm-3",
                    htmltools::tags$p(
                      class = "h4",
                      style = htmltools::css(
                        `margin-bottom` = "0",
                        `margin-top` = "-2px"
                      ),
                      title = x$column_name,
                      htmltools::tags$a(
                        href = paste0("#pp_var_", id_val),
                        x$column_name,
                        htmltools::tags$br(),
                        if (!is.na(x$column_label)) {
                          htmltools::tagList(
                            htmltools::tags$p(
                              x$column_label,
                              style = htmltools::css(
                                `font-size` = "small",
                                color = "gray",
                                `margin-bottom` = "0"
                              )
                            )
                          )
                        },
                        htmltools::tags$small(
                          style = htmltools::css(
                            `margin-top` = "0"
                          ),
                          htmltools::tags$code(x$column_type)
                        )
                      )
                    )
                  ),
                  htmltools::tags$div(
                    class = "col-sm-5",
                    x$column_description_gt
                  ),
                  htmltools::tags$div(
                    class = "col-sm-4",
                    " "
                  ),
                  htmltools::tags$div(
                    class = "col-sm-12 text-left",
                    htmltools::tags$button(
                      class = "btn btn-default btn-sm",
                      style = "margin-top: 5px;",
                      `data-toggle` = "collapse",
                      `data-target` = paste0(
                        "#bottom-", id_val, ", #minifreqtable", id_val
                      ),
                      `aria-expanded` = "true",
                      `aria-controls` = "collapseExample",
                      get_lsv("table_scan/btn_toggle_details")[[lang]]
                    )
                  ),
                  htmltools::tags$div(
                    id = paste0("bottom-", id_val),
                    class = "collapse",
                    `aria-expanded` = "false",
                    style = "height: 5px;",
                    htmltools::tags$div(
                      class = "row spacing",
                      htmltools::tags$ul(
                        class = "nav nav-tabs",
                        role = "tablist",
                        htmltools::tags$li(
                          role = "presentation",
                          class = "active",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0(
                              "#", id_val, "bottom-",
                              id_val, "common_values"
                            ),
                            `aria-controls` = paste0(
                              id_val, "bottom-",
                              id_val, "common_values"
                            ),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv(text = c(
                              "table_scan",
                              "tab_label_variables_common_values_ts"
                            ))[[lang]]
                          )
                        ),
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0(
                              "#", id_val, "bottom-",
                              id_val, "lengths"
                            ),
                            `aria-controls` = paste0(
                              id_val, "bottom-",
                              id_val, "lengths"
                            ),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv(text = c(
                              "table_scan",
                              "subsection_title_variables_string_lengths"
                            ))[[lang]]
                          )
                        )
                      ),
                      htmltools::tags$div(
                        class = "tab-content",
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12 active",
                          id = paste0(
                            id_val, "bottom-",
                            id_val, "common_values"
                          ),
                          htmltools::tags$div(
                            class = "col-sm-12",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_common_values"
                              ))[[lang]]
                            ),
                            x$column_common_gt
                          )
                        ),
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(
                            id_val, "bottom-",
                            id_val, "lengths"
                          ),
                          htmltools::tags$div(
                            class = "col-sm-4",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_string_lengths"
                              ))[[lang]]
                            ),
                            x$column_nchar_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-8",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv(text = c(
                                "table_scan",
                                "subsection_title_variables_histogram"
                              ))[[lang]]
                            ),
                            x$column_nchar_plot
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          
        } else if (x$column_type %in% 
                   c("logical", "factor", "date", "datetime")) {
          
          htmltools::tagList(
            htmltools::tags$div(
              class = "row spacing",
              htmltools::tags$a(
                class = "anchor-pos anchor-pos-variable",
                id = paste0("pp_var_", id_val),
                htmltools::tags$div(
                  class = "variable",
                  htmltools::tags$div(
                    class = "col-sm-3",
                    htmltools::tags$p(
                      class = "h4",
                      style = htmltools::css(
                        `margin-bottom` = "0",
                        `margin-top` = "-2px"
                      ),
                      title = x$column_name,
                      htmltools::tags$a(
                        href = paste0("#pp_var_", id_val),
                        x$column_name,
                        htmltools::tags$br(),
                        if (!is.na(x$column_label)) {
                          htmltools::tagList(
                            htmltools::tags$p(
                              x$column_label,
                              style = htmltools::css(
                                `font-size` = "small",
                                color = "gray",
                                `margin-bottom` = "0"
                              )
                            )
                          )
                        },
                        htmltools::tags$small(
                          style = htmltools::css(
                            `margin-top` = "0"
                          ),
                          htmltools::tags$code(x$column_type)
                        )
                      )
                    )
                  ),
                  htmltools::tags$div(
                    class = "col-sm-5",
                    x$column_description_gt
                  ),
                  htmltools::tags$div(
                    class = "col-sm-4",
                    " "
                  ),
                )
              )
            )
          )
        } else {
          
          htmltools::tagList(
            htmltools::tags$div(
              class = "row spacing",
              htmltools::tags$a(
                class = "anchor-pos anchor-pos-variable",
                id = paste0("pp_var_", id_val),
                htmltools::tags$div(
                  class = "variable",
                  htmltools::tags$div(
                    class = "col-sm-12",
                    htmltools::tags$p(
                      class = "h4",
                      title = x$column_name,
                      htmltools::tags$a(
                        href = paste0("#pp_var_", id_val),
                        x$column_name,
                        htmltools::tags$br(),
                        htmltools::tags$small(
                          htmltools::tags$code(x$column_type)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        }
      }
    )
  
  columns_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        columns_tag_lists
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.variables ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  columns_tags
}

probe_interactions_assemble <- function(
    data,
    lang
) {
  
  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
  
  header <- get_lsv("table_scan/nav_interactions_ts")[[lang]]
  
  row_header <- row_header(id = "interactions", header = header)
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.interactions Starting assembly of 'Interactions' section...}"
    )
  }
  
  interactions_data <- suppressWarnings(probe_interactions(data = data))
  
  interactions_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        htmltools::tags$div(
          class = "row spacing",
          htmltools::tags$div(
            id = "sample-container",
            class = "col-sm-12",
            interactions_data$probe_interactions
          )
        )
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.interactions ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  interactions_tags
}

probe_correlations_assemble <- function(
    data,
    lang
) {
  
  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
  
  header <- get_lsv("table_scan/nav_correlations_ts")[[lang]]
  
  row_header <- row_header(id = "correlations", header = header)
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.correlations Starting assembly of 'Correlations' section...}"
    )
  }
  
  correlations_data <- probe_correlations(data = data)
  
  correlations_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        htmltools::tags$div(
          class = "row spacing",
          htmltools::tags$ul(
            class = "nav nav-pills",
            role = "tablist",
            nav_pill_li(
              label = "Pearson",
              id = "correlations-pearson",
              active = TRUE
            ),
            nav_pill_li(
              label = "Kendall",
              id = "correlations-kendall",
              active = FALSE
            ),
            nav_pill_li(
              label = "Spearman",
              id = "correlations-spearman",
              active = FALSE
            ),
          ),
          htmltools::tags$div(
            class = "tab-content",
            style = "padding-top: 10px;",
            tab_panel(
              id = "correlations-pearson",
              active = TRUE,
              panel_component_list = list(
                panel_component(
                  size = 12,
                  title = NULL,
                  content = correlations_data$probe_corr_pearson
                )
              )
            ),
            tab_panel(
              id = "correlations-kendall",
              active = FALSE,
              panel_component_list = list(
                panel_component(
                  size = 12,
                  title = NULL,
                  content = correlations_data$probe_corr_kendall
                )
              )
            ),
            tab_panel(
              id = "correlations-spearman",
              active = FALSE,
              panel_component_list = list(
                panel_component(
                  size = 12,
                  title = NULL,
                  content = correlations_data$probe_corr_spearman
                )
              )
            )
          )
        )
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.correlations ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  correlations_tags
}

probe_missing_assemble <- function(
    data,
    lang
) {
  
  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
  
  header <- get_lsv("table_scan/nav_missing_values_ts")[[lang]]
  
  row_header <- row_header(id = "missing", header = header)
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.missing_values Starting assembly of 'Missing Values' section...}"
    )
  }
  
  missing_data <- probe_missing(data = data)
  
  missing_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        htmltools::tags$div(
          class = "row spacing",
          htmltools::tags$div(
            id = "sample-container",
            class = "col-sm-12",
            missing_data$probe_missing
          )
        )
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.missing_values ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  missing_tags
}

probe_sample_assemble <- function(
    data,
    lang
) {

  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
  
  header <- get_lsv("table_scan/nav_sample_values_ts")[[lang]]
  
  row_header <- row_header(id = "sample", header = header)
  
  # Get the starting time for the section
  section_start_time <- Sys.time()
  
  if (interactive()) {
    cli::cli_alert_info(
      "{.sample Starting assembly of 'Sample' section...}"
    )
  }
  
  sample_data <- probe_sample(data = data)
  
  sample_tags <-
    htmltools::tagList(
      row_header,
      htmltools::tags$div(
        class = "section-items",
        htmltools::tags$div(
          class = "row spacing",
          htmltools::tags$div(
            id = "sample-container",
            class = "col-sm-12",
            sample_data$probe_sample
          )
        )
      )
    )
  
  # Get the ending time for the section
  section_end_time <- Sys.time()
  
  # Get the time duration for the section    
  time_diff_s <- 
    get_time_duration(
      start_time = section_start_time,
      end_time = section_end_time
    )
  
  if (interactive()) {
    cli::cli_alert_success(
      paste0("{.sample ...Finished!} ", print_time(time_diff_s))
    )
  }
  
  sample_tags
}

#
# Components of the page
#

navbar <- function(
    sections,
    lang
) {
  
  # Compose the list of navigational links for the navbar
  item_list <-
    sections %>%
    lapply(
      FUN = function(x) {
        
        label <-
          switch(
            x,
            overview = get_lsv("table_scan/nav_overview_ts")[[lang]],
            variables = get_lsv("table_scan/nav_variables_ts")[[lang]],
            interactions = get_lsv("table_scan/nav_interactions_ts")[[lang]],
            correlations = get_lsv("table_scan/nav_correlations_ts")[[lang]],
            missing = get_lsv("table_scan/nav_missing_values_ts")[[lang]],
            sample = get_lsv("table_scan/nav_sample_values_ts")[[lang]]
          )
        
        htmltools::tags$li(
          htmltools::tags$a(
            class = "anchor",
            href = paste0("#", x),
            label
          )
        )
      }
    )
  
  htmltools::tags$nav(
    class = "navbar navbar-default navbar-fixed-top",
    htmltools::tags$div(
      class = "container-fluid",
      htmltools::tags$div(
        class = "navbar-header",
        htmltools::tags$button(
          type = "button",
          class = "navbar-toggle collapsed",
          style = "margin-top: 5px;",
          `data-toggle` = "collapse",
          `data-target` = "#navbar",
          `aria-expanded` = "false",
          `aria-controls` = "navbar",
          htmltools::tags$span(class = "sr-only", "Toggle navigation"),
          htmltools::tags$span(class = "icon-bar"),
          htmltools::tags$span(class = "icon-bar"),
          htmltools::tags$span(class = "icon-bar")
        ),
        htmltools::tags$a(
          class = "navbar-brand anchor",
          href = "#top",
          style = "font-size: 14.5px;",
          get_lsv("table_scan/nav_title_ts")[[lang]]
        ),
      ),
      htmltools::tags$div(
        id = "navbar",
        class = "navbar-collapse collapse",
        htmltools::tags$ul(
          class = "nav navbar-nav navbar-right",
          item_list
        )
      )
    )
  )
}

tab_panel <- function(
    id,
    panel_component_list,
    active = FALSE
) {
  
  htmltools::tags$div(
    role = "tabpanel",
    class = if (active) "tab-pane col-sm-12 active" else "tab-pane col-sm-12",
    id = id,
    panel_component_list
  )
}

row_header <- function(
    id,
    header
) {
  
  htmltools::tags$div(
    class = "row header",
    htmltools::tags$a(
      class = "anchor-pos",
      id = id
    ),
    htmltools::tags$h1(
      class = "page-header",
      header
    )
  )
}

# nolint start

panel_component <- function(
    size,
    content,
    title = NULL
) {
  
  glue_list <- list(size = size, title = title)
  
  htmltools::tags$div(
    class = pb_glue_data(glue_list, "col-sm-{size}"),
    htmltools::tags$p(
      class = "h4",
      if (!is.null(title)) pb_glue_data(glue_list, "{title}") else ""
    ),
    content
  )
}

nav_pill_li <- function(
    label,
    id,
    active = FALSE
) { 
  
  glue_list <- list(label = label, id = id)
  
  htmltools::tags$li(
    role = "presentation",
    class = if (active) "active" else "",
    htmltools::tags$a(
      href = pb_glue_data(glue_list, "#{id}"),
      `aria-controls` = pb_glue_data(glue_list, "{id}"),
      role = "tab",
      `data-toggle` = "tab",
      `aria-expanded` = if (active) "true" else "false",
      pb_glue_data(glue_list, "{label}")
    )
  )
}

pb_glue_data <- function(.x, ...) {
  glue::glue_data(.x, ..., .transformer = get, .envir = emptyenv())
}

use_cli_theme <- function() {

  cli::cli_div(
    theme = list(
      span.overview = list(color = "red"),
      span.variables = list(color = "orange"),
      span.interactions = list(color = "yellow"),
      span.correlations = list(color = "green"),
      span.missing_values = list(color = "blue"),
      span.sample = list(color = "purple")
    )
  )
}

# nolint end

# nocov end
