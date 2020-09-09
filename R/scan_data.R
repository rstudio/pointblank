#' Thoroughly scan the table data so as to understand it better
#' 
#' Generate an HTML report that scours the input table data. Before calling up
#' an *agent* to validate the data, it's a good idea to understand the data with
#' some level of precision. Make this the initial step of a well-balanced *data
#' quality reporting* workflow. The reporting output contains several sections
#' to make everything more digestible, and these are:
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
#' The output HTML report is viewable in the RStudio Viewer and can also be
#' integrated in R Markdown HTML reports. If you need the output HTML as a
#' string, it's possible to get that by using `as.character()` (e.g.,
#' `scan_data(tbl = mtcars) %>% as.character()`). The resulting HTML string is a
#' complete HTML document where Bootstrap and jQuery are embedded within.
#' 
#' @param tbl The input table. This can be a data frame, tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object.
#' @param sections The sections to include in the finalized `Table Scan` report.
#'   A character vector with section names is required here. The sections in
#'   their default order are: `"overview"`, `"variables"`, `"interactions"`,
#'   `"correlations"`, `"missing"`, and `"sample"`. This vector can be comprised
#'   of less elements and the order can be changed to suit the desired layout of
#'   the report. For `tbl_dbi` and `tbl_spark` objects, the `"interactions"` and
#'   `"correlations"` sections are excluded.
#' @param navbar Should there be a navigation bar anchored to the top of the
#'   report page? By default this is `TRUE`.
#' @param lang The language to use for label text in the report. By default,
#'   `NULL` will create English (`"en"`) text. Other options include French
#'   (`"fr"`), German (`"de"`), Italian (`"it"`), and Spanish (`"es"`).
#' @param locale An optional locale ID to use for formatting values in the
#'   report according the locale's rules. Examples include `"en_US"` for English
#'   (United States) and `"fr_FR"` for French (France); more simply, this can be
#'   a language identifier without a country designation, like `"es"` for
#'   Spanish (Spain).
#'   
#' @examples
#' # Get an HTML report that describes all of
#' # the data in the `dplyr::storms` dataset
#' # scan_data(tbl = dplyr::storms)
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-1
#' 
#' @export
scan_data <- function(tbl,
                      sections = c("overview", "variables", "interactions",
                                   "correlations", "missing", "sample"),
                      navbar = TRUE,
                      lang = NULL,
                      locale = NULL) {

  # nocov start
  
  # Limit components if a `tbl_dbi` object is supplied as the `tbl`
  if (inherits(tbl, "tbl_dbi") || inherits(tbl, "tbl_spark")) {
    sections <- setdiff(sections, c("interactions", "correlations"))
  }
  
  # nocov end
  
  # Stop function if the length of `sections` is 0
  if (length(sections) == 0) {
    stop("At least one `section` is required.", call. = FALSE)
  }
  
  # Stop function if their are unrecognized sections in `sections`
  if (!all(sections %in% c("overview", "variables", "interactions",
                           "correlations", "missing", "sample"))) {
    
    stop("All values provided in `sections` must be a valid keyword:\n",
         " * Allowed values are \"overview\", \"variables\", \"interactions\", ",
         "\"correlations\", \"missing\", and \"sample\".",
         call. = FALSE)
  }
  
  # Normalize the reporting language identifier and stop if necessary
  lang <- normalize_reporting_language(lang)

  # Attempt to get the table name through `match.call()` and `deparse()`
  tbl_name <- deparse(match.call()$tbl)
  
  # In the case where the table is piped in a `"."` is the
  # result; since it's unknown, we treat it as NA
  if (tbl_name == ".") {
    tbl_name <- NA_character_
  }

  build_examination_page(
    data = tbl,
    tbl_name = tbl_name,
    sections = sections,
    navbar = navbar,
    lang = lang,
    locale = locale
  )
}

# nocov start

#' Print the reporting produced by [scan_data()]
#'
#' This facilitates printing of the HTML report to the R console.
#'
#' @param x An object of class `examination_page`.
#' @param ... Any additional parameters.
#' @param view The value for `print()`s `browse` argument.
#'
#' @keywords internal
#'
#' @export
print.examination_page <- function(x, ..., view = interactive()) {

  class(x) <- c("shiny.tag.list", "list")

  print(x, browse = view, ...)
}

#' Knit print the reporting produced by [scan_data()] 
#'
#' This facilitates printing of the HTML report within a knitr code chunk.
#'
#' @param x An object of class `examination_page`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.examination_page <- function(x, ...) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(x, ...)
}

#
# Generate section components such as tables and plots
#

probe_overview_stats <- function(data,
                                 lang,
                                 locale) {
  
  n_cols <- ncol(data)
  n_rows <- data %>% dplyr::count(name = "n") %>% dplyr::pull(n) %>% as.numeric()
  
  suppressWarnings(
    na_cells <- 
      data %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::summarise_all(~ sum(ifelse(is.na(.), 1, 0))) %>%
      dplyr::collect() %>% 
      t() %>%
      as.vector() %>%
      sum()
  )
  
  tbl_info <- get_tbl_information(tbl = data)
  
  tbl_src <- tbl_info$tbl_src
  r_col_types <- tbl_info$r_col_types
  
  n_rows_distinct <- 
    data %>%
    dplyr::distinct() %>%
    dplyr::count(name = "n") %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  duplicate_rows <- n_rows - n_rows_distinct
  
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
    dplyr::arrange(dplyr::desc(count))
  
  data_overview_gt <-
    gt::gt(data_overview_tbl) %>%
    gt::fmt_markdown(columns = gt::vars(label)) %>%
    gt::fmt_number(columns = gt::vars(value), decimals = 0, locale = locale) %>%
    gt::fmt_percent(columns = gt::vars(pct), decimals = 2, locale = locale) %>%
    gt::cols_merge(columns = gt::vars(value, pct), pattern = "{1} ({2})") %>%
    gt::cols_align(align = "right", columns = gt::vars(value)) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = gt::vars(value), rows = 1:2),
      fn = function(x) {
        gsub(" (NA)", "", x, fixed = TRUE)
      }
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  r_col_types_gt <-
    r_col_types_tbl %>%
    gt::gt(r_col_types_tbl) %>%
    gt::fmt_number(columns = gt::vars(count), decimals = 0, locale = locale) %>%
    gt::cols_align(align = "right", columns = gt::vars(count)) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  # Reproducibility Summary
  reproducibility_gt <-
    dplyr::tribble(
      ~label,                                                    ~value,
      get_lsv("table_scan/tbl_lab_scan_build_time")[[lang]],     paste0("`", Sys.time() %>% as.character(), "`"),
      get_lsv("table_scan/tbl_lab_pointblank_version")[[lang]],  paste0("`", utils::packageVersion("pointblank") %>% as.character(), "`"),
      get_lsv("table_scan/tbl_lab_r_version")[[lang]],           paste0(R.version$version.string, "<br><span style=\"font-size: smaller;\"><em>", R.version$nickname, "</em></span>") %>% gsub("-", "&ndash;", .),
      get_lsv("table_scan/tbl_lab_system_os")[[lang]],           paste0("`", R.version$platform, "`")
    ) %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::vars(label)) %>%
    gt::fmt_markdown(columns = TRUE) %>%
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

probe_columns <- function(data,
                          lang,
                          locale) {
  
  n_rows <- 
    data %>%
    dplyr::count(name = "n") %>%
    dplyr::pull(n) %>%
    as.numeric()
  
  tbl_info <- get_tbl_information(tbl = data)
  
  col_names <- tbl_info$col_names
  col_types <- tbl_info$r_col_types %>% gsub("integer64", "integer", ., fixed = TRUE)
  
  column_descriptions <- 
    lapply(
      seq_along(col_names),
      FUN = function(x) {
        
        col_name <- col_names[x]
        col_type <- col_types[x]
        
        switch(
          col_type,
          character = probe_columns_character(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          Date = probe_columns_date(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          factor = probe_columns_factor(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          integer = probe_columns_integer(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          logical = probe_columns_logical(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          numeric = probe_columns_numeric(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          POSIXct = probe_columns_posix(data = data, column = col_name, n_rows = n_rows, lang = lang, locale = locale),
          probe_columns_other(data = data, column = col_name, n_rows = n_rows)
        )
      })
  
  column_descriptions
}

get_column_description_gt <- function(data_column,
                                      n_rows,
                                      lang,
                                      locale) {

  distinct_count <- 
    data_column %>%
    dplyr::distinct() %>%
    dplyr::group_by() %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.integer()
  
  na_cells <- 
    data_column %>%
    dplyr::select(dplyr::everything()) %>%
    dplyr::summarise_all(~ sum(ifelse(is.na(.), 1, 0))) %>%
    dplyr::collect() %>% 
    t() %>%
    as.vector() %>%
    sum()
  
  # Get a count of Inf/-Inf values for non-DB table cells
  if (!inherits(data_column, "tbl_dbi") &&
      !inherits(data_column, "tbl_spark")) {
    
    inf_cells <-
      data_column %>%
      dplyr::pull(1) %>%
      is.infinite() %>%
      sum()
    
  } else {
    inf_cells <- 0L
  }
  
  column_description_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_distinct")[[lang]],
        get_lsv("table_scan/tbl_lab_NAs")[[lang]],
        "`Inf`/`-Inf`"
      ),
      value = c(distinct_count, na_cells, inf_cells),
      pct = NA_real_
    ) %>%
    dplyr::mutate(pct = dplyr::case_when(
      dplyr::row_number() == 1 ~ distinct_count / n_rows,
      dplyr::row_number() == 2 ~ na_cells / n_rows,
      dplyr::row_number() == 3 ~ inf_cells / n_rows,
      TRUE ~ NA_real_
    ))

  column_description_gt <-
    gt::gt(column_description_tbl) %>%
    gt::fmt_markdown(columns = gt::vars(label)) %>%
    gt::fmt_number(columns = gt::vars(value), decimals = 0, locale = locale) %>%
    gt::fmt_percent(columns = gt::vars(pct), decimals = 2, locale = locale) %>%
    gt::cols_merge(columns = gt::vars(value, pct), pattern = "{1} ({2})") %>%
    gt::cols_align(align = "right", columns = gt::vars(value)) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = gt::vars(value)),
      fn = function(x) {
        gsub("^0 \\(.*", "0", x)
      }
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_description_gt
}

get_numeric_stats_gt <- function(data_column,
                                 lang,
                                 locale) {
  
  summary_stats <- 
    data_column %>%
    dplyr::summarize_all(
      .funs = list(
        ~ mean(., na.rm = TRUE),
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::collect() %>%
    dplyr::summarize_all(~ round(., 2)) %>%
    dplyr::mutate_all(.funs = as.numeric)
  
  mean <- summary_stats$mean
  min <- summary_stats$min
  max <- summary_stats$max
  
  column_stats_tbl <-
    dplyr::tibble(
      label = c(
        get_lsv("table_scan/tbl_lab_mean")[[lang]],   
        get_lsv("table_scan/tbl_lab_minimum")[[lang]],
        get_lsv("table_scan/tbl_lab_maximum")[[lang]]
      ),
      value = c(mean, min, max)
    )

  column_stats_gt <-
    gt::gt(column_stats_tbl) %>%
    gt::fmt_markdown(columns = gt::vars(label)) %>%
    gt::fmt_number(
      columns = gt::vars(value),
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

get_quantile_stats_gt <- function(data_column,
                                  lang,
                                  locale) {
  
  if (inherits(data_column, "tbl_dbi")) {
    
    data_column <- data_column %>% dplyr::filter(!is.na(1))
    
    n_rows <- 
      data_column %>%
      dplyr::count(name = "n") %>%
      dplyr::pull(n) %>%
      as.numeric()
    
    if (n_rows <= 1000) {
      
      data_column <- data_column %>% dplyr::collect()
      quantile_stats <- calculate_quantile_stats(data_column = data_column)
      
    } else {
      
      data_arranged <- 
        data_column %>%
        dplyr::rename(a = 1) %>%
        dplyr::filter(!is.na(a)) %>%
        dplyr::arrange(a)
      
      n_rows_data <-  
        data_arranged %>%
        dplyr::count(name = "n") %>%
        dplyr::pull(n) %>%
        as.numeric()
      
      quantile_rows <- floor(c(0.05, 0.25, 0.5, 0.75, 0.95) * n_rows_data)
      
      quantile_stats <-
        dplyr::tibble(
          min = data_arranged %>% dplyr::summarize(a = min(a, na.rm = TRUE)) %>% dplyr::pull(a) %>% as.numeric(),
          p05 = data_arranged %>% utils::head(quantile_rows[1]) %>% dplyr::arrange(desc(a)) %>% utils::head(1) %>% dplyr::pull(a) %>% as.numeric(),
          q_1 = data_arranged %>% utils::head(quantile_rows[2]) %>% dplyr::arrange(desc(a)) %>% utils::head(1) %>% dplyr::pull(a) %>% as.numeric(),
          med = data_arranged %>% utils::head(quantile_rows[3]) %>% dplyr::arrange(desc(a)) %>% utils::head(1) %>% dplyr::pull(a) %>% as.numeric(),
          q_3 = data_arranged %>% utils::head(quantile_rows[4]) %>% dplyr::arrange(desc(a)) %>% utils::head(1) %>% dplyr::pull(a) %>% as.numeric(),
          p95 = data_arranged %>% utils::head(quantile_rows[5]) %>% dplyr::arrange(desc(a)) %>% utils::head(1) %>% dplyr::pull(a) %>% as.numeric(),
          max = data_arranged %>% dplyr::summarize(a = max(a, na.rm = TRUE)) %>% dplyr::pull(a) %>% as.numeric()
        ) %>%
        dplyr::mutate(
          range = max - min,
          iqr = q_3 - q_1
        ) %>%
        dplyr::summarize_all(~ round(., 2)) %>%
        as.list()
    }
  } else {
    quantile_stats <- calculate_quantile_stats(data_column)
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
      columns = gt::vars(value),
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

calculate_quantile_stats <- function(data_column) {
  
  if (inherits(data_column, "tbl_spark")) {
    
    column_name <- colnames(data_column)
    
    quantiles <- 
      sparklyr::sdf_quantile(
        data_column, column_name,
        probabilities = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
      ) %>% 
      unname()
    
    quantile_stats <- 
      list(
        min = quantiles[1],
        p05 = quantiles[2],
        q_1 = quantiles[3],
        med = quantiles[4],
        q_3 = quantiles[5],
        p95 = quantiles[6],
        max = quantiles[7],
        iqr = quantiles[5] - quantiles[3],
        range = quantiles[7] - quantiles[1]
      ) %>% 
      lapply(FUN = function(x) round(x, 2))
    
  } else {
    
    quantile_stats <- 
      data_column %>%
      dplyr::summarize_all(
        .funs = list(
          min = ~ min(., na.rm = TRUE),
          p05 = ~ stats::quantile(., probs = 0.05, na.rm = TRUE),
          q_1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE),
          med = ~ stats::median(., na.rm = TRUE),
          q_3 = ~ stats::quantile(., probs = 0.75, na.rm = TRUE),
          p95 = ~ stats::quantile(., probs = 0.95, na.rm = TRUE),
          max = ~ max(., na.rm = TRUE),
          iqr = ~ stats::IQR(., na.rm = TRUE)
        )
      ) %>%
      dplyr::mutate(range = max - min) %>%
      dplyr::summarize_all(~ round(., 2)) %>%
      as.list()
  }
  
  quantile_stats
}

get_descriptive_stats_gt <- function(data_column,
                                     lang,
                                     locale) {
  
  if (inherits(data_column, "tbl_dbi") ||
      inherits(data_column, "tbl_spark")) {
    
    data_column <- 
      data_column %>%
      dplyr::filter(!is.na(1))
    
    mean <-
      data_column %>%
      dplyr::rename(a = 1) %>%
      dplyr::group_by() %>%
      dplyr::summarize("__mean__" = mean(a, na.rm = TRUE)) %>%
      dplyr::pull(`__mean__`)
    
    variance <-
      data_column %>%
      dplyr::rename(a = 1) %>%
      dplyr::mutate(
        "__diff__" = (!!mean - a)^2
      ) %>%
      dplyr::group_by() %>%
      dplyr::summarize(
        "__var__"  = mean(`__diff__`, na.rm = TRUE)
      ) %>%
      dplyr::pull(`__var__`)
    
    sd <- variance^0.5
    cv <- sd / mean
    
    descriptive_stats <- 
      dplyr::tibble(
        mean = mean,
        variance = variance,
        sd = sd,
        cv = cv
      ) %>%
      dplyr::summarize_all(~ round(., 2)) %>%
      as.list()
    
  } else {
    
    # Create simple function to obtain the coefficient of variation
    cv <- function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    
    descriptive_stats <- 
      data_column %>%
      dplyr::summarize_all(
        .funs = list(
          mean = ~ mean(., na.rm = TRUE),
          variance = ~ stats::var(., na.rm = TRUE),
          sd = ~ stats::sd(., na.rm = TRUE),
          cv = ~ cv(.)#,
          #mad = ,
          #kur = ,
          #skwns = 
        )
      ) %>%
      dplyr::summarize_all(~ round(., 2)) %>%
      as.list()
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
      columns = gt::vars(value),
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

get_common_values_gt <- function(data_column,
                                 lang,
                                 locale) {
  
  n_rows <- data_column %>% dplyr::count(name = "n") %>% dplyr::pull(n)
  
  common_values_tbl <- 
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup()
  
  n_rows_common_values_tbl <- 
    common_values_tbl %>%
    dplyr::count(name = "n", wt = n) %>%
    dplyr::pull(n)
  
  if (n_rows_common_values_tbl > 10) {
    
    top_ten_rows <- common_values_tbl %>% utils::head(10)
    
    other_values_tbl <-
      common_values_tbl %>% 
      dplyr::anti_join(top_ten_rows, by = colnames(common_values_tbl)) %>% 
      dplyr::arrange(desc(n))
    
    other_values_distinct <- 
      other_values_tbl %>% dplyr::count(name = "n", wt = n) %>% dplyr::pull(n)
    
    other_values_n <- 
      other_values_tbl %>%
      dplyr::select(n) %>%
      dplyr::group_by() %>%
      dplyr::summarize(sum = sum(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(sum)
    
    common_values_gt <-
      dplyr::bind_rows(
        top_ten_rows %>% 
          utils::head(9) %>%
          dplyr::collect() %>%
          dplyr::mutate(
            n = as.numeric(n),
            frequency = n / n_rows) %>%
          dplyr::rename(value = 1) %>%
          dplyr::mutate(value = as.character(value)),
        dplyr::tibble(
          value = paste0("**", get_lsv("table_scan/other_values_text")[[lang]], "** (", other_values_distinct , ")"),
          n = other_values_n,
          frequency = other_values_n / n_rows
        )
      ) %>%
      gt::gt() %>%
      gt::cols_label(
        value = get_lsv("table_scan/tbl_lab_value")[[lang]],
        n = get_lsv("table_scan/tbl_lab_count")[[lang]],
        frequency = get_lsv("table_scan/tbl_lab_frequency")[[lang]],
      ) %>%
      gt::fmt_missing(columns = gt::vars(value), missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(value)),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::fmt_percent(
        columns = gt::vars(frequency),
        decimals = 1,
        locale = locale
      ) %>%
      gt::fmt_markdown(columns = gt::vars(value)) %>%
      gt::tab_options(
        table.border.top.style = "none",
        table.width = "100%"
      )
    
  } else {
    
    common_values_gt <-
      common_values_tbl %>%
      dplyr::collect() %>%
      dplyr::mutate(frequency = n / n_rows) %>%
      dplyr::rename(value = 1) %>%
      dplyr::mutate(value = as.character(value)) %>%
      gt::gt() %>%
      gt::cols_label(
        value = get_lsv("table_scan/tbl_lab_value")[[lang]],
        n = get_lsv("table_scan/tbl_lab_count")[[lang]],
        frequency = get_lsv("table_scan/tbl_lab_frequency")[[lang]],
      ) %>%
      gt::fmt_missing(columns = gt::vars(value), missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(value)),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::fmt_percent(
        columns = gt::vars(frequency),
        decimals = 1,
        locale = locale
      ) %>%
      gt::fmt_markdown(columns = gt::vars(value)) %>%
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

get_top_bottom_slice <- function(data_column,
                                 lang,
                                 locale) {
  
  n_rows <- data_column %>% dplyr::count(name = "n") %>% dplyr::pull(n)
  
  data_column_freq <-
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count() %>%
    dplyr::ungroup()
  
  name_1 <- rlang::sym(get_lsv("table_scan/tbl_lab_value")[[lang]])
  name_2 <- rlang::sym(get_lsv("table_scan/tbl_lab_count")[[lang]])
  
  data_column_freq <- 
    data_column_freq %>%
    dplyr::select(!! name_1 := 1, !! name_2 := 2)
  
  data_column_top_n <-
    data_column_freq %>%
    dplyr::arrange(dplyr::desc(!! name_2)) %>%
    utils::head(10) %>%
    dplyr::collect()
  
  data_column_top_n[, 3] <- data_column_top_n[, 2, drop = TRUE] / n_rows
  colnames(data_column_top_n)[3] <- get_lsv("table_scan/tbl_lab_frequency")[[lang]]
  
  data_column_bottom_n <- 
    data_column_freq %>%
    dplyr::arrange(!! name_2) %>%
    utils::head(10) %>%
    dplyr::collect()
  
  data_column_bottom_n[, 3] <- data_column_bottom_n[, 2, drop = TRUE] / n_rows
  colnames(data_column_bottom_n)[3] <- get_lsv("table_scan/tbl_lab_frequency")[[lang]]
  
  get_slice_gt <- function(data_column, slice = "max") {
    
    data_column %>%
      gt::gt() %>%
      gt::fmt_percent(columns = 3, locale = locale) %>%
      gt::fmt_missing(columns = 1, missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = 1),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::tab_options(
        table.border.top.style = "none",
        table.width = "100%"
      )
  }
  
  list(
    top_slice = get_slice_gt(data_column = data_column_top_n),
    bottom_slice = get_slice_gt(data_column = data_column_bottom_n)
  )
}

get_character_nchar_stats_gt <- function(data_column,
                                         lang,
                                         locale) {
  
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
      columns = gt::vars(value),
      decimals = 1,
      locale = locale
    ) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_character_nchar_histogram <- function(data_column,
                                          lang,
                                          locale) {
  
  x_label <- get_lsv("table_scan/plot_lab_string_length")[[lang]]
  y_label <- get_lsv("table_scan/plot_lab_count")[[lang]]
  
  suppressWarnings(
    plot_histogram <- 
      data_column %>%
      dplyr::mutate_all(.funs = nchar) %>%
      dplyr::rename(nchar = 1) %>%
      dplyr::group_by(nchar) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::collect() %>%
      dplyr::filter(!is.na(nchar)) %>%
      dplyr::mutate_all(.funs = as.numeric) %>%
      ggplot2::ggplot(ggplot2::aes(x = nchar, y = n)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::geom_hline(yintercept = 0, color = "#B2B2B2") +
      ggplot2::labs(x = x_label, y = y_label) +
      ggplot2::scale_x_continuous(limits = c(0, NA)) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::theme_minimal()
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
  Sys.sleep(0.5)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(filename = "temp_histogram_ggplot.png", height = "500px") %>%
          gsub("height:500px", "width: 100%", .)
      )
    )
  
  file.remove("temp_histogram_ggplot.png")
  
  image_html
}

probe_columns_numeric <- function(data,
                                  column,
                                  n_rows,
                                  lang,
                                  locale) {
  
  data_column <- 
    data %>% 
    dplyr::select({{ column }})
  
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

probe_columns_integer <- function(data,
                                  column,
                                  n_rows,
                                  lang,
                                  locale) {
  
  probe_columns_integer_list <- 
    probe_columns_numeric(
      data = data,
      column = column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  probe_columns_integer_list$column_type <- "integer"
  
  probe_columns_integer_list
}

probe_columns_character <- function(data,
                                    column,
                                    n_rows,
                                    lang,
                                    locale) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
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
    get_character_nchar_histogram(
      data_column = data_column,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_type = "character",
    column_description_gt = column_description_gt,
    column_common_gt = column_common_values_gt,
    column_nchar_gt = column_nchar_stats_gt,
    column_nchar_plot = column_nchar_plot
  )
}

probe_columns_logical <- function(data,
                                  column,
                                  n_rows,
                                  lang,
                                  locale) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_type = "logical",
    column_description_gt = column_description_gt
  )
}

probe_columns_factor <- function(data,
                                 column,
                                 n_rows,
                                 lang,
                                 locale) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_type = "factor",
    column_description_gt = column_description_gt
  )
}

probe_columns_date <- function(data,
                               column,
                               n_rows,
                               lang,
                               locale) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_type = "date",
    column_description_gt = column_description_gt
  )
}

probe_columns_posix <- function(data,
                                column,
                                n_rows,
                                lang,
                                locale) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      lang = lang,
      locale = locale
    )
  
  list(
    column_name = column,
    column_type = "datetime",
    column_description_gt = column_description_gt
  )
}

probe_columns_other <- function(data,
                                column,
                                n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_classes <- paste(class(data_column), collapse = ", ")
  
  list(
    column_name = column,
    column_type = column_classes
  )
}

probe_interactions <- function(data) {
  
  category_cutoff <- 5
  #max_dim <- 8
  
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
      rows = gt::vars(gt::everything()), layer.diag = 2, layer.upper = 3, 
      grid.y.diag = FALSE) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
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
  Sys.sleep(0.5)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(filename = "temp_matrix_ggplot.png", height = "500px") %>%
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
  corr_pearson <- stats::cor(data_corr, method = "pearson", use = "pairwise.complete.obs")
  corr_kendall <- stats::cor(data_corr, method = "kendall", use = "pairwise.complete.obs")
  corr_spearman <- stats::cor(data_corr, method = "spearman", use = "pairwise.complete.obs")
  
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

get_corr_plot <- function(mat,
                          labels_vec) {
  
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
  Sys.sleep(0.5)
  
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

# TODO: report missing values based on user input (e.g., "9999", empty strings)

probe_missing <- function(data) {
  
  n_cols <- ncol(data)
  n_rows <- data %>% dplyr::count(name = "n") %>% dplyr::pull(n) %>% as.numeric()
  
  if (n_rows < 20) {
    n_breaks <- n_rows
  } else {
    n_breaks <- 20
  }
  
  if (n_rows <= 1000) {
    data <- dplyr::collect(data)
  }
  
  col_names <- colnames(data)
  
  cuts <- floor(seq(from = 1, to = n_rows, length.out = n_breaks + 1))[-1]
  bin_size <- cuts[1]
  
  if (inherits(data, "tbl_dbi") || inherits(data, "tbl_spark")) {
    
    frequency_list <- 
      lapply(
        col_names,
        FUN = function(`_x_`) {
          
          col_num <- which(col_names %in% `_x_`)
          bin_num <- 1:20
          missing_tally <- 0L
          
          missing_freq <- 
            vapply(
              bin_num,
              FUN.VALUE = numeric(1),
              FUN = function(x) {
                
                missing_n_span <- 
                  data %>% 
                  dplyr::select(1, dplyr::one_of(`_x_`))
                
                if (ncol(missing_n_span) == 1) {
                  
                  missing_n_span <- 
                    missing_n_span %>%
                    dplyr::rename(a = 1)
                  
                } else {
                  
                  missing_n_span <- 
                    missing_n_span %>%
                    dplyr::rename(a = 2)
                }
                
                missing_n_span <- 
                  missing_n_span %>%
                  utils::head(cuts[x]) %>%
                  dplyr::summarize_all(~ sum(ifelse(is.na(.), 1, 0))) %>%
                  dplyr::pull(a) %>%
                  as.integer()
                
                missing_bin <- missing_n_span - missing_tally
                
                missing_tally <<- missing_tally + missing_bin
                
                (missing_bin / bin_size) %>% as.numeric()
              }
            )
          
          dplyr::tibble(
            value = missing_freq,
            col_num = col_num,
            bin_num = bin_num,
            col_name = `_x_`
          )
        })
    
  } else {
    
    frequency_list <- 
      lapply(
        col_names,
        FUN = function(`_x_`) {
          
          data %>%
            dplyr::select(dplyr::one_of(`_x_`)) %>%
            dplyr::mutate(`::cut_group::` = dplyr::case_when(
              dplyr::row_number() < !!cuts[1] ~ 1L,
              dplyr::row_number() < !!cuts[2] ~ 2L,
              dplyr::row_number() < !!cuts[3] ~ 3L,
              dplyr::row_number() < !!cuts[4] ~ 4L,
              dplyr::row_number() < !!cuts[5] ~ 5L,
              dplyr::row_number() < !!cuts[6] ~ 6L,
              dplyr::row_number() < !!cuts[7] ~ 7L,
              dplyr::row_number() < !!cuts[8] ~ 8L,
              dplyr::row_number() < !!cuts[9] ~ 9L,
              dplyr::row_number() < !!cuts[10] ~ 10L,
              dplyr::row_number() < !!cuts[11] ~ 11L,
              dplyr::row_number() < !!cuts[12] ~ 12L,
              dplyr::row_number() < !!cuts[13] ~ 13L,
              dplyr::row_number() < !!cuts[14] ~ 14L,
              dplyr::row_number() < !!cuts[15] ~ 15L,
              dplyr::row_number() < !!cuts[16] ~ 16L,
              dplyr::row_number() < !!cuts[17] ~ 17L,
              dplyr::row_number() < !!cuts[18] ~ 18L,
              dplyr::row_number() < !!cuts[19] ~ 19L,
              dplyr::row_number() < !!cuts[20] ~ 20L,
              TRUE ~ 20L
            )) %>%
            dplyr::group_by(`::cut_group::`) %>% 
            dplyr::summarize_all(~ sum(is.na(.)) / dplyr::n()) %>%
            dplyr::collect() %>%
            dplyr::select(-1) %>%
            dplyr::mutate(col_num = which(col_names %in% `_x_`)) %>%
            dplyr::mutate(bin_num = 1:n_breaks) %>%
            dplyr::mutate(col_name = `_x_`) %>%
            dplyr::rename(value = 1)
        })
  }
  
  frequency_tbl <-
    frequency_list %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(value = ifelse(value == 0, NA_real_, value)) %>%
    dplyr::mutate(col_name = factor(col_name, levels = colnames(data)))
  
  missing_tbl <- 
    lapply(
      col_names,
      FUN = function(`_x_`) {
        data %>% 
          dplyr::select(dplyr::one_of(`_x_`)) %>%
          dplyr::group_by() %>% 
          dplyr::summarize_all(~ sum(ifelse(is.na(.), 1, 0)) / dplyr::n()) %>%
          dplyr::collect() %>%
          dplyr::mutate(col_num = which(col_names %in% `_x_`)) %>%
          dplyr::mutate(col_name = `_x_`) %>%
          dplyr::rename(value = 1)
      }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(value = round(value, 2)) %>%
    dplyr::mutate(col_name = factor(col_name, levels = colnames(data)))
  
  plot_missing <- 
    frequency_tbl %>%
    ggplot2::ggplot(ggplot2::aes(x = col_name, y = bin_num, fill = value)) +
    ggplot2::geom_tile(color = "white", linejoin = "bevel") +
    ggplot2::scale_fill_gradientn(
      colours = c("gray85", "black"),
      na.value = "#A1C1E5",
      limits = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 1, 20),
      labels = c("", as.character(n_rows), "1")
    ) +
    ggplot2::geom_label(
      data = missing_tbl,
      mapping = ggplot2::aes(x = col_name, y = -0.2, label = value, color = value),
      fill = "white",
      show.legend = FALSE
    ) +
    ggplot2::labs(x = "", y = "") + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1, size = 10, margin = ggplot2::margin(t = -1)
      ),
      axis.text.y = ggplot2::element_text(
        angle = 90, hjust = 0, margin = ggplot2::margin(r = -3)
      ),
      panel.grid = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.5, 1.0),
      plot.margin = ggplot2::unit(c(1, 0.5, 0, 0), "cm"),
      legend.key.width = ggplot2::unit(2.0, "cm"),
      legend.key.height = ggplot2::unit(3.0, "mm")
    )
  
  # Save PNG file to disk
  ggplot2::ggsave(
    filename = "temp_missing_ggplot.png",
    plot = plot_missing,
    device = "png",
    dpi = 300,
    width = length(col_names) * 0.8,
    height = 6
  )
  
  # Wait longer for file to be written on async file systems
  Sys.sleep(0.5)
  
  image_html <- 
    htmltools::tags$div(
      style = "text-align: center",
      htmltools::HTML(
        gt::local_image(filename = "temp_missing_ggplot.png", height = "500px") %>%
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
    gt::fmt_missing(columns = TRUE, missing_text = "**NA**") %>%
    gt::text_transform(
      locations = gt::cells_body(columns = TRUE),
      fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
    ) %>%
    gt::tab_options(table.width = "100%")
  
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

build_examination_page <- function(data,
                                   tbl_name,
                                   sections,
                                   navbar,
                                   lang,
                                   locale) {
  
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
  
  # htmltools::attachDependencies(
  #   htmltools::tags$html(
  #     lang = "en",
  #     htmltools::tags$body(
  #       probe_overview_stats_assemble(data = data),
  #       probe_overview_stats_assemble(data = data)
  #     )
  #   ),
  #   bootstrap_lib()
  # )
  
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
  
  examination_page <- 
    htmltools::tagList(
      htmltools::HTML("<!doctype html>"),
      htmltools::tags$html(
        lang = lang,
        htmltools::HTML(
          "<head>\n",
          "   <meta charset=\"utf-8\">\n",
          "   <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">\n",
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
  
  class(examination_page) <- c("examination_page", class(examination_page))
  examination_page
}

probe_overview_stats_assemble <- function(data,
                                          tbl_name,
                                          lang,
                                          locale) {
  
  if (is.na(tbl_name)) {
    header <- get_lsv("table_scan/nav_overview_ts")[[lang]]
  } else {
    header <- glue::glue(get_lsv("table_scan/section_title_overview_of_ts")[[lang]])
  }
  
  row_header <- row_header(id = "overview", header = htmltools::HTML(header))
  
  overview_stats <- probe_overview_stats(data = data, lang = lang, locale = locale)
  
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
            label = get_lsv("table_scan/button_label_overview_reproducibility_ts")[[lang]],
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
                title = get_lsv("table_scan/subsection_title_overview_table_overview")[[lang]],
                content = overview_stats$data_overview_gt
              ),
              panel_component(
                size = 6,
                title = get_lsv("table_scan/subsection_title_overview_column_types")[[lang]],
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
                title = get_lsv("table_scan/subsection_title_overview_reproducibility_information")[[lang]],
                content = overview_stats$reproducibility_gt
              )
            )
          )
        )
      )
    )
  )
}

probe_columns_assemble <- function(data,
                                   lang,
                                   locale) {
  
  header <- get_lsv("table_scan/nav_variables_ts")[[lang]]
  
  row_header <- row_header(id = "variables", header = header)
  
  columns_data <- probe_columns(data = data, lang = lang, locale = locale)
  
  columns_tagLists <- 
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
                            href = paste0("#", id_val, "bottom-", id_val, "statistics"),
                            `aria-controls` = paste0(id_val, "bottom-", id_val, "statistics"),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv("table_scan/tab_label_variables_statistics_ts")[[lang]]
                          )
                        ),
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0("#", id_val, "bottom-", id_val, "common_values"),
                            `aria-controls` = paste0(id_val, "bottom-", id_val, "common_values"),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv("table_scan/tab_label_variables_common_values_ts")[[lang]]
                          )
                        ),
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0("#", id_val, "bottom-", id_val, "max_min_slices"),
                            `aria-controls` = paste0(id_val, "bottom-", id_val, "max_min_slices"),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv("table_scan/tab_label_variables_max_min_slices_ts")[[lang]]
                          )
                        )
                      ),
                      htmltools::tags$div(
                        class = "tab-content",
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12 active",
                          id = paste0(id_val, "bottom-", id_val, "statistics"),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_quantile_statistics")[[lang]]
                            ),
                            x$column_quantile_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_descriptive_statistics")[[lang]]
                            ),
                            x$column_descriptive_gt
                          )
                        ),
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(id_val, "bottom-", id_val, "common_values"),
                          htmltools::tags$div(
                            class = "col-sm-12",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_common_values")[[lang]]
                            ),
                            x$column_common_gt
                          )
                        ),
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(id_val, "bottom-", id_val, "max_min_slices"),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_maximum_values")[[lang]]
                            ),
                            x$column_top_slice_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_minimum_values")[[lang]]
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
                            href = paste0("#", id_val, "bottom-", id_val, "common_values"),
                            `aria-controls` = paste0(id_val, "bottom-", id_val, "common_values"),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv("table_scan/tab_label_variables_common_values_ts")[[lang]]
                          )
                        ),
                        
                        htmltools::tags$li(
                          role = "presentation",
                          class = "",
                          style = "padding-top: 5px;",
                          htmltools::tags$a(
                            href = paste0("#", id_val, "bottom-", id_val, "lengths"),
                            `aria-controls` = paste0(id_val, "bottom-", id_val, "lengths"),
                            role = "tab",
                            `data-toggle` = "tab",
                            get_lsv("table_scan/subsection_title_variables_string_lengths")[[lang]]
                          )
                        ),
                        
                      ),
                      htmltools::tags$div(
                        class = "tab-content",
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12 active",
                          id = paste0(id_val, "bottom-", id_val, "common_values"),
                          htmltools::tags$div(
                            class = "col-sm-12",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_common_values")[[lang]]
                            ),
                            x$column_common_gt
                          )
                        ),
                        
                        htmltools::tags$div(
                          role = "tabpanel",
                          class = "tab-pane col-sm-12",
                          id = paste0(id_val, "bottom-", id_val, "lengths"),
                          htmltools::tags$div(
                            class = "col-sm-4",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_string_lengths")[[lang]]
                            ),
                            x$column_nchar_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-8",
                            htmltools::tags$p(
                              class = "h4",
                              get_lsv("table_scan/subsection_title_variables_histogram")[[lang]]
                            ),
                            x$column_nchar_plot
                          )
                        ),
                        
                      )
                    )
                  )
                )
              )
            )
          )
          
        } else if (x$column_type %in% c("logical", "factor", "date", "datetime")) {
          
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
  
  htmltools::tagList(
    row_header,
    htmltools::tags$div(
      class = "section-items",
      columns_tagLists
    )
  )
}


probe_interactions_assemble <- function(data,
                                        lang) {
  
  header <- get_lsv("table_scan/nav_interactions_ts")[[lang]]
  
  row_header <- row_header(id = "interactions", header = header)
  
  interactions_data <- suppressWarnings(probe_interactions(data = data))
  
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
}


probe_correlations_assemble <- function(data,
                                        lang) {
  
  header <- get_lsv("table_scan/nav_correlations_ts")[[lang]]
  
  row_header <- row_header(id = "correlations", header = header)
  
  correlations_data <- probe_correlations(data = data)
  
  htmltools::tagList(
    row_header,
    htmltools::tags$div(
      class = "section-items",
      htmltools::tags$div(
        class = "row spacing",
        htmltools::tags$ul(
          class = "nav nav-pills",
          role = "tablist",
          nav_pill_li(label = "Pearson", id = "correlations-pearson", active = TRUE),
          nav_pill_li(label = "Kendall", id = "correlations-kendall", active = FALSE),
          nav_pill_li(label = "Spearman", id = "correlations-spearman", active = FALSE),
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
}

probe_missing_assemble <- function(data,
                                   lang) {
  
  header <- get_lsv("table_scan/nav_missing_values_ts")[[lang]]
  
  row_header <- row_header(id = "missing", header = header)
  
  missing_data <- probe_missing(data = data)
  
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
}

probe_sample_assemble <- function(data,
                                  lang) {

  header <- get_lsv("table_scan/nav_sample_values_ts")[[lang]]
  
  row_header <- row_header(id = "sample", header = header)
  
  sample_data <- probe_sample(data = data)
  
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
}

#
# Components of the page
#

navbar <- function(sections,
                   lang) {
  
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
          style = "font-size: 14.5px",
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

tab_panel <- function(id, panel_component_list, active = FALSE) {
  
  htmltools::tags$div(
    role = "tabpanel",
    class = if (active) "tab-pane col-sm-12 active" else "tab-pane col-sm-12",
    id = id,
    panel_component_list
  )
}

row_header <- function(id, header) {
  
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

panel_component <- function(size, content, title = NULL) {
  
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

nav_pill_li <- function(label, id, active = FALSE) { 
  
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

# nocov end
