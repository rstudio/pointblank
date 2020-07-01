# nocov start

#
# Generate section components such as tables and plots
#

probe_overview_stats <- function(data,
                                 reporting_lang) {

  n_cols <- ncol(data)
  n_rows <- data %>% dplyr::count(name = "n") %>% dplyr::pull(n) %>% as.numeric()

  suppressWarnings(
    na_cells <- 
      data %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::summarise_all(~ sum(is.na(.))) %>%
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
  
  na_cells_pct <- 
    if (na_cells == 0) {
      ""
    } else {
      ((na_cells / (n_cols * n_rows)) * 100) %>% round(1) %>% as.character() %>% paste0("(", ., "%)")
    }
  
  duplicate_rows_pct <- 
    if (duplicate_rows == 0) {
      ""
    } else {
      ((duplicate_rows / n_rows) * 100) %>% round(1) %>% as.character() %>% paste0("(", ., "%)")
    }

  data_overview_tbl <-
    dplyr::tribble(
      ~label,                                 ~value,
      tbl_lab_columns[[reporting_lang]],        as.character(n_cols),
      tbl_lab_rows[[reporting_lang]],           as.character(n_rows),
      tbl_lab_NAs[[reporting_lang]],            glue::glue("{na_cells} {na_cells_pct}", .transformer = get) %>% as.character(),
      tbl_lab_duplicate_rows[[reporting_lang]], glue::glue("{duplicate_rows} {duplicate_rows_pct}", .transformer = get) %>% as.character(),
    )
  
  r_col_types_tbl <- 
    dplyr::tibble(r_col_types = r_col_types) %>%
    dplyr::group_by(r_col_types) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count))
  
  data_overview_gt <-
    gt::gt(data_overview_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::cols_align(align = "right", columns = gt::vars(value)) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  r_col_types_gt <-
    r_col_types_tbl %>%
    gt::gt(r_col_types_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  # Reproducibility Summary
  reproducibility_gt <-
    dplyr::tribble(
      ~label,                                      ~value,
      tbl_lab_scan_build_time[[reporting_lang]],     paste0("`", Sys.time() %>% as.character(), "`"),
      tbl_lab_pointblank_version[[reporting_lang]],  paste0("`", utils::packageVersion("pointblank") %>% as.character(), "`"),
      tbl_lab_r_version[[reporting_lang]],           paste0(R.version$version.string, "<br><span style=\"font-size: smaller;\"><em>", R.version$nickname, "</em></span>") %>% gsub("-", "&ndash;", .),
      tbl_lab_system_os[[reporting_lang]],           paste0("`", R.version$platform, "`")
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
                          reporting_lang) {

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
          character = probe_columns_character(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          Date = probe_columns_date(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          factor = probe_columns_factor(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          integer = probe_columns_integer(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          logical = probe_columns_logical(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          numeric = probe_columns_numeric(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          POSIXct = probe_columns_posix(data = data, column = col_name, n_rows = n_rows, reporting_lang = reporting_lang),
          probe_columns_other(data = data, column = col_name, n_rows = n_rows)
        )
      })
    
  column_descriptions
}

get_column_description_gt <- function(data_column,
                                      n_rows,
                                      reporting_lang) {

  distinct_count <- 
    data_column %>%
    dplyr::distinct() %>%
    dplyr::group_by() %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    as.integer()
  
  distinct_pct <- 
    if (distinct_count == 0) {
      ""
    } else {
      ((distinct_count / n_rows) * 100) %>% round(1) %>% as.character() %>% paste0("(", ., "%)")
    }
  
  na_cells <- 
    data_column %>%
    dplyr::select(dplyr::everything()) %>%
    dplyr::summarise_all(~ sum(is.na(.))) %>%
    dplyr::collect() %>% 
    t() %>%
    as.vector() %>%
    sum()
  
  na_cells_pct <- 
    if (na_cells == 0) {
      ""
    } else {
      ((na_cells / n_rows) * 100) %>% 
        round(1) %>% 
        as.character() %>% 
        paste0("(", ., "%)")
    }
  
  # Get a count of Inf/-Inf values for non-DB table cells
  if (!inherits(data_column, "tbl_dbi")) {
    
    inf_cells <-
      data_column %>%
      dplyr::pull(1) %>%
      is.infinite() %>%
      sum()
    
  } else {
    inf_cells <- 0L
  }
  
  inf_cells_pct <- 
    if (inf_cells == 0) {
      ""
    } else {
      ((inf_cells / n_rows) * 100) %>% 
        round(1) %>% 
        as.character() %>% 
        paste0("(", ., "%)")
    }
  
  column_description_tbl <-
    dplyr::tribble(
      ~label,                                ~value,
      tbl_lab_distinct[[reporting_lang]],    glue::glue("{distinct_count} {distinct_pct}", .transformer = get),
      tbl_lab_NAs[[reporting_lang]],         glue::glue("{na_cells} {na_cells_pct}", .transformer = get),
      "<code>Inf</code>/<code>-Inf</code>",  glue::glue("{inf_cells} {inf_cells_pct}", .transformer = get),
    )
  
  column_description_gt <-
    gt::gt(column_description_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::cols_align(align = "right", columns = gt::vars(value)) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_description_gt
}

get_numeric_stats_gt <- function(data_column,
                                 reporting_lang) {

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
    dplyr::tribble(
      ~label,                             ~value,
      tbl_lab_mean[[reporting_lang]],     mean,
      tbl_lab_minimum[[reporting_lang]],  min,
      tbl_lab_maximum[[reporting_lang]],  max
    )
  
  column_stats_gt <-
    gt::gt(column_stats_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_stats_gt
}

get_quantile_stats_gt <- function(data_column,
                                  reporting_lang) {
  
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
    dplyr::tribble(
      ~label,                                   ~value,
      tbl_lab_minimum[[reporting_lang]],        as.character(quantile_stats$min),
      tbl_lab_5_percentile[[reporting_lang]],   as.character(quantile_stats$p05),
      "Q1",                                     as.character(quantile_stats$q_1),
      tbl_lab_median[[reporting_lang]],         as.character(quantile_stats$med),
      "Q3",                                     as.character(quantile_stats$q_3),
      tbl_lab_95_percentile[[reporting_lang]],  as.character(quantile_stats$p95),
      tbl_lab_maximum[[reporting_lang]],        as.character(quantile_stats$max),
      tbl_lab_range[[reporting_lang]],          as.character(quantile_stats$range),
      "IQR",                                    as.character(quantile_stats$iqr)
    )
  
  quantile_stats_gt <-
    gt::gt(quantile_stats_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

calculate_quantile_stats <- function(data_column) {
  
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

get_descriptive_stats_gt <- function(data_column,
                                     reporting_lang) {
  
  if (inherits(data_column, "tbl_dbi")) {
    
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
    dplyr::tribble(
      ~label,                                        ~value,
      tbl_lab_mean[[reporting_lang]],                descriptive_stats$mean,
      tbl_lab_variance[[reporting_lang]],            descriptive_stats$variance,
      tbl_lab_standard_deviation[[reporting_lang]],  descriptive_stats$sd,
      tbl_lab_cov[[reporting_lang]],                 descriptive_stats$cv,
      # "Median Absolute Deviation",        ,
      # "Kurtosis",                         ,
      # "Skewness",                         ,
    )
  
  descriptive_stats_tbl %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_common_values_gt <- function(data_column,
                                 reporting_lang) {
  
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
          value = paste0("**", other_values_text[[reporting_lang]], "** (", other_values_distinct , ")"),
          n = other_values_n,
          frequency = other_values_n / n_rows
        )
      ) %>%
      gt::gt() %>%
      gt::cols_label(
        value = tbl_lab_value[[reporting_lang]],
        n = tbl_lab_count[[reporting_lang]],
        frequency = tbl_lab_frequency[[reporting_lang]],
      ) %>%
      gt::fmt_missing(columns = gt::vars(value), missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(value)),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::fmt_percent(columns = gt::vars(frequency), decimals = 1) %>%
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
        value = tbl_lab_value[[reporting_lang]],
        n = tbl_lab_count[[reporting_lang]],
        frequency = tbl_lab_frequency[[reporting_lang]],
      ) %>%
      gt::fmt_missing(columns = gt::vars(value), missing_text = "**NA**") %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(value)),
        fn = function(x) ifelse(x == "**NA**", "<code>NA</code>", x)
      ) %>%
      gt::fmt_percent(columns = gt::vars(frequency), decimals = 1) %>%
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
                                 reporting_lang) {

  n_rows <- data_column %>% dplyr::count(name = "n") %>% dplyr::pull(n)

  data_column_freq <-
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count() %>%
    dplyr::ungroup()
  
  name_1 <- rlang::sym(tbl_lab_value[[reporting_lang]])
  name_2 <- rlang::sym(tbl_lab_count[[reporting_lang]])
  
  data_column_freq <- 
    data_column_freq %>%
    dplyr::select(!! name_1 := 1, !! name_2 := 2)
  
  data_column_top_n <-
    data_column_freq %>%
    dplyr::arrange(dplyr::desc(!! name_2)) %>%
    utils::head(10) %>%
    dplyr::collect()
  
  data_column_top_n[, 3] <- data_column_top_n[, 2, drop = TRUE] / n_rows
  colnames(data_column_top_n)[3] <- tbl_lab_frequency[[reporting_lang]]
  
  data_column_bottom_n <- 
    data_column_freq %>%
    dplyr::arrange(!! name_2) %>%
    utils::head(10) %>%
    dplyr::collect()
  
  data_column_bottom_n[, 3] <- data_column_bottom_n[, 2, drop = TRUE] / n_rows
  colnames(data_column_bottom_n)[3] <- tbl_lab_frequency[[reporting_lang]]
  
  get_slice_gt <- function(data_column, slice = "max") {

    data_column %>%
      gt::gt() %>%
      gt::fmt_percent(columns = 3) %>%
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
                                         reporting_lang) {

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
    ~label,                             ~value,
    tbl_lab_mean[[reporting_lang]],     character_nchar_stats$mean,
    tbl_lab_minimum[[reporting_lang]],  character_nchar_stats$min,
    tbl_lab_maximum[[reporting_lang]],  character_nchar_stats$max
  ) %>%
    gt::gt() %>%
    gt::fmt_number(columns = gt::vars(value), decimals = 1) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_character_nchar_histogram <- function(data_column,
                                          reporting_lang) {
  
  x_label <- plot_lab_string_length[[reporting_lang]]
  y_label <- plot_lab_count[[reporting_lang]]

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
                                  reporting_lang) {

  data_column <- 
    data %>% 
    dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
    )
  
  column_numeric_stats_gt <-
    get_numeric_stats_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  column_quantile_stats_gt <-
    get_quantile_stats_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  column_descriptive_stats_gt <-
    get_descriptive_stats_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  column_common_values_gt <- 
    get_common_values_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  top_bottom_slices_gt <-
    get_top_bottom_slice(
      data_column = data_column,
      reporting_lang = reporting_lang
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
                                  reporting_lang) {

  probe_columns_integer_list <- 
    probe_columns_numeric(
      data = data,
      column = column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
    )
  
  probe_columns_integer_list$column_type <- "integer"
  
  probe_columns_integer_list
}

probe_columns_character <- function(data,
                                    column,
                                    n_rows,
                                    reporting_lang) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
    )
  
  column_common_values_gt <- 
    get_common_values_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  column_nchar_stats_gt <-
    get_character_nchar_stats_gt(
      data_column = data_column,
      reporting_lang = reporting_lang
    )
  
  column_nchar_plot <- 
    get_character_nchar_histogram(
      data_column = data_column,
      reporting_lang = reporting_lang
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
                                  reporting_lang) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
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
                                 reporting_lang) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
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
                               reporting_lang) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
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
                                reporting_lang) {
  
  data_column <- data %>% dplyr::select({{ column }})

  column_description_gt <- 
    get_column_description_gt(
      data_column = data_column,
      n_rows = n_rows,
      reporting_lang = reporting_lang
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
  
  if (inherits(data, "tbl_dbi")) {
    
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
                  dplyr::summarize_all(~ sum(is.na(.))) %>%
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
          dplyr::summarize_all(~ sum(is.na(.)) / dplyr::n()) %>%
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
                                   reporting_lang) {
  
  if (navbar) {
    navbar <- navbar(sections = sections, reporting_lang = reporting_lang)
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
            reporting_lang = reporting_lang
          ),
          variables = probe_columns_assemble(
            data = data,
            reporting_lang = reporting_lang
          ),
          interactions = probe_interactions_assemble(
            data = data,
            reporting_lang = reporting_lang
          ),
          correlations = probe_correlations_assemble(
            data = data,
            reporting_lang = reporting_lang
          ),
          missing = probe_missing_assemble(
            data = data,
            reporting_lang = reporting_lang
          ),
          sample = probe_sample_assemble(
            data = data,
            reporting_lang = reporting_lang
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
        lang = reporting_lang,
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
                    footer_text_fragment[[reporting_lang]]
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
                                          reporting_lang) {

  if (is.na(tbl_name)) {
    header <- section_title_overview_ts[[reporting_lang]]
  } else {
    header <- glue::glue(section_title_overview_of_ts[[reporting_lang]])
  }
  
  row_header <- row_header(id = "overview", header = htmltools::HTML(header))
  
  overview_stats <- probe_overview_stats(data = data, reporting_lang = reporting_lang)
  
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
            label = button_label_overview_overview_ts[[reporting_lang]],
            id = "overview-dataset_overview",
            active = TRUE
          ),
          nav_pill_li(
            label = button_label_overview_reproducibility_ts[[reporting_lang]],
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
                title = subsection_title_overview_table_overview[[reporting_lang]],
                content = overview_stats$data_overview_gt
              ),
              panel_component(
                size = 6,
                title = subsection_title_overview_column_types[[reporting_lang]],
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
                title = subsection_title_overview_reproducibility_information[[reporting_lang]],
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
                                   reporting_lang) {
  
  header <- section_title_variables_ts[[reporting_lang]]
  
  row_header <- row_header(id = "variables", header = header)
  
  columns_data <- probe_columns(data = data, reporting_lang = reporting_lang)
  
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
                      btn_toggle_details[[reporting_lang]]
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
                            tab_label_variables_statistics_ts[[reporting_lang]]
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
                            tab_label_variables_common_values_ts[[reporting_lang]]
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
                            tab_label_variables_max_min_slices_ts[[reporting_lang]]
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
                              subsection_title_variables_quantile_statistics[[reporting_lang]]
                            ),
                            x$column_quantile_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              subsection_title_variables_descriptive_statistics[[reporting_lang]]
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
                              subsection_title_variables_common_values[[reporting_lang]]
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
                              subsection_title_variables_maximum_values[[reporting_lang]]
                            ),
                            x$column_top_slice_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              subsection_title_variables_minimum_values[[reporting_lang]]
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
                      btn_toggle_details[[reporting_lang]]
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
                            subsection_title_variables_common_values[[reporting_lang]]
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
                            subsection_title_variables_string_lengths[[reporting_lang]]
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
                              subsection_title_variables_common_values[[reporting_lang]]
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
                              subsection_title_variables_string_lengths[[reporting_lang]]
                            ),
                            x$column_nchar_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-8",
                            htmltools::tags$p(
                              class = "h4",
                              subsection_title_variables_histogram[[reporting_lang]]
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
                                        reporting_lang) {
  
  header <- section_title_interactions_ts[[reporting_lang]]
  
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
                                        reporting_lang) {
  
  header <- section_title_correlations_ts[[reporting_lang]]
  
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
                                   reporting_lang) {
  
  
  header <- section_title_missing_values_ts[[reporting_lang]]
  
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
                                  reporting_lang) {
  
  header <- section_title_sample_values_ts[[reporting_lang]]
  
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
                   reporting_lang) {
  
  # Compose the list of navigational links for the navbar
  item_list <-
    sections %>%
    lapply(
      FUN = function(x) {
        
        label <-
          switch(
            x,
            overview = nav_overview_ts[[reporting_lang]],
            variables = nav_variables_ts[[reporting_lang]],
            interactions = nav_interactions_ts[[reporting_lang]],
            correlations = nav_correlations_ts[[reporting_lang]],
            missing = nav_missing_values_ts[[reporting_lang]],
            sample = nav_sample_values_ts[[reporting_lang]]
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
          nav_title_ts[[reporting_lang]]
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

