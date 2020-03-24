

probe_overview_stats <- function(data) {

  n_cols <- ncol(data)
  n_rows <- nrow(data)

  na_cells_1 <- data %>% dplyr::select_if(function(x) any(is.na(x)))
  
  if (ncol(na_cells_1) > 0) {
    na_cells <-
      na_cells_1 %>%
      dplyr::summarise_each(~ sum(is.na(.))) %>%
      dplyr::mutate(`::all_na::` = sum(.)) %>%
      dplyr::pull(`::all_na::`) %>%
      as.integer()
  } else {
    na_cells <- 0L
  }
  
  tbl_info <- get_tbl_information(tbl = data)
  
  tbl_src <- tbl_info$tbl_src
  r_col_types <- tbl_info$r_col_types
  
  duplicate_rows <- 
    suppressMessages(
      create_agent(tbl = data %>% dplyr::select_at(which(r_col_types != "list"))) %>%
        rows_distinct() %>%
        interrogate() %>%
        unclass() %>%
        .$validation_set %>%
        .$tbl_checked %>% .[[1]] %>% .[[1]] %>%
        dplyr::select(pb_is_good_) %>%
        dplyr::summarize(duplicates = sum(!pb_is_good_)) %>%
        dplyr::pull(duplicates) %>%
        as.integer()
    )
  
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
      ~label,              ~value,
      "Columns",           n_cols,
      "Rows",              n_rows,
      "<code>NA</code>s",  glue::glue("{na_cells} {na_cells_pct}", .transformer = get),
      "Duplicate Rows",    glue::glue("{duplicate_rows} {duplicate_rows_pct}", .transformer = get),
    )
  
  r_col_types_tbl <- 
    dplyr::tibble(r_col_types = r_col_types) %>%
    dplyr::group_by(r_col_types) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count))
  
  data_overview_gt <-
    gt::gt(data_overview_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
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
      ~label,                     ~value,
      "Scan Build Time",          paste0("`", Sys.time() %>% as.character(), "`"),
      "**pointblank** Version",   paste0("`", utils::packageVersion("pointblank") %>% as.character(), "`"),
      "**R** Version",            paste0(R.version$version.string, "<br><span style=\"font-size: smaller;\"><em>", R.version$nickname, "</em></span>") %>% gsub("-", "&ndash;", .),
      "System OS",                paste0("`", R.version$platform, "`")
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

probe_columns <- function(data) {

  n_cols <- ncol(data)
  n_rows <- nrow(data)
  
  tbl_info <- get_tbl_information(tbl = data)
  
  col_names <- tbl_info$col_names
  col_types <- tbl_info$r_col_types
  
  column_descriptions <- 
    lapply(
      seq_along(col_names),
      FUN = function(x) {

        col_name <- col_names[x]
        col_type <- col_types[x]
        
        switch(
          col_type,
          character = probe_columns_character(data = data, column = col_name, n_rows = n_rows),
          Date = probe_columns_date(data = data, column = col_name, n_rows = n_rows),
          factor = probe_columns_factor(data = data, column = col_name, n_rows = n_rows),
          integer = probe_columns_integer(data = data, column = col_name, n_rows = n_rows),
          logical = probe_columns_logical(data = data, column = col_name, n_rows = n_rows),
          numeric = probe_columns_numeric(data = data, column = col_name, n_rows = n_rows),
          POSIXct = probe_columns_posix(data = data, column = col_name, n_rows = n_rows),
          probe_columns_other(data = data, column = col_name, n_rows = n_rows)
        )
      })
    
  column_descriptions
}

get_column_description_gt <- function(data_column, n_rows) {
  
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
    dplyr::summarise_each(~ sum(is.na(.))) %>%
    dplyr::pull(1) %>%
    as.integer()
  
  na_cells_pct <- 
    if (na_cells == 0) {
      ""
    } else {
      ((na_cells / n_rows) * 100) %>% round(1) %>% as.character() %>% paste0("(", ., "%)")
    }
  
  inf_cells <-
    data_column %>%
    dplyr::summarize_each(~ sum(is.infinite(.))) %>%
    dplyr::pull(1) %>%
    as.integer()
  
  inf_cells_pct <- 
    if (inf_cells == 0) {
      ""
    } else {
      ((inf_cells / n_rows) * 100) %>% round(1) %>% as.character() %>% paste0("(", ., "%)")
    }
  
  column_description_tbl <-
    dplyr::tribble(
      ~label,              ~value,
      "Distinct Units",    glue::glue("{distinct_count} {distinct_pct}", .transformer = get),
      "<code>NA</code>s",  glue::glue("{na_cells} {na_cells_pct}", .transformer = get),
      "<code>Inf</code>/<code>-Inf</code>", glue::glue("{inf_cells} {inf_cells_pct}", .transformer = get),
    )
  
  column_description_gt <-
    gt::gt(column_description_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  column_description_gt
}

get_numeric_stats_gt <- function(data_column) {
  
  summary_stats <- 
    data_column %>%
    dplyr::summarize_all(
      .funs = list(
        ~ mean(., na.rm = TRUE),
        ~ min(., na.rm = TRUE),
        ~ max(., na.rm = TRUE)
      )
    ) %>%
    dplyr::summarize_all(~ round(., 2))
  
  mean <- summary_stats$mean
  min <- summary_stats$min
  max <- summary_stats$max
  
  column_stats_tbl <-
    dplyr::tribble(
      ~label,   ~value,
      "Mean",   mean,
      "Min",    min,
      "Max",    max
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

get_quantile_stats_gt <- function(data_column) {
  
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
  
  quantile_stats_tbl <-
    dplyr::tribble(
      ~label,              ~value,
      "Min",               quantile_stats$min,
      "5th Percentile",    quantile_stats$p05,
      "Q1",                quantile_stats$q_1,
      "Median",            quantile_stats$med,
      "Q3",                quantile_stats$q_3,
      "95th Percentile",   quantile_stats$p95,
      "Max",               quantile_stats$max,
      "Range",             quantile_stats$range,
      "IQR",               quantile_stats$iqr
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

get_descriptive_stats_gt <- function(data_column) {
  
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
  
  descriptive_stats_tbl <-
    dplyr::tribble(
      ~label,                       ~value,
      "Mean",                       descriptive_stats$mean,
      "Variance",                   descriptive_stats$variance,
      "Standard Deviation",         descriptive_stats$sd,
      "Coefficient of Variation",   descriptive_stats$cv,
      # "Median Absolute Deviation",,
      # "Kurtosis",                 ,
      # "Skewness",                 ,
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

get_common_values_gt <- function(data_column) {
  
  n_rows <- nrow(data_column)
  
  common_values_tbl <- 
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count() %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup()
  
  if (nrow(common_values_tbl) > 10) {
    
    other_values_tbl <- common_values_tbl %>% dplyr::slice(-c(1:10))
    other_values_distinct <- nrow(other_values_tbl)
    
    other_values_n <- 
      other_values_tbl %>%
      dplyr::select(n) %>%
      dplyr::group_by() %>%
      dplyr::summarize(sum = sum(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::pull(sum)
    
    common_values_gt <-
      dplyr::bind_rows(
        common_values_tbl %>%
          dplyr::slice(1:9) %>%
          dplyr::mutate(frequency = n / n_rows) %>%
          dplyr::rename(value = 1) %>%
          dplyr::mutate(value = as.character(value)),
        dplyr::tibble(
          value = paste0("**Other Values** (", other_values_distinct , ")"),
          n = other_values_n,
          frequency = other_values_n / n_rows
        )
      ) %>%
      gt::gt() %>%
      gt::cols_label(
        value = "Value",
        n = "Count",
        frequency = "Frequency",
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
      dplyr::mutate(frequency = n / n_rows) %>%
      dplyr::rename(value = 1) %>%
      dplyr::mutate(value = as.character(value)) %>%
      gt::gt() %>%
      gt::cols_label(
        value = "Value",
        n = "Count",
        frequency = "Frequency",
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

get_top_bottom_slice <- function(data_column) {
  
  n_rows <- nrow(data_column)
  
  data_column_freq <-
    data_column %>%
    dplyr::group_by_at(1) %>%
    dplyr::count(name = "Count") %>%
    dplyr::rename("Value" = 1) %>%
    dplyr::ungroup()
  
  data_column_top_n <-
    data_column_freq %>%
    dplyr::arrange_at(1, .funs = list(~ dplyr::desc(.))) %>%
    dplyr::slice(1:10) %>%
    dplyr::mutate(Frequency = Count / n_rows)
  
  data_column_bottom_n <- 
    data_column_freq %>%
    dplyr::arrange_at(1) %>%
    dplyr::slice(1:10) %>%
    dplyr::mutate(Frequency = Count / n_rows)
  
  get_slice_gt <- function(data_column, slice = "max") {
    
    data_column %>%
      gt::gt() %>%
      gt::fmt_percent(columns = gt::vars(Frequency)) %>%
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

get_character_nchar_stats_gt <- function(data_column) {
  
  character_nchar_stats <- 
    data_column %>%
    dplyr::mutate_all(.funs = nchar) %>%
    dplyr::rename(nchar = 1) %>%
    dplyr::summarize_all(
      .funs = list(
        mean = ~ mean(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )
    ) %>%
    as.list()
  
  dplyr::tribble(
    ~label,  ~value,
    "Mean",  character_nchar_stats$mean,
    "Min",   character_nchar_stats$min,
    "Max",   character_nchar_stats$max
  ) %>%
    gt::gt() %>%
    gt::fmt_number(columns = gt::vars(value), decimals = 1) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
}

get_character_nchar_histogram <- function(data_column) {
  
  suppressWarnings(
    plot_histogram <- 
      data_column %>%
      dplyr::mutate_all(.funs = nchar) %>%
      dplyr::rename(nchar = 1) %>%
      ggplot2::ggplot(ggplot2::aes(nchar)) +
      ggplot2::geom_histogram(stat = "count", bins = 10, fill = "steelblue") +
      ggplot2::labs(x = "String Length", y = "Count") +
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
  
  # Wait longer for file to be written on async filesystems
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

probe_columns_numeric <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  column_numeric_stats_gt <-
    get_numeric_stats_gt(data_column = data_column)
  
  column_quantile_stats_gt <-
    get_quantile_stats_gt(data_column = data_column)
  
  column_descriptive_stats_gt <-
    get_descriptive_stats_gt(data_column = data_column)
  
  column_common_values_gt <- 
    get_common_values_gt(data_column = data_column)
  
  top_bottom_slices_gt <-
    get_top_bottom_slice(data_column = data_column)
  
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

probe_columns_integer <- function(data, column, n_rows) {
  
  probe_columns_integer_list <- 
    probe_columns_numeric(data = data, column = column, n_rows = n_rows)
  
  probe_columns_integer_list$column_type <- "integer"
  
  probe_columns_integer_list
}

probe_columns_character <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  column_common_values_gt <- 
    get_common_values_gt(data_column = data_column)
  
  column_nchar_stats_gt <-
    get_character_nchar_stats_gt(data_column = data_column)
  
  column_nchar_plot <- 
    get_character_nchar_histogram(data_column = data_column)
  
  list(
    column_name = column,
    column_type = "character",
    column_description_gt = column_description_gt,
    column_common_gt = column_common_values_gt,
    column_nchar_gt = column_nchar_stats_gt,
    column_nchar_plot = column_nchar_plot
  )
}

probe_columns_logical <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(
    column_name = column,
    column_type = "logical",
    column_description_gt = column_description_gt
  )
}

probe_columns_factor <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(
    column_name = column,
    column_type = "factor",
    column_description_gt = column_description_gt
  )
}

probe_columns_date <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(
    column_name = column,
    column_type = "date",
    column_description_gt = column_description_gt
  )
}

probe_columns_posix <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})

  column_description_gt <- 
    get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(
    column_name = column,
    column_type = "datetime",
    column_description_gt = column_description_gt
  )
}

probe_columns_other <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  
  column_classes <- paste(class(data_column), collapse = ", ")
  
  list(
    column_name = column,
    column_type = column_classes
  )
}

probe_interactions <- function(data) {
  
  category_cutoff <- 5
  max_dim <- 8
  
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
          dplyr::select(gt::one_of(x)) %>%
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
  
  plot_matrix <-
    data %>%
    dplyr::select(gt::one_of(col_names)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .panel_x, y = .panel_y)) + 
    ggplot2::geom_point(alpha = 0.50, shape = 16, size = 1) + 
    ggforce::geom_autodensity() +
    ggplot2::geom_density2d() +
    ggforce::facet_matrix(
      rows = gt::vars(gt::everything()), layer.diag = 2, layer.upper = 3, 
      grid.y.diag = FALSE) +
    ggplot2::theme_minimal()
  
  # Save PNG file to disk
  ggplot2::ggsave(
    filename = "temp_matrix_ggplot.png",
    plot = plot_matrix,
    device = "png",
    dpi = 300,
    width = length(col_names),
    height = length(col_names)
  )
  
  # Wait longer for file to be written on async filesystems
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
    
  data_corr <- 
    data %>%
    dplyr::select(dplyr::one_of(columns_numeric))
  
  corr_pearson <- stats::cor(data_corr, method = "pearson", use = "pairwise.complete.obs")
  corr_kendall <- stats::cor(data_corr, method = "kendall", use = "pairwise.complete.obs")
  corr_spearman <- stats::cor(data_corr, method = "spearman", use = "pairwise.complete.obs")
  
  labels_vec <- seq_along(columns_numeric)
  names(labels_vec) <- columns_numeric
  labels_list <- as.list(labels_vec)
  
  labels_notes <- 
    paste(
      paste0("**", seq_along(columns_numeric), ":**&nbsp;<code>", columns_numeric, "</code>"),
      collapse = ", "
    )
  
  probe_corr_pearson <- 
    get_corr_matrix_gt_tbl(
      corr_mat = corr_pearson,
      labels_vec = labels_vec,
      labels_list = labels_list,
      labels_notes = labels_notes
    )
  
  probe_corr_kendall <-
    get_corr_matrix_gt_tbl(
      corr_mat = corr_kendall,
      labels_vec = labels_vec,
      labels_list = labels_list,
      labels_notes = labels_notes
    )
  
  probe_corr_spearman <-
    get_corr_matrix_gt_tbl(
      corr_mat = corr_spearman,
      labels_vec = labels_vec,
      labels_list = labels_list,
      labels_notes = labels_notes
    )
    
  list(
    probe_corr_pearson  = probe_corr_pearson,
    probe_corr_kendall  = probe_corr_kendall, 
    probe_corr_spearman = probe_corr_spearman
  )
}

get_corr_matrix_gt_tbl <- function(corr_mat,
                                   labels_vec,
                                   labels_list,
                                   labels_notes) {
  
  corr_mat %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(`::labels::` = labels_vec) %>%
    dplyr::select(`::labels::`, dplyr::everything()) %>%
    gt::gt() %>%
    gt::cols_label(
      .list = labels_list
    ) %>%
    gt::cols_label(
      `::labels::` = ""
    ) %>%
    gt::cols_align(align = "center") %>%
    gt::data_color(
      columns = gt::one_of(names(labels_vec)),
      colors = scales::col_numeric(
        palette = c("blue", "white", "red"),
        domain = c(-1, 1))
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = gt::one_of(names(labels_vec))),
      fn = function(x) "<br>"
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = 1),
      fn = function(x) paste0(x, "&nbsp;")
    ) %>%
    gt::opt_table_lines("none") %>%
    gt::tab_style(
      style = gt::cell_text(size = "x-small", align = "right", weight = "bold"),
      locations = gt::cells_body(columns = 1)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = "x-small", weight = "bold"),
      locations = gt::cells_column_labels(columns = TRUE)
    ) %>%
    gt::tab_source_note(source_note = gt::md(labels_notes)) %>%
    gt::cols_width(
      gt::everything() ~ gt::px(35)
    )
}

get_corr_matrix_legend <- function() {
  
  dplyr::tibble(color = seq(1, -1, by = -0.25),) %>%
    gt::gt() %>%
    gt::tab_style(
      locations = gt::cells_body(columns = TRUE),
      style = gt::cell_text(size = gt::px(10))
    ) %>%
    gt::data_color(
      columns = gt::vars(color),
      colors = scales::col_numeric(
        palette = c("blue", "white", "red"),
        domain = c(-1, 1))
    ) %>%
    gt::opt_table_lines("none") %>%
    gt::tab_options(column_labels.hidden = TRUE) %>%
    gt::cols_width(
      gt::everything() ~ gt::px(40)
    ) %>%
    gt::opt_table_lines(extent = "all") %>%
    gt::text_transform(
      locations = gt::cells_body(columns = TRUE),
      fn = function(x) {
        gsub("-", "&ndash;", x)
      }
    )
}

# TODO: missing_matrix, missing_heatmap
# TODO: report missing values based on user input (e.g., "9999", empty strings)

probe_missing <- function(data) {
  
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  
  col_names <- colnames(data)

  missing <- 
    data %>% 
    dplyr::mutate(`::cut_group::` = cut(seq(nrow(data)), breaks = 20)) %>% 
    dplyr::group_by(`::cut_group::`) %>% 
    dplyr::summarize_all(~ sum(is.na(.))) %>%
    dplyr::select(-1)
  
  cols_any_missing <-
    col_names[
      missing %>%
        dplyr::group_by() %>%
        dplyr::summarize_all(~ sum(.)) %>%
        dplyr::mutate_all(~ . > 0) %>%
        t() %>% .[, 1]
    ]
  
  data_vals_per_bin <- (n_rows / 20) %>% floor()
    
  probe_missing <-
    missing %>%
    gt::gt() %>%
    gt::data_color(
      columns = TRUE,
      colors = scales::col_numeric(
        palette = c("lightblue", "gray35", "black"),
        domain = c(0, 1, data_vals_per_bin))
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(),
      fn = function(x) ""
    ) %>%
    gt::tab_options(
      table_body.vlines.style = "solid",
      column_labels.vlines.style = "dotted",
      table_body.hlines.style = "none") %>%
    gt::summary_rows(
      columns = TRUE,
      fns = list(
        PCT = ~ sum(.) / n_rows
      ),
      formatter = gt::fmt_percent,
      decimals = 1
    ) %>%
    gt::tab_stubhead(label = gt::md(paste0(n_rows, "<br>ROWS"))) %>%
    gt::tab_style(
      locations = gt::cells_stubhead(),
      style = gt::cell_text(
        transform = "capitalize",
        align = "right",
        size = "x-small"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_stub(), 
      style = gt::cell_borders(
        sides = c("left", "right", "top"),
        style = "solid",
        color = "white"
      ) 
    )
  
  if (length(cols_any_missing) > 0) {
    
    probe_missing <-
      probe_missing %>%
      gt::tab_style(
        locations = gt::cells_body(columns = cols_any_missing, rows = 1), 
        style = gt::cell_borders(sides = "top", color = "red", weight = gt::px(2))
      )
  }

  list(probe_missing = probe_missing)
}

probe_sample <- function(data) {
  
  probe_sample <-
    data %>%
    gt::gt_preview(top_n = 5, bottom_n = 5) %>%
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
                                   tbl_name) {
  
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
  
  bootstrap_lib <- readr::read_file(file = file.path("inst", "lib", "bootstrap", "css", "bootstrap.min.css"))
  jquery_lib <- readr::read_file(file = file.path("inst", "lib", "jquery", "jquery-1.12.4.min.js"))
  extra_js <- readr::read_file(file = file.path("inst", "javascript", "toggle_anchor.js"))
  extra_css <- readr::read_file(file = file.path("inst", "css", "extra.css"))
  
  examination_page <- 
    htmltools::tagList(
      htmltools::HTML("<!doctype html>"),
      htmltools::tags$html(
        lang = "en",
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
          htmltools::tags$a(
            class = "anchor-pos", id = "top"
          ),
          navbar(),
          htmltools::tags$div(
            class = "content",
            htmltools::tags$div(
              class = "container",
              # Use `probe_*()` functions to generate row headers and section items
              probe_overview_stats_assemble(data = data, tbl_name = tbl_name),
              probe_columns_assemble(data = data),
              probe_interactions_assemble(data = data),
              probe_correlations_assemble(data = data),
              probe_missing_assemble(data = data),
              probe_sample_assemble(data = data)
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
                    paste0(
                      "Table scan generated with ",
                      htmltools::tags$a(
                        href = "https://www.github.com/rich-iannone/pointblank",
                        "pointblank"
                      ),
                      "."
                    )
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

probe_overview_stats_assemble <- function(data, tbl_name) {
 
  header <- if (is.na(tbl_name)) "Overview" else paste0("Overview of <code>", tbl_name, "</code>")
  
  row_header <- row_header(id = "overview", header = htmltools::HTML(header))
  
  overview_stats <- probe_overview_stats(data = data)
  
  htmltools::tagList(
    row_header,
    htmltools::tags$div(
      class = "section-items",
      htmltools::tags$div(
        class = "row spacing",
        htmltools::tags$ul(
          class = "nav nav-pills",
          role = "tablist",
          nav_pill_li(label = "Overview", id = "overview-dataset_overview", active = TRUE),
          nav_pill_li(label = "Reproducibility", id = "overview-reproducibility", active = FALSE)
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
                title = "Table Overview",
                content = overview_stats$data_overview_gt
              ),
              panel_component(
                size = 6,
                title = "Column Types",
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
                title = "Reproducibility Information",
                content = overview_stats$reproducibility_gt
              )
            )
          )
        )
      )
    )
  )
}

probe_columns_assemble <- function(data) {
  
  row_header <- row_header(id = "variables", header = "Variables")
  
  columns_data <- probe_columns(data = data)
  
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
                      "Toggle details"
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
                            "Statistics"
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
                            "Common Values"
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
                            "Max/Min Slices"
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
                              "Quantile Statistics"
                            ),
                            x$column_quantile_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              "Descriptive Statistics"
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
                              "Common Values"
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
                              "Maximum Values"
                            ),
                            x$column_top_slice_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-6",
                            htmltools::tags$p(
                              class = "h4",
                              "Minimum Values"
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
                      "Toggle details"
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
                            "Common Values"
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
                            "String Lengths"
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
                              "Common Values"
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
                              "String Lengths"
                            ),
                            x$column_nchar_gt
                          ),
                          htmltools::tags$div(
                            class = "col-sm-8",
                            htmltools::tags$p(
                              class = "h4",
                              "Histogram"
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


probe_interactions_assemble <- function(data) {
  
  row_header <- row_header(id = "interactions", header = "Interactions")
  
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


probe_correlations_assemble <- function(data) {
  
  row_header <- row_header(id = "correlations", header = "Correlations")
  
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
                size = 10,
                title = NULL,
                content = correlations_data$probe_corr_pearson
              ),
              panel_component(
                size = 2,
                title = NULL,
                content = get_corr_matrix_legend()
              )
            )
          ),
          tab_panel(
            id = "correlations-kendall",
            active = FALSE,
            panel_component_list = list(
              panel_component(
                size = 10,
                title = NULL,
                content = correlations_data$probe_corr_kendall
              ),
              panel_component(
                size = 2,
                title = NULL,
                content = get_corr_matrix_legend()
              )
            )
          ),
          tab_panel(
            id = "correlations-spearman",
            active = FALSE,
            panel_component_list = list(
              panel_component(
                size = 10,
                title = NULL,
                content = correlations_data$probe_corr_spearman
              ),
              panel_component(
                size = 2,
                title = NULL,
                content = get_corr_matrix_legend()
              )
            )
          )
        )
      )
    )
  )
}

probe_missing_assemble <- function(data) {
  
  row_header <- row_header(id = "missing", header = "Missing Values")
  
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

probe_sample_assemble <- function(data) {
  
  row_header <- row_header(id = "sample", header = "Sample")
  
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

navbar <- function() {
  
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
          "Table Scan"
        ),
      ),
      htmltools::tags$div(
        id = "navbar",
        class = "navbar-collapse collapse",
        htmltools::tags$ul(
          class = "nav navbar-nav navbar-right",
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#overview",
              "Overview"
            )
          ),
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#variables",
              "Variables"
            )
          ),
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#interactions",
              "Interactions"
            )
          ),
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#correlations",
              "Correlations"
            )
          ),
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#missing",
              "Missing Values"
            )
          ),
          htmltools::tags$li(
            htmltools::tags$a(
              class = "anchor",
              href = "#sample",
              "Sample"
            )
          )
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
