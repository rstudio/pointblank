

probe_overview_stats <- function(data) {
  
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  
  na_cells <- 
    data %>%
    dplyr::select_if(function(x) any(is.na(x))) %>% 
    dplyr::summarise_each(~ sum(is.na(.))) %>%
    dplyr::mutate(`::all_na::` = sum(.)) %>%
    dplyr::pull(`::all_na::`) %>%
    as.integer()
  
  duplicate_rows <- 
    suppressMessages(
      create_agent(tbl = data) %>%
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
  
  tbl_info <- get_tbl_information(tbl = data)
  
  tbl_src <- tbl_info$tbl_src
  r_col_types <- tbl_info$r_col_types
  
  data_overview_tbl <-
    dplyr::tribble(
      ~label,              ~value,
      "Columns",           glue::glue("<code>{n_cols}</code>", .transformer = get),
      "Rows",              glue::glue("<code>{n_rows}</code>", .transformer = get),
      "<code>NA</code>s",  glue::glue("<code>{na_cells} {na_cells_pct}</code>", .transformer = get),
      "Duplicate Rows",    glue::glue("<code>{duplicate_rows} {duplicate_rows_pct}</code>", .transformer = get),
    )
  
  r_col_types_tbl <- 
    dplyr::tibble(r_col_types = r_col_types) %>%
    dplyr::group_by(r_col_types) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count))
  
  data_overview_gt <-
    gt::gt(data_overview_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_header(title = gt::md("**Table Overview**")) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  r_col_types_gt <-
    r_col_types_tbl %>%
    dplyr::mutate(r_col_types = paste0("<code>", r_col_types, "</code>")) %>%
    dplyr::mutate(count = paste0("<code>", count, "</code>")) %>%
    gt::gt(r_col_types_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_header(title = gt::md("**Column Types**")) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "100%"
    )
  
  list(
    data_overview_gt = data_overview_gt,
    r_col_types_gt = r_col_types_gt
  )
}

#probe_overview_repro
#probe_overview_vital

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
          numeric = probe_columns_numeric(data = data, column = col_name, n_rows = n_rows)
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
  
  column_description_tbl <-
    dplyr::tribble(
      ~label,              ~value,
      "Distinct Units",    glue::glue("<code>{distinct_count} {distinct_pct}</code>", .transformer = get),
      "<code>NA</code>s",  glue::glue("<code>{na_cells} {na_cells_pct}</code>", .transformer = get),
    )
  
  column_description_gt <-
    gt::gt(column_description_tbl) %>%
    gt::fmt_markdown(columns = TRUE) %>%
    gt::tab_options(
      column_labels.hidden = TRUE,
      table.border.top.style = "none",
      table.width = "80%"
    )
  
  column_description_gt
}

probe_columns_character <- function(data, column, n_rows) {

  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_date <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_factor <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_integer <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_logical <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_numeric <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}

probe_columns_posix <- function(data, column, n_rows) {
  
  data_column <- data %>% dplyr::select({{ column }})
  column_description_gt <- get_column_description_gt(data_column = data_column, n_rows = n_rows)
  
  list(column_description_gt = column_description_gt)
}



#probe_interactions

#probe_correlations
#probe_correlations_pearson
#probe_correlations_spearman
#probe_correlations_cramer
#probe_correlations_kendall

#probe_missing
#probe_missing_matrix
#probe_missing_heatmap


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

build_examination_page <- function(data) {
  
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
            probe_overview_stats_assemble(data = data),
            probe_sample_assemble(data = data)
          )
        ),
        htmltools::tags$footer(
          
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
}

probe_overview_stats_assemble <- function(data) {
 
  row_header <- row_header(id = "overview", header = "Overview")
  
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
          nav_pill_li(label = "Reproduction", id = "overview-reproduction", active = FALSE),
          nav_pill_li(label = "Warnings", id = "overview-warnings", active = FALSE),
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
                title = NULL,
                content = overview_stats$data_overview_gt
              ),
              panel_component(
                size = 6,
                title = NULL,
                content = overview_stats$r_col_types_gt
              )
            )
          ),
          tab_panel(
            id = "overview-reproduction",
            active = FALSE,
            panel_component_list = list(
              panel_component(
                size = 12,
                title = NULL,
                content = overview_stats$r_col_types_gt
              )
            )
          ),
          tab_panel(
            id = "overview-warnings",
            active = FALSE,
            panel_component_list = list(
              panel_component(
                size = 12,
                title = NULL,
                content = overview_stats$data_overview_gt
              )
            )
          )
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
          "Examination"
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
              "Missing values"
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
