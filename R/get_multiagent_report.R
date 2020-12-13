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


#' Get a summary report using multiple agents
#' 
#' @description 
#' We can get an informative summary table from a collective of agents by using
#' the `get_multiagent_report()` function. The table can be provided in either
#' of two very different forms: as a **gt** based display table (the default),
#' or, as a tibble with packed columns. The display table variant of the
#' multiagent report, the default form, will have the following columns:
#' 
#' \itemize{
#' \item STEP: the SHA1 hash for the validation step, possibly shared among
#' several interrogations.
#' \item *subsequent columns*: each column beyond `STEP` represents a separate
#' interrogation from an *agent* object. The time stamp for the completion of
#' each interrogation is shown as the column label.
#' }
#' 
#' Each step is represented with an icon standing in for the name of the
#' validation function and the associated SHA1 hash. This is a highly
#' trustworthy way for ascertaining which validation steps are effectively
#' identical across interrogations. This way of organizing the report is
#' beneficial because different agents may have used different steps and we want
#' to track the validation results where the validation step doesn't change but
#' the target table does.
#' 
#' @param multiagent A multiagent object of class `ptblank_multiagent`.
#' @param display_table Should a display table be generated? If `TRUE` (the
#'   default) a display table for the report will be shown in the Viewer. If
#'   `FALSE` then a tibble will be returned.
#' 
#' @return A **gt** table object if `display_table = TRUE` or a tibble if
#'   `display_table = FALSE`.
#'
#' @export
get_multiagent_report <- function(multiagent,
                                  display_table = TRUE) {

  for (i in seq_along(multiagent[["agents"]])) {
    
    time_end <- gsub(" ", "T", multiagent[["agents"]][[i]][["time_end"]])
    
    i_name <- paste0(formatC(i, width = 3, flag = 0), "_", time_end)
    i_name <- rlang::quo_name(i_name)
    
    val_set_packed <- 
      multiagent[["agents"]][[i]][["validation_set"]] %>%
      dplyr::select(
        sha1, i, step_id, assertion_type, column, values,
        na_pass, preconditions, actions, label, brief,
        eval_active, eval_error, eval_warning, all_passed,
        n, f_passed, f_failed, warn, notify, stop
      ) %>%
      tidyr::pack(validation = c(
        i, step_id, assertion_type, column, values,
        na_pass, preconditions, actions, label, brief,
        eval_active, eval_error, eval_warning, all_passed,
        n, f_passed, f_failed, warn, notify, stop
      )) %>%
      dplyr::rename(!!i_name := "validation")
    
    if (i == 1) {
      report_tbl <- val_set_packed
      next
    }
    
    report_tbl <-
      report_tbl %>%
      dplyr::full_join(val_set_packed, by = "sha1")
  }
  
  report_tbl <-
    report_tbl %>%
    dplyr::select(-sha1)
  
  if (!display_table) {
    return(report_tbl)
  }

  # nocov start

  # Generate table type HTML
  table_type <- 
    create_table_type_html(
      tbl_src = multiagent[["agents"]][[i]][["tbl_src"]],
      tbl_name = multiagent[["agents"]][[i]][["tbl_name"]]
    )
  
  # Combine label, table type, and action levels into
  # a table subtitle <div>
  combined_subtitle <-
    htmltools::tagList(
      htmltools::tags$div(
        style = htmltools::css(
          height = "25px"
        ),
        htmltools::HTML(table_type)
      ) 
    ) %>% as.character()

  
  create_report_time_html <- function(time) {
    
    time <- Sys.time()
    
    paste0(
      "<span style=\"background-color: #FFF;",
      "color: #444;padding: 0.5em 0.5em;",
      "position: inherit;text-transform: uppercase;margin-left: 10px;",
      "border: solid 1px #999999;font-variant-numeric: tabular-nums;",
      "border-radius: 0;padding: 2px 10px 2px 10px;font-size: smaller;\">",
      format(time, "%Y-%m-%d %H:%M:%S %Z"),
      "</span>"
    )
  }
  
  # Create vector that gathers date-time values for
  # each interrogation
  date_time <- list()
  
  agent_count <- length(multiagent[["agents"]])
  
  if (agent_count <= 4) {
    layout_type <- "4UP"
  } else if (agent_count <= 8) {
    layout_type <- "8UP"
  } else {
    layout_type <- "16UP"
  }
  
  for (i in seq_along(multiagent[["agents"]])) {
    
    time_end <- gsub(" ", "\n", multiagent[["agents"]][[i]][["time_end"]])
    
    i_name <- formatC(i, width = 3, flag = 0)
    i_name <- rlang::quo_name(i_name)
    
    date_time_el <- list(paste0(time_end))
    names(date_time_el) <- i_name
    
    date_time <- c(date_time, date_time_el)
    
    val_set <- 
      multiagent[["agents"]][[i]][["validation_set"]] %>%
      dplyr::select(
        sha1, i, eval_active, eval_error, eval_warning, all_passed,
        n, f_passed, f_failed, warn, notify, stop
      )
    
    html_cells <- c()
    
    for (j in seq_len(nrow(val_set))) {
      
      vals_step_j <- 
        val_set %>%
        dplyr::select(-sha1) %>%
        .[j, ] %>%
        as.list()
      
      cell_content <-
        generate_cell_content(
          layout_type = layout_type,
          vals_step = vals_step_j
        )
      
      html_cells <- c(html_cells, cell_content)
    }

    val_set <- 
      val_set %>%
      dplyr::mutate(!!i_name := html_cells) %>%
      dplyr::select(
        -c(
          i, eval_active, eval_error, eval_warning, all_passed,
          n, f_passed, f_failed, warn, notify, stop
        )
      )
    
    if (i == 1) {
      report_tbl <- val_set
      next
    }
    
    report_tbl <-
      report_tbl %>%
      dplyr::full_join(val_set, by = "sha1")
  }
  
  if (i < 4) {
    # 4UP display
    i_names <- formatC((i + 1):4, width = 3, flag = 0)
    
    for (j in seq_along(i_names)) {
      
      i_name <- rlang::quo_name(i_names[j])
      
      report_tbl <-
        report_tbl %>%
        dplyr::bind_cols(
          dplyr::tibble(pb_extra_ = rep(NA_character_, nrow(report_tbl)))
        ) %>%
        dplyr::rename(!!i_name := "pb_extra_")
    }

    empty_cols <- i_names
    
  } else if (i > 4 && i < 8) {
    # 8UP display
    i_names <- formatC((i + 1):8, width = 3, flag = 0)
    
    for (j in seq_along(i_names)) {
      
      i_name <- rlang::quo_name(i_names[j])
      
      report_tbl <-
        report_tbl %>%
        dplyr::bind_cols(
          dplyr::tibble(pb_extra_ = rep(NA_character_, nrow(report_tbl)))
        ) %>%
        dplyr::rename(!!i_name := "pb_extra_")
    }
    
    empty_cols <- i_names
    
  } else if (i > 8 && i < 16)  {
    
    i_names <- formatC((i + 1):16, width = 3, flag = 0)
    
    for (j in seq_along(i_names)) {
      
      i_name <- rlang::quo_name(i_names[j])
      
      report_tbl <-
        report_tbl %>%
        dplyr::bind_cols(
          dplyr::tibble(pb_extra_ = rep(NA_character_, nrow(report_tbl)))
        ) %>%
        dplyr::rename(!!i_name := "pb_extra_")
    }
    
    empty_cols <- i_names
    
  } else {
    empty_cols <- NULL
  }

  if (layout_type == "16UP") {
    
    mean_f_passed <- 
      vapply(
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE,
        multiagent[["agents"]],
        FUN = function(x) {
          mean_f_passed <- 
            mean(x$validation_set$f_passed, na.rm = TRUE)
          
          if (is.nan(mean_f_passed)) {
            mean_f_passed <- NA_real_
          }
          
          mean_f_passed
        }
      )
    
    green_vals <- floor(mean_f_passed * 255)
    red_vals <- floor((1 - mean_f_passed) * 255)
    blue_vals <- 
      floor(round(1 - (abs((255 / 2) - green_vals) / (255 / 2)), 1) * 255)
    
    offsets <- seq(from = 100 / i, to = 100, by = 100 / i) - ((100 / i) / 2)
    
    stop_tags <- 
      paste0(
        "<stop offset=\"",
        offsets, "%\" style=\"stop-color:rgb(",
        red_vals, ",", green_vals, ",", blue_vals, ");stop-opacity:1\"/>\n"
      )

    gradient_line <-
      paste0(
        "<svg height=\"10\" width=\"", 
        (length(multiagent[["agents"]]) * 50) - 5, "\">\n",
        "<defs>\n",
        "<linearGradient id=\"grad1\" x1=\"0%\" y1=\"0%\" ",
        "x2=\"100%\" y2=\"0%\">\n",
        paste(stop_tags, collapse = ""),
        "</linearGradient>\n",
        "</defs>\n",
        "<rect x=\"0\" y=\"0\" width=\"", 
        (length(multiagent[["agents"]]) * 50) - 5,
        "\" height=\"10\" fill=\"url(#grad1)\" />\n",
        "</svg>"
      )
    
    gradient_line <-
      paste0(
        "<div style=\"padding-bottom: 2px; overflow-x: visible;\">\n",
        gradient_line,
        "</div>"
      )
    
    date_time[[1]] <- paste0(gradient_line, "\n", date_time[[1]])
  }
  
  # Complete the list of column label replacements
  if (!is.null(empty_cols)) {
    
    for (j in seq_along(empty_cols)) {
      
      date_time_el <- list("")
      names(date_time_el) <- empty_cols[j]
      
      date_time <- c(date_time, date_time_el)
    }
    
  }
    
  date_time <- c(list(sha1 = "STEP"), date_time)
  n_columns <- ncol(report_tbl)
  columns_used_tbl <- 1 + seq_len(i)
  columns_not_used <- setdiff(seq(2, n_columns), columns_used_tbl)
  
  # Insert the icon for each step
  assertion_tbl <- 
    dplyr::tibble(
      sha1 = character(0),
      assertion_type = character(0)
    )
  
  for (x in seq_along(multiagent[["agents"]])) {

    assertion_tbl <-
      dplyr::bind_rows(
        assertion_tbl,
        multiagent[["agents"]][[x]][["validation_set"]] %>%
          dplyr::select(sha1, assertion_type)
      )
  }

  report_tbl <-
    report_tbl %>%
    dplyr::left_join(
      assertion_tbl %>% dplyr::distinct(),
      by = "sha1"
    )

  for (x in seq_len(nrow(report_tbl))) {

    report_tbl$sha1[x] <-
      gsub(
        "\\s\\s+", "", 
        paste0(
          "<div>",
          "&nbsp;",
          add_icon_svg(icon = report_tbl$assertion_type[x]),
          "<code style=\"font-size: 8px;\">",
          "&nbsp;", substr(report_tbl$sha1[x], 1, 6),
          "</code>",
          "</div>"
        )
      )
  }
  
  # Overall width should be 875px (agent report is 876px)
  report_tbl <- 
    report_tbl %>%
    dplyr::select(-assertion_type) %>%
    gt::gt(id = "report")

  if (layout_type == "4UP") {
    
    report_tbl <- 
      report_tbl %>%
      gt::cols_width(
        gt::matches("[0-9]{3}") ~ gt::px(200),
        gt::everything() ~ gt::px(75)
      ) %>%
      gt::tab_style(
        list(
          style = gt::cell_text(
            weight = "bold",
            color = "#666666"
          ),
          paste(
            "padding-top: 8px; padding-bottom: 8px;"
          )
        ),
        locations = gt::cells_column_labels(columns = TRUE)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", color = "#666666"),
        locations = gt::cells_column_labels(columns = "sha1")
      ) %>%
      gt::tab_style(
        locations = gt::cells_body(columns = gt::everything()),
        style = "height: 56px; margin: 0;"
      )
    
  } else if (layout_type == "8UP") {

    report_tbl <- 
      report_tbl %>%
      gt::cols_width(
        gt::matches("[0-9]{3}") ~ gt::px(100),
        gt::everything() ~ gt::px(75)
      ) %>%
      gt::tab_style(
        list(
          style = gt::cell_text(
            weight = "bold",
            color = "#666666",
            size = "12px",
            align = "center"
          ),
          paste(
            "line-height: 1.15em; padding-top: 4px;",
            "padding-bottom: 5px;"
          )
        ),
        locations = gt::cells_column_labels(columns = TRUE)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(
          weight = "bold",
          color = "#666666",
          size = "100%",
          v_align = "middle",
          align = "left"
        ),
        locations = gt::cells_column_labels(columns = "sha1")
      ) %>%
      gt::tab_style(
        locations = gt::cells_body(columns = gt::everything()),
        style = "height: 56px; margin: 0;"
      )
    
  } else {
    
    report_tbl <- 
      report_tbl %>%
      gt::cols_width(
        gt::matches("[0-9]{3}") ~ gt::px(50),
        gt::everything() ~ gt::px(75)
      ) %>%
      gt::tab_style(
        list(
          style = gt::cell_text(
            weight = "bold",
            color = "#666666",
            size = "8px"
          ),
          paste(
            "line-height: 1.15em;", "padding-left: 3px;",
            "padding-right: 0; padding-bottom: 3px;",
            "padding-top: 0; height: 30px;",
            "overflow-x: visible;"
          )
        ),
        locations = gt::cells_column_labels(columns = TRUE)
      ) %>%
      gt::tab_style(
        list(
          style = gt::cell_text(
            weight = "bold",
            color = "#666666",
            size = "100%",
            v_align = "middle",
            align = "left"
          ),
          paste(
            "padding-top: 8px; padding-bottom: 8px;"
          )
        ),
        locations = gt::cells_column_labels(columns = "sha1")
      ) %>%
      gt::tab_style(
        locations = gt::cells_body(columns = gt::everything()),
        style = "height: 56px; margin: 0;"
      )
  }
  
  report_tbl <- 
    report_tbl %>%
    gt::tab_header(
      title = "Pointblank Validation Series",
      subtitle = gt::md(combined_subtitle)
    ) %>%
    gt::tab_source_note(
      source_note = gt::md(create_report_time_html(time = Sys.time()))
    ) %>%
    gt::tab_options(
      table.font.size = gt::pct(90),
      row.striping.include_table_body = FALSE
    ) %>%
    gt::fmt_markdown(columns = 2:n_columns) %>%
    gt::fmt_markdown(columns = vars(sha1)) %>%
    gt::fmt_missing(
      columns = columns_used_tbl,
      missing_text = gt::html(
        as.character(
          htmltools::tags$hr(
            style = htmltools::css(
              transform = "rotate(-10deg)",
              border_style = "solid",
              border_width = "1px",
              border_color = "#F5F5F5"
            )
          )
        )
      )
    ) %>%
    gt::fmt_missing(
      columns = columns_not_used,
      missing_text = ""
    ) %>%
    gt::tab_style(
      style = gt::cell_text(font = "'IBM Plex Mono'", size = "small"),
      locations = gt::cells_body(columns = gt::vars(sha1))
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(28),
        weight = 500,
        align = "left",
        color = "#444444"
      ),
      locations = gt::cells_title("title")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        size = gt::px(12),
        align = "left"
      ),
      locations = gt::cells_title("subtitle")
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(sides = "right", color = "#D3D3D3"),
        gt::cell_fill(color = "#FCFCFC")
      ),
      locations = gt::cells_body(columns = vars(sha1))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(sides = "right", color = "#D3D3D3", style = "dotted"),
        gt::cell_fill(color = "#FCFCFC")
      ),
      locations = gt::cells_body(columns = gt::matches("[0-9]{3}"))
    ) %>%
    gt::cols_label(.list = date_time) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_table_font(font = gt::google_font("IBM Plex Sans")) %>%
    gt::opt_css(
      paste0(
        "@import url(\"https://unpkg.com/",
        "balloon-css/balloon.min.css\");"
      )
    ) %>%
    gt::opt_css(
      paste0(
        "@import url(\"https://fonts.googleapis.com/",
        "css2?family=IBM+Plex+Mono&display=swap\");"
      )
    ) %>%
    gt::opt_css(
      css = "
          #pb_information {
            -webkit-font-smoothing: antialiased;
          }
          #report .gt_sourcenote {
            height: 35px;
            padding: 0
          }
          #report code {
            font-family: 'IBM Plex Mono', monospace, courier;
            font-size: 11px;
            background-color: transparent;
            padding: 0;
          }
          #report td {
            overflow: visible;
            margin: 0 !important;
            padding: 0 !important;
          }
          #report .gt_stub {
            overflow: visible;
            font-family: 'IBM Plex Mono', monospace, courier;
            font-size: 11px;
            padding-left: 5px !important;
          }
        "
    )

  if (layout_type == "16UP") {
    report_tbl <-
      report_tbl %>% 
      gt::cols_label(`001` = gt::html(date_time[[2]]))
  }
  
  report_tbl
}

generate_cell_content <- function(layout_type,
                                  vals_step) {
  
  if (!vals_step$eval_active) {
    border_indicator <- "#777777"
  } else if (vals_step$eval_error) {
    border_indicator <- "transparent"
  } else if (vals_step$all_passed) {
    border_indicator <- "#4CA64C"
  } else if (vals_step$notify) {
    border_indicator <- "#499FFE"
  } else if (vals_step$stop) {
    border_indicator <- "#CF142B"
  } else if (vals_step$warn) {
    border_indicator <- "#FFBF00"
  } else {
    border_indicator <- "#A5D2A5"
  }
  
  if (layout_type == "4UP") {
    
    cell_content <- 
      htmltools::tagList(
        htmltools::tags$div(
          style = htmltools::css(
            padding_left = "2px",
            padding_right = "2px",
            padding_top = "2px"
          ),
          htmltools::tags$div(
            style = htmltools::css(
              width = "20%",
              float = "left",
              text_align = "right",
              margin_left = "4px"
            ),
            htmltools::tags$div(
              style = htmltools::css(
                padding_left = "2px",
                padding_right = "2px",
                margin_top = "1px",
                border_color = ifelse(
                  !vals_step$eval_active, "#777777",
                  ifelse(
                    vals_step$eval_error, "#FD9893",
                    ifelse(
                      vals_step$eval_warning, "#FFC880",
                      "#DDD"
                    )
                  )
                ),
                border_width = "1px",
                border_style = "solid",
                border_radius = "0 4px 4px 0",
                border_left_color = border_indicator,
                border_left_width = "2px",
                height = "32px",
                background = ifelse(
                  !vals_step$eval_active, "#E5E5E5",
                  ifelse(
                    vals_step$eval_error, "#FFE9E9",
                    ifelse(
                      vals_step$eval_warning, "#FFEAA2",
                      "white"
                    )
                  )
                )
              ),
              htmltools::tags$span(
                style = htmltools::css(
                  font_size = ifelse(vals_step$i < 100, "x-large", "16px"),
                  color = "#777"
                ),
                vals_step$i
              )
            )
          ),
          htmltools::tags$div(
            style = htmltools::css(
              float = "right",
              width = "75%",
              line_height = "1.1em"
            ),
            htmltools::tags$div(
              style = htmltools::css(
                width = "80%",
                float = "left",
                text_align = "left",
                font_size = "smaller",
                padding_top = "0"
              ),
              htmltools::tags$span(
                htmltools::tags$div(
                  style = htmltools::css(
                    width = "60px",
                    display = "inline-block"
                  ),
                  htmltools::tags$span(
                    style = htmltools::css(
                      color = ifelse(
                        !vals_step$eval_active, "inherit", "green"
                      ),
                      font_size = "x-small"
                    ),
                    "PASS"
                  ),
                  htmltools::tags$code(
                    htmltools::HTML(
                      ifelse(
                        !vals_step$eval_active,
                        "&mdash;",
                        ifelse(
                          !is.na(vals_step$f_passed),
                          pb_fmt_number(
                            vals_step$f_passed,
                            decimals = 2,
                          ),
                          "&mdash;"
                        )
                      )
                    )
                  )
                ),
                htmltools::tags$div(
                  style = htmltools::css(
                    width = "50px",
                    display = "inline-block"
                  ),
                  htmltools::tags$span(
                    style = htmltools::css(
                      color = ifelse(!vals_step$eval_active, "inherit", "red"),
                      font_size = "x-small"
                    ),
                    "FAIL"
                  ),
                  htmltools::tags$code(
                    htmltools::HTML(
                      ifelse(
                        !vals_step$eval_active,
                        "&mdash;",
                        ifelse(
                          !is.na(vals_step$f_failed),
                          pb_fmt_number(
                            vals_step$f_failed,
                            decimals = 2,
                          ),
                          "&mdash;"
                        )
                      )
                    )
                  )
                )
              )
            ),
            htmltools::tags$div(
              style = htmltools::css(
                width = "80%",
                float = "left",
                text_align = "left",
                font_size = "smaller",
                padding_bottom = "5px"
              ),
              htmltools::tags$div(
                style = htmltools::css(
                  width = "30px",
                  display = "inline-block",
                  margin_right = "8px"
                ),
                htmltools::tags$span(
                  style = htmltools::css(
                    border = "solid 1px #D5D5D5",
                    padding_left = "3px",
                    padding_right = "1px",
                    margin_right = "4px",
                    font_size = "smaller"
                  ),
                  htmltools::tags$code("W")
                ),
                htmltools::tags$span(
                  htmltools::HTML(
                    ifelse(
                      is.na(vals_step$warn),
                      "&ndash;",
                      ifelse(
                        !is.na(vals_step$warn) && vals_step$warn,
                        paste0(
                          "<span style=\"color: #F2AA3B; font-size: 11px;\">",
                          "&#9679;</span>"
                        ),
                        paste0(
                          "<span style=\"color: #999; font-size: smaller;\">",
                          "&#9711;</span>"
                        )
                      )
                    )
                  )
                )
              ),
              htmltools::tags$div(
                style = htmltools::css(
                  width = "30px",
                  display = "inline-block",
                  margin_right = "8px"
                ),
                htmltools::tags$span(
                  style = htmltools::css(
                    border = "solid 1px #D5D5D5",
                    padding_left = "3px",
                    padding_right = "1px",
                    margin_right = "4px",
                    font_size = "smaller"
                  ),
                  htmltools::tags$code("S")
                ),
                htmltools::tags$span(
                  htmltools::HTML(
                    ifelse(
                      is.na(vals_step$stop),
                      "&ndash;",
                      ifelse(
                        !is.na(vals_step$stop) && vals_step$stop,
                        paste0(
                          "<span style=\"color: #CE5B4D; font-size: 11px;\">",
                          "&#9679;</span>"
                        ),
                        paste0(
                          "<span style=\"color: #999; font-size: smaller;\">",
                          "&#9711;</span>"
                        )
                      )
                    )
                  )
                )
              ),
              htmltools::tags$div(
                style = htmltools::css(
                  width = "30px",
                  display = "inline-block"
                ),
                htmltools::tags$span(
                  style = htmltools::css(
                    border = "solid 1px #D5D5D5",
                    padding_left = "3px",
                    padding_right = "1px",
                    margin_right = "4px",
                    font_size = "smaller"
                  ),
                  htmltools::tags$code("N")
                ),
                htmltools::tags$span(
                  htmltools::HTML(
                    ifelse(
                      is.na(vals_step$notify),
                      "&ndash;",
                      ifelse(
                        !is.na(vals_step$notify) && vals_step$notify,
                        paste0(
                          "<span style=\"color: #52ACE6; font-size: 11px;\">",
                          "&#9679;</span>"
                        ),
                        paste0(
                          "<span style=\"color: #999; font-size: smaller;\">",
                          "&#9711;</span>"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) %>% 
      as.character()
    
    return(cell_content) 
  }
  
  if (layout_type == "8UP") {

    if (border_indicator == "transparent") {
      border_indicator <- "#FD9893"
    }
    
    if (!vals_step$eval_active) {
      background_color <- "#F5F5F5"
    } else if (vals_step$eval_error) {
      background_color <- "#FFE9E9"
    } else if (vals_step$eval_warning) {
      background_color <- "#FFEECA"
    } else {
      background_color <- "none"
    }

    cell_content <- 
      htmltools::tagList(
        htmltools::tags$div(
          style = htmltools::css(
            padding_left = "2px",
            padding_right = "2px",
            background_color = background_color
          ),
          htmltools::tags$div(
            style = htmltools::css(
              width = "100%",
              text_align = "center"
            ),
            htmltools::tags$div(
              htmltools::tags$span(
                style = htmltools::css(
                  font_size = "14px",
                  color = "#777"
                ),
                vals_step$i
              ),
              htmltools::tags$hr(
                style = htmltools::css(
                  border_style = "solid",
                  border_width = "1px",
                  border_color = border_indicator,
                  margin = "1px"
                )
              )
            )
          ),
          htmltools::tags$div(
            htmltools::tags$div(
              style = htmltools::css(
                width = "100%",
                font_size = "x-small",
                text_align = "center",
                padding_top = "2px"
              ),
              htmltools::tags$span(
                htmltools::tags$span(
                  style = htmltools::css(
                    color = ifelse(!vals_step$eval_active, "inherit", "green"),
                    font_size = "8px"
                  ),
                  "PASS"
                ),
                htmltools::tags$code(
                  style = htmltools::css(
                    font_size = "smaller"
                  ),
                  htmltools::HTML(
                    ifelse(
                      !vals_step$eval_active,
                      "&mdash;",
                      ifelse(
                        !is.na(vals_step$f_passed),
                        pb_fmt_number(
                          vals_step$f_passed,
                          decimals = 2,
                        ),
                        "&mdash;"
                      )
                    )
                  )
                ),
                htmltools::HTML("&nbsp;"),
                htmltools::tags$span(
                  style = htmltools::css(
                    color = ifelse(!vals_step$eval_active, "inherit", "red"),
                    font_size = "8px"
                  ),
                  "FAIL"
                ),
                htmltools::tags$code(
                  style = htmltools::css(
                    font_size = "smaller"
                  ),
                  htmltools::HTML(
                    ifelse(
                      !vals_step$eval_active,
                      "&mdash;",
                      ifelse(
                        !is.na(vals_step$f_failed),
                        pb_fmt_number(
                          vals_step$f_failed,
                          decimals = 2,
                        ),
                        "&mdash;"
                      )
                    )
                  )
                )
              )
            ),
            htmltools::tags$div(
              style = htmltools::css(
                width = "100%",
                font_size = "x-small",
                text_align = "center",
                padding_top = "0",
                padding_bottom = "4px"
              ),
              htmltools::tags$span(
                style = htmltools::css(
                  font_family = "'IBM Plex Mono'"
                ),
                "W"
              ),
              htmltools::tags$span(
                htmltools::HTML(
                  ifelse(
                    is.na(vals_step$warn),
                    "&mdash;",
                    ifelse(
                      !is.na(vals_step$warn) && vals_step$warn,
                      "<span style=\"color: #F2AA3B;\">&#9679;</span>",
                      paste0(
                        "<span style=\"color: #999; font-size: smaller;\">",
                        "&#9711;</span>"
                      )
                    )
                  )
                )
              ),
              htmltools::HTML("&nbsp;"),
              htmltools::tags$span(
                style = htmltools::css(
                  font_family = "'IBM Plex Mono'"
                ),
                "S"
              ),
              htmltools::tags$span(
                htmltools::HTML(
                  ifelse(
                    is.na(vals_step$stop),
                    "&mdash;",
                    ifelse(
                      !is.na(vals_step$stop) && vals_step$stop,
                      "<span style=\"color: #CE5B4D;\">&#9679;</span>",
                      paste0(
                        "<span style=\"color: #999; font-size: smaller;\">",
                        "&#9711;</span>"
                      )
                    )
                  )
                )
              ),
              htmltools::HTML("&nbsp;"),
              htmltools::tags$span(
                style = htmltools::css(
                  font_family = "'IBM Plex Mono'"
                ),
                "N"
              ),
              htmltools::tags$span(
                htmltools::HTML(
                  ifelse(
                    is.na(vals_step$notify),
                    "&mdash;",
                    ifelse(
                      !is.na(vals_step$notify) && vals_step$notify,
                      "<span style=\"color: #52ACE6;\">&#9679;</span>",
                      paste0(
                        "<span style=\"color: #999; font-size: smaller;\">",
                        "&#9711;</span>"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) %>% 
      as.character()
    
    return(cell_content)
  }
  
  if (layout_type == "16UP") {

    if (border_indicator == "transparent") {
      border_indicator <- "#FD9893"
    }
    
    cell_content <- 
      htmltools::tagList(
          htmltools::tags$div(
            style = htmltools::css(
              width = "100%",
              text_align = "center"
            ),
            htmltools::tags$div(
              htmltools::tags$span(
                style = htmltools::css(
                  font_size = "10px",
                  color = "#777",
                  vertical_align = "top",
                  border_bottom_style = "solid",
                  border_bottom_color = border_indicator,
                  border_bottom_width = "2px",
                  padding_left = "12px",
                  padding_right = "12px",
                  width = "100%"
                ),
                vals_step$i
              )
            )
          ),
          htmltools::tags$div(
            htmltools::tags$div(
              style = htmltools::css(
                width = "100%",
                font_size = "12px",
                text_align = "center",
                padding_top = "0",
                padding_bottom = "2px"
              ),
              htmltools::HTML(
                paste0(
                  "<svg width=\"50\" height=\"10\">",
                  "<rect x=\"0.75\" y=\"0\" height=\"10\" width=\"",
                  vals_step$f_passed * 47, "\"",
                  "style=\"fill: #60C853\"></rect>",
                  "<rect x=\"",
                  (vals_step$f_passed * 47) + 0.75, "\" y=\"0\" ",
                  "height=\"10\" width=\"",
                  vals_step$f_failed * 47, "\"",
                  "style=\"fill: #E35A42\"></rect>",
                  "</svg>"
                )
              ),
              htmltools::HTML(
                paste0(
                  "<svg width=\"50\" height=\"10\">",
                  "<rect x=\"1.25\" y=\"0\" height=\"10\" width=\"14\"",
                  "style=\"stroke: ",
                  ifelse(
                    is.na(vals_step$warn), 
                    "#E5E5E5;",
                    "#F2AA3B;" # color: Tulip Tree
                  ), 
                  "fill: ",
                  ifelse(
                    !is.na(vals_step$warn) && vals_step$warn, 
                    "#F7CC89;",
                    "none;"
                  ), 
                  "stroke-width: 1.5;\">",
                  "</rect>",
                  "<rect x=\"17.25\" y=\"0\" height=\"10\" width=\"14\"",
                  "style=\"stroke: ",
                  ifelse(
                    is.na(vals_step$stop), 
                    "#E5E5E5;",
                    "#CE5B4D;" # color: Valencia
                  ),
                  "fill: ",
                  ifelse(
                    !is.na(vals_step$stop) && vals_step$stop, 
                    "#E19C94;",
                    "none;"
                  ), 
                  "stroke-width: 1.5;\">",
                  "</rect>",
                  "<rect x=\"33.25\" y=\"0\" height=\"10\" width=\"14\"",
                  "style=\"stroke: ",
                  ifelse(
                    is.na(vals_step$notify), 
                    "#E5E5E5;",
                    "#52ACE6;" # color: Picton Blue
                  ),
                  "fill: ",
                  ifelse(
                    !is.na(vals_step$notify) && vals_step$notify, 
                    "#97CDF0;",
                    "none;"
                  ), 
                  "stroke-width: 1.5;\">",
                  "</rect>",
                  "</svg>"
                )
              )
            )
          )
      ) %>% 
      as.character()
    
    return(cell_content)
  }
  
  # nocov end
}
