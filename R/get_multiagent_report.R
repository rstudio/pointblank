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
#' \item STEP: the SHA1 hash for the validation step, possibly shared amongst
#' several interrogations.
#' \item *subsequent columns*: each column beyond `STEP` represents a separate
#' interrogation from an *agent* object. The time stamp for the completion of
#' each interrogation is shown as the column label.
#' }
#' 
#' Each step is represented as a SHA1 hash. This is a highly trustworthy way for
#' ascertaining which validation steps are effectively identical across
#' interrogations.
#' 
#' @param multiagent An multiagent object of class `ptblank_multiagent`.
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
  
  for (i in seq_along(multiagent[["agents"]])) {
    
    time_end <- gsub(" ", "T", multiagent[["agents"]][[i]][["time_end"]])
    
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
      
      vars_step_j <- 
        val_set %>%
        dplyr::select(-sha1) %>%
        .[j, ] %>%
        as.list()
      
      # TODO: Round numbers in `f_passed` and `f_failed`
      
      # TODO: Generate HTML with elements
      cell_content <-
        htmltools::tagList(
          htmltools::tags$div(
            style = htmltools::css(
              padding_left = "2px",
              padding_right = "2px"
            ),
            htmltools::tags$div(
              style = htmltools::css(
                width = "20%",
                float = "left",
                text_align = "right"
              ),
              htmltools::tags$div(
                style = htmltools::css(
                  padding_left = "2px",
                  padding_right = "2px",
                  border_color = "#DDD",
                  border_width = "1px",
                  border_style = "solid"
                ),
                htmltools::tags$span(
                  style = htmltools::css(
                    font_size = "x-large",
                    color = "#999"
                  ),
                  vars_step_j$i
                )
              )
            ),
            htmltools::tags$div(
              style = htmltools::css(
                float = "right",
                width = "75%"
              ),
              htmltools::tags$div(
                style = htmltools::css(
                  width = "80%",
                  float = "left",
                  text_align = "left",
                  font_size = "smaller"
                ),
                htmltools::tags$span(
                  htmltools::tags$span(
                    style = htmltools::css(
                      color = "green"
                    ),
                    
                    htmltools::HTML("&#9679;")
                  ),
                  pb_fmt_number(
                    vars_step_j$f_passed,
                    decimals = 2,
                  ),
                  htmltools::HTML("&nbsp;"),
                  htmltools::tags$span(
                    style = htmltools::css(
                      color = "red"
                    ),
                    htmltools::HTML("&#9679;")
                  ),
                  pb_fmt_number(
                    vars_step_j$f_failed,
                    decimals = 2,
                  ),
                )
              ),
              htmltools::tags$br(),
              htmltools::tags$div(
                style = htmltools::css(
                  width = "80%",
                  float = "left",
                  text_align = "left",
                  font_size = "smaller"
                ),
                htmltools::tags$span(
                  "W"
                ),
                htmltools::tags$span(
                  htmltools::HTML(ifelse(
                    is.na(vars_step_j$warn),
                    "&mdash;", vars_step_j$warn
                  ))
                ),
                htmltools::tags$span(
                  htmltools::HTML("&nbsp;"),
                  "S"
                ),
                htmltools::tags$span(
                  htmltools::HTML(ifelse(
                    is.na(vars_step_j$stop),
                    "&mdash;", vars_step_j$stop
                  ))
                ),
                htmltools::tags$span(
                  htmltools::HTML("&nbsp;"),
                  "N"
                ),
                htmltools::tags$span(
                  htmltools::HTML(ifelse(
                    is.na(vars_step_j$notify),
                    "&mdash;", vars_step_j$notify
                  ))
                )
              )
            )
          )
        ) %>% 
        as.character()
      
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
  
  # TODO: Format `report_tbl` to adhere to 4UP, 8UP, and 16UP+
  # displays of information
  
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
    
  } else {
    empty_cols <- NULL
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
  
  # Overall width should be 875px (agent report is 876px)
  
  report_tbl <-
    report_tbl %>%
    gt::gt(id = "report") %>%
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
    gt::fmt_markdown(columns = 2:ncol(report_tbl)) %>%
    gt::cols_width(
      gt::matches("[0-9]{3}") ~ gt::px(200),
      gt::everything() ~ gt::px(75)
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = gt::vars(sha1)),
      fn = function(x) {
        substr(x, 1, 6)
      }
    ) %>%
    gt::tab_style(
      style = gt::cell_text(font = "monospace", size = "small"),
      locations = gt::cells_body(columns = gt::vars(sha1))
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = "#666666"),
      locations = gt::cells_column_labels(columns = TRUE)
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(columns = gt::everything()),
      style = "height: 40px; margin: 0;"
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
    gt::fmt_missing(columns = gt::everything(), missing_text = "") %>%
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
          #report .gt_row {
            overflow: visible;
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
        "
    )
  
  report_tbl
}
