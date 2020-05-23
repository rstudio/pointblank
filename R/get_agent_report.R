#' Get a simple report from an agent
#'
#' We can get the essential information from an agent by using the
#' `get_agent_report()` function. The amount of fields with intel is different
#' depending on whether or not the agent performed an interrogation (with
#' `interrogate()`). The tibble that is returned has the following columns:
#' \itemize{
#' \item i: the validation step number
#' \item type: the validation type, which mirrors the name of the validation
#' step function
#' \item columns: the names of the columns used in the validation step
#' \item values: the values used in the validation step, where applicable; for
#' a `conjointly()` validation step, this is a listing of all sub-validations
#' \item precon: indicates whether any there are any preconditions to apply
#' before interrogation and, if so, the number of statements used
#' \item active: a logical value that indicates whether a validation step is
#' set to 'active' during an interrogation
#' \item eval: a character value that denotes the result of each validation
#' step functions' evaluation during interrogation
#' \item units: the total number of validation units for the validation step
#' \item n_pass: the number of validation units that received a *pass*
#' \item f_pass: the fraction of validation units that received a *pass*
#' \item W: a logical value stating whether the `warn` state was entered
#' \item S: a logical value stating whether the `stop` state was entered
#' \item N: a logical value stating whether the `notify` state was entered
#' \item extract: a logical value that indicates whether a data extract is
#' available for the validation step
#' }
#' If the **gt** package is installed (and if `display_table = TRUE`, which is
#' the default) then a **gt** table will be displayed with the same information.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' @param arrange_by A choice to arrange the report table rows by the validation
#'   step number (`"i"`, the default), or, to arrange in descending order by
#'   severity of the failure state (with `"severity"`).
#' @param keep An option to keep `"all"` of the report's table rows (the
#'   default), or, keep only those rows that reflect one or more
#'   `"fail_states"`.
#' @param display_table Should a display table be generated? If `TRUE` (the
#'   default), and if the **gt** package is installed, a display table for the
#'   report will be shown in the Viewer. If `FALSE`, or if **gt** is not
#'   available, then a tibble will be returned.
#' @param ... Additional options passed to downstream functions.
#' 
#' @return A gt table object if `display_table = TRUE` or a tibble if
#'   `display_table = FALSE`.
#' 
#' @examples
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- 
#'   dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' # Validate that values in column
#' # `a` are always greater than 4
#' agent <-
#'   create_agent(tbl = tbl) %>%
#'   col_vals_gt(vars(a), 4) %>%
#'   interrogate()
#' 
#' # Get a tibble-based report from the
#' # agent by using `get_agent_report()`
#' # with `display_table = FALSE`
#' agent %>%
#'   get_agent_report(display_table = FALSE)
#'   
#' # View a the report by printing the
#' # `agent` object anytime, but, return a
#' # gt table object by using this with
#' # `display_table = TRUE` (the default)
#' gt_tbl <- get_agent_report(agent)
#' 
#' @family Post-interrogation
#' @section Function ID:
#' 5-1
#' 
#' @export
get_agent_report <- function(agent,
                             arrange_by = c("i", "severity"),
                             keep = c("all", "fail_states"),
                             display_table = TRUE,
                             ...) {

  arrange_by <- match.arg(arrange_by)
  keep <- match.arg(keep)
  
  validation_set <- agent$validation_set
  
  agent_name <- agent$name
  agent_time <- agent$time
  
  lang <- agent$reporting_lang
  
  eval <- 
    validation_set %>%
    dplyr::select(eval_error, eval_warning) %>%
    dplyr::mutate(condition = dplyr::case_when(
      !eval_error & !eval_warning ~ "OK",
      eval_error & eval_warning ~ "W + E",
      eval_error ~ "ERROR",
      eval_warning ~ "WARNING"
    )) %>%
    dplyr::pull(condition)
  
  columns <- 
    validation_set$column %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(
          is.null(x),
          NA_character_,
          unlist(x)
        )
      }
    )
  
  values <- 
    validation_set$values %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(
          is.null(x),
          NA_character_,
          paste(x %>% gsub("~", "", .), collapse = ", ")
        )
      } 
    )
  
  precon_count <-
    validation_set$preconditions %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(
          is.null(x),
          NA_character_,
          x %>% rlang::as_function() %>% length() %>% as.character()
        )
      }
    )
  
  if (!has_agent_intel(agent)) {
    
    extract_count <- rep(NA, nrow(validation_set))
    
  } else {
    
    extract_count <- as.character(validation_set[["i"]]) %in% names(agent$extracts)
    
    extract_count[extract_count == FALSE] <- NA_integer_
    
    extract_count[!is.na(extract_count)] <- 
      vapply(
        agent$extracts,
        FUN.VALUE = integer(1),
        USE.NAMES = FALSE,
        FUN = nrow
      )
  }

  report_tbl <- 
    dplyr::tibble(
      i = validation_set$i,
      type = validation_set$assertion_type,
      columns = columns,
      values = values,
      precon = precon_count,
      active = validation_set$active,
      eval = eval,
      units = validation_set$n,
      n_pass = validation_set$n_passed,
      f_pass = validation_set$f_passed,
      W = validation_set$warn,
      S = validation_set$stop,
      N = validation_set$notify,
      extract = extract_count
    )
  
  report_tbl <-
    report_tbl %>%
    dplyr::mutate(
      eval_pts = ifelse(eval != "OK", 10, 0),
      N_pts = ifelse(!is.na(N) & N, 3, 0),
      S_pts = ifelse(!is.na(S) & S, 2, 0),
      W_pts = ifelse(!is.na(W) & W, 1, 0),
      total_pts = eval_pts + N_pts + S_pts + W_pts
    )
  
  if (arrange_by == "severity") {
    report_tbl <-
      report_tbl %>%
      dplyr::arrange(dplyr::desc(total_pts))
  }
  
  if (keep == "fail_states") {
    report_tbl <- report_tbl %>% dplyr::filter(total_pts > 0)
  }

  report_tbl <-
    report_tbl %>%
    dplyr::select(-dplyr::ends_with("pts"))
  
  # nocov start
  
  if (display_table) {
    
    x_list <- list(...)

    if (length(x_list) > 0) {
      if (x_list$size == "small") {
        scale <- 1.0
        email_table <- TRUE
      }
    } else {
      scale <- 1.0
      email_table <- FALSE
    }
    
    make_button <- function(x, scale, color, background) {
      
      paste0(
        "<button style=\"background: ", background, "; padding: ",
        5 * scale, "px ", 5 * scale, "px; ",
        "color: ", color, "; font-size: ", 15 * scale, "px; border: none;\">",
        x, "</button>"
      )
    }
    
    validation_set <- validation_set[report_tbl$i, ]
  
    # Reformat `columns`
    columns_upd <- 
      validation_set$column %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {

          if (is.null(x) | (is.list(x) && is.na(unlist(x)))) {
            x <- NA_character_
          } else if (is.na(x)) {
            x <- NA_character_
          } else {
            text <- x %>% unlist() %>% strsplit(", ") %>% unlist()
            text <- 
              paste(
                paste0(
                  "<span style=\"color: purple; ",
                  "font-size: bigger;\">&#8643;</span>",
                  text
                ),
                collapse = ", "
              )
            x <- 
              paste0(
                "<div><p style=\"margin-top: 0px;margin-bottom: 0px; ",
                "font-family: monospace; white-space: nowrap; ",
                "text-overflow: ellipsis; overflow: hidden;\">",
                text,
                "</p></div>"
              )
          }
          x
        }
      )

    # Reformat `values`
    values_upd <- 
      validation_set$values %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {

          if (is.list(x) && length(x) > 0 && inherits(x, "col_schema")) {
            
            column_schema_text <- report_column_schema[lang]
            column_schema_type_text <- 
              if (inherits(x, "r_type")) {
                report_r_col_types[lang]
              } else {
                report_r_sql_types[lang]
              }
            
            x <- 
              paste0(
                "<div>",
                "<p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-size: 0.75rem;\">", column_schema_text, "</p>",
                "<p style=\"margin-top: 2px; margin-bottom: 0px; ",
                "font-size: 0.65rem;\">", column_schema_type_text, "</p>",
                "</div>"
              )
            
          } else if (is_call(x)) {
            
            text <- rlang::as_label(x)
            
            x <- 
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-family: monospace; white-space: nowrap; ",
                "text-overflow: ellipsis; overflow: hidden;\">",
                text,
                "</p></div>"
              )
            
          } else if (is.list(x) && length(x) > 0 && !inherits(x, "quosures")) {
            
            step_text <- 
              if (length(x) > 1) {
                paste0(length(x), " ", report_col_steps[lang])
              } else {
                paste0(length(x), " ", report_col_step[lang])
              }
            
            x <- 
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-size: 0.75rem;\">", step_text, "</p></div>"
              )
            
          } else if (is.null(x)) {
            
            x <- NA_character_
            
          } else {
            
            text <-
              x %>%
              tidy_gsub(
                "~",
                "<span style=\"color: purple; font-size: bigger;\">&#8643;</span>"
              ) %>%
              unname()
            
            text <- paste(text, collapse = ", ")
            
            x <- 
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-family: monospace; white-space: nowrap; ",
                "text-overflow: ellipsis; overflow: hidden;\">",
                text,
                "</p></div>"
              )
          }
          x
        } 
      )

    # Reformat `precon`
    precon_upd <- 
      validation_set$preconditions %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {

          if (is.null(x)) {
            x <- 
              make_button(
                x = "&Iscr;",
                scale = scale,
                color = "#333333",
                background = "#FFFFFF"
              )
            
          } else if (rlang::is_formula(x) || rlang::is_function(x)) {

            x <- 
              make_button(
                x = "&#10174;",
                scale = scale,
                color = "#FFFFFF",
                background = "#67C2DC"
              )
          }
          x
        } 
      )

    # Reformat W, S, and N
    W_upd <- 
      validation_set$warn %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          if (is.na(x)) {
            x <- "&mdash;"
          } else if (x == TRUE) {
            x <- "<span style=\"color: #FFBF00;\">&#9679;</span>"
          } else if (x == FALSE) {
            x <- "<span style=\"color: #FFBF00;\">&cir;</span>"
          }
          x
        }
      )
    
    S_upd <- 
      validation_set$stop %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          if (is.na(x)) {
            x <- "&mdash;"
          } else if (x == TRUE) {
            x <- "<span style=\"color: #CF142B;\">&#9679;</span>"
          } else if (x == FALSE) {
            x <- "<span style=\"color: #CF142B;\">&cir;</span>"
          }
          x
        }
      )
    
    N_upd <- 
      validation_set$notify %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          if (is.na(x)) {
            x <- "&mdash;"
          } else if (x == TRUE) {
            x <- "<span style=\"color: #439CFE;\">&#9679;</span>"
          } else if (x == FALSE) {
            x <- "<span style=\"color: #439CFE;\">&cir;</span>"
          }
          x
        }
      )

    f_pass_val <- report_tbl$f_pass
    f_pass_val <- ifelse(f_pass_val > 0 & f_pass_val < 0.01, 0.01, f_pass_val)
    f_pass_val <- ifelse(f_pass_val < 1 & f_pass_val > 0.99, 0.99, f_pass_val)
    f_pass_val <- as.numeric(f_pass_val)
    
    f_fail_val <- 1 - report_tbl$f_pass
    f_fail_val <- ifelse(f_fail_val > 0 & f_fail_val < 0.01, 0.01, f_fail_val)
    f_fail_val <- ifelse(f_fail_val < 1 & f_fail_val > 0.99, 0.99, f_fail_val)
    f_fail_val <- as.numeric(f_fail_val)

    gt_agent_report <- 
      report_tbl %>%
      dplyr::mutate(
        eval_sym = dplyr::case_when(
          eval == "OK" ~ "&check;",
          eval == "W + E" ~ "&#9888; + &#128165;",
          eval == "WARNING" ~ "&#9888;",
          eval == "ERROR" ~ "&#128165;",
          is.na(eval) ~ "&mdash;"
        )
      ) %>%
      dplyr::mutate(
        columns = columns_upd,
        values = values_upd,
        precon = precon_upd,
        units = units,
        n_pass = n_pass,
        n_fail = units - n_pass,
        f_pass = f_pass_val,
        f_fail = f_fail_val,
        W_val = W,
        S_val = S,
        N_val = N,
        W = W_upd,
        S = S_upd,
        N = N_upd,
        extract = extract
      ) %>%
      dplyr::select(
        i, type, columns, values, precon, eval_sym, units,
        n_pass, f_pass, n_fail, f_fail, W, S, N, extract,
        W_val, S_val, N_val, eval, active
      ) %>%
      gt::gt() %>%
      gt::cols_merge(columns = gt::vars(n_pass, f_pass), hide_columns = gt::vars(f_pass)) %>%
      gt::cols_merge(columns = gt::vars(n_fail, f_fail), hide_columns = gt::vars(f_fail)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(n_pass, n_fail)),
        fn = function(x) {
          dplyr::case_when(
            x == "NA NA"  ~ "&mdash;",
            TRUE ~ x %>%
              tidy_gsub(" ", "</code><br><code>") %>%
              paste0("<code>", ., "</code>")
          )
        }
      ) %>%
      gt::cols_label(
        i = "",
        type = report_col_step[lang],
        columns = report_col_columns[lang],
        values = report_col_values[lang],
        precon = "TBL",
        eval_sym = "EVAL",
        units = report_col_units[lang],
        n_pass = "PASS",
        n_fail = "FAIL",
        extract = "EXTRACT"
      ) %>%
      gt::tab_header(
        title = pointblank_validation_title_text[lang],
        subtitle = gt::md(paste0("`", agent_name, " (", agent_time, ")`<br><br>"))
      ) %>%
      gt::tab_options(
        table.font.size = gt::pct(90 * scale),
        row.striping.include_table_body = FALSE
      ) %>%
      gt::cols_align(
        align = "center",
        columns = gt::vars(precon, eval_sym, W, S, N, extract)
      ) %>%
      gt::cols_align(
        align = "center",
        columns = gt::vars(f_pass, f_fail)
      ) %>%
      gt::fmt_number(
        columns = gt::vars(units, n_pass, n_fail, f_pass, f_fail),
        decimals = 0, drop_trailing_zeros = TRUE, suffixing = TRUE
      ) %>%
      gt::fmt_number(columns = gt::vars(f_pass, f_fail), decimals = 2) %>%
      gt::fmt_markdown(columns = gt::vars(columns, values, eval_sym, precon, W, S, N)) %>%
      gt::fmt_missing(columns = gt::vars(columns, values, units, extract)) %>%
      gt::cols_hide(columns = gt::vars(W_val, S_val, N_val, eval, active)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(type)),
        fn = function(x) {
          paste0("<code>", x, "</code>")
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = gt::vars(units, extract)),
        fn = function(x) {
          dplyr::case_when(
            x == "&mdash;" ~ x,
            TRUE ~ paste0("<code>", x, "</code>")
          )
        }
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left", indent = gt::px(5)),
        locations = gt::cells_title("title")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left", indent = gt::px(5)),
        locations = gt::cells_title("subtitle")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", color = "#666666"),
        locations = gt::cells_body(columns = gt::vars(i))
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#F2F2F2", alpha = 0.75),
          gt::cell_text(color = "#8B8B8B")
        ),
        locations = gt::cells_body(
          columns = TRUE,
          rows = active == FALSE
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "#FFC1C1", alpha = 0.35),
        locations = gt::cells_body(
          columns = TRUE,
          rows = eval == "ERROR"
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          color = "#4CA64C",
          weight = gt::px(7)
        ),
        locations = gt::cells_body(
          columns = gt::vars(i),
          rows = units == n_pass
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          color = "#4CA64C66",
          weight = gt::px(5)
        ),
        locations = gt::cells_body(
          columns = gt::vars(i),
          rows = units != n_pass
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          color = "#FFBF00",
          weight = gt::px(7)
        ),
        locations = gt::cells_body(
          columns = gt::vars(i),
          rows = W_val
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = "left",
          color = "#CF142B",
          weight = gt::px(7)
        ),
        locations = gt::cells_body(
          columns = gt::vars(i),
          rows = S_val
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_text(size = gt::px(20)),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(size = gt::px(12)),
        locations = gt::cells_title(groups = "subtitle")
      )
    
    if (!has_agent_intel(agent)) {

      gt_agent_report <-
        gt_agent_report %>%
        gt::text_transform(
          locations = gt::cells_body(
            columns = gt::vars(eval_sym, units, f_pass, f_fail, n_pass, n_fail, W, S, N, extract)
            ),
          fn = function(x) {
            ""
          }
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#F2F2F2"),
          locations = gt::cells_body(
            columns = gt::vars(eval_sym, units, f_pass, f_fail, n_pass, n_fail, W, S, N, extract)
          )
        ) %>%
        gt::tab_header(
          title = gt::md(
            paste0(
              "<div>",
              "<span style=\"float: left;\">", pointblank_validation_plan_text[lang], "</span>",
              "<span style=\"float: right; text-decoration-line: underline; ",
              "font-size: 16px; text-decoration-color: #008B8B;",
              "padding-top: 0.1em; padding-right: 0.4em;\">",
              no_interrogation_performed_text[lang], "</span>",
              "</div>"
            )
          ),
          subtitle = gt::md(paste0("`", agent_name, "`<br><br>"))
        )
    }
    
    if (email_table) {

      gt_agent_report <- 
        gt_agent_report %>%
        gt::cols_hide(gt::vars(columns, values, precon, extract)) %>%
        gt::cols_width(
          gt::vars(i) ~ gt::px(30),
          gt::vars(type) ~ gt::px(170),
          gt::vars(precon) ~ gt::px(30),
          gt::vars(eval_sym) ~ gt::px(40),
          gt::vars(units) ~ gt::px(50),
          gt::vars(n_pass) ~ gt::px(50),
          gt::vars(n_fail) ~ gt::px(50),
          gt::vars(W) ~ gt::px(30),
          gt::vars(S) ~ gt::px(30),
          gt::vars(N) ~ gt::px(30),
          TRUE ~ gt::px(20)
        ) %>%
        gt::tab_options(data_row.padding = gt::px(4)) %>%
        gt::tab_style(
          style = gt::cell_text(size = gt::px(10), weight = "bold", color = "#666666"),
          locations = gt::cells_column_labels(columns = TRUE)
        )
      
    } else {
      
      gt_agent_report <- 
        gt_agent_report %>%
        gt::cols_width(
          gt::vars(i) ~ gt::px(50),
          gt::vars(type) ~ gt::px(170),
          gt::vars(columns) ~ gt::px(120),
          gt::vars(values) ~ gt::px(140),
          gt::vars(precon) ~ gt::px(35),
          gt::vars(extract) ~ gt::px(75),
          gt::vars(W) ~ gt::px(30),
          gt::vars(S) ~ gt::px(30),
          gt::vars(N) ~ gt::px(30),
          TRUE ~ gt::px(50)
        ) %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold", color = "#666666"),
          locations = gt::cells_column_labels(columns = TRUE)
        )
    }

    return(gt_agent_report)
  }
  
  # nocov end
  
  report_tbl
}
