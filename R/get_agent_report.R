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
#' \item regex: the regex used for a `col_vals_regex()` validation step
#' \item preconds: a logical value indicating whether any preconditions where
#' applied before interrogation
#' \item units: the total number of validation units for the validation step
#' \item n_pass: the number of validation units that received a *pass*
#' \item f_pass: the fraction of validation units that received a *pass*
#' \item W: a logical value stating whether the `warn` state was entered
#' \item S: a logical value stating whether the `stop` state was entered
#' \item N: a logical value stating whether the `notify` state was entered
#' \item extract: a logical value that indicates whether a data extract is
#' available for the validation step
#' }
#' If the **gt** package is installed (and if `display_table = TRUE`) then a
#' **gt** table will be displayed with the same information.
#' 
#' @param agent An agent object of class `ptblank_agent`.
#' @param display_table Should a display table be generated? If `TRUE` (the
#'   default), and if the **gt** package is installed, a display table for the
#'   report will be shown in the Viewer. If `FALSE`, or if **gt** is not
#'   available, then a tibble will be returned.
#' @param ... Additional options passed to downstream functions.
#' 
#' @return A tibble.
#' 
#' @examples
#' library(dplyr)
#' 
#' # Create a simple table with a
#' # column of numerical values
#' tbl <- tibble(a = c(5, 7, 8, 5))
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
#' agent %>%
#'   get_agent_report(display_table = FALSE)
#' 
#' @family Interrogate and Get Info
#' @section Function ID:
#' 3-3
#' 
#' @export
get_agent_report <- function(agent,
                             display_table = TRUE,
                             ...) {
  
  validation_set <- agent$validation_set
  
  agent_name <- agent$name
  agent_time <- agent$time
  
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
      eval = eval,
      units = validation_set$n,
      n_pass = validation_set$n_passed,
      f_pass = validation_set$f_passed,
      W = validation_set$warn,
      S = validation_set$stop,
      N = validation_set$notify,
      extract = extract_count
    )
  
  # nocov start
  
  if (requireNamespace("gt", quietly = TRUE) && display_table) {
    
    # Reformat `columns`
    columns_upd <- 
      validation_set$column %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          if (is.null(x)) {
            x <- NA_character_
          } else {
            x <- x %>% unlist() %>% strsplit(", ") %>% unlist()
            x <- paste(paste0("&#8643;", x), collapse = ", ")
            x <- paste0("<code>", x, "</code>")
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
          if (is.null(x)) {
            x <- NA_character_
          } else {
            x <- x %>% gsub("~", "&#8643;", .)
            x <- paste(x, collapse = ", ")
            x <- ifelse(nchar(x) > 18, paste0(substr(x, 1, 15), "..."), x)
            x <- gsub(" ", "&nbsp;", x)
            x <- paste0("<code>", x, "</code>")
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
            x <- paste0(
              "<button style=\"background: #FFFFFF; padding: 5px 5px; color: #333333; font-size: 15px; border: none;\"><code>",
              "I", "</code></button>"
            )
          } else {
            x <- x %>% as.character() %>% base::setdiff("~")
            x <- paste0(
              "<button style=\"background: #67C2DC; padding: 3px 3px; color: #FFFFFF; font-size: 15px; border: none;\"><code>",
              length(x), "</code></button>"
            )
            x
          }
        } 
      )

    f_pass_val <- report_tbl$f_pass
    f_pass_val <- ifelse(f_pass_val > 0 & f_pass_val < 0.01, 0.01, f_pass_val)
    f_pass_val <- ifelse(f_pass_val < 1 & f_pass_val > 0.99, 0.99, f_pass_val)
    
    f_fail_val <- 1 - report_tbl$f_pass
    f_fail_val <- ifelse(f_fail_val > 0 & f_fail_val < 0.01, 0.01, f_fail_val)
    f_fail_val <- ifelse(f_fail_val < 1 & f_fail_val > 0.99, 0.99, f_fail_val)
    
    gt_agent_report <- 
      report_tbl %>%
      dplyr::mutate(
        eval = dplyr::case_when(
          eval == "OK" ~ "&#10004;",
          eval == "W + E" ~ "&#9888; + &#128165;",
          eval == "WARNING" ~ "&#9888;",
          eval == "ERROR" ~ "&#128165;"
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
        extract = extract
      ) %>%
      dplyr::select(
        i, type, columns, values, precon, eval, units,
        n_pass, f_pass, n_fail, f_fail, W, S, N, extract
      ) %>%
      gt::gt() %>%
      gt::cols_merge(columns = vars(n_pass, f_pass), hide_columns = vars(f_pass)) %>%
      gt::cols_merge(columns = vars(n_fail, f_fail), hide_columns = vars(f_fail)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(n_pass, n_fail)),
        fn = function(x) {
          dplyr::case_when(
            x == "NA NA"  ~ "&mdash;",
            TRUE ~ paste0("<code>", x, "</code>")
          )
        }
      ) %>%
      gt::cols_label(
        i = "",
        type = "STEP FN",
        columns = "COLUMNS",
        values = "VALUES",
        precon = "TBL",
        eval = "EVAL",
        units = "UNITS",
        n_pass = "PASS",
        n_fail = "FAIL",
        extract = "EXTRACT"
      ) %>%
      gt::tab_header(
        title = "Pointblank Validation",
        subtitle = gt::md(paste0("`", agent_name, " (", agent_time, ")`<br><br>"))
      ) %>%
      gt::tab_options(
        table.font.size = gt::pct(90),
        row.striping.include_table_body = FALSE
      ) %>%
      gt::cols_align(align = "center", columns = gt::vars(precon, eval, W, S, N, extract)) %>%
      gt::fmt_number(columns = gt::vars(units, n_pass, n_fail, f_pass, f_fail), decimals = 0, drop_trailing_zeros = TRUE, suffixing = TRUE) %>%
      gt::fmt_number(columns = gt::vars(f_pass, f_fail), decimals = 2) %>%
      gt::fmt_markdown(columns = gt::vars(columns, values, eval, precon)) %>%
      gt::fmt_missing(columns = vars(units, values, W, S, N, extract)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(W)),
        fn = function(x) {
          dplyr::case_when(
            x == "TRUE"  ~ "<span style=\"color: #FFBF00;\">&#9679;</span>",
            x == "FALSE" ~ "<span style=\"color: #FFBF00;\">&cir;</span>",
            TRUE ~ "&mdash;"
          )
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(S)),
        fn = function(x) {
          dplyr::case_when(
            x == "TRUE"  ~ "<span style=\"color: #CF142B;\">&#9679;</span>",
            x == "FALSE" ~ "<span style=\"color: #CF142B;\">&cir;</span>",
            TRUE ~ "&mdash;"
          )
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(N)),
        fn = function(x) {
          dplyr::case_when(
            x == "TRUE"  ~ "<span style=\"color: #439CFE;\">&#9679;</span>",
            x == "FALSE" ~ "<span style=\"color: #439CFE;\">&cir;</span>",
            TRUE ~ "&mdash;")
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(type)),
        fn = function(x) {
          paste0("<code>", x, "()</code>")
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(units, extract)),
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
        locations = gt::cells_column_labels(columns = TRUE)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", color = "#666666"),
        locations = gt::cells_body(columns = vars(i))
      ) %>%
      gt::cols_width(
        vars(i) ~ px(50),
        vars(type) ~ px(170),
        vars(columns) ~ px(120),
        vars(values) ~ px(140),
        vars(precon) ~ px(30),
        vars(extract) ~ px(75),
        everything() ~ px(50)
      )
    
    return(gt_agent_report)
  }
  
  # nocov end
  
  report_tbl
}
