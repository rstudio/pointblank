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
#' @param display_table Should a display table be generated? If `TRUE`, and if
#'   the **gt** package is installed, a display table for the report will be
#'   shown in the Viewer. If `FALSE`, or if **gt** is not available, then a
#'   tibble will be returned.
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
                             display_table = TRUE) {
  
  validation_set <- agent$validation_set
  
  agent_name <- agent$name
  agent_time <- agent$time
  
  columns <- 
    validation_set$column %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(is.null(x), NA_character_, paste(paste_around(x, "`"), collapse = ", "))
      }
    )
  
  values <- 
    validation_set$values %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(is.null(x), NA_character_, paste(paste_around(x, "`"), collapse = ", "))
      } 
    )

  precon_count <-
    validation_set$preconditions %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        ifelse(is.null(x), NA_character_, paste_around(x %>% rlang::as_function() %>% length(), "`"))
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

    gt_agent_report <- 
      report_tbl %>%
      dplyr::mutate(
        units = paste_around(units, "`"),
        n_pass = paste_around(n_pass, "`"),
        f_pass = paste_around(f_pass, "`"),
        extract = paste_around(extract, "`")
      ) %>%
      gt::gt(rowname_col = "i") %>%
      gt::cols_label(
        type = "Validation Type",
        columns = "Cols",
        values = "Values",
        precon = "Precon",
        units = "Units",
        extract = "Extracts"
      ) %>%
      gt::tab_header(
        title = "Pointblank Validation",
        subtitle = gt::md(paste0("`", agent_name, " (", agent_time, ")`<br><br>"))
      ) %>%
      gt::tab_options(
        table.font.size = gt::pct(90),
        row.striping.include_table_body = FALSE
      ) %>%
      gt::fmt_markdown(columns = vars(columns, values, precon, units, n_pass, f_pass, extract)) %>%
      gt::fmt_missing(columns = everything()) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(W)),
        fn = function(x) {
          dplyr::case_when(
            x == "TRUE"  ~ "<span style=\"color: #FFBF00;\">&#9679;</span>",
            x == "FALSE" ~ "<span style=\"color: #FFBF00;\">&cir;</span>",
            TRUE ~ "&mdash;")
        }
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = vars(S)),
        fn = function(x) {
          dplyr::case_when(
            x == "TRUE"  ~ "<span style=\"color: #CF142B;\">&#9679;</span>",
            x == "FALSE" ~ "<span style=\"color: #CF142B;\">&cir;</span>",
            TRUE ~ "&mdash;")
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
          paste0("<code style=\"font-size:85%;\">", x, "()</code>")
        }
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left", indent = gt::px(5)),
        locations = gt::cells_title("title")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left", indent = gt::px(5)),
        locations = gt::cells_title("subtitle")
      )
    
    return(gt_agent_report)
  }
  
  # nocov end
  
  report_tbl
}
