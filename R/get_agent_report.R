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


#' Get a summary report from an agent
#' 
#' @description
#' 
#' We can get an informative summary table from an agent by using the
#' `get_agent_report()` function. The table can be provided in two substantially
#' different forms: as a **gt** based display table (the default), or, as a
#' tibble. The amount of fields with intel is different depending on whether or
#' not the agent performed an interrogation (with the [interrogate()] function).
#' Basically, before [interrogate()] is called, the agent will contain just the
#' validation plan (however many rows it has depends on how many validation
#' functions were supplied a part of that plan). Post-interrogation, information
#' on the passing and failing test units is provided, along with indicators on
#' whether certain failure states were entered (provided they were set through
#' `actions`). The display table variant of the agent report, the default form,
#' will have the following columns:
#' 
#' \itemize{
#' \item i (unlabeled): the validation step number.
#' \item STEP: the name of the validation function used for the validation step,
#' \item COLUMNS: the names of the target columns used in the validation step
#' (if applicable).
#' \item VALUES: the values used in the validation step, where applicable; this
#' could be as literal values, as column names, an expression, etc.
#' \item TBL: indicates whether any there were any changes to the target table
#' just prior to interrogation. A rightward arrow from a small circle indicates
#' that there was no mutation of the table. An arrow from a circle to a purple
#' square indicates that preconditions were used to modify the target table. An
#' arrow from a circle to a half-filled circle indicates that the target table
#' has been segmented.
#' \item EVAL: a symbol that denotes the success of interrogation evaluation
#' for each step. A checkmark indicates no issues with evaluation. A warning
#' sign indicates that a warning occurred during evaluation. An explosion symbol
#' indicates that evaluation failed due to an error. Hover over the symbol for
#' details on each condition.
#' \item UNITS: the total number of test units for the validation step
#' \item PASS: on top is the absolute number of *passing* test units and below
#' that is the fraction of *passing* test units over the total number of test
#' units. 
#' \item FAIL: on top is the absolute number of *failing* test units and below
#' that is the fraction of *failing* test units over the total number of test
#' units. 
#' \item W, S, N: indicators that show whether the `warn`, `stop`, or `notify`
#' states were entered; unset states appear as dashes, states that are set with
#' thresholds appear as unfilled circles when not entered and filled when
#' thresholds are exceeded (colors for W, S, and N are amber, red, and blue)
#' \item EXT: a column that provides buttons to download data extracts as CSV
#' files for row-based validation steps having **failing** test units. Buttons
#' only appear when there is data to collect.
#' }
#' 
#' The small version of the display table (obtained using `size = "small"`)
#' omits the `COLUMNS`, `TBL`, and `EXT` columns. The width of the small table
#' is 575px; the standard table is 875px wide.
#' 
#' The `ptblank_agent_report` can be exported to a standalone HTML document
#' with the [export_report()] function.
#' 
#' If choosing to get a tibble (with `display_table = FALSE`), it will have the
#' following columns:
#' 
#' \itemize{
#' \item i: the validation step number.
#' \item type: the name of the validation function used for the validation step.
#' \item columns: the names of the target columns used in the validation step
#' (if applicable).
#' \item values: the values used in the validation step, where applicable; for
#' a [conjointly()] validation step, this is a listing of all sub-validations.
#' \item precon: indicates whether any there are any preconditions to apply
#' before interrogation and, if so, the number of statements used.
#' \item active: a logical value that indicates whether a validation step is
#' set to `"active"` during an interrogation.
#' \item eval: a character value that denotes the success of interrogation
#' evaluation for each step. A value of `"OK"` indicates no issues with
#' evaluation. The `"WARNING"` value indicates a warning occurred during
#' evaluation. The `"ERROR"` VALUES indicates that evaluation failed due to an
#' error. With `"W+E"` both warnings and an error occurred during evaluation.
#' \item units: the total number of test units for the validation step.
#' \item n_pass: the number of *passing* test units.
#' \item f_pass: the fraction of *passing* test units.
#' \item W, S, N: logical value stating whether the `warn`, `stop`, or `notify`
#' states were entered. Will be `NA` for states that are unset.
#' \item extract: an integer value that indicates the number of rows available
#' in a data extract. Will be `NA` if no extract is available.
#' }
#' 
#' @param agent *The pointblank agent object*
#' 
#'   `obj:<ptblank_agent>` // **required**
#' 
#'   A **pointblank** *agent* object that is commonly created through the use of
#'   the [create_agent()] function.
#' 
#' @param arrange_by *Method of arranging the report's table rows*
#' 
#'   `singl-kw:[i|severity]` // *default:* `"i"`
#' 
#'   A choice to arrange the report table rows by the validation step number
#'   (`"i"`, the default), or, to arrange in descending order by severity of the
#'   failure state (with `"severity"`).
#'   
#' @param keep *Which table rows should be kept?*
#' 
#'   `singl-kw:[all|fail_states]` // *default:* `"all"`
#' 
#'   An option to keep `"all"` of the report's table rows (the
#'   default), or, keep only those rows that reflect one or more
#'   `"fail_states"`.
#'   
#' @param display_table *Return a display-table report via gt*
#' 
#'   `scalar<logical>` // *default:* `TRUE`
#' 
#'   Should a display table be generated? If `TRUE`, and if the **gt** package
#'   is installed, a display table for the report will be shown in the Viewer.
#'   If `FALSE`, or if **gt** is not available, then a tibble will be returned.
#'   
#' @param size *Size option for display-table report*
#' 
#'   `scalar<character>` // *default:* `"standard"`
#' 
#'   The size of the display table, which can be either `"standard"` (the
#'   default) or `"small"`. This only applies to a display table (where
#'   `display_table = TRUE`).
#'   
#' @param title *Title customization options*
#' 
#'   `scalar<character>` // *default:* `":default:"`
#' 
#'   Options for customizing the title of the report. The default is the keyword
#'   `":default:"` which produces generic title text that refers to the
#'   **pointblank** package in the language governed by the `lang` option.
#'   Another keyword option is `":tbl_name:"`, and that presents the name of the
#'   table as the title for the report. If no title is wanted, then the
#'   `":none:"` keyword option can be used. Aside from keyword options, text can
#'   be provided for the title and `glue::glue()` calls can be used to construct
#'   the text string. If providing text, it will be interpreted as Markdown text
#'   and transformed internally to HTML. To circumvent such a transformation,
#'   use text in [I()] to explicitly state that the supplied text should not be
#'   transformed.
#'   
#' @param lang *Reporting language*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   The language to use for automatic creation of briefs (short descriptions
#'   for each validation step) and for the *agent report* (a summary table that
#'   provides the validation plan and the results from the interrogation. By
#'   default, `NULL` will create English (`"en"`) text. Other options include
#'   French (`"fr"`), German (`"de"`), Italian (`"it"`), Spanish (`"es"`),
#'   Portuguese (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`), Russian (`"ru"`),
#'   Polish (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and Dutch (`"nl"`).
#'   This `lang` option will override any previously set language setting (e.g.,
#'   by the [create_agent()] call).
#'   
#' @param locale *Locale for value formatting*
#' 
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#' 
#'   An optional locale ID to use for formatting values in the
#'   *agent report* summary table according the locale's rules. Examples include
#'   `"en_US"` for English (United States) and `"fr_FR"` for French (France);
#'   more simply, this can be a language identifier without a country
#'   designation, like `"es"` for Spanish (Spain, same as `"es_ES"`). This
#'   `locale` option will override any previously set locale value (e.g., by the
#'   [create_agent()] call).
#' 
#' @return A `ptblank_agent_report` object if `display_table = TRUE` or a tibble
#'   if `display_table = FALSE`.
#' 
#' @section Examples:
#' 
#' For the example here, we'll use a simple table with a single numerical column
#' `a`.
#' 
#' ```{r}
#' tbl <- dplyr::tibble(a = c(5, 7, 8, 5))
#' 
#' tbl
#' ```
#' 
#' Let's create an *agent* and validate that values in column `a` are always
#' greater than `4`.
#' 
#' ```r
#' agent <-
#'   create_agent(
#'     tbl = tbl,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>%
#'   col_vals_gt(columns = a, value = 4) %>%
#'   interrogate()
#' ```
#' 
#' We can get a tibble-based report from the agent by using `get_agent_report()`
#' with `display_table = FALSE`.
#' 
#' ```r
#' agent %>% get_agent_report(display_table = FALSE)
#' ```
#' 
#' \preformatted{## # A tibble: 1 × 14
#' ##       i type    columns values precon active eval  units n_pass
#' ##   <int> <chr>   <chr>   <chr>  <chr>  <lgl>  <chr> <dbl>  <dbl>
#' ## 1     1 col_va… a       4      NA     TRUE   OK        4      4
#' ## # … with 5 more variables: f_pass <dbl>, W <lgl>, S <lgl>,
#' ## #   N <lgl>, extract <int>}
#' 
#' 
#'
#' The full-featured display-table-based report can be viewed by printing the
#' `agent` object, but, we can get a `"ptblank_agent_report"` object returned to
#' us when using `display_table = TRUE` (the default for `get_agent_report`).
#' 
#' ```r
#' report <- get_agent_report(agent)
#' 
#' report
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_get_agent_report_1.png")`
#' }
#' }
#' 
#' What can you do with the `report` object? Print it at will wherever, and, it
#' can serve as an input to the [export_report()] function.
#' 
#' However, the better reason to use `get_agent_report()` over just printing the
#' agent for display-table purposes is to make use of the different display
#' options.
#' 
#' The agent report as a **gt** display table comes in two sizes: `"standard"`
#' (the default, 875px wide) and `"small"` (575px wide). Let's take a look at
#' the smaller-sized version of the report.
#' 
#' ```r
#' small_report <- 
#'   get_agent_report(
#'     agent = agent,
#'     size = "small"
#'   )
#'   
#' small_report
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_get_agent_report_2.png")`
#' }
#' }
#' 
#' We can use our own title by supplying it to the `title` argument, or, use
#' a special keyword like `":tbl_name:"` to get the table name (set in the
#' [create_agent()] call) as the title.
#' 
#' ```r
#' report_title <- get_agent_report(agent, title = ":tbl_name:")
#' 
#' report_title
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_get_agent_report_3.png")`
#' }
#' }
#' 
#' There are more options! You can change the language of the display table with
#' the `lang` argument (this overrides the language set in [create_agent()]),
#' validation steps can be rearranged using the `arrange_by` argument, and we
#' can also apply some filtering with the `keep` argument in
#' `get_agent_report()`.
#' 
#' @family Interrogate and Report
#' @section Function ID:
#' 6-2
#' 
#' @export
get_agent_report <- function(
    agent,
    arrange_by = c("i", "severity"),
    keep = c("all", "fail_states"),
    display_table = TRUE,
    size = "standard",
    title = ":default:",
    lang = NULL,
    locale = NULL
) {
  
  arrange_by <- match.arg(arrange_by)
  keep <- match.arg(keep)
  
  validation_set <- agent$validation_set
  
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
        if (is.null(x)) {
          NA_character_
        } else {
          toString(unique(x))
        }
      }
    )

  values <- 
    validation_set$values %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        if (is.function(x)) {
          x <- capture_function(x)
        }
        
        ifelse(
          is.null(x),
          NA_character_,
          paste(gsub("~", "", x), collapse = ", ")
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
          as.character(length(rlang::as_function(x)))
        )
      }
    )
  
  if (!has_agent_intel(agent)) {
    
    extract_count <- rep(NA, nrow(validation_set))
    
  } else {
    
    extract_count <- 
      as.character(validation_set[["i"]]) %in% names(agent$extracts)
    
    extract_count[extract_count == FALSE] <- NA_integer_
    
    suppressWarnings(
      extract_count[!is.na(extract_count)] <- 
        vapply(
          agent$extracts,
          FUN.VALUE = integer(1),
          USE.NAMES = FALSE,
          FUN = nrow
        )
    )
  }
  
  report_tbl <- 
    dplyr::tibble(
      i = validation_set$i,
      type = validation_set$assertion_type,
      columns = columns,
      values = values,
      precon = precon_count,
      active = validation_set$eval_active,
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
  
  if (!display_table) {
    return(report_tbl)
  }
  
  #
  # Generate a gt table if `display_table == TRUE`
  #
  
  # nocov start
  
  validation_set <- validation_set %>% 
    dplyr::filter(.data$i %in% report_tbl$i)
  eval <- eval[report_tbl$i]
  extracts <- 
    agent$extracts[
      as.character(
        base::intersect(as.numeric(names(agent$extracts)), report_tbl$i)
      )
    ]
  step_indices <- seq_len(nrow(validation_set))
  
  if (is.null(lang)) {
    
    lang <- agent$lang
    if (is.null(locale)) locale <- agent$locale
    
  } else {
    
    normalize_reporting_language(lang = lang)
    
    # Set the `locale` to the `lang` value if `locale` isn't set
    if (is.null(locale)) locale <- lang
  }

  assertion_type <- validation_set$assertion_type
  briefs <- validation_set$brief
  label <- validation_set$label
  tbl_src <- agent$tbl_src
  tbl_name <- agent$tbl_name
  
  # Initialize a table for gt footnotes
  footnotes_tbl <- initialize_footnotes_tbl()
  
  # Generate the report title with the `title` option
  title_text <- 
    process_title_text(
      title = title,
      tbl_name = tbl_name,
      report_type = "agent",
      lang = lang
    )
  
  # Generate agent label HTML
  agent_label_styled <- create_agent_label_html(agent = agent)
  
  # Generate table type HTML
  table_type <- 
    create_table_type_html(
      tbl_src = tbl_src,
      tbl_name = tbl_name
    )

  # Generate action levels HTML
  action_levels <- make_action_levels_html(agent = agent, locale = locale)
  
  # Combine label, table type, and action levels into
  # a table subtitle <div>
  combined_subtitle <-
    as.character(
      htmltools::tagList(
        htmltools::HTML(agent_label_styled),
        htmltools::tags$div(
          style = htmltools::css(
            height = "25px",
            `padding-top` = "10px"
          ),
          htmltools::HTML(paste0(table_type, action_levels))
        ) 
      )
    )

  # Generate table execution start/end time (and duration)
  # as a table source note
  table_time <- 
    create_table_time_html(
      time_start = agent$time_start,
      time_end = agent$time_end,
      size = size,
      locale = locale
    )
  
  #
  # Reformat the `type` column
  #
  type_upd <- 
    vapply(
      step_indices,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        # Get the `assertion_type` as a string
        assertion_str <- icon_name <- assertion_type[x]
        
        # Append `()` to the `assertion_str`
        assertion_str <- paste0(assertion_str, "()")
        
        # Get the `label` as a string
        label_str <- label[x]
        
        # Get the `brief` as a string
        brief_str <- briefs[x]
        
        if (
          assertion_type[x] == "serially" && 
          !is.na(agent$validation_set[x, ]$eval_active) &&
          agent$validation_set[x, ]$eval_active
          ) {

          interrogation_notes <-
            agent$validation_set[x, ]$interrogation_notes[[1]]
          
          failed_testing <- interrogation_notes$failed_testing
          
          final_validation_str <- 
            if (interrogation_notes$has_final_validation) "+V" else ""
          
          assertion_str <-
            as.character(
              paste0(
                htmltools::HTML(paste0(assertion_str, ": ")),
                htmltools::tags$span(
                  style = htmltools::css(
                    `text-decoration-style` = 
                      if (failed_testing) "solid" else NULL,
                    `text-decoration-line` = 
                      if (failed_testing) "underline" else NULL,
                    `text-decoration-color` = 
                      if (failed_testing) "red" else NULL,
                    `text-underline-position` =
                      if (failed_testing) "under" else NULL
                  ), 
                  htmltools::HTML(
                    paste0(interrogation_notes$total_test_steps, "T")
                  )
                ),
                final_validation_str
              )
            )
        }
        
        # Obtain the number of characters contained in the assertion
        # string; this is important for sizing components appropriately
        assertion_type_nchar <- nchar(assertion_str)
        
        # Declare the text size based on the length of `assertion_str`
        # and also whether the report is of the standard size or `"small"` 
        text_size <- ifelse(assertion_type_nchar + 2 >= 20, 10, 11)
        text_size <- ifelse(size == "small", 11, text_size)
        
        if (size == "small") {
          
          as.character(
            htmltools::tags$code(
              style = htmltools::css(
                `font-size` = paste0(text_size, "px")
              ),
              htmltools::HTML(paste0("&nbsp;", assertion_str))
            )
          )
          
        } else {
          
          if (!is.na(label_str)) {
            
            as.character(
              htmltools::tags$div(
                `aria-label` = brief_str,
                `data-balloon-pos` = "right",
                style = htmltools::css(width = "fit-content"),
                htmltools::tags$div(
                  style = htmltools::css(
                    float = "left",
                    width = "30px"
                  ),
                  htmltools::HTML(add_icon_svg(icon = icon_name))
                ),
                htmltools::tags$div(
                  style = htmltools::css(
                    `line-height` = "0.7em",
                    `margin-left` = "32px",
                    `padding-left` = "3px"
                  ),
                  htmltools::tags$p(
                    style = htmltools::css(
                      margin = "0",
                      `padding-top` = "4px",
                      `font-size` = "9px"
                    ),
                    htmltools::HTML(label_str)
                  ),
                  htmltools::tags$p(
                    style = htmltools::css(margin = "0"),
                    htmltools::tags$code(
                      style = htmltools::css(`font-size` = "11px"),
                      htmltools::HTML(assertion_str)
                    )
                  )
                )
              )
            )
            
          } else {
            
            as.character(
              htmltools::tags$div(
                `aria-label` = brief_str,
                `data-balloon-pos` = "right",
                style = htmltools::css(width = "fit-content"),
                htmltools::HTML(add_icon_svg(icon = icon_name)),
                htmltools::tags$code(
                  style = htmltools::css(`font-size` = paste0(text_size, "px")),
                  htmltools::HTML(paste0("&nbsp;", assertion_str))
                )
              )
            )
          }
        }
      }
    )
  
  #
  # Reformat the `columns` column
  #
  columns_upd <- 
    vapply(
      step_indices,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        # TODO: Display all columns used in conjointly case
        
        # Get the `column` value
        column_i <- validation_set$column[[x]]
        
        # Get the `assertion_type` as a string
        assertion_str <- assertion_type[x]
        
        if (
          assertion_str == "serially" &&
          has_agent_intel(agent)
          ) {
          
          if (
            !is.na(agent$validation_set[x, ]$eval_active) &&
            !agent$validation_set[x, ]$eval_active
            ) {
            
            return(NA_character_)
          }
          
          interrogation_notes <-
            agent$validation_set[x, ]$interrogation_notes[[1]]
          
          if (
            !interrogation_notes$has_final_validation &&
            !interrogation_notes$failed_testing
          ) {
            
            return(NA_character_)
            
          } else if (
            !interrogation_notes$has_final_validation &&
            interrogation_notes$failed_testing
          ) {
            
            # Case where `serially()` does not have a final validation
            # and testing failed
            # T -> T ->|
            
            # Replace `column_i` with the value at the failing step
            column_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$column[[1]]
            
          } else if (
            interrogation_notes$has_final_validation &&
            interrogation_notes$failed_testing
          ) {
            
            # Case where tests where unsuccessful and the final
            # validation step was not reached
            # T -> T ->| (V)
            
            # Replace `column_i` with the value at the failing step
            column_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$column[[1]]
            
          } else if (
            interrogation_notes$has_final_validation &&
            !interrogation_notes$failed_testing
          ) {
            
            # Case where all tests passed and the final
            # validation step was reached
            
            # Replace `column_i` with the value at the final step (validation)
            column_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$column[[1]]
          }
        }
        
        # If column missing
        if (is.null(column_i) || identical(unlist(column_i), NA_character_)) {
          
          columns_expr <- validation_set$columns_expr[[x]]
          not_interrogated <- is.na(validation_set$eval_error[[x]])
          eval_error <- isTRUE(validation_set$eval_error[[x]])
          
          # If column selection attempted AND:
          # - in validation planning, OR
          # - the evaluation errors, OR
          # - is a col_exists() step
          columns_expr_exists <- !is.na(columns_expr) && columns_expr != "NULL"
          show_column_expr <- columns_expr_exists &&
            (not_interrogated || eval_error || assertion_str == "col_exists")
          # Then display the original column selection expression for debugging
          if (show_column_expr) {
            as.character(
              htmltools::tags$p(
                title = columns_expr,
                style = htmltools::css(
                  `margin-top` = "0",
                  `margin-bottom` = "0",
                  `font-family` = "monospace",
                  `font-size` = "10px",
                  `white-space` = "nowrap",
                  `text-overflow` = "ellipsis",
                  overflow = "hidden",
                  color = if (eval_error) "firebrick",
                  `font-face` = "maroon"
                ),
                columns_expr
              )
            )
          } else {
            NA_character_
          }
          
        } else {
          
          text <- 
            column_i %>%
            unlist() %>%
            strsplit(", ") %>%
            unlist()
          
          title <- text
          
          text_fragments <- 
            paste0(
              htmltools::tags$span(
                style = htmltools::css(color = "purple"),
                htmltools::HTML("&marker;")
              ),
              text,
              collapse = ", "
            )
          
          if (size == "small") {
            
            as.character(
              htmltools::tags$div(
                htmltools::tags$p(
                  title = paste(title, collapse = ", "),
                  style = htmltools::css(
                    `margin-top` = "0",
                    `margin-bottom` = "0",
                    `font-family` = "monospace",
                    `white-space` = "nowrap",
                    `text-overflow` = "ellipsis",
                    overflow = "hidden"
                  ),
                  htmltools::HTML(text_fragments)
                )
              )
            )
            
          } else {
            
            as.character(
              htmltools::tags$div(
                `aria-label` = paste(title, collapse = ", "),
                `data-balloon-pos` = "left",
                htmltools::tags$p(
                  style = htmltools::css(
                    `margin-top` = "0",
                    `margin-bottom` = "0",
                    `font-size` = "11px",
                    `white-space` = "nowrap",
                    `text-overflow` = "ellipsis",
                    overflow = "hidden",
                    `line-height` = "2em"
                  ),
                  htmltools::tags$code(htmltools::HTML(text_fragments))
                )
              )
            )
          }
        }
      }
    )
  
  #
  # Reformat the `values` column
  #
  values_upd <- 
    vapply(
      step_indices,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        # Get the `values` value
        values_i <- validation_set$values[[x]]
        
        # Get the `assertion_type` as a string
        assertion_str <- assertion_type[x]
        
        if (assertion_str == "specially") {
          values_i <- capture_function(values_i)
        }
        
        # In the `serially()` step, there are two possibilities for what
        # should be displayed in the values column
        # [1] has final validation: show the values for validation step
        # [2] has no final validation and all tests passed: display the
        #     number of tests performed
        # [3] has no final validation and a test failed: show the values
        #     for the failing test step
        if (assertion_str == "serially") {

          if (
            !has_agent_intel(agent) ||
            !agent$validation_set[x, ]$eval_active
          ) {
            
            # TODO: Get the exact number of test steps rather than
            # getting the number of expressions (each expr could
            # expand to multiple steps)
            
            step_text <- 
              paste0(
                length(values_i), " ",
                get_lsv(
                  paste0(
                    "agent_report/report_col_step",
                    ifelse(length(values_i) > 1, "s", "")
                  )
                )[[lang]]
              )
            
            return(
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-size: 0.75rem;\">", step_text, "</p></div>"
              )
            )
          }
          
          interrogation_notes <-
            agent$validation_set[x, ]$interrogation_notes[[1]]
          
          if (
            !interrogation_notes$has_final_validation &&
            !interrogation_notes$failed_testing
          ) {
            
            # Case where `serially()` does not have a final validation
            # but all tests passed
            
            total_test_steps <- interrogation_notes$total_test_steps
            
            # TODO: change localized string to be: `x TESTS`
            step_text <- 
              paste0(
                total_test_steps, " ",
                get_lsv(
                  paste0(
                    "agent_report/report_col_step",
                    ifelse(total_test_steps > 1, "s", "")
                  )
                )[[lang]]
              )
            
            return(
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-size: 0.75rem;\">", step_text, "</p></div>"
              )
            )
            
          } else if (
            !interrogation_notes$has_final_validation &&
            interrogation_notes$failed_testing
          ) {
            
            # Case where `serially()` does not have a final validation
            # and testing failed
            # T -> T ->|
            
            # Replace `values_i` with the value at the failing step
            values_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$values[[1]]
            
          } else if (
            interrogation_notes$has_final_validation &&
            interrogation_notes$failed_testing
          ) {
            
            # Case where tests where unsuccessful and the final
            # validation step was not reached
            # T -> T ->| (V)
            
            # Replace `values_i` with the value at the failing step
            
            values_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$values[[1]]
            
          } else if (
            interrogation_notes$has_final_validation &&
            !interrogation_notes$failed_testing
          ) {
            
            # Case where all tests passed and the final
            # validation step was reached
            
            # Replace `values_i` with the value at the final step (validation)
            values_i <- 
              interrogation_notes$testing_validation_set[
                nrow(interrogation_notes$testing_validation_set), ]$values[[1]]
          }
        }
        
        if (assertion_str %in% c("row_count_match", "col_count_match")) {
          
          if (!is.numeric(values_i)) {
            
            return(
              paste0(
                "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
                "font-size: 0.75rem;\">EXTERNAL TABLE</p></div>"
              )
            )
          }
        }
        
        if (assertion_str == "tbl_match") {
          
          return(
            paste0(
              "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
              "font-size: 0.75rem;\">EXTERNAL TABLE</p></div>"
            )
          )
        }
        
        if (assertion_str == "conjointly") {
          
          length_values_i <- length(values_i)
          
          step_text <- 
            paste0(
              length_values_i, " ",
              get_lsv(
                paste0(
                  "agent_report/report_col_step",
                  ifelse(length_values_i > 1, "s", "")
                )
              )[[lang]]
            )
          
          paste0(
            "<div><p style=\"margin-top: 0px; margin-bottom: 0px; ",
            "font-size: 0.75rem;\">", step_text, "</p></div>"
          )
          
        } else if (
          !is.null(values_i) &&
          !is.null(names(values_i)) &&
          all(names(values_i) %in% c("TRUE", "FALSE"))
        ) {
          
          # Case of in-between comparison validation where there are
          # one or two columns specified as bounds
          bounds_incl <- as.logical(names(values_i))
          
          if (rlang::is_quosure(values_i[[1]])) {
            
            x_left <- 
              paste0(
                "<span style=\"color: purple;\">&marker;</span>",
                rlang::as_label(values_i[[1]])
              )
            
          } else {
            
            x_left <- 
              pb_fmt_number(
                values_i[[1]],
                decimals = 4,
                drop_trailing_zeros = TRUE,
                locale = locale
              )
          }
          
          if (rlang::is_quosure(values_i[[2]])) {
            
            x_right <- 
              paste0(
                "<span style=\"color: purple;\">&marker;</span>",
                rlang::as_label(values_i[[2]])
              )
            
          } else {
            
            x_right <-
              pb_fmt_number(
                values_i[[2]],
                decimals = 4,
                drop_trailing_zeros = TRUE,
                locale = locale
              )
          }
          
          text <-
            paste0(
              ifelse(bounds_incl[1], "[", "("),
              x_left,
              ", ",
              x_right,
              ifelse(bounds_incl[2], "]", ")")
            )
          
          title <- 
            paste0(
              ifelse(bounds_incl[1], "[", "("),
              pb_fmt_number(
                rlang::as_label(values_i[[1]]),
                decimals = 4,
                drop_trailing_zeros = TRUE,
                locale = locale
              ),
              ", ",
              pb_fmt_number(
                rlang::as_label(values_i[[2]]),
                decimals = 4,
                drop_trailing_zeros = TRUE,
                locale = locale
              ),
              ifelse(bounds_incl[2], "]", ")")
            )
          
          if (size == "small") {
            
            paste0(
              "<div><p title=\"", title, "\" style=\"margin-top: 0px; ",
              "margin-bottom: 0px; ",
              "font-family: monospace; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              text,
              "</p></div>"
            )
            
          } else {
            
            paste0(
              "<div aria-label=\"", title, "\" data-balloon-pos=\"left\">",
              "<p style=\"margin-top: 0px; margin-bottom: 0px; ",
              "font-size: 11px; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              "<code>", text, "</code>",
              "</p></div>"
            )
          }
          
        } else if (
          is.list(values_i) &&
          length(values_i) > 0 &&
          inherits(values_i, "col_schema")
        ) {
          
          # Case of column schema as a value
          
          column_schema_text <- 
            get_lsv("agent_report/report_column_schema")[[lang]]
          
          column_schema_type_text <- 
            if (inherits(values_i, "r_type")) {
              get_lsv("agent_report/report_r_col_types")[[lang]]
            } else {
              get_lsv("agent_report/report_r_sql_types")[[lang]]
            }
          
          paste0(
            "<div>",
            "<p style=\"margin-top: 0px; margin-bottom: 0px; ",
            "font-size: 0.75rem;\">", column_schema_text, "</p>",
            "<p style=\"margin-top: 2px; margin-bottom: 0px; ",
            "font-size: 0.65rem;\">", column_schema_type_text, "</p>",
            "</div>"
          )
          
        } else if (is_call(values_i)) {
          
          text <- rlang::as_label(values_i)
          
          if (size == "small") {
            
            paste0(
              "<div><p title=\"", text, "\" style=\"margin-top: 0px; ",
              "margin-bottom: 0px; ",
              "font-family: monospace; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              text,
              "</p></div>"
            )
            
          } else {
            
            paste0(
              "<div aria-label=\"", text, "\" data-balloon-pos=\"left\">",
              "<p style=\"margin-top: 0px; margin-bottom: 0px; ",
              "font-size: 11px; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              "<code>", text, "</code>",
              "</p></div>"
            )
          }
          
        } else if (is.null(values_i)) {
          
          NA_character_
          
        } else {

          text <-
            unname(
              tidy_gsub(
                values_i,
                "~",
                "<span style=\"color: purple;\">&marker;</span>"
              )
            )
            
          text <- gsub("$", "&dollar;", values_i, fixed = TRUE)

          text <- paste(text, collapse = ", ")
          
          if (size == "small") {
            
            paste0(
              "<div><p title=\"",
              values_i %>% tidy_gsub("~", "") %>% paste(., collapse = ", "),
              "\" style=\"margin-top: 0px; margin-bottom: 0px; ",
              "font-size: 11px; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              text,
              "</p></div>"
            )
            
          } else {
            
            paste0(
              "<div aria-label=\"",
              values_i %>% tidy_gsub("~", "") %>% paste(., collapse = ", "),
              "\" data-balloon-pos=\"left\"><p style=\"margin-top: 0px; ",
              "margin-bottom: 0px; ",
              "font-size: 11px; white-space: nowrap; ",
              "text-overflow: ellipsis; overflow: hidden;\">",
              "<code>", text, "</code>",
              "</p></div>"
            )
          }
        }
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
            make_boxed_text_html(
              x = icon_status("unchanged"),
              size = size,
              color = "#333333",
              background = "transparent",
              font_size = "10px",
              padding = 0,
              tt_text = get_lsv(text = c(
                "agent_report",
                "report_no_table_preconditions"
              ))[[lang]],
              border_radius = "4px"
            )
          
        } else if (rlang::is_formula(x) || rlang::is_function(x)) {
          
          if (rlang::is_formula(x)) {
            text <- rlang::as_label(x) %>% tidy_gsub("^~", "")
          } else {
            text <- rlang::as_label(body(x))
          }
          
          x <- 
            make_boxed_text_html(
              x = icon_status("modified"),
              size = size,
              color = "#3C898A",
              background = "transparent",
              font_size = "18px",
              padding = 0,
              tt_text = get_lsv(text = c(
                "agent_report",
                "report_some_table_preconditions"
              ))[[lang]],
              border_radius = "4px"
            )
        }
        x
      } 
    )
  
  # Obtain the `seg_col` and `seg_val` segmentation parameters
  suppressWarnings({
    seg_col <- validation_set$seg_col
    seg_val <- validation_set$seg_val
  })
  
  # Make changes to the `precon` column if there is segmentation
  if (!is.null(seg_col) || !is.null(seg_val)) {
    
    precon_upd <- 
      seq_along(seg_col) %>%
      vapply(
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(x) {
          
          if (is.na(seg_col[x])) {
            return(precon_upd[x])
          }
          
          seg_col_x <- seg_col[x]
          seg_val_x <- seg_val[x]
          
          precon_upd[x] <- 
            make_boxed_text_html(
              x = icon_status("segmented"),
              size = size, 
              color = "#3C898A",
              background = "transparent",
              font_size = "10px",
              padding = 0,
              tt_text = glue::glue(get_lsv(text = c(
                "agent_report",
                "report_on_segmentation"
              ))[[lang]]),
              border_radius = "4px"
            )
          
          precon_upd[x]
        }
      )
  }
  
  # Reformat `eval`
  eval_upd <- 
    seq_along(eval) %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {

        # Reformat error/warning to string
        msg_error <- pointblank_cnd_to_string(
          cnd = agent$validation_set$capture_stack[[x]]$error,
          pb_call = agent$validation_set$capture_stack[[x]]$pb_call
        )
        msg_warning <- pointblank_cnd_to_string(
          cnd = agent$validation_set$capture_stack[[x]]$warning,
          pb_call = agent$validation_set$capture_stack[[x]]$pb_call
        )
        
        if (is.na(eval[x])) {
          
          out <- "&mdash;"
          
        } else if (eval[x] == "OK") {
          
          out <- 
            make_boxed_text_html(
              x = "&check;",
              size = size,
              color = "#4CA64C",
              background = "transparent",
              tt_text = get_lsv(text = c(
                "agent_report",
                "report_no_evaluation_issues"
              ))[[lang]],
            )
          
        } else if (eval[x] == "W + E") {
          
          text <- 
            htmltools::htmlEscape(
              msg_error %>%
                tidy_gsub("\"", "'")
            )
          
          if (!is.null(text)) {
            text <- as.character(text)
            text_size <- "large"
          } else {
            text <- ""
            text_size <- NULL
          }
          
          out <- 
            make_boxed_text_html(
              x = "&#128165;",
              size = size,
              color = "#FFFFFF",
              background = "transparent",
              tt_text = text,
              tt_text_size = text_size
            )
          
        } else if (eval[x] == "WARNING") {
          
          text <- 
            htmltools::htmlEscape(
              msg_warning %>%
                tidy_gsub("\"", "'")
            )
          
          if (!is.null(text)) {
            text <- as.character(text)
            text_size <- "large"
          } else {
            text <- ""
            text_size <- NULL
          }
          
          out <- 
            make_boxed_text_html(
              x = "&#9888;",
              size = size,
              color = "#222222",
              background = "transparent",
              tt_text = text,
              tt_text_size = text_size
            )
          
        } else if (eval[x] == "ERROR") {
          
          text <-
            htmltools::htmlEscape(
              msg_error %>%
                tidy_gsub("\"", "'")
            )
          
          if (!is.null(text)) {
            text <- as.character(text)
            text_size <- "large"
          } else {
            text <- ""
            text_size <- NULL
          }
          
          out <- 
            make_boxed_text_html(
              x = "&#128165;",
              size = size,
              color = "#FFFFFF",
              background = "transparent",
              tt_text = text,
              tt_text_size = text_size
            )
        }
        out
      } 
    )
  
  # Reformat `extract`
  extract_upd <-
    validation_set$i %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        
        if (is.null(extracts[as.character(x)][[1]])) {
          x <- "&mdash;"
        } else {
          
          df <- 
            extracts[as.character(x)][[1]] %>%
            as.data.frame(stringsAsFactors = FALSE)

          fail_rows_extract <- 
            pb_fmt_number(nrow(df), decimals = 0, locale = locale)
          
          title_text <- 
            glue::glue(
              get_lsv(
                text = c(
                  "agent_report",
                  "report_fail_rows_available"
                )
              )[[lang]]
            )
          
          temp_file <- 
            tempfile(pattern = paste0("csv_file_", x), fileext = ".csv")
          
          utils::write.csv(df, file = temp_file, row.names = FALSE)
          
          on.exit(unlink(temp_file))
          
          file_encoded <- base64enc::base64encode(temp_file)
          
          output_file_name <- 
            paste0(
              "extract_",
              formatC(x, width = 4, format = "d", flag = "0"),
              ".csv"
            )
          
          x <-
            as.character(
              htmltools::a(
                href = paste0("data:text/csv;base64,", file_encoded),
                download = output_file_name,
                htmltools::tags$button(
                  `aria-label` = title_text,
                  `data-balloon-pos` = "left",
                  style = htmltools::css(
                    `background-color` = "#67C2DC",
                    color = "#FFFFFF",
                    border = "none",
                    padding = "5px",
                    `font-weight` = "bold",
                    cursor = "pointer",
                    `border-radius` = "4px"
                  ),
                  "CSV"
                )
              )
            )
        }
        x
      } 
    )
  
  #
  # Reformat W, S, and N
  #
  
  w_upd <- 
    validation_set$warn %>%
    vapply(
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (is.na(x)) {
          x <- "&mdash;"
        } else if (x == TRUE) {
          x <- "<span style=\"color: #E5AB00;\">&#9679;</span>"
        } else if (x == FALSE) {
          x <- "<span style=\"color: #E5AB00;\">&cir;</span>"
        }
        x
      }
    )
  
  s_upd <- 
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
  
  n_upd <- 
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

  # TODO: Generate footnotes for certain steps
  
  # Generate a gt table
  agent_report <- 
    report_tbl %>%
    dplyr::mutate(
      status_color = NA_character_,
      type = type_upd,
      columns = columns_upd,
      values = values_upd,
      precon = precon_upd,
      eval_sym = eval_upd,
      units = units,
      n_pass = n_pass,
      n_fail = units - n_pass,
      f_pass = f_pass_val,
      f_fail = f_fail_val,
      W_val = W,
      S_val = S,
      N_val = N,
      W = w_upd,
      S = s_upd,
      N = n_upd,
      extract = extract_upd
    ) %>%
    dplyr::select(
      status_color, i, type, columns, values, precon, eval_sym, units,
      n_pass, f_pass, n_fail, f_fail, W, S, N, extract,
      W_val, S_val, N_val, eval, active
    ) %>%
    gt::gt(id = "pb_agent") %>%
    gt::tab_header(
      title = title_text,
      subtitle = gt::md(combined_subtitle)
    ) %>%
    gt::cols_merge(
      columns = c("n_pass", "f_pass"),
      hide_columns = "f_pass"
    ) %>%
    gt::cols_merge(
      columns = c("n_fail", "f_fail"),
      hide_columns = "f_fail"
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = c("n_pass", "n_fail")),
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
      status_color = "",
      i = "",
      type = get_lsv("agent_report/report_col_step")[[lang]],
      columns = get_lsv("agent_report/report_col_columns")[[lang]],
      values = get_lsv("agent_report/report_col_values")[[lang]],
      precon = "TBL",
      eval_sym = "EVAL",
      units = "UNITS",
      n_pass = "PASS",
      n_fail = "FAIL",
      extract = "EXT"
    ) %>%
    gt::tab_source_note(source_note = gt::md(table_time)) %>%
    gt::tab_options(
      table.font.size = gt::pct(90),
      row.striping.include_table_body = FALSE
    ) %>%
    gt::cols_align(
      align = "center",
      columns = c("precon", "eval_sym", "W", "S", "N", "extract")
    ) %>%
    gt::cols_align(
      align = "center",
      columns = c("f_pass", "f_fail")
    ) %>%
    gt::cols_align(
      align = "right",
      columns = "i"
    ) %>%
    gt::fmt_number(
      columns = c("units", "n_pass", "n_fail", "f_pass", "f_fail"),
      decimals = 0, drop_trailing_zeros = TRUE, suffixing = TRUE,
      locale = locale
    ) %>%
    gt::fmt_number(
      columns = c("f_pass", "f_fail"),
      decimals = 2,
      locale = locale
    ) %>%
    gt::fmt_markdown(
      columns = c(
        "type", "columns", "values", "precon",
        "eval_sym", "W", "S", "N", "extract"
      )
    ) %>%
    gt_missing(columns = c("columns", "values", "units", "extract")) %>%
    gt_missing(columns = "status_color", missing_text = "") %>%
    gt::cols_hide(columns = c("W_val", "S_val", "N_val", "active", "eval")) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = "units"),
      fn = function(x) {
        dplyr::case_when(
          x == "&mdash;" ~ x,
          TRUE ~ paste0("<code>", x, "</code>")
        )
      }
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
      style = gt::cell_text(
        weight = "bold",
        color = "#666666",
        size = ifelse(size == "small", gt::px(10), gt::px(13))
      ),
      locations = gt::cells_body(columns = "i")
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#4CA64C"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = units == n_pass
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#4CA64C66", alpha = 0.5),
      locations = gt::cells_body(
        columns = "status_color",
        rows = units != n_pass
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#FFBF00"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = W_val
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#CF142B"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = S_val
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(sides = "left", color = "#D3D3D3"),
        gt::cell_fill(color = "#FCFCFC")
      ),
      locations = gt::cells_body(columns = c("precon", "W"))
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        color = "#E5E5E5",
        style = "dashed"
      ),
      locations = gt::cells_body(columns = c("n_pass", "n_fail"))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(sides = "right", color = "#D3D3D3"),
        gt::cell_fill(color = "#FCFCFC")
      ),
      locations = gt::cells_body(columns = c("eval_sym", "N"))
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#FCFCFC"),
      locations = gt::cells_body(columns = "S")
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "left",
        color = "#E5E5E5",
        style = "dashed"
      ),
      locations = list(
        gt::cells_body(columns = c("columns", "values"))
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#F2F2F2", alpha = 0.75),
        gt::cell_text(color = "#8B8B8B")
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = active == FALSE
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#FFC1C1", alpha = 0.35),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = eval == "ERROR"
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(11)),
      locations = gt::cells_body(
        columns = c("units", "n_pass", "n_fail")
      )
    )
  
  if (!has_agent_intel(agent)) {
    
    agent_report <-
      agent_report %>%
      gt::text_transform(
        locations = gt::cells_body(
          columns = c(
            "precon", "eval_sym", "units", "f_pass", "f_fail",
            "n_pass", "n_fail", "W", "S", "N", "extract"
          )
        ),
        fn = function(x) {
          ""
        }
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#F2F2F2"),
          gt::cell_borders(
            sides = "right",
            style = "solid",
            color = "#F2F2F2"
          )
        ),
        locations = gt::cells_body(
          columns = c(
            "precon", "eval_sym", "units", "f_pass", "f_fail",
            "n_pass", "n_fail", "W", "S", "N", "extract"
          )
        )
      ) %>%
      gt::tab_header(
        title = gt::md(
          paste0(
            "<div>",
            "<span style=\"float: left;\">", 
            get_lsv("agent_report/pointblank_validation_plan_text")[[lang]],
            "</span>",
            "<span style=\"float: right; text-decoration-line: underline; ",
            "text-underline-position: under;",
            "font-size: 16px; text-decoration-color: #9C2E83;",
            "padding-top: 0.1em; padding-right: 0.4em;\">",
            get_lsv("agent_report/no_interrogation_performed_text")[[lang]],
            "</span>",
            "</div>"
          )
        ),
        subtitle = gt::md(
          paste0(
            agent_label_styled, " ", table_type, " ", table_time, " <br><br>"
          )
        )
      )
  }
  
  if (size == "small") {
    
    agent_report <- 
      agent_report %>%
      gt::cols_hide(c("columns", "values", "eval_sym", "precon", "extract")) %>%
      gt::cols_width(
        "status_color" ~ gt::px(4),
        "i" ~ gt::px(25),
        "type" ~ gt::px(190),
        "precon" ~ gt::px(30),
        "units" ~ gt::px(50),
        "n_pass" ~ gt::px(50),
        "n_fail" ~ gt::px(50),
        "W" ~ gt::px(30),
        "S" ~ gt::px(30),
        "N" ~ gt::px(30),
        gt::everything() ~ gt::px(20)
      ) %>%
      gt::tab_style(
        locations = gt::cells_body(columns = gt::everything()),
        style = "height: 35px"
      )
    
    if (!has_agent_intel(agent)) {
      
      agent_report <- 
        agent_report %>%
        gt::tab_header(
          title = gt::md(
            paste0(
              "<div>",
              "<span style=\"float: left;\">", 
              get_lsv("agent_report/pointblank_validation_plan_text")[[lang]],
              "</span>",
              "<span style=\"float: right; text-decoration-line: underline; ",
              "font-size: 16px; text-decoration-color: #008B8B;",
              "padding-top: 0.1em; padding-right: 0.4em;\">",
              get_lsv("agent_report/no_interrogation_performed_text")[[lang]],
              "</span>",
              "</div>"
            )
          ),
          subtitle = gt::md(
            paste0(agent_label_styled, " ", table_type, " <br><br>")
          )
        )
      
    } else {
      
      agent_report <- 
        agent_report %>%
        gt::tab_header(
          title = title_text,
          subtitle = gt::md(
            paste0(agent_label_styled, " ", table_type, " <br><br>")
          )
        )
    }
    
  } else {
    
    agent_report <- 
      agent_report %>%
      gt::cols_width(
        "status_color" ~ gt::px(6),
        "i" ~ gt::px(35),
        "type" ~ gt::px(190),
        "columns" ~ gt::px(120),
        "values" ~ gt::px(120),
        "precon" ~ gt::px(50),
        "eval_sym" ~ gt::px(50),
        "W" ~ gt::px(30),
        "S" ~ gt::px(30),
        "N" ~ gt::px(30),
        "extract" ~ gt::px(65),
        gt::everything() ~ gt::px(50)
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", color = "#666666"),
        locations = gt::cells_column_labels(columns = gt::everything())
      ) %>%
      gt::tab_style(
        locations = gt::cells_body(columns = gt::everything()),
        style = "height: 40px"
      ) %>%
      gt::opt_table_font(font = gt::google_font("IBM Plex Sans")) %>%
      gt::opt_css(
        paste0(
          "@import url(\"https://unpkg.com/",
          "balloon-css/balloon.min.css\");"
        )
      ) %>%
      gt::opt_css(
        css = "
          #pb_agent {
            -webkit-font-smoothing: antialiased;
          }
          #pb_agent .gt_row {
            overflow: visible;
          }
          #pb_agent .gt_sourcenote {
            height: 35px;
            padding: 0
          }
          #pb_agent code {
            font-family: 'IBM Plex Mono', monospace, courier;
            color: black;
            background-color: transparent;
            padding: 0;
          }
        "
      )
  }
  
  # TODO: Process footnotes
  
  # Add the `ptblank_agent_report` class to the gt table object
  class(agent_report) <- c("ptblank_agent_report", class(agent_report))
  
  # nocov end
  
  # Quarto rendering workaround
  if (check_quarto()) {
    agent_report <- gt::fmt(agent_report, fns = identity)
  }
  
  agent_report
}

get_default_title_text <- function(report_type, lang) {
  
  if (report_type == "informant") {
    title_text <- 
      get_lsv(text = c(
        "informant_report",
        "pointblank_information_title_text"
      ))[[lang]]
    
  } else if (report_type == "agent") {
    
    title_text <- 
      get_lsv(text = c(
        "agent_report",
        "pointblank_validation_title_text"
      ))[[lang]]
    
  } else if (grepl("multiagent", report_type)) {
    
    title_text <- 
      get_lsv(text = c(
        "multiagent_report",
        "pointblank_multiagent_title_text"
      ))[[lang]]
  }
  
  title_text
}

process_title_text <- function(
    title,
    tbl_name,
    report_type,
    lang
) {
  
  if (report_type == "multiagent:wide") {
    if (title == ":tbl_name:") {
      stop(
        "The `:tbl_name:` option can't be used with `get_multiagent_report()`.",
        call. = FALSE
      )
    }
  }
  
  if (is.null(title)) {
    title_text <- ""
  } else if (is.na(title)) {
    title_text <- ""
  } else if (title == ":default:") {
    title_text <- get_default_title_text(report_type = report_type, lang = lang)
  } else if (title == ":none:") {
    title_text <- ""
  } else if (title == ":tbl_name:") {
    if (!is.na(tbl_name) && tbl_name != "NA") {
      title_text <- gt::md(paste0("<code>", tbl_name, "</code>"))
    } else {
      title_text <- ""
    }
  } else if (inherits(title, "AsIs")) {
    title_text <- unclass(title)
  } else if (inherits(title, "character")) {
    title_text <- gt::md(title)
  }
  title_text
}

create_table_time_html <- function(
    time_start,
    time_end,
    size = "standard",
    locale = NULL
) {
  
  if (length(time_start) < 1) {
    return("")
  }
  
  time_duration <- get_time_duration(time_start, time_end)
  time_duration_formatted <- 
    print_time_duration_report(time_duration, locale = locale)
  
  as.character(
    htmltools::tagList(
      htmltools::tags$span(
        style = htmltools::css(
          `background-color` = "#FFF",
          color = "#444",
          padding = if (size != "small") "0.5em 0.5em" else "",
          position = "inherit",
          `text-transform` = "uppercase",
          `margin-left` = if (size != "small") "10px" else "",
          border = if (size != "small") "solid 1px #999999" else "",
          `font-variant-numeric` = "tabular-nums",
          `border-radius` = "0",
          padding = "2px 10px 2px 10px",
          padding = if (size != "small") {
            "2px 10px 2px 10px" 
          } else {
            "2px 10px 2px 5px"
          },
          `border-right` = if (size == "small") "solid 1px #333" else ""
        ),
        format(time_start, "%Y-%m-%d %H:%M:%S %Z")
      ),
      htmltools::tags$span(
        style = htmltools::css(
          `background-color` = "#FFF",
          color = "#444",
          padding = if (size != "small") "0.5em 0.5em" else "",
          position = "inherit",
          margin = "5px 1px 5px 0",
          border = if (size != "small") "solid 1px #999999" else "",
          `border-left` = if (size == "small") "none" else "",
          `font-variant-numeric` = "tabular-nums",
          `border-radius` = "0",
          padding = "2px 10px 2px 10px"
        ),
        time_duration_formatted,
      ),
      htmltools::tags$span(
        style = htmltools::css(
          `background-color` = "#FFF",
          color = "#444",
          padding = if (size != "small") "0.5em 0.5em" else "",
          position = "inherit",
          `text-transform` = "uppercase",
          `margin` = "5px 1px 5px -1px",
          border = if (size != "small") "solid 1px #999999" else "",
          `font-variant-numeric` = "tabular-nums",
          `border-left` = if (size == "small") "solid 1px #333" else "",
          `border-radius` = "0",
          padding = "2px 10px 2px 10px"
        ),
        format(time_end, "%Y-%m-%d %H:%M:%S %Z")
      )
    )
  )
}

print_time_duration_report <- function(
    time_diff_s,
    locale = NULL
) {
  
  if (time_diff_s < 1) {
    "< 1 s"
  } else {
    paste0(
      pb_fmt_number(
        round(time_diff_s, 1),
        decimals = 1, locale = locale
      ),
      " s"
    )
  }
}

create_agent_label_html <- function(agent) {
  
  as.character(
    htmltools::tags$span(
      agent$label,
      style = htmltools::css(
        `text-decoration-style` = "solid",
        `text-decoration-color` = "#ADD8E6",
        `text-decoration-line` = "underline",
        `text-underline-position` = "under",
        color = "#333333",
        `font-variant-numeric` = "tabular-nums",
        `padding-left` = "4px",
        `margin-right` = "5px",
        `padding-right` = "2px"
      )
    )
  )
}

create_table_type_html <- function(
    tbl_src,
    tbl_name
) {

  if (is.null(tbl_src)) {
    
    text <- c("#C2C2C2", "#222222", "?")
    
  } else {
    
    text <- 
      switch(
        tbl_src,
        data.frame = c("#9933CC", "#FFFFFF", "data frame"),
        tbl_df = c("#F1D35A", "#222222", "tibble"),
        sqlite = c("#BACBEF", "#222222", "SQLite"),
        duckdb = c("#000000", "#FFFFFF", "DuckDB"),
        mysql = c("#EBAD40", "#222222", "MySQL"),
        postgres = c("#3E638B", "#FFFFFF", "PostgreSQL"),
        bigquery = c("#5283EC", "#FFFFFF", "BigQuery"),
        tbl_spark = c("#E66F21", "#FFFFFF", "Spark DataFrame"),
        Arrow = c("#353A3F", "#FFFFFF", "Apache Arrow"),
        c("#E2E2E2", "#222222", tbl_src)
      )
  }
  
  if (all(!is.na(text)) && (is.na(tbl_name) || tbl_name == "NA")) {
    
    paste0(
      "<span style=\"background-color: ", text[1], ";",
      "color: ", text[2], ";padding: 0.5em 0.5em;",
      "position: inherit;text-transform: uppercase;margin: 5px 1px 5px 4px;",
      "font-weight: bold;border: solid 1px ", text[1], ";",
      "padding: 2px 10px 2px 10px;font-size: smaller;\">",
      text[3],
      "</span>"
    )
    
  } else if (all(!is.na(text)) && !is.na(tbl_name)) {
    
    as.character(
      htmltools::tagList(
        htmltools::tags$span(
          text[3],
          style = htmltools::css(
            `background-color` = text[1],
            color = text[2],
            padding = "0.5em 0.5em",
            position = "inherit",
            `text-transform` = "uppercase",
            margin = "5px 0px 5px 5px",
            `font-weight` = "bold",
            border = paste0("solid 1px ", text[1]),
            padding = "2px 15px 2px 15px",
            `font-size` = "smaller"
          )
        ),
        htmltools::tags$span(
          tbl_name,
          style = htmltools::css(
            `background-color` = "none",
            color = "#222222",
            padding = "0.5em 0.5em",
            position = "inherit",
            margin = "5px 10px 5px -4px",
            `font-weight` = "bold",
            border = paste0("solid 1px ", text[1]),
            padding = "2px 15px 2px 15px",
            `font-size` = "smaller"
          )
        )
      )
    )
    
  } else {
    ""
  }
}

make_action_levels_html <- function(
    agent,
    locale
) {
  
  actions <- agent$actions
  
  if (is.null(unlist(actions[1:6]))) {
    return("")
  }

  warn <- 
    c(
      pb_fmt_number(actions$warn_fraction, decimals = 2, locale = locale),
      pb_fmt_number(actions$warn_count, decimals = 0, locale = locale)
    ) %||% "&mdash;"
  
  stop <-
    c(
      pb_fmt_number(actions$stop_fraction, decimals = 2, locale = locale),
      pb_fmt_number(actions$stop_count, decimals = 0, locale = locale)
    ) %||% "&mdash;"
  
  
  notify <-
    c(
      pb_fmt_number(actions$notify_fraction, decimals = 2, locale = locale),
      pb_fmt_number(actions$notify_count, decimals = 0, locale = locale)
    ) %||% "&mdash;"
  
  as.character(
    htmltools::tagList(
      htmltools::tags$span(
        "WARN",
        style = htmltools::css(
          `background-color` = "#E5AB00",
          color = "white",
          padding = "0.5em 0.5em",
          position = "inherit",
          `text-transform` = "uppercase",
          margin = "5px 0px 5px 5px",
          `font-weight` = "bold",
          border = paste0("solid 1px #E5AB00"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      ),
      htmltools::tags$span(
        htmltools::HTML(warn),
        style = htmltools::css(
          `background-color` = "none",
          color = "#333333",
          padding = "0.5em 0.5em",
          position = "inherit",
          margin = "5px 0px 5px -4px",
          `font-weight` = "bold",
          border = paste0("solid 1px #E5AB00"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      ),
      htmltools::tags$span(
        "STOP",
        style = htmltools::css(
          `background-color` = "#D0182F",
          color = "white",
          padding = "0.5em 0.5em",
          position = "inherit",
          `text-transform` = "uppercase",
          margin = "5px 0px 5px 1px",
          `font-weight` = "bold",
          border = paste0("solid 1px #D0182F"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      ),
      htmltools::tags$span(
        htmltools::HTML(stop),
        style = htmltools::css(
          `background-color` = "none",
          color = "#333333",
          padding = "0.5em 0.5em",
          position = "inherit",
          margin = "5px 0px 5px -4px",
          `font-weight` = "bold",
          border = paste0("solid 1px #D0182F"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      ),
      htmltools::tags$span(
        "NOTIFY",
        style = htmltools::css(
          `background-color` = "#499FFE",
          color = "white",
          padding = "0.5em 0.5em",
          position = "inherit",
          `text-transform` = "uppercase",
          margin = "5px 0px 5px 1px",
          `font-weight` = "bold",
          border = paste0("solid 1px #499FFE"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      ),
      htmltools::tags$span(
        htmltools::HTML(notify),
        style = htmltools::css(
          `background-color` = "none",
          color = "#333333",
          padding = "0.5em 0.5em",
          position = "inherit",
          margin = "5px 0px 5px -4px",
          `font-weight` = "bold",
          border = paste0("solid 1px #499FFE"),
          padding = "2px 15px 2px 15px",
          `font-size` = "smaller"
        )
      )
    )
  )
}

make_boxed_text_html <- function(
    x,
    size = "standard",
    color = "#333333",
    background = "transparent",
    font_size = "15px",
    padding = "5px",
    tt_text = NULL,
    tt_position = "left",
    tt_text_size = NULL,
    border_radius = NULL,
    v_align = "middle"
) {
  
  if (!is.null(tt_position) && size == "standard") {
    text_type <- "aria-label"
  } else {
    text_type <- "title"
  }
  
  text_html <- 
    htmltools::tags$span(
      htmltools::HTML(x),
      style = htmltools::css(
        background = background,
        padding = padding,
        color = color,
        `vertical-align` = v_align,
        `font-size` = font_size,
        border = "none",
        `border-radius` = border_radius
      )
    )
  
  if (size == "standard") {
    text_html <-
      text_html %>%
      htmltools::tagAppendAttributes(
        `aria-label` = tt_text,
        `data-balloon-pos` = tt_position,
        `data-balloon-length` = if (!is.null(tt_text_size)) tt_text_size
      )
  } else {
    text_html <- text_html %>% htmltools::tagAppendAttributes(`title` = tt_text)
  }
  
  as.character(text_html)
}

icon_status <- function(icon = c("unchanged", "modified", "segmented")) {
  
  icon <- match.arg(icon)
  
  as.character(
    htmltools::HTML(
      paste(
        readLines(
          con = system.file(
            "img", "status_icons", paste0(icon, ".svg"),
            package = "pointblank"
          ), 
          warn = FALSE
        ),
        collapse = ""
      )
    )
  )
}

# Function for initializing an empty footnotes table
initialize_footnotes_tbl <- function() {
  
  dplyr::tibble(
    col_idx = integer(0),
    row_idx = integer(0),
    note = character(0)
  )
}

# Function for adding a footnote to the `footnotes_tbl`
store_footnote <- function(
    footnotes_tbl,
    note,
    col_idx,
    row_idx
) {
  
  dplyr::bind_rows(
    footnotes_tbl,
    dplyr::tibble(
      col_idx = as.integer(col_idx),
      row_idx = as.integer(row_idx),
      note = note
    )
  )
}

# Function for formatting error in `$capture_stack`
pointblank_cnd_to_string <- function(cnd, pb_call) {
  if (is.null(cnd)) return(character(0))
  # Reformatting not yet implemented for warnings 
  if (rlang::is_warning(cnd)) return(cnd)
  # Reconstruct trimmed down error and rethrow without cli
  new <- rlang::error_cnd(
    call = rlang::call2(":::", quote(pointblank), rlang::sym(pb_call)),
    message = cnd$parent$message %||% cnd$message,
    use_cli_format = FALSE
  )
  as.character(try(rlang::cnd_signal(new), silent = TRUE))
}
