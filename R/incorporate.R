#' Given an *informant* object, update and incorporate table snippets
#' 
#' When the *informant* object has a number of snippets available (by using
#' [info_snippet()]) and the strings to use them (by using the `info_*()`
#' functions and `{<snippet_name>}` in the text elements), the process of
#' incorporating aspects of the table into the info text can occur by
#' using the `incorporate()` function. After that, the information will be fully
#' updated (getting the current state of table dimensions, re-rendering the
#' info text, etc.) and we can print the *informant* object or use the
#' [get_informant_report()] function to see the information report.
#' 
#' @param informant An informant object of class `ptblank_informant`.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' # Take the `small_table` and
#' # assign it to `test_table`; we'll
#' # modify it later
#' test_table <- small_table
#' 
#' # Generate an informant object, add
#' # two snippets with `info_snippet()`,
#' # add information with some other
#' # `info_*()` functions and then
#' # `incorporate()` the snippets into
#' # the info text
#' informant <- 
#'   create_informant(
#'     read_fn = ~test_table,
#'     tbl_name = "test_table"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "col_count",
#'     fn = ~ . %>% ncol()
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   ) %>%
#'   info_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `informant` object
#' # to see the information report
#' 
#' # Let's modify `test_table` to give
#' # it more rows and an extra column
#' test_table <- 
#'   dplyr::bind_rows(test_table, test_table) %>%
#'   dplyr::mutate(h = a + c)
#' 
#' # Using `incorporate()` will cause
#' # the snippets to be reprocessed, and,
#' # the strings to be updated
#' informant <-
#'   informant %>% incorporate()
#' 
#' @export
incorporate <- function(informant) {
  
  # Get the target table for this informant object
  # TODO: Use the same scheme as the `agent` does
  tbl <- informant$tbl
  read_fn <- informant$read_fn
  
  # TODO: Verify that either `tbl` or `read_fn` is available
  
  # Prefer reading a table from a `read_fn` if it's available
  # TODO: Verify that the table is a table object
  # and provide an error if it isn't
  if (!is.null(read_fn)) {
    if (inherits(read_fn, "function")) {
      tbl <- rlang::exec(read_fn)
    } else if (rlang::is_formula(read_fn)) {
      tbl <- read_fn %>% rlang::f_rhs() %>% rlang::eval_tidy()
    } else {
      stop(
        "The `read_fn` object must be a function or an R formula.\n",
        "* A function can be made with `function()` {<table reading code>}.\n",
        "* An R formula can also be used, with the expression on the RHS.",
        call. = FALSE
      )
    }
  }
  
  # Update the following property values without user intervention
  #  - _columns
  #  - _rows
  #  - _type
  
  x <- create_agent(tbl = tbl, read_fn = read_fn)
  
  table.type <- x$tbl_src
  column_names <- x$col_names
  column_types_r <- x$col_types
  
  table.columns <- length(column_names)
  table.rows <- dplyr::count(tbl, name = "n") %>% dplyr::pull(n)
  
  # TODO: Sync column names, determining which are newly seen
  # and those that are no longer seen
  
  # TODO: Sync column types
  
  #
  # Incorporate snippets
  #
  
  meta_snippets <- informant$meta_snippets

  for (i in seq_along(meta_snippets)) {
    
    snippet_fn <- 
      informant$meta_snippets[[i]] %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
    
    if (inherits(snippet_fn, "fseq")) {
      
      snippet <- snippet_fn(tbl)
      assign(x = names(informant$meta_snippets[i]), value = snippet)
    }
  }
  
  metadata_meta_label <- 
    glue_safely(informant$metadata[["info_label"]], .otherwise = "(SNIPPET MISSING)")
  
  metadata_table <-
    lapply(informant$metadata[["table"]], function(x) {
      glue_safely(x, .otherwise = "(SNIPPET MISSING)")
    })
  
  metadata_columns <- 
    lapply(informant$metadata[["columns"]], lapply, function(x) {
      glue_safely(x, .otherwise = "(SNIPPET MISSING)")
    })
  
  extra_sections <- 
    base::setdiff(
      names(informant$metadata),
      c("info_label", "table", "columns")
    )
  
  metadata_extra <- informant$metadata[extra_sections]
  
  for (i in seq_along(extra_sections)) {
    
    metadata_extra[[i]] <-
      lapply(metadata_extra[[i]], function(x) {
        glue_safely(x, .otherwise = "(SNIPPET MISSING)")
      })
  }
  
  metadata_rev <-
    c(
      list(info_label = metadata_meta_label),
      list(table = metadata_table),
      list(columns = metadata_columns),
      metadata_extra,
      list(updated = Sys.time())
    )
  
  metadata_rev$table$`_columns` <- as.character(table.columns)
  metadata_rev$table$`_rows` <- as.character(table.rows)
  metadata_rev$table$`_type` <- table.type
  
  informant$metadata_rev <- metadata_rev
  informant
}
