#' Given a metadata object, update and incorporate table snippets
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' 
#' @return A `ptblank_metadata` object.
#' 
#' @examples 
#' # Take the `small_table` and
#' # assign it to `test_table`; we'll
#' # modify it later
#' test_table <- small_table
#' 
#' # Generate a metadata object, add
#' # two snippets with `meta_snippet()`,
#' # add metadata with some other
#' # `meta_*()` functions and then
#' # `incorporate()` the snippets into
#' # the metadata text
#' metadata <- 
#'   create_metadata(
#'     read_fn = ~test_table,
#'     tbl_name = "test_table"
#'   ) %>%
#'   meta_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   meta_snippet(
#'     snippet_name = "col_count",
#'     fn = ~ . %>% ncol()
#'   ) %>%
#'   meta_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. (SIMPLE)"
#'   ) %>%
#'   meta_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   meta_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. (CALC)"
#'   ) %>%
#'   meta_section(
#'     section_name = "rows",
#'     row_count = "There are {row_count} rows available."
#'   ) %>%
#'   incorporate()
#' 
#' # We can print the `metadata`
#' # object to see the metadata report
#' # metadata
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
#' # metadata %>% incorporate()
#' 
#' @export
incorporate <- function(metadata) {
  
  # Get the target table for this metadata object
  # TODO: Use the same scheme as the `agent` does
  tbl <- metadata$tbl
  read_fn <- metadata$read_fn
  
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
  
  meta_snippets <- metadata$meta_snippets

  for (i in seq_along(meta_snippets)) {
    
    snippet_fn <- 
      metadata$meta_snippets[[i]] %>%
      rlang::f_rhs() %>%
      rlang::eval_tidy()
    
    if (inherits(snippet_fn, "fseq")) {
      
      snippet <- snippet_fn(tbl)
      assign(x = names(metadata$meta_snippets[i]), value = snippet)
    }
  }
  
  metadata_meta_label <- 
    glue_safely(metadata$metadata[["meta_label"]], .otherwise = "(SNIPPET MISSING)")
  
  metadata_table <-
    lapply(metadata$metadata[["table"]], function(x) {
      glue_safely(x, .otherwise = "(SNIPPET MISSING)")
    })
  
  metadata_columns <- 
    lapply(metadata$metadata[["columns"]], lapply, function(x) {
      glue_safely(x, .otherwise = "(SNIPPET MISSING)")
    })
  
  extra_sections <- 
    base::setdiff(
      names(metadata$metadata),
      c("meta_label", "table", "columns")
    )
  
  metadata_extra <- metadata$metadata[extra_sections]
  
  for (i in seq_along(extra_sections)) {
    
    metadata_extra[[i]] <-
      lapply(metadata_extra[[i]], function(x) {
        glue_safely(x, .otherwise = "(SNIPPET MISSING)")
      })
  }
  
  metadata_rev <-
    c(
      list(meta_label = metadata_meta_label),
      list(table = metadata_table),
      list(columns = metadata_columns),
      metadata_extra,
      list(updated = Sys.time())
    )
  
  metadata_rev$table$`_columns` <- as.character(table.columns)
  metadata_rev$table$`_rows` <- as.character(table.rows)
  metadata_rev$table$`_type` <- table.type
  
  metadata$metadata_rev <- metadata_rev
  metadata
}
