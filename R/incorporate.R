#' Given a metadata object, update and incorporate table snippets
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
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
    as.character(glue::glue(metadata$metadata[["meta_label"]]))
  
  metadata_table <-
    lapply(metadata$metadata[["table"]], function(x) {
      glue::glue(x) %>% as.character()
    })
  
  metadata_columns <- 
    lapply(metadata$metadata[["columns"]], lapply, function(x) {
      glue::glue(x) %>% as.character()
    })
  
  extra_sections <- 
    setdiff(names(metadata$metadata), c("meta_label", "table", "columns"))
  
  metadata_extra <- metadata$metadata[extra_sections]
  
  for (i in seq_along(extra_sections)) {
    
    metadata_extra[[i]] <-
      lapply(metadata_extra[[i]], function(x) {
        glue::glue(x) %>% as.character()
      })
  }
  
  metadata_rev <-
    c(list(meta_label = metadata_meta_label),
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
