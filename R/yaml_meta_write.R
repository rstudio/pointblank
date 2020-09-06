#' Write table metadata to a YAML file
#' 
#' With `meta_yaml_write()` we can take a data table and generate
#' a metadata YAML file. With that **pointblank** metadata YAML file, we can
#' further modify the YAML markup and add informational fields. Once that file
#' has been edited, we can get a tabular reporting object with the 
#' [get_metadata_report()] function.
#' 
#' @param x The input table. This can be a data frame, a tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object.
#' @param filename The name of the YAML file to create on disk. It is
#'   recommended that either the `.yaml` or `.yml` extension be used for this
#'   file.
#' @param path An optional path to which the YAML file should be saved (combined
#'   with `filename`).
#' 
#' @export
meta_yaml_write <- function(x = NULL,
                            filename,
                            path = NULL) {
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  
  filename <- fs::path_expand(filename)
  
  # Using an agent for `x`
  if (inherits(x, "ptblank_agent")) {
    
    label <- agent$label
    table.name <- agent$tbl_name
    table.type <- agent$tbl_src
    
    column_names <- agent$col_names
    column_types_r <- agent$col_types
    table.columns <- length(column_names)
    
    
    tbl <- agent$tbl
    if (inherits(tbl, "data.frame")) {
      table.rows <- nrow(tbl)
    } else {
      table.rows <- NULL
    }
    
    column_list <-
      list(
        columns = col_schema(.tbl = tbl) %>% unclass() %>% lapply(as.list)
      )
    
    for (i in seq_along(column_names)) {
      
      column_list[["columns"]][[column_names[i]]] <- 
        list(`_type` = paste(unlist(column_list[["columns"]][[column_names[i]]]), collapse = ", "))
      
    }
  }
  
  meta_yaml_list <-
    c(
      list(
        label = label,
        table = list(
          name = table.name,
          `_columns` = table.columns,
          `_rows` = table.rows,
          `_type` = table.type
        )
      ),
      column_list
    )
  
  
  meta_yaml_list %>%
    yaml::write_yaml(
      file = filename,
      handlers = list(
        logical = function(x) {
          result <- ifelse(x, "true", "false")
          class(result) <- "verbatim"
          result
        }
      )
    )
}
