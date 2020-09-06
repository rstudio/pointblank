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
  
  # If `x` is a table, generate an agent
  if (inherits(x, "data.frame") ||
      inherits(x, "tbl_dbi") ||
      inherits(x, "tbl_spark")) {
    
    tbl_name <- deparse(match.call()$x)
    if (tbl_name == ".") {
      tbl_name <- NA_character_
    }
    
    x <- create_agent(tbl = x, tbl_name = tbl_name)
  }
  
  # Using an agent for `x`
  if (inherits(x, "ptblank_agent")) {
    
    label <- x$label
    table.name <- x$tbl_name
    table.type <- x$tbl_src
    
    column_names <- x$col_names
    column_types_r <- x$col_types
    table.columns <- length(column_names)
    
    tbl <- x$tbl
    
    table.rows <-
      tbl %>%
      dplyr::count(name = "n") %>%
      dplyr::pull(n) %>%
      as.integer()
    
    column_list <-
      list(columns = col_schema(.tbl = tbl) %>% unclass() %>% lapply(as.list))
    
    for (i in seq_along(column_names)) {
      
      column_list[["columns"]][[column_names[i]]] <- 
        list(`_type` = paste(
          unlist(column_list[["columns"]][[column_names[i]]]), collapse = ", "
        ))
    }
  }
  
  # Generate a list to be transformed to metadata YAML
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
