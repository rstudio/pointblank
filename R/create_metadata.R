

#' @export
create_metadata <- function(tbl = NULL,
                            read_fn = NULL,
                            agent = NULL,
                            tbl_name = NULL,
                            metadata_id = NULL,
                            label = NULL,
                            lang = NULL,
                            locale = NULL) {
  
  # Generate a label if none provided
  if (is.null(label)) {
    label <- paste0("[", gsub(" ", "|", as.character(Sys.time())), "]")
  }
 
  # Set the `locale` to the `lang` value if `locale` isn't set
  if (is.null(locale)) locale <- lang
  
  # If nothing is provided for either `tbl`, `read_fn`, or `agent`,
  # this function needs to be stopped
  if (is.null(tbl) && is.null(read_fn) && is.null(agent)) {
    
    stop(
      "A table object, table-reading function, or agent must be supplied:\n",
      " * Use a table object in the `tbl` argument.\n",
      " * Or supply a table-reading function in `read_fn`.\n",
      " * Or even an agent with some connection to a table.",
      call. = FALSE
    )
  }
  
  # Stop function if both a table and an agent are provided 
  if (!is.null(tbl) && !is.null(agent)) {
    stop("A `tbl` and a `agent` cannot both be provided.", call. = FALSE)
  }
  
  if (!is.null(tbl) || !is.null(read_fn)) {
    
    x <- 
      create_agent(
        tbl = tbl,
        read_fn = read_fn,
        tbl_name = tbl_name
      )
    
  } else {
    x <- agent
  }
  
  label <- x$label
  table.name <- x$tbl_name
  table.type <- x$tbl_src
  
  column_names <- x$col_names
  column_types_r <- x$col_types
  table.columns <- length(column_names)
  
  tbl <- x$tbl
  
  table.rows <- dplyr::count(tbl, name = "n") %>% dplyr::pull(n)
  
  column_list <- list(columns = lapply(col_schema(.tbl = tbl), as.list))
  
  for (i in seq_along(column_names)) {
    
    column_list[["columns"]][[column_names[i]]] <- 
      list(`_type` = paste(
        unlist(column_list[["columns"]][[column_names[i]]]),
        collapse = ", "
      ))
  }
  
  metadata_list <-
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
    
  # Create the metadata list object
  metadata <-
    list(
      read_fn = read_fn,
      tbl_name = table.name,
      label = label,
      lang = lang,
      locale = locale,
      metadata = metadata_list
    )
  
  # Assign the class attribute value `ptblank_metadata` to
  # the `metadata` object
  attr(metadata, "class") <- "ptblank_metadata"
  
  metadata
}

#' @export
meta_table <- function(metadata,
                       ...) {
  
  metadata_items <- list(...)

  metadata_list <- metadata$metadata
  metadata_table <- metadata_list$table

  for (i in seq_along(metadata_items)) {
    
    item_name <- names(metadata_items[i])
    item_value <- metadata_items[[i]]
    
    if (!(item_name %in% names(metadata_table))) {
      # Case where `item_name` doesn't exist for column
      metadata_table <- 
        c(
          metadata_table,
          metadata_items[i]
        )
    } else {
      # Case where `item_name` exists for the column
      
      metadata_table[[item_name]] <- item_value
    }
  }

  metadata$metadata$table <- metadata_table
  
  metadata
}

#' @export
meta_columns <- function(metadata,
                         columns,
                         ...) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  metadata_items <- list(...)
  
  metadata_list <- metadata$metadata
  metadata_columns <- metadata_list$columns

  x <- dplyr::as_tibble(metadata_columns %>% lapply(function(x) 1))
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  for (column in columns) {
    for (i in seq_along(metadata_items)) {

      item_name <- names(metadata_items[i])
      item_value <- metadata_items[[i]]
      
      if (!(item_name %in% names(metadata_columns[[column]]))) {
      # Case where `item_name` doesn't exist for column
        metadata_columns[[column]] <- 
          c(
            metadata_columns[[column]],
            metadata_items[i]
          )
      } else {
      # Case where `item_name` exists for the column
        
        metadata_columns[[column]][[item_name]] <- item_value
      }
    }
  }
  
  metadata$metadata$columns <- metadata_columns
  
  metadata
}

#' @export
meta_section <- function(metadata,
                         section_name,
                         ...) {
  
  metadata_items <- list(...)
  
  metadata_list <- metadata$metadata
  
  if (is.null(metadata_list[[section_name]])) {
    metadata_section <- list()
    section_new <- TRUE
  } else {
    metadata_section <- metadata_list[[section_name]]
    section_new <- FALSE
  }

  for (i in seq_along(metadata_items)) {
    
    item_name <- names(metadata_items[i])
    item_value <- metadata_items[[i]]
    
    if (!(item_name %in% names(metadata_section))) {
      # Case where `item_name` doesn't exist for the section
      metadata_section <- 
        c(
          metadata_section,
          metadata_items[i]
        )
    } else {
      # Case where `item_name` exists for the section
      metadata_section[[item_name]] <- item_value
    }
  }
  
  section_list <- list(a = metadata_section)
  names(section_list) <- section_name

  if (section_new) {
    metadata$metadata <- c(metadata$metadata, section_list)
  }
  
  if (!section_new) {
    metadata$metadata[section_name] <- section_list
  }
  
  metadata
}
