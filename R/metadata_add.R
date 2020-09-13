#' Add metadata that focuses on aspects of the data table as a whole
#' 
#' When a metadata object is created with the [create_metadata()] function, it
#' has two starter sections: (1) 'table' and (2) 'columns'. We can further The
#' 'table' section should contain a few properties upon creation, such as the
#' supplied table name (`name`) and table dimensions (as `_columns` and
#' `_rows`). We can add more table-based properties with the `meta_table()`
#' function. By providing a series of named arguments (in the form
#' `property_name = "Description of property."`), we can add more metadata that
#' makes sense for describing the table as a whole.
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param ... Metadata parameters as a series of named arguments.
#'
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

#' Add metadata that focuses on aspects of a data table's columns
#' 
#' Upon creation of a metadata object (with the [create_metadata()] function),
#' there are two sections containing properties: (1) 'table' and (2) 'columns'.
#' The 'columns' section is initialized with the table's column names and their
#' types (as `_type`). Beyond that, it is useful to provide details about the
#' nature of each column and we can do that with the `meta_columns()` function.
#' A single column (or multiple columns) is targeted, and then a series of named
#' arguments (in the form `property_name = "Description of property."`) serves
#' as additional metadata for the column or columns.
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param columns The column or set of columns to focus on.
#' @param ... Metadata parameters as a series of named arguments.
#'
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

#' Add metadata that focuses on some key aspect of the data table
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param section_name The name of the section for which this metadata pertains.
#' @param ... Metadata parameters as a series of named arguments.
#'
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
