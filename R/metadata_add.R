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
#' @return A `ptblank_metadata` object.
#' 
#' @examples 
#' # Create a pointblank `metadata`
#' # object with the `create_metadata()` and
#' # the `small_table` dataset
#' metadata <- create_metadata(small_table)
#' 
#' # The `metadata` object has the 'table'
#' # and 'columns' sections; we can add more
#' # properties to the 'table' section
#' metadata <-
#'   metadata %>%
#'   meta_table(
#'     row_definition = "A row has randomized values.",
#'     source = c(
#'       "- From the **pointblank** package",
#'       "- [https://rich-iannone.github.io/pointblank/]()"
#'      )
#'    )
#' 
#' # Upon printing the `metadata` object,
#' # we see the additions made to the 'table'
#' # section (before it wasn't visible at all
#' # since the properties where using in the
#' # report header)
#' # metadata
#' 
#' # The `metadata` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, metadata properties can
#' # be directly edited or modified
#' # yaml_write(
#' #   metadata = metadata,
#' #   filename = "metadata.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into a metadata object with the
#' # `meta_yaml_read()` function
#' # metadata <-
#' #   meta_yaml_read(path = "metadata.yml")
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
#' @param columns The column or set of columns to focus on. Can be defined as a
#'   column name in quotes (e.g., `"<column_name>"`), one or more column names
#'   in `vars()` (e.g., `vars(<column_name>)`), or with a select helper (e.g.,
#'   `starts_with("date")`).
#' @param ... Metadata parameters as a series of named arguments.
#' @param .add Should new text be added to existing text? This is `TRUE` by
#'   default; setting to `FALSE` replaces any existing text for a property.
#' 
#' @return A `ptblank_metadata` object.
#' 
#' @examples 
#' # Create a pointblank `metadata`
#' # object with the `create_metadata()` and
#' # the `small_table` dataset
#' metadata <- create_metadata(small_table)
#' 
#' # The `metadata` object has the 'table'
#' # and 'columns' sections; we can add more
#' # properties to individual columns in
#' # the 'columns' section
#' metadata <-
#'   metadata %>%
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
#'   )
#' 
#' # Upon printing the `metadata` object, we see
#' # the additions made to the 'columns' section
#' # metadata
#' 
#' # The `metadata` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, metadata properties can
#' # be directly edited or modified
#' # yaml_write(
#' #   metadata = metadata,
#' #   filename = "metadata.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into a metadata object with the
#' # `meta_yaml_read()` function
#' # metadata <-
#' #   meta_yaml_read(path = "metadata.yml")
#'
#' @export
meta_columns <- function(metadata,
                         columns,
                         ...,
                         .add = TRUE) {
  
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
        
        if (.add) {
        # Appending case: if `.add = TRUE` append `item_value`
        # to existing string
          metadata_columns[[column]][[item_name]] <- 
            paste(
              metadata_columns[[column]][[item_name]],
              item_value
            )
        } else {
          # Replacement case: if `.add = FALSE` replace
          # existing string with `item_value`
          metadata_columns[[column]][[item_name]] <- item_value
        }
      }
    }
  }
  
  metadata$metadata$columns <- metadata_columns
  
  metadata
}

#' Add metadata that focuses on some key aspect of the data table
#' 
#' While the [meta_table()] and [meta_columns()] functions allow us to
#' add/modify metadata properties for specific sections, the `meta_section()`
#' makes it possible to add sections of our own choosing and the properties that
#' make sense for those sections. Define a `section_name` and provide a series
#' of named arguments (in the form `property_name = "Description of property."`)
#' to build the metadata content for that section.
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param section_name The name of the section for which this metadata pertains.
#' @param ... Metadata parameters as a series of named arguments.
#' 
#' @return A `ptblank_metadata` object.
#' 
#' @examples 
#' # Create a pointblank `metadata`
#' # object with the `create_metadata()` and
#' # the `small_table` dataset
#' metadata <- create_metadata(small_table)
#' 
#' # The `metadata` object has the 'table'
#' # and 'columns' sections; we can create
#' # entirely different sections with their
#' # own properties using `meta_section()`
#' metadata <-
#'   metadata %>%
#'   meta_section(
#'     section_name = "notes",
#'     creation = "Dataset generated on (2020-01-15).",
#'     usage = "`small_table %>% dplyr::glimpse()`"
#'   )
#' 
#' # Upon printing the `metadata` object, we see
#' # the addition of the 'notes' section and its
#' # own metadata
#' 
#' # The `metadata` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, metadata properties can
#' # be directly edited or modified
#' # yaml_write(
#' #   metadata = metadata,
#' #   filename = "metadata.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into a metadata object with the
#' # `meta_yaml_read()` function
#' # metadata <-
#' #   meta_yaml_read(path = "metadata.yml")
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
