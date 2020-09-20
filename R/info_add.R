#' Add information that focuses on aspects of the data table as a whole
#' 
#' When an *informant* object is created with the [create_informant()] function,
#' it has two starter sections: (1) 'table' and (2) 'columns'. The
#' 'table' section should contain a few properties upon creation, such as the
#' supplied table name (`name`) and table dimensions (as `_columns` and
#' `_rows`). We can add more table-based properties with the `info_tabular()`
#' function. By providing a series of named arguments (in the form
#' `entry_name = "The information."`), we can add more information that makes
#' sense for describing the table as a whole.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param ... Information entries as a series of named arguments.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' 
#' # The `informant` object has the 'table'
#' # and 'columns' sections; we can add more
#' # properties to the 'table' section
#' informant <-
#'   informant %>%
#'   info_tabular(
#'     row_definition = "A row has randomized values.",
#'     source = c(
#'       "- From the **pointblank** package.",
#'       "- [https://rich-iannone.github.io/pointblank/]()"
#'      )
#'    )
#' 
#' # Upon printing the `informant` object, we see
#' # the additions made to the 'Table' section
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then information can
#' # be directly edited or modified
#' # yaml_write(
#' #   informant = informant,
#' #   filename = "informant.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' # informant <-
#' #   yaml_read_informant(path = "informant.yml")
#'
#' @section Figures:
#' \if{html}{\figure{man_info_tabular_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-1
#'
#' @export
info_tabular <- function(x,
                         ...) {
  
  metadata_items <- list(...)
  
  metadata <- x
  
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

#' Add information that focuses on aspects of a data table's columns
#' 
#' Upon creation of an *informant* object (with the [create_informant()]
#' function), there are two sections containing properties: (1) 'table' and (2)
#' 'columns'. The 'columns' section is initialized with the table's column names
#' and their types (as `_type`). Beyond that, it is useful to provide details
#' about the nature of each column and we can do that with the `info_columns()`
#' function. A single column (or multiple columns) is targeted, and then a
#' series of named arguments (in the form 
#' `entry_name = "The information."`) serves as additional
#' information for the column or columns.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param columns The column or set of columns to focus on. Can be defined as a
#'   column name in quotes (e.g., `"<column_name>"`), one or more column names
#'   in `vars()` (e.g., `vars(<column_name>)`), or with a select helper (e.g.,
#'   `starts_with("date")`).
#' @param ... Information entries as a series of named arguments.
#' @param .add Should new text be added to existing text? This is `TRUE` by
#'   default; setting to `FALSE` replaces any existing text for a property.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' 
#' # The `informant` object has the 'table'
#' # and 'columns' sections; we can add more
#' # properties to individual columns in
#' # the 'columns' section
#' informant <-
#'   informant %>%
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
#'   )
#' 
#' # Upon printing the `informant` object, we see
#' # the additions made to the 'Columns' section
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, information can
#' # be directly edited or modified
#' # yaml_write(
#' #   informant = informant,
#' #   filename = "informant.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' # informant <-
#' #   yaml_read_informant(path = "informant.yml")
#' 
#' @section Figures:
#' \if{html}{\figure{man_info_columns_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-2
#'
#' @export
info_columns <- function(x,
                         columns,
                         ...,
                         .add = TRUE) {

  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  metadata_items <- list(...)
  
  metadata <- x
  
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

#' Add information that focuses on some key aspect of the data table
#' 
#' While the [info_tabular()] and [info_columns()] functions allow us to
#' add/modify info text for specific sections, the `info_section()`
#' makes it possible to add sections of our own choosing and the information
#' that make sense for those sections. Define a `section_name` and provide a
#' series of named arguments (in the form `entry_name = "The information."`) to
#' build the informational content for that section.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param section_name The name of the section for which this information
#'   pertains.
#' @param ... Information entries as a series of named arguments.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' # Create a pointblank `informant`
#' # object with `create_informant()`;
#' # we specify a `read_fn` with the
#' # `~` followed by a statement that
#' # gets the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' 
#' # The `informant` object has the 'table'
#' # and 'columns' sections; we can create
#' # entirely different sections with their
#' # own properties using `info_section()`
#' informant <-
#'   informant %>%
#'   info_section(
#'     section_name = "notes",
#'     creation = "Dataset generated on (2020-01-15).",
#'     usage = "`small_table %>% dplyr::glimpse()`"
#'   )
#' 
#' # Upon printing the `informant` object, we see
#' # the addition of the 'Notes' section and its
#' # own information
#' 
#' # The `informant` object can be written to
#' # a YAML file with the `yaml_write()`
#' # function; then, information can
#' # be directly edited or modified
#' # yaml_write(
#' #   informant = informant,
#' #   filename = "informant.yml"
#' # )
#' 
#' # The YAML file can then be read back
#' # into an informant object with the
#' # `yaml_read_informant()` function
#' # informant <-
#' #   yaml_read_informant(path = "informant.yml")
#' 
#' @section Figures:
#' \if{html}{\figure{man_info_section_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-3
#'
#' @export
info_section <- function(x,
                         section_name,
                         ...) {
  
  metadata_items <- list(...)
  
  metadata <- x
  
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

#' Generate a useful text 'snippet' from the target table
#' 
#' Getting little snippets of information from a table goes hand-in-hand with
#' mixing those bits of info with your table info. Call `info_snippet()` to
#' define a snippet and how you'll get that from the target table (it's with a
#' function). So long as you know how to interact with a table and extract
#' information, you can easily define snippets for a *informant* object. And
#' once those snippets are defined, you can insert them into the info text as
#' defined through the `info_*()` functions. Just use curly braces with the
#' `snippet_name` inside (e.g., `"This column has {n_cat} categories."`).
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param snippet_name The name for snippet, which is used for interpolating the
#'   snippet itself into info text.
#' @param fn A function that obtains a snippet of data from the target table.
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
#'     tbl_name = "test_table",
#'     label = "An example."
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
#' # the info text to be updated
#' informant <-
#'   informant %>% incorporate()
#'   
#' @section Figures:
#' \if{html}{\figure{man_info_snippet_1.png}{options: width=100\%}}
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-4
#' 
#' @export
info_snippet <- function(x,
                         snippet_name,
                         fn) {
  
  metadata <- x
  
  if (!(snippet_name %in% names(metadata$meta_snippets))) {
    # Case where `snippet_name` doesn't exist in `meta_snippets`
    
    meta_snippet <- list(a = fn)
    names(meta_snippet) <- snippet_name
    
    metadata$meta_snippets <- c(metadata$meta_snippets, meta_snippet)
    
  } else {
    # Case where `snippet_name` exists in `meta_snippets`
    
    metadata$meta_snippets[[snippet_name]] <- fn
  }
  
  metadata
}
