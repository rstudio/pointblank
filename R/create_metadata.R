#' Create a pointblank metadata object
#'
#' The `create_metadata()` function creates an *metadata* object, which is used
#' in a *metadata enrichment* workflow. The overall aim of this workflow is to
#' collect and generate useful information on data tables. We can supply as many
#' metadata parameters that are useful for describing a particular data table.
#' The *metadata* object created by the `create_metadata()` function takes
#' metadeta-focused functions.
#'
#' @param tbl The input table. This can be a data frame, a tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object. Alternatively, a function can be used to
#'   read in the input data table with the `read_fn` argument (in which case,
#'   `tbl` can be `NULL`).
#' @param read_fn A function that's used for reading in the data. If a `tbl` is
#'   not provided, then this function will be invoked. However, if both a `tbl`
#'   *and* a `read_fn` is specified, then the supplied `tbl` will take priority.
#'   There are two ways to specify a `read_fn`: (1) using a function (e.g.,
#'   `function() { <table reading code> }`) or, (2) with an R formula
#'   expression.
#' @param agent A pointblank *agent* object. This object can be used instead of
#'   supplying a table in `tbl` or a table-reading function in `read_fn`.
#' @param tbl_name A optional name to assign to the input table object. If no
#'   value is provided, a name will be generated based on whatever information
#'   is available.
#' @param label An optional label for the metadata report. If no value is
#'   provided, a label will be generated based on the current system time.
#'   Markdown can be used here to make the label more visually appealing (it
#'   will appear in the header area of the metadata report).
#' @param lang The language to use for the metadata report (a summary table that
#'   provides all of the available metadata for the table. By default, `NULL`
#'   will create English (`"en"`) text. Other options include French (`"fr"`),
#'   German (`"de"`), Italian (`"it"`), Spanish (`"es"`), Portuguese, (`"pt"`),
#'   and Chinese (`"zh"`).
#' @param locale An optional locale ID to use for formatting values in the
#'   metadata report summary table according the locale's rules. Examples
#'   include `"en_US"` for English (United States) and `"fr_FR"` for French
#'   (France); more simply, this can be a language identifier without a country
#'   designation, like "es" for Spanish (Spain, same as `"es_ES"`).
#'   
#' @return A `ptblank_metadata` object.
#' 
#' @examples 
#' # Let's walk through how we can
#' # generate some useful metadata for an
#' # extremely small table; it's actually
#' # called `small_table` and we can find
#' # it as a dataset in this package
#' small_table
#' 
#' # We create a pointblank `metadata`
#' # object with `create_metadata()`;
#' # let's use that function with the
#' # `small_table` dataset
#' metadata <- create_metadata(small_table)
#' 
#' # This function creates some metadata
#' # without any extra help by profiling
#' # the supplied table object; it adds
#' # the sections: (1) 'table' and
#' # (2) 'columns' and we can print the
#' # object to see the metadata report
#' # metadata
#' 
#' # Alternatively, we can visualize this
#' # metadata into the same report by using
#' # `get_metadata_report()`
#' report <- get_metadata_report(metadata)
#' class(report)
#' 
#' @export
create_metadata <- function(tbl = NULL,
                            read_fn = NULL,
                            agent = NULL,
                            tbl_name = NULL,
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
        meta_label = label,
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
      meta_label = label,
      lang = lang,
      locale = locale,
      metadata = metadata_list
    )
  
  # Assign the class attribute value `ptblank_metadata` to
  # the `metadata` object
  attr(metadata, "class") <- "ptblank_metadata"
  
  metadata
}

#' Add metadata that focuses on aspects of the data table as a whole
#' 
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param ... Metadata parameters.
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
#' @param metadata A metadata object of class `ptblank_metadata`.
#' @param columns The column or set of columns to focus on.
#' @param ... Metadata parameters.
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
#' @param ... Metadata parameters.
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
