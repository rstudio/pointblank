#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


#' Add information that focuses on aspects of the data table as a whole
#' 
#' When an *informant* object is created with the [create_informant()] function,
#' it has two starter sections: (1) 'table' and (2) 'columns'. The
#' 'table' section should contain a few properties upon creation, such as the
#' supplied table name (`name`) and table dimensions (as `_columns` and
#' `_rows`). We can add more table-based properties with the `info_tabular()`
#' function. By providing a series of named arguments (in the form
#' `entry_name = "The *info text*."`), we can add more information that makes
#' sense for describing the table as a whole.
#' 
#' The *info text* readily accepts Markdown formatting. Also, there are a few
#' *Text Tricks* that are good to know. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label text by using: (1) double parentheses
#' around text for a rectangular label as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular label like `(((label
#' text)))`. Finally, CSS styles can be applied to spans of *info text* with
#' the following form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within the `TABLE` section and the RHS is the
#'   *info text* (informational text that can be written as Markdown and further
#'   styled with *Text Tricks*).
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
#' series of named arguments (in the form `entry_name = "The *info text*."`)
#' serves as additional information for the column or columns.
#' 
#' The *info text* readily accepts Markdown formatting. Also, there are a few
#' *Text Tricks* that are good to know. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label text by using: (1) double parentheses
#' around text for a rectangular label as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular label like `(((label
#' text)))`. Finally, CSS styles can be applied to spans of *info text* with
#' the following form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param columns The column or set of columns to focus on. Can be defined as a
#'   column name in quotes (e.g., `"<column_name>"`), one or more column names
#'   in `vars()` (e.g., `vars(<column_name>)`), or with a select helper (e.g.,
#'   `starts_with("date")`).
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within `COLUMN` -> `<COLUMN_NAME>` and the RHS
#'   contains the *info text* (informational text that can be written as
#'   Markdown and further styled with *Text Tricks*).
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
#' series of named arguments (in the form `entry_name = "The *info text*."`) to
#' build the informational content for that section.
#' 
#' The *info text* readily accepts Markdown formatting. Also, there are a few
#' *Text Tricks* that are good to know. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label text by using: (1) double parentheses
#' around text for a rectangular label as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular label like `(((label
#' text)))`. Finally, CSS styles can be applied to spans of *info text* with
#' the following form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param section_name The name of the section for which this information
#'   pertains.
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within the section defined as `section_name` and
#'   the RHS is the *info text* (informational text that can be written as
#'   Markdown and further styled with *Text Tricks*).
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

#' A `fn` for `info_snippet()`: get a list of column categories
#' 
#' The `snip_list()` function can be used as an [info_snippet()] function (i.e.,
#' provided to `fn`) to get a catalog list from a table column. You can limit
#' the of items in that list with the `limit` value.
#' 
#' @param column The name of the column that contains the target values.
#' @param limit A limit of items put into the generated list. The returned text
#'   will state the remaining number of items beyond the `limit`. By default,
#'   the limit is `5`.
#' @param sep The separator to use between list items. By default, this is a
#'   comma.
#' @param and_or The type of conjunction to use between the final and
#'   penultimate list items (should the item length be below the `limit` value).
#'   If `NULL` (the default) is used, then the 'and' conjunction will be used.
#'   Alternatively, the following keywords can be used: `\"and\"`, `\"or\"`, or
#'   an empty string (for no conjunction at all).
#' @param oxford Whether to use an Oxford comma under certain conditions. By
#'   default, this is `TRUE`.
#' @param as_code Should each list item appear in a 'code font' (i.e., as
#'   monospaced text)? By default this is `TRUE`. Using `FALSE` keeps all list
#'   items in the same font as the rest of the information report.
#' @param quot_str An option for whether list items should be set in double
#'   quotes. If `NULL` (the default), the quotation marks are mainly associated
#'   with list items derived from `character` or `factor` values; numbers,
#'   dates, and logical values won't have quotation marks. We can explicitly use
#'   quotations (or not) with either `TRUE` or `FALSE` here.
#' @param lang The language to use for any joining words (from the `and_or`
#'   option) or additional words in the generated list string. By default,
#'   `NULL` will use whichever `lang` setting is available in the parent
#'   *informant* object (this is settable in the [create_informant()] `lang`
#'   argument). If specified here as an override, the language options are
#'   English (`"en"`), French (`"fr"`), German (`"de"`), Italian (`"it"`),
#'   Spanish (`"es"`), Portuguese, (`"pt"`), Chinese (`"zh"`), and Russian
#'   (`"ru"`).
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @export
snip_list <- function(column,
                      limit = 5,
                      sep = ",",
                      and_or = NULL,
                      oxford = TRUE,
                      as_code = TRUE,
                      quot_str = NULL,
                      lang = NULL) {

  if (is.character(and_or)) {
    and_or <- paste0("'", and_or[1], "'")
  } else if (is.null(and_or)) {
    and_or <- "NULL"
  }

  if (is.null(quot_str)) {
    quot_str <- "NULL"
  } else {
    if (!is.logical(quot_str)) {
      stop(
        "The value given to `quot_str` must be one of three things:\n",
        "* `NULL`: automatically sets quotes depending on the column values\n",
        "* `TRUE`: uses double quotes for every value.\n",
        "* `FALSE`: suppresses use of quotes for values.",
        call. = FALSE
      )
    }
  }
  
  if (!is.character(sep)) {
    stop("A character value must be given for `sep` in `snip_list()`",
         call. = FALSE)
  }
  
  sep <- paste0("'", sep[1], "'")

  stats::as.formula(
    as.character(
      glue::glue(
        "~ . %>% dplyr::select(<<column>>) %>% 
        dplyr::distinct() %>%
        dplyr::pull(<<column>>) %>% 
        pb_str_catalog(
          limit = <<limit[1]>>,
          sep = <<sep>>,
          and_or = <<and_or>>,
          oxford = <<oxford>>,
          as_code = <<as_code>>,
          quot_str = <<quot_str>>
        )",
        .open = "<<", .close = ">>"   
      )
    )
  )
}

#' A `fn` for `info_snippet()`: get the lowest value from a column
#' 
#' The `snip_lowest()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to get the lowest numerical, time value, or
#' alphabetical value from a column in the target table.
#' 
#' @param column The name of the column that contains the target values.
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @export
snip_lowest <- function(column) {
  
  stats::as.formula(
    as.character(
      glue::glue(
        "~ . %>%
    dplyr::select(<<column>>) %>% dplyr::distinct() %>%
    dplyr::summarize(`pb_summary` = min(<<column>>, na.rm = TRUE)) %>%
    dplyr::pull(`pb_summary`) %>% as.character()",
    .open = "<<", .close = ">>"
      )
    )
  )
}

#' A `fn` for `info_snippet()`: get the highest value from a column
#' 
#' The `snip_lowest()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to get the highest numerical, time value, or
#' alphabetical value from a column in the target table.
#' 
#' @param column The name of the column that contains the target values.
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @export
snip_highest <- function(column) {
  
  stats::as.formula(
    as.character(
      glue::glue(
        "~ . %>%
    dplyr::select(<<column>>) %>% dplyr::distinct() %>%
    dplyr::summarize(`pb_summary` = max(<<column>>, na.rm = TRUE)) %>%
    dplyr::pull(`pb_summary`) %>% as.character()",
    .open = "<<", .close = ">>"
      )
    )
  )
}
