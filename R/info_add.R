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
#' @description 
#' When an *informant* object is created with the [create_informant()] function,
#' it has two starter sections: (1) 'table' and (2) 'columns'. The 'table'
#' section should contain a few properties upon creation, such as the supplied
#' table name (`name`) and table dimensions (as `_columns` and `_rows`). We can
#' add more table-based properties with the `info_tabular()` function. By
#' providing a series of named arguments (in the form `entry_name = "The *info
#' text*."`), we can add more information that makes sense for describing the
#' table as a whole.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). When `info_tabular()` is
#' represented in YAML, *info text* goes into subsections of the top-level
#' `table` key. Here is an example of how a call of `info_tabular()` is
#' expressed in R code and in the corresponding YAML representation.
#' 
#' R statement:
#' 
#' ```r
#' informant %>% 
#'   info_tabular(
#'     section_1 = "*info text* 1.",
#'     `section 2` = "*info text* 2 and {snippet_1}"
#'   )
#' ```
#' 
#' YAML representation:
#' 
#' ```yaml
#' table:
#'   _columns: 23
#'   _rows: 205.0
#'   _type: tbl_df
#'   section_1: '*info text* 1.'
#'   section 2: '*info text* 2 and {snippet_1}'
#' ```
#' 
#' Subsection titles as defined in `info_tabular()` can be set in backticks if
#' they are not syntactically correct as an argument name without them (e.g.,
#' when using spaces, hyphens, etc.).
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet_1}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param ... Information entries as a series of named arguments. The names
#'   refer to subsection titles within the `TABLE` section and the values are
#'   the *info text* (informational text that can be written as Markdown and
#'   further styled with *Text Tricks*).
#' 
#' @return A `ptblank_informant` object.
#' 
#' @section Examples:
#'  
#' Create a pointblank `informant` object with [create_informant()]. We can
#' specify a `tbl` with the `~` followed by a statement that gets the
#' `small_table` dataset.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' ```
#' 
#' We can add *info text* to describe the table with the various `info_*()`
#' functions. In this example, we'll use [info_tabular()] to generally describe
#' the `small_table` dataset.
#' 
#' ```r
#' informant <-
#'   informant %>%
#'   info_tabular(
#'     `Row Definition` = "A row has randomized values.",
#'     Source = c(
#'       "- From the **pointblank** package.",
#'       "- [https://rich-iannone.github.io/pointblank/]()"
#'      )
#'    )
#' ```
#' 
#' Upon printing the `informant` object, we see the additions made to the
#' 'Table' section of the report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_info_tabular_1.png")`
#' }
#' }
#' 
#'
#' @family Information Functions
#' @section Function ID:
#' 3-1
#'
#' @export
info_tabular <- function(
    x,
    ...
) {
  
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
#' @description
#' Upon creation of an *informant* object (with the [create_informant()]
#' function), there are two sections containing properties: (1) 'table' and (2)
#' 'columns'. The 'columns' section is initialized with the table's column names
#' and their types (as `_type`). Beyond that, it is useful to provide details
#' about the nature of each column and we can do that with the `info_columns()`
#' function. A single column (or multiple columns) is targeted, and then a
#' series of named arguments (in the form `entry_name = "The *info text*."`)
#' serves as additional information for the column or columns.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). The way that information on table
#' columns is represented in YAML works like this: *info text* goes into
#' subsections of YAML keys named for the columns, which are themselves part of
#' the top-level `columns` key. Here is an example of how several calls of
#' `info_columns()` are expressed in R code and how the result corresponds to
#' the YAML representation.
#' 
#' ```
#' # R statement
#' informant %>% 
#'   info_columns(
#'     columns = "date_time",
#'     info = "*info text* 1."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "*info text* 2."
#'   ) %>%
#'   info_columns(
#'     columns = "item_count",
#'     info = "*info text* 3. Statistics: {snippet_1}."
#'   ) %>%
#'   info_columns(
#'     columns = vars(date, date_time),
#'     info = "UTC time."
#'   )
#' 
#' # YAML representation
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'     info: '*info text* 1. UTC time.'
#'   date:
#'     _type: Date
#'     info: '*info text* 2. UTC time.'
#'   item_count:
#'     _type: integer
#'     info: '*info text* 3. Statistics: {snippet_1}.'
#' ```
#' 
#' Subsections represented as column names are automatically generated when
#' creating an informant. Within these, there can be multiple subsections used
#' for holding *info text* on each column. The subsections used across the
#' different columns needn't be the same either, the only commonality that
#' should be enforced is the presence of the `_type` key (automatically updated
#' at every [incorporate()] invocation). 
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet_1}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
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
#' @section Examples:
#'  
#' Create a pointblank `informant` object with [create_informant()]. We can
#' specify a `tbl` with the `~` followed by a statement that gets the
#' `small_table` dataset.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' ```
#' 
#' We can add *info text* to describe the table with the various `⁠info_*(`
#' functions. In this example, we'll use `info_columns()` multiple times to
#' describe some of the columns in the `small_table` dataset. Note here that
#' *info text* calls are additive to the existing content inside of the
#' various subsections (i.e., the text will be appended and won't overwrite
#' existing if it lands in the same area).
#' 
#' ```r
#' informant <-
#'   informant %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to 10. ((SIMPLE))"
#'   ) %>%
#'   info_columns(
#'     columns = starts_with("date"),
#'     info = "Time-based values (e.g., `Sys.time()`)."
#'   ) %>%
#'   info_columns(
#'     columns = "date",
#'     info = "The date part of `date_time`. ((CALC))"
#'   )
#' ```
#' 
#' Upon printing the `informant` object, we see the additions made to the
#' 'Columns' section.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_info_columns_1.png")`
#' }
#' }
#' 
#' @section Figures:
#' \if{html}{\figure{man_info_columns_1.png}{options: width=100\%}}
#'
#' @family Information Functions
#' @section Function ID:
#' 3-2
#'
#' @export
info_columns <- function(
    x,
    columns,
    ...,
    .add = TRUE
) {
  
  # Capture the `columns` expression
  columns <- rlang::enquo(columns)
  
  metadata_items <- list(...)
  
  metadata <- x
  
  metadata_list <- metadata$metadata
  metadata_columns <- metadata_list$columns
  
  x <- dplyr::as_tibble(metadata_columns %>% lapply(function(x) 1))
  
  # Resolve the columns based on the expression
  columns <- resolve_columns(x = x, var_expr = columns, preconditions = NULL)
  
  if (length(columns) == 1 && is.na(columns)) {
   
    warning(
      "No columns were matched with the expression used, so, no ",
      "info was added.",
      call. = FALSE
    )
    
    return(metadata)
  }
  
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


#' Add column information from another data table
#' 
#' @description
#' The `info_columns_from_tbl()` function is a wrapper around the
#' [info_columns()] function and is useful if you wish to apply *info text* to
#' columns where that information already exists in a data frame (or in some
#' form that can readily be coaxed into a data frame). The form of the input
#' `tbl` (the one that contains column metadata) has a few basic requirements:
#' 
#' - the data frame must have two columns
#' - both columns must be of class `character`
#' - the first column should contain column names and the second should contain
#' the *info text*
#' 
#' Each column that matches across tables (i.e., the `tbl` and the target table
#' of the informant) will have a new entry for the `"info"` property. Empty or
#' missing info text will be pruned from `tbl`.
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param tbl The two-column data frame which contains metadata about the target
#'   table in the informant object. 
#' @param .add Should new text be added to existing text? This is `TRUE` by
#'   default; setting to `FALSE` replaces any existing text for the `"info"`
#'   property.
#' 
#' @return A `ptblank_informant` object.
#' 
#' @section Examples:
#' 
#' Create a pointblank `informant` object with [create_informant()]. We can
#' specify a `tbl` with the `~` followed by a statement that gets the
#' `game_revenue` dataset.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ game_revenue,
#'     tbl_name = "game_revenue",
#'     label = "An example."
#'   )
#' ```
#' 
#' We can add *info text* to describe the data in the various columns of the
#' table by using [info_columns()] or information in another table (with
#' [info_columns_from_tbl()]). Here, we'll do the latter. The
#' `game_revenue_info` dataset is included in **pointblank** and it contains
#' metadata for `game_revenue`.
#' 
#' ```{r}
#' game_revenue_info
#' ```
#' 
#' The `info_columns_from_tbl()` function takes a table object where the first
#' column has the column names and the second contains the *info text*.
#' 
#' ```r
#' informant <-
#'   informant %>%
#'   info_columns_from_tbl(tbl = game_revenue_info)
#' ```
#' 
#' Upon printing the `informant` object, we see the additions made to the
#' 'Columns' section by the `info_columns_from_tbl(tbl = game_revenue_info)`
#' call.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_info_columns_from_tbl_1.png")`
#' }
#' }
#' 
#' We can continue to add more *info text* to describe the columns since the
#' process is additive. The `info_columns_from_tbl()` function populates the
#' `info` subsection and any calls of [info_columns()] that also target a `info`
#' subsection will append text. Here, we'll add content for the `item_revenue`
#' and `acquisition` columns and view the updated report.
#' 
#' ```r
#' informant <-
#'   informant %>%
#'   info_columns(
#'     columns = "item_revenue",
#'     info = "Revenue reported in USD."
#'   ) %>%
#'   info_columns(
#'     columns = "acquisition",
#'     `top list` = "{top5_aq}"
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "top5_aq",
#'     fn = snip_list(column = "acquisition")
#'   ) %>%
#'   incorporate()
#' 
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_info_columns_from_tbl_2.png")`
#' }
#' }
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-3
#' 
#' @seealso The [info_columns()] function, which allows for manual entry of
#'   *info text*.
#'
#' @export
info_columns_from_tbl <- function(
    x,
    tbl,
    .add = TRUE
) {

  # Ensure that `tbl` passes a validation check 
  tbl <- check_info_columns_tbl(tbl = tbl)
  
  # Call `info_columns()` for every row in `tbl`
  for (i in seq_along(tbl$column)) {

    x <- 
      info_columns(
        x = x,
        columns = tbl$column[i],
        info = tbl$info[i],
        .add = .add
      )
  }  
  
  x
}

check_info_columns_tbl <- function(tbl) {
  
  if (
    !inherits(tbl, "data.frame") &&
    !ncol(tbl) == 2 &&
    !inherits(dplyr::pull(tbl, 1), "character") &&
    !inherits(dplyr::pull(tbl, 2), "character")
  ) {
    
    stop(
      "The input `tbl` must fulfill the following conditions:\n",
      "* inherits from data.frame\n",
      "* has two columns\n",
      "* both columns must be of type `character`",
      call. = FALSE
    )
  }
  
  # Standardize the column names in `tbl`
  colnames(tbl) <- c("column", "info")
  
  # Filter out any missing or NA values in the `info` column
  tbl <- 
    tbl %>%
    dplyr::filter(!is.na(info) & !grepl("^\\s*$", info))
  
  colnames_in_tbl <- dplyr::pull(tbl, column)
  
  if (anyDuplicated(colnames_in_tbl) != 0) {
    
    stop(
      "The input `tbl` contains duplicate column names in the first column.",
      call. = FALSE
    )
  }
  
  tbl
}

#' Add information that focuses on some key aspect of the data table
#' 
#' @description 
#' While the [info_tabular()] and [info_columns()] functions allow us to
#' add/modify info text for specific sections, the `info_section()` makes it
#' possible to add sections of our own choosing and the information that make
#' sense for those sections. Define a `section_name` and provide a series of
#' named arguments (in the form `entry_name = "The *info text*."`) to build the
#' informational content for that section.
#' 
#' @section Info Text:
#' The *info text* that's used for any of the `info_*()` functions readily
#' accepts Markdown formatting, and, there are a few *Text Tricks* that can be
#' used to spice up the presentation. Markdown links written as `< link url >`
#' or `[ link text ]( link url )` will get nicely-styled links. Any dates
#' expressed in the ISO-8601 standard with parentheses, `"(2004-12-01)"`, will
#' be styled with a font variation (monospaced) and underlined in purple. Spans
#' of text can be converted to label-style text by using: (1) double parentheses
#' around text for a rectangular border as in `((label text))`, or (2) triple
#' parentheses around text for a rounded-rectangular border like `(((label
#' text)))`.
#'
#' CSS style rules can be applied to spans of *info text* with the following
#' form:
#' 
#' `[[ info text ]]<< CSS style rules >>`
#' 
#' As an example of this in practice suppose you'd like to change the color of
#' some text to red and make the font appear somewhat thinner. A variation on
#' the following might be used:
#' 
#' `"This is a [[factor]]<<color: red; font-weight: 300;>> value."`
#' 
#' The are quite a few CSS style rules that can be used to great effect. Here
#' are a few you might like:
#' 
#' - `color: <a color value>;` (text color)
#' - `background-color: <a color value>;` (the text's background color)
#' - `text-decoration: (overline | line-through | underline);`
#' - `text-transform: (uppercase | lowercase | capitalize);`
#' - `letter-spacing: <a +/- length value>;`
#' - `word-spacing: <a +/- length value>;`
#' - `font-style: (normal | italic | oblique);`
#' - `font-weight: (normal | bold | 100-900);`
#' - `font-variant: (normal | bold | 100-900);`
#' - `border: <a color value> <a length value> (solid | dashed | dotted);`
#' 
#' In the above examples, 'length value' refers to a CSS length which can be
#' expressed in different units of measure (e.g., `12px`, `1em`, etc.). Some
#' lengths can be expressed as positive or negative values (e.g., for
#' `letter-spacing`). Color values can be expressed in a few ways, the most
#' common being in the form of hexadecimal color values or as CSS color names.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). Extra sections (i.e., neither the
#' `table` nor the `columns` sections) can be generated and filled with *info
#' text* by using one or more calls of `info_section()`. This is how it is
#' expressed in both R code and in the YAML representation.
#' 
#' ```
#' # R statement
#' informant %>% 
#'   info_section(
#'     section_name = "History",
#'     Changes = "
#' - Change 1
#' - Change 2
#' - Change 3",
#'     `Last Update` = "(2020-10-23) at 3:28 PM."
#'   ) %>%
#'   info_section(
#'     section_name = "Additional Notes",
#'     `Notes 1` = "Notes with a {snippet}.",
#'     `Notes 2` = "**Bold notes**."
#'   )
#' 
#' # YAML representation
#' History:
#'   Changes: |2-
#'   
#'     - Change 1
#'     - Change 2
#'     - Change 3
#'   Last Update: (2020-10-23) at 3:28 PM.
#' Additional Notes:
#'   Notes 1: Notes with a {snippet}.
#'   Notes 2: '**Bold notes**.'
#' ```
#' 
#' Subsections represented as column names are automatically generated when
#' creating an informant. Within each of the top-level sections (i.e., `History`
#' and `Additional Notes`) there can be multiple subsections used for holding
#' *info text*.
#' 
#' It's safest to use single quotation marks around any *info text* if directly
#' editing it in a YAML file. Note that Markdown formatting and *info snippet*
#' placeholders (shown here as `{snippet}`, see [info_snippet()] for more
#' information) are preserved in the YAML. The Markdown to HTML conversion is
#' done when printing an informant (or invoking [get_informant_report()] on an
#' *informant*) and the processing of snippets (generation and insertion) is
#' done when using the [incorporate()] function. Thus, the source text is always
#' maintained in the YAML representation and is never written in processed form.
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
#' @section Examples:
#' 
#' Create a pointblank `informant` object with [create_informant()]. We can
#' specify a `tbl` with the `~` followed by a statement that gets the
#' `small_table` dataset.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' ```
#' 
#' An `informant` typically has the 'Table' and 'Columns' sections. We can also
#' create entirely different sections (that follow these) with their
#' own properties using the `info_section()` function. Let's create a subsection
#' in the report called `"Notes"` and add text to two parts of that:
#' `"creation"` and `"usage"`.
#' 
#' ```r
#' informant <-
#'   informant %>%
#'   info_section(
#'     section_name = "Notes",
#'     creation = "Dataset generated on (2020-01-15).",
#'     usage = "`small_table %>% dplyr::glimpse()`"
#'   ) %>%
#'   incorporate()
#' ```
#' 
#' Upon printing the `informant` object, we see the addition of the 'Notes'
#' section and its own information.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_info_section_1.png")`
#' }
#' }
#'
#' @family Information Functions
#' @section Function ID:
#' 3-4
#'
#' @export
info_section <- function(
    x,
    section_name,
    ...
) {
  
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
#' @description 
#' Getting little snippets of information from a table goes hand-in-hand with
#' mixing those bits of info with your table info. Call `info_snippet()` to
#' define a snippet and how you'll get that from the target table. The snippet
#' definition is supplied either with a formula, or, with a
#' **pointblank**-supplied `snip_*()` function. So long as you know how to
#' interact with a table and extract information, you can easily define snippets
#' for a *informant* object. And once those snippets are defined, you can insert
#' them into the *info text* as defined through the other `info_*()` functions
#' ([info_tabular()], [info_columns()], and [info_section()]). Use curly braces
#' with just the `snippet_name` inside (e.g., `"This column has {n_cat}
#' categories."`).
#' 
#' @section Snip functions provided in **pointblank**:
#' For convenience, there are several `snip_*()` functions provided in the
#' package that work on column data from the *informant*'s target table. These
#' are:
#' 
#' - [snip_list()]: get a list of column categories
#' - [snip_stats()]: get an inline statistical summary
#' - [snip_lowest()]: get the lowest value from a column
#' - [snip_highest()] : get the highest value from a column
#' 
#' As it's understood what the target table is, only the `column` in each of
#' these functions is necessary for obtaining the resultant text.
#' 
#' @section YAML:
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). Snippets are stored in the YAML
#' representation and here is is how they are expressed in both R code and in
#' the YAML output (showing both the `meta_snippets` and `columns` keys to
#' demonstrate their relationship here).
#' 
#' ```
#' # R statement
#' informant %>% 
#'   info_columns(
#'     columns = "date_time",
#'     `Latest Date` = "The latest date is {latest_date}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "latest_date",
#'     fn = ~ . %>% dplyr::pull(date) %>% max(na.rm = TRUE)
#'   ) %>%
#'   incorporate()
#' 
#' # YAML representation
#' meta_snippets:
#'   latest_date: ~. %>% dplyr::pull(date) %>% max(na.rm = TRUE)
#' ...
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'     Latest Date: The latest date is {latest_date}.
#'   date:
#'     _type: Date
#'   item_count:
#'     _type: integer
#' ```
#' 
#' @param x An informant object of class `ptblank_informant`.
#' @param snippet_name The name for snippet, which is used for interpolating the
#'   result of the snippet formula into *info text* defined by an `info_*()`
#'   function.
#' @param fn A formula that obtains a snippet of data from the target table.
#'   It's best to use a leading dot (`.`) that stands for the table itself and
#'   use pipes to construct a series of operations to be performed on the table
#'   (e.g., `~ . %>% dplyr::pull(column_2) %>% max(na.rm = TRUE)`). So long as
#'   the result is a length-1 vector, it'll likely be valid for insertion into
#'   some info text. Alternatively, a `snip_*()` function can be used here
#'   (these functions always return a formula that's suitable for all types of
#'   data sources).
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
#'     tbl = ~ test_table,
#'     tbl_name = "test_table",
#'     label = "An example."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "row_count",
#'     fn = ~ . %>% nrow()
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "max_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   info_columns(
#'     columns = vars(a),
#'     info = "In the range of 1 to {max_a}. (SIMPLE)"
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
#' 3-5
#' 
#' @export
info_snippet <- function(
    x,
    snippet_name,
    fn
) {
  
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
#' @param sorting A keyword used to designate the type of sorting to use for the
#'   list. The three options are `"inorder"` (the default), `"infreq"`, and
#'   `"inseq"`. With `"inorder"`, distinct items are listed in the order in
#'   which they first appear. Using `"infreq"` orders the items by the
#'   decreasing frequency of each item. The `"inseq"` option applies an
#'   alphanumeric sorting to the distinct list items.
#' @param reverse An option to reverse the ordering of list items. By default,
#'   this is `FALSE` but using `TRUE` will reverse the items before applying the
#'   `limit`.
#' @param sep The separator to use between list items. By default, this is a
#'   comma.
#' @param and_or The type of conjunction to use between the final and
#'   penultimate list items (should the item length be below the `limit` value).
#'   If `NULL` (the default) is used, then the 'and' conjunction will be used.
#'   Alternatively, the following keywords can be used: `"and"`, `"or"`, or
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
#'   Spanish (`"es"`), Portuguese (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`),
#'   Russian (`"ru"`), Polish (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and
#'   Dutch (`"nl"`).
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @section Examples:
#'  
#' Generate an informant object, add a snippet with [info_snippet()] and
#' `snip_list()` (giving us a method to get a distinct list of column values for
#' column `f`). Define a location for the snippet result in `{ }` and then
#' [incorporate()] the snippet into the info text. Note here that the order of
#' the [info_columns()] and [info_snippet()] calls doesn't matter.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>% 
#'   info_columns(
#'     columns = "f",
#'     `Items` = "This column contains {values_f}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "values_f",
#'     fn = snip_list(column = "f")
#'   ) %>%
#'   incorporate()
#' ```
#' 
#' We can print the `informant` object to see the information report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_snip_list_1.png")`
#' }
#' }
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-6
#' 
#' @export
snip_list <- function(
    column,
    limit = 5,
    sorting = c("inorder", "infreq", "inseq"),
    reverse = FALSE,
    sep = ",",
    and_or = NULL,
    oxford = TRUE,
    as_code = TRUE,
    quot_str = NULL,
    lang = NULL
) {

  sorting <- match.arg(sorting)
  
  if (is.character(and_or)) {
    and_or <- paste0("'", and_or[1], "'")
  } else if (is.null(and_or)) {
    and_or <- "NULL"
  }
  
  if (is.character(lang)) {
    
    # Normalize the reporting language identifier and stop if necessary
    if (!(tolower(lang) %in% reporting_languages)) {
      
      stop(
        "The text ", lang, " doesn't correspond to a pointblank ",
        "reporting language.",
        call. = FALSE
      )
    }
    
    lang <- paste0("'", tolower(lang[1]), "'")
    
  } else if (is.null(lang)) {
    
    lang <- "NULL"
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
    
    stop(
      "A character value must be given for `sep` in `snip_list()`",
      call. = FALSE
    )
  }
  
  sep <- paste0("'", sep[1], "'")
  
  if (sorting == "inorder") {
    
    formula <- 
      stats::as.formula(
        as.character(
          glue::glue(
            "~ . %>% dplyr::select(<<column>>) %>%",
            "dplyr::distinct() %>%",
            "dplyr::pull(<<column>>) %>%",
            ifelse(reverse, "rev() %>%", ""),
            "pb_str_catalog(
            limit = <<limit[1]>>,
            sep = <<sep>>,
            and_or = <<and_or>>,
            oxford = <<oxford>>,
            as_code = <<as_code>>,
            quot_str = <<quot_str>>,
            lang = <<lang>>
          )",
          .open = "<<", .close = ">>"   
          )
        )
      )
    
  } else if (sorting == "infreq") {
    
    formula <-
      stats::as.formula(
        as.character(
          glue::glue(
            "~ . %>% dplyr::select(<<column>>) %>%",
            "dplyr::group_by(<<column>>) %>%",
            "dplyr::summarize(`_count_` = dplyr::n(), .groups = 'keep') %>%",
            ifelse(
              reverse,
              "dplyr::arrange(`_count_`) %>%",
              "dplyr::arrange(dplyr::desc(`_count_`)) %>%"
            ),
            "dplyr::select(<<column>>) %>%",
            "dplyr::pull(<<column>>) %>%",
            "pb_str_catalog(
            limit = <<limit[1]>>,
            sep = <<sep>>,
            and_or = <<and_or>>,
            oxford = <<oxford>>,
            as_code = <<as_code>>,
            quot_str = <<quot_str>>,
            lang = <<lang>>
          )",
          .open = "<<", .close = ">>"   
          )
        )
      )
    
  } else {

    # Arranging with "inseq" option
    formula <- 
      stats::as.formula(
        as.character(
          glue::glue(
            "~ . %>% dplyr::select(<<column>>) %>%",
            "dplyr::distinct() %>%",
            "dplyr::pull(<<column>>) %>%",
            ifelse(
              reverse,
              "sort(decreasing = TRUE) %>%",
              "sort() %>%"
            ),
            "pb_str_catalog(
            limit = <<limit[1]>>,
            sep = <<sep>>,
            and_or = <<and_or>>,
            oxford = <<oxford>>,
            as_code = <<as_code>>,
            quot_str = <<quot_str>>,
            lang = <<lang>>
          )",
          .open = "<<", .close = ">>"   
          )
        )
      )
  }
  
  formula
}

#' A `fn` for `info_snippet()`: get an inline statistical summary
#'
#' @description
#' The `snip_stats()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to produce a five- or seven-number statistical
#' summary. This inline summary works well within a paragraph of text and can
#' help in describing the distribution of numerical values in a column.
#'
#' For a given column, three different types of inline statistical summaries can
#' be provided:
#' 
#' 1. a five-number summary (`"5num"`): minimum, Q1, median, Q3, maximum
#' 2. a seven-number summary (`"7num"`): P2, P9, Q1, median, Q3, P91, P98
#' 3. Bowley's seven-figure summary (`"bowley"`): minimum, P10, Q1, median, Q3,
#' P90, maximum
#'
#' @param column The name of the column that contains the target values.
#' @param type The type of summary. By default, the `"5num"` keyword is used to
#'   generate a five-number summary. Two other options provide seven-number
#'   summaries: `"7num"` and `"bowley"`.
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @section Examples:
#' 
#' Generate an informant object, add a snippet with [info_snippet()] and
#' `snip_stats()` (giving us a method to get some summary stats for column `d`).
#' Define a location for the snippet result in `{ }` and then [incorporate()]
#' the snippet into the info text. Note here that the order of the
#' [info_columns()] and [info_snippet()] calls doesn't matter.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>% 
#'   info_columns(
#'     columns = "d",
#'     `Stats` = "Stats (fivenum): {stats_d}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "stats_d",
#'     fn = snip_stats(column = "d")
#'   ) %>%
#'   incorporate()
#' ```
#' 
#' We can print the `informant` object to see the information report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_snip_stats_1.png")`
#' }
#' }
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-7
#' 
#' @export
snip_stats <- function(
    column,
    type = c("5num", "7num", "bowley")
) {
  
  type <- match.arg(type)
  
  stats::as.formula(
    as.character(
      glue::glue(
        "~ . %>%
    dplyr::select(<<column>>) %>%
    pb_str_summary(type = '<<type>>')",
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
#' @section Examples:
#' 
#' Generate an informant object, add a snippet with [info_snippet()] and
#' `snip_lowest()` (giving us a method to get the lowest value in column `a`).
#' Define a location for the snippet result in `{ }` and then [incorporate()]
#' the snippet into the info text. Note here that the order of the
#' [info_columns()] and [info_snippet()] calls doesn't matter.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>% 
#'   info_columns(
#'     columns = "a",
#'     `Lowest Value` = "Lowest value is {lowest_a}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "lowest_a",
#'     fn = snip_lowest(column = "a")
#'   ) %>%
#'   incorporate()
#' ```
#' 
#' We can print the `informant` object to see the information report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_snip_lowest_1.png")`
#' }
#' }
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-8
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
#' The `snip_highest()` function can be used as an [info_snippet()] function
#' (i.e., provided to `fn`) to get the highest numerical, time value, or
#' alphabetical value from a column in the target table.
#' 
#' @param column The name of the column that contains the target values.
#'   
#' @return A formula needed for [info_snippet()]'s `fn` argument.
#' 
#' @section Examples:
#' 
#' Generate an informant object, add a snippet with [info_snippet()] and
#' `snip_highest()` (giving us a method to get the highest value in column `a`);
#' define a location for the snippet result in `{ }` and then [incorporate()]
#' the snippet into the info text. Note here that the order of the
#' [info_columns()] and [info_snippet()] calls doesn't matter.
#' 
#' ```r
#' informant <- 
#'   create_informant(
#'     tbl = ~ small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   ) %>% 
#'   info_columns(
#'     columns = "a",
#'     `Highest Value` = "Highest value is {highest_a}."
#'   ) %>%
#'   info_snippet(
#'     snippet_name = "highest_a",
#'     fn = snip_highest(column = "a")
#'   ) %>%
#'   incorporate()
#' ```
#' 
#' We can print the `informant` object to see the information report.
#' 
#' ```r
#' informant
#' ```
#' 
#' \if{html}{
#' \out{
#' `r pb_get_image_tag(file = "man_snip_highest_1.png")`
#' }
#' }
#' 
#' @family Information Functions
#' @section Function ID:
#' 3-9
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
