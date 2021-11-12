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


#' Create a **pointblank** *informant* object
#'
#' @description
#' The `create_informant()` function creates an *informant* object, which is
#' used in an *information management* workflow. The overall aim of this
#' workflow is to record, collect, and generate useful information on data
#' tables. We can supply any information that is useful for describing a
#' particular data table. The *informant* object created by the
#' `create_informant()` function takes information-focused functions:
#' [info_columns()], [info_tabular()], [info_section()], and [info_snippet()].
#'
#' The `info_*()` series of functions allows for a progressive build up of
#' information about the target table. The [info_columns()] and [info_tabular()]
#' functions facilitate the entry of *info text* that concerns the table columns
#' and the table proper; the [info_section()] function allows for the creation
#' of arbitrary sections that can have multiple subsections full of additional
#' *info text*. The system allows for dynamic values culled from the target
#' table by way of [info_snippet()], for getting named text extracts from
#' queries, and the use of `{<snippet_name>}` in the *info text*. To make the
#' use of [info_snippet()] more convenient for common queries, a set of
#' `snip_*()` functions are provided in the package ([snip_list()],
#' [snip_stats()], [snip_lowest()], and [snip_highest()]) though you are free to
#' use your own expressions.
#' 
#' Because snippets need to query the target table to return fragments of *info
#' text*, the [incorporate()] function needs to be used to initiate this action.
#' This is also necessary for the *informant* to update other metadata elements
#' such as row and column counts. Once the incorporation process is complete,
#' snippets and other metadata will be updated. Calling the *informant* itself
#' will result in a reporting table. This reporting can also be accessed with
#' the [get_informant_report()] function, where there are more reporting
#' options.
#' 
#' @section YAML: 
#' A **pointblank** informant can be written to YAML with [yaml_write()] and the
#' resulting YAML can be used to regenerate an informant (with
#' [yaml_read_informant()]) or perform the 'incorporate' action using the target
#' table (via [yaml_informant_incorporate()]). Here is an example of how a
#' complex call of `create_informant()` is expressed in R code and in the
#' corresponding YAML representation.
#' 
#' ```
#' # R statement
#' create_informant(
#'   read_fn = ~ small_table,
#'   tbl_name = "small_table",
#'   label = "An example.",
#'   lang = "fr", 
#'   locale = "fr_CA"
#' )
#' 
#' # YAML representation
#' type: informant
#' read_fn: ~small_table
#' tbl_name: small_table
#' info_label: An example.
#' lang: fr
#' locale: fr_CA
#' table:
#'   name: small_table
#'   _columns: 8
#'   _rows: 13.0
#'   _type: tbl_df
#' columns:
#'   date_time:
#'     _type: POSIXct, POSIXt
#'   date:
#'     _type: Date
#'   a:
#'     _type: integer
#'   b:
#'     _type: character
#'   c:
#'     _type: numeric
#'   d:
#'     _type: numeric
#'   e:
#'     _type: logical
#'   f:
#'     _type: character
#' ```
#' 
#' The generated YAML includes some top-level keys where `type` and `read_fn`
#' are mandatory, and, two metadata sections: `table` and `columns`. Keys that
#' begin with an underscore character are those that are updated whenever
#' [incorporate()] is called on an *informant*. The `table` metadata section can
#' have multiple subsections with *info text*. The `columns` metadata section
#' can similarly have have multiple subsections, so long as they are children to
#' each of the column keys (in the above YAML example, `date_time` and `date`
#' are column keys and they match the table's column names). Additional sections
#' can be added but they must have key names on the top level that don't
#' duplicate the default set (i.e., `type`, `table`, `columns`, etc. are treated
#' as reserved keys).
#' 
#' @section Writing an Informant to Disk:
#' An *informant* object can be written to disk with the [x_write_disk()]
#' function. Informants are stored in the serialized RDS format and can be
#' easily retrieved with the [x_read_disk()] function.
#'
#' It's recommended that table-prep formulas are supplied to the `read_fn`
#' argument of `create_informant()`. In this way, when an *informant* is read
#' from disk through [x_read_disk()], it can be reused to access the target
#' table (which may changed, hence the need to use an expression for this).
#'
#' @param tbl The input table. This can be a data frame, a tibble, a `tbl_dbi`
#'   object, or a `tbl_spark` object. Alternatively, a function can be used to
#'   read in the input data table with the `read_fn` argument (in which case,
#'   `tbl` can be `NULL`).
#' @param read_fn A function that's used for reading in the data. Even if a
#'   `tbl` is provided, this function will be invoked to obtain the data (i.e.,
#'   the `read_fn` takes priority). There are two ways to specify a `read_fn`:
#'   (1) using a function (e.g., `function() { <table reading code> }`) or, (2)
#'   with an R formula expression.
#' @param agent A pointblank *agent* object. This object can be used instead of
#'   supplying a table in `tbl` or a table-prep formula in `read_fn`.
#' @param tbl_name A optional name to assign to the input table object. If no
#'   value is provided, a name will be generated based on whatever information
#'   is available.
#' @param label An optional label for the information report. If no value is
#'   provided, a label will be generated based on the current system time.
#'   Markdown can be used here to make the label more visually appealing (it
#'   will appear in the header area of the information report).
#' @param lang The language to use for the information report (a summary table
#'   that provides all of the available information for the table. By default,
#'   `NULL` will create English (`"en"`) text. Other options include French
#'   (`"fr"`), German (`"de"`), Italian (`"it"`), Spanish (`"es"`), Portuguese
#'   (`"pt"`), Turkish (`"tr"`), Chinese (`"zh"`), Russian (`"ru"`), Polish
#'   (`"pl"`), Danish (`"da"`), Swedish (`"sv"`), and Dutch (`"nl"`).
#' @param locale An optional locale ID to use for formatting values in the
#'   information report according the locale's rules. Examples include `"en_US"`
#'   for English (United States) and `"fr_FR"` for French (France); more simply,
#'   this can be a language identifier without a country designation, like "es"
#'   for Spanish (Spain, same as `"es_ES"`).
#'   
#' @return A `ptblank_informant` object.
#' 
#' @examples 
#' # Let's walk through how we can
#' # generate some useful information for a
#' # really small table; it's actually
#' # called `small_table` and we can find
#' # it as a dataset in this package
#' small_table
#' 
#' # Create a pointblank `informant`
#' # object with `create_informant()`
#' # and the `small_table` dataset
#' informant <- 
#'   create_informant(
#'     read_fn = ~small_table,
#'     tbl_name = "small_table",
#'     label = "An example."
#'   )
#' 
#' # This function creates some information
#' # without any extra help by profiling
#' # the supplied table object; it adds
#' # the sections: (1) 'table' and
#' # (2) 'columns' and we can print the
#' # object to see the information report
#' 
#' # Alternatively, we can get the same report
#' # by using `get_informant_report()`
#' report <- get_informant_report(informant)
#' class(report)
#' 
#' @section Figures:
#' \if{html}{\figure{man_create_informant_1.png}{options: width=100\%}}
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-3
#' 
#' @export
create_informant <- function(tbl = NULL,
                             read_fn = NULL,
                             agent = NULL,
                             tbl_name = NULL,
                             label = NULL,
                             lang = NULL,
                             locale = NULL) {

  read_fn_given <- !is.null(read_fn)
  
  # Generate a label if none provided
  label <- generate_label(label = label)
  
  # Normalize the reporting language identifier and stop if necessary
  lang <- normalize_reporting_language(lang)
 
  # Set the `locale` to the `lang` value if `locale` isn't set
  if (is.null(locale)) locale <- lang
  
  # If nothing is provided for either `tbl`, `read_fn`, or `agent`,
  # this function needs to be stopped
  if (is.null(tbl) && is.null(read_fn) && is.null(agent)) {
    
    stop(
      "A table object, table-prep formula, or agent must be supplied:\n",
      " * Use a table object in the `tbl` argument.\n",
      " * Or supply a table-prep formula in `read_fn`.\n",
      " * Or even an agent with some association to a table.",
      call. = FALSE
    )
  }
  
  # Stop function if both a table and an agent are provided 
  if (!is.null(tbl) && !is.null(agent)) {
    
    stop("A `tbl` and a `agent` cannot both be provided.", call. = FALSE)
  }
  
  # Try to infer the table name if one isn't
  # explicitly given in `tbl_name`
  if (!is.null(tbl) && is.null(tbl_name)) {
    tbl_name <- deparse(match.call()$tbl)
    if (tbl_name == ".") {
      tbl_name <- NA_character_
    }
  } 
  if (is.null(tbl_name)) {
    tbl_name <- NA_character_
  }
  
  # Prefer reading a table from a `read_fn` if it's available
  # TODO: Verify that the table is a table object
  # and provide an error if it isn't
  if (!is.null(read_fn)) {
    
    if (inherits(read_fn, "function")) {
      
      tbl <- rlang::exec(read_fn)
    
    } else if (rlang::is_formula(read_fn)) {

      tbl <- 
        read_fn %>% 
        rlang::f_rhs() %>% 
        rlang::eval_tidy(env = caller_env(n = 1))
      
      if (inherits(tbl, "read_fn")) {
        
        if (inherits(tbl, "with_tbl_name") && is.na(tbl_name)) {
          tbl_name <- tbl %>% rlang::f_lhs() %>% as.character()
        }
        
        tbl <-
          tbl %>%
          rlang::f_rhs() %>%
          rlang::eval_tidy(env = caller_env(n = 1))
      }
      
    } else {
      stop(
        "The `read_fn` object must be a function or an R formula.\n",
        "* A function can be made with `function()` {<table reading code>}.\n",
        "* An R formula can also be used, with the expression on the RHS.",
        call. = FALSE
      )
    }
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
    if (is.null(read_fn)) read_fn <- agent$read_fn
  }
  
  table.name <- x$tbl_name
  table.type <- x$tbl_src
  
  column_names <- x$col_names
  column_types_r <- x$col_types
  column_types_sql <- x$db_col_types

  .tbl <- x$tbl
  
  table.columns <- length(column_names)
  
  if (inherits(.tbl, "ArrowObject")) {
    table.rows <- nrow(.tbl)
  } else {
    table.rows <- 
      dplyr::count(.tbl, name = "n") %>%
      dplyr::pull(n) %>%
      as.numeric()
  }
  
  column_list <- list(columns = lapply(col_schema(.tbl = .tbl), as.list))
  
  for (i in seq_along(column_names)) {
    
    column_list[["columns"]][[column_names[i]]] <- 
      list(`_type` = paste(
        unlist(column_list[["columns"]][[column_names[i]]]),
        collapse = ", "
      ))
  }
  
  if (!all(is.na(column_types_sql))) {
    
    for (i in seq_along(column_names)) {

      column_list[["columns"]][[i]] <- 
        c(
          column_list[["columns"]][[i]],
          list(`_sql_type` = column_types_sql[i])
        )
    }
  }
  
  metadata_list <-
    c(
      list(
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
      tbl = if (!read_fn_given) tbl,
      read_fn = read_fn,
      tbl_name = table.name,
      info_label = label,
      meta_snippets = list(),
      lang = lang,
      locale = locale,
      metadata = metadata_list
    )
  
  # Assign the class attribute value `ptblank_informant` to
  # the `metadata` object
  attr(metadata, "class") <- "ptblank_informant"
  
  metadata
}
