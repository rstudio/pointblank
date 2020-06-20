#' Thoroughly scan the table data so as to understand it better
#' 
#' Generates an HTML report that scours the input table data. Before calling up
#' an *agent* to validate the data, it's a good idea to understand the data with
#' some level of precision. Make this the initial step of a well-balanced *data
#' quality reporting* workflow. The reporting output contains several sections
#' to make everything more digestible, and these are:
#' \describe{
#' \item{Overview}{Table dimensions, duplicate row count, column types, and
#' reproducibility information}
#' \item{Variables}{A summary for each table variable and further statistics and
#' summaries depending on the variable type}
#' \item{Interactions}{A matrix plot that shows interactions between variables}
#' \item{Correlations}{A set of correlation matrix plots for numerical
#' variables}
#' \item{Missing Values}{A summary figure that shows the degree of missingness
#' across variables}
#' \item{Sample}{A table that provides the head and tail rows of the dataset}
#' }
#' The output HTML report is viewable in the RStudio Viewer and can also be
#' integrated in R Markdown HTML reports. If you need the output HTML as a
#' string, it's possible to get that by using `as.character()` (e.g.,
#' `scan_data(tbl = mtcars) %>% as.character()`). The resulting HTML string is a
#' complete HTML document where Bootstrap and jQuery are embedded within.
#' 
#' @param tbl The input table. This can be a data frame or a tibble.
#' @param sections The sections to include in the finalized `Table Scan` report.
#'   A character vector with section names is required here. The sections in
#'   their default order are: `"overview"`, `"variables"`, `"interactions"`,
#'   `"correlations"`, `"missing"`, and `"sample"`. This vector can be comprised
#'   of less elements and the order can be changed to suit the desired layout of
#'   the report.
#' @param navbar Should there be a navigation bar anchored to the top of the
#'   report page? By default this is `TRUE`.
#' @param reporting_lang The language to use for label text in the report. By
#'   default, `NULL` will create English (`"en"`) text. Other options include
#'   French (`"fr"`), German (`"de"`), Italian (`"it"`), and Spanish (`"es"`).
#' 
#' @examples
#' # Get an HTML report that describes all of
#' # the data in the `dplyr::storms` dataset
#' # scan_data(tbl = dplyr::storms)
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-1
#' 
#' @export
scan_data <- function(tbl,
                      sections = c("overview", "variables", "interactions",
                                   "correlations", "missing", "sample"),
                      navbar = TRUE,
                      reporting_lang = NULL) {
  
  # nocov start
  
  # Limit components if a `tbl_dbi` object is supplied as the `tbl`
  if (inherits(tbl, "tbl_dbi")) {
    sections <- setdiff(sections, c("interactions", "correlations"))
  }
  
  # nocov end
  
  # Stop function if the length of `sections` is 0
  if (length(sections) == 0) {
    stop("At least one `section` is required.", call. = FALSE)
  }
  
  # Stop function if their are unrecognized sections in `sections`
  if (!all(sections %in% c("overview", "variables", "interactions",
                           "correlations", "missing", "sample"))) {
    
    stop("All values provided in `sections` must be a valid keyword:\n",
         " * Allowed values are \"overview\", \"variables\", \"interactions\", ",
         "\"correlations\", \"missing\", and \"sample\".",
         call. = FALSE)
  }
  
  # Normalize the reporting language identifier and stop if necessary
  reporting_lang <- normalize_reporting_language(reporting_lang)

  # Attempt to get the table name through `match.call()` and `deparse()`
  tbl_name <- deparse(match.call()$tbl)
  
  # In the case where the table is piped in a `"."` is the
  # result; since it's unknown, we treat it as NA
  if (tbl_name == ".") {
    tbl_name <- NA
  }

  build_examination_page(
    data = tbl,
    tbl_name = tbl_name,
    sections = sections,
    navbar = navbar,
    reporting_lang = reporting_lang
  )
}

# nocov start

#' Print the reporting produced by [scan_data()]
#'
#' This facilitates printing of the HTML report to the R console.
#'
#' @param x An object of class `examination_page`.
#' @param ... Any additional parameters.
#' @param view The value for `print()`s `browse` argument.
#'
#' @keywords internal
#'
#' @export
print.examination_page <- function(x, ..., view = interactive()) {

  class(x) <- c("shiny.tag.list", "list")

  print(x, browse = view, ...)
}

#' Knit print the reporting produced by [scan_data()] 
#'
#' This facilitates printing of the HTML report within a knitr code chunk.
#'
#' @param x An object of class `examination_page`.
#' @param ... Any additional parameters.
#'
#' @keywords internal
#' @noRd
knit_print.examination_page <- function(x, ...) {
  
  class(x) <- c("shiny.tag.list", "list")
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(x, ...)
}

# nocov end
