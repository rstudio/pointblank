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
#' `scan_data(tbl = iris) %>% as.character()`). The resulting HTML string is a
#' complete HTML document where Bootstrap and jQuery are embedded within.
#' 
#' @inheritParams create_agent
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
scan_data <- function(tbl) {
  
  if (inherits(tbl, "tbl_dbi")) {
    stop("Tables of class `tbl_dbi` aren't supported in `scan_data()`")
  }
  
  # nocov start
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    
    stop("Creating an HTML report with `scan_data()` requires the gt package:\n",
         " * Install gt with `devtools::install_github(\"rstudio/gt\")`.",
         call. = FALSE)
  }
  
  # nocov end
  
  # Attempt to get the table name through `match.call()` and `deparse()`
  tbl_name <- deparse(match.call()$tbl)
  
  # In the case where the table is piped in a `"."` is the
  # result; since it's unknown, we treat it as NA
  if (tbl_name == ".") {
    tbl_name <- NA
  }
  
  build_examination_page(data = tbl, tbl_name = tbl_name)
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
