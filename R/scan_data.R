#' Thoroughly scan the table data so as to understand it better
#' 
#' Generates an HTML report that scours the input table data. Before calling up
#' an *agent* to validate the data, it's a good idea to understand the data with
#' some level of precision. Make this the initial step of a well-balanced *data
#' quality reporting* workflow. The reporting output contains several sections
#' to make everything more digestible.
#' 
#' @inheritParams create_agent
#' 
#' @export
scan_data <- function(tbl) {
  
  if (inherits(tbl, "tbl_dbi")) {
    stop("Tables of class `tbl_dbi` aren't supported in `scan_data()`")
  }
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    
    stop("Creating an HTML report with `scan_data()` requires the gt package:\n",
         " * Install gt with `devtools::install_github(\"rstudio/gt\")`.",
         call. = FALSE)
  }
  
  build_examination_page(data = tbl)
}

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
print.examination_page <- function(x) {
  
  # Use `html_print()` to print to the console
  htmltools::html_print(x)
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
  
  # Use `knit_print()` to print in a code chunk
  knitr::knit_print(x, ...)
}
