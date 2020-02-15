#' A small table that is useful for testing
#'
#' This is a small table with a few different types of columns. It's probably
#' just useful when testing the functions from **pointblank**. Rows 9 and 10 are
#' exact duplicates. The `c` column contains two `NA` values.
#'
#' @format A tibble with 13 rows and 8 variables:
#' \describe{
#' \item{date_time}{A date-time column (of the `POSIXct` class) with dates that
#' correspond exactly to those in the `date` column. Time values are somewhat
#' randomized but all 'seconds' values are `00`.}
#' \item{date}{A `Date` column with dates from `2016-01-04` to `2016-01-30`.}
#' \item{a}{An `integer` column with values ranging from `1` to `8`.}
#' \item{b}{A `character` column with values that adhere to a common pattern.}
#' \item{c}{An `integer` column with values ranging from `2` to `9`. Contains two `NA` values.}
#' \item{d}{A numeric column with values ranging from `108` to `10000`.}
#' \item{e}{A `logical` column.}
#' \item{f}{A `character` column with `"low"`, `"mid"`, and `"high"` values.}
#' }
#'
#' @examples
#' library(dplyr)
#' 
#' # Here is a glimpse at the data
#' # available in `small_table`
#' dplyr::glimpse(small_table)
#'
#' @family Datasets
#' @section Function ID:
#' 5-1
#'
"small_table"

#' A SQLite version of the `small_table` dataset
#' 
#' The `small_table_sqlite()` function creates a SQLite, `tbl_dbi` version of
#' the `small_table` dataset. A requirement is the availability of the **DBI**
#' and **RSQLite** packages. These packages can be installed by using 
#' `install.packages("DBI")` and `install.packages("RSQLite")`.
#' 
#' @examples
#' # Use `small_table_sqlite()` to
#' # create a SQLite version of the
#' # `small_table` table
#' #
#' # small_table_sqlite <- small_table_sqlite()
#' 
#' @export
small_table_sqlite <- function() {

  # nocov start
  
  if (!requireNamespace("DBI", quietly = TRUE) &&
      !requireNamespace("RSQLite", quietly = TRUE)) {
    
    stop("Creating the SQLite table object requires both the DBI and RSQLite packages:\n",
         " * Install them with `install.packages(\"DBI\")` and `install.packages(\"RSQLite\")`.",
         call. = FALSE)
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/small_table.db")
  dplyr::tbl(con, "small_table")
  
  # nocov end
}
