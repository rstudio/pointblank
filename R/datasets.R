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
#' \item{c}{An `integer` column with values ranging from `2` to `9`. Contains
#' two `NA` values.}
#' \item{d}{A numeric column with values ranging from `108` to `10000`.}
#' \item{e}{A `logical` column.}
#' \item{f}{A `character` column with `"low"`, `"mid"`, and `"high"` values.}
#' }
#'
#' @examples
#' # Here is a glimpse at the data
#' # available in `small_table`
#' dplyr::glimpse(small_table)
#'
#' @family Datasets
#' @section Function ID:
#' 14-1
#'
"small_table"

#' An SQLite version of the `small_table` dataset
#' 
#' The `small_table_sqlite()` function creates an SQLite, `tbl_dbi` version of
#' the `small_table` dataset. A requirement is the availability of the **DBI**
#' and **RSQLite** packages. These packages can be installed by using 
#' `install.packages("DBI")` and `install.packages("RSQLite")`.
#' 
#' @examples
#' # Use `small_table_sqlite()` to
#' # create an SQLite version of the
#' # `small_table` table
#' #
#' # small_table_sqlite <- small_table_sqlite()
#' 
#' @family Datasets
#' @section Function ID:
#' 14-2
#' 
#' @export
small_table_sqlite <- function() {

  # nocov start
  
  if (!requireNamespace("DBI", quietly = TRUE) &&
      !requireNamespace("RSQLite", quietly = TRUE)) {
    
    stop(
      "Creating the SQLite table object requires both the DBI and RSQLite ",
      "packages:\n",
      "* Install them with `install.packages(\"DBI\")` and ",
      "`install.packages(\"RSQLite\")`.",
      call. = FALSE
    )
  }
  
  con <- 
    DBI::dbConnect(
      RSQLite::SQLite(), 
      dbname = system.file(
        "data_files", "small_table.db",
        package = "pointblank"
      )
    )
  
  dplyr::tbl(con, "small_table")
  
  # nocov end
}

#' A table containing data pertaining to various specifications
#'
#' The `specifications` dataset is useful for testing the
#' [col_vals_within_spec()], [test_col_vals_within_spec()], and
#' [expect_col_vals_within_spec()] functions. For each column, holding character
#' values for different specifications, rows 1-5 contain valid values, the 6th
#' row is an NA value, and the final two values (rows 7 and 8) are invalid.
#' Different specification (`spec`) keywords apply to each of columns when
#' validating with any of the aforementioned functions.
#'
#' @format A tibble with 8 rows and 12 variables:
#' \describe{
#' \item{isbn_numbers}{ISBN-13 numbers; can be validated with the `"isbn"`
#' specification.}
#' \item{vin_numbers}{VIN numbers (identifiers for motor vehicles); can be
#' validated with the `"vin"` specification.}
#' \item{zip_codes}{Postal codes for the U.S.; can be validated with the
#' `"postal[USA]"` specification or its `"zip"` alias.}
#' \item{credit_card_numbers}{Credit card numbers; can be validated with the
#' `"credit_card"` specification or the `"cc"` alias.}
#' \item{iban_austria}{IBAN numbers for Austrian accounts; can be validated with
#' the `"iban[AUT]"` specification.}
#' \item{swift_numbers}{Swift-BIC numbers; can be validated with the `"swift"`
#' specification.}
#' \item{phone_numbers}{Phone numbers; can be validated with the `"phone"`
#' specification.}
#' \item{email_addresses}{Email addresses; can be validated with the `"email"`
#' specification.}
#' \item{urls}{URLs; can be validated with the  `"url"` specification.}
#' \item{ipv4_addresses}{IPv4 addresses; can be validated with the `"ipv4"`
#' specification}
#' \item{ipv6_addresses}{IPv6 addresses; can be validated with the `"ipv6"`
#' specification}
#' \item{mac_addresses}{MAC addresses; can be validated with the `"mac"`
#' specification}
#' }
#'
#' @examples
#' # Here is a glimpse at the data
#' # available in `specifications`
#' dplyr::glimpse(specifications)
#'
#' @family Datasets
#' @section Function ID:
#' 14-3
#'
"specifications"

#' A table with game revenue data
#'
#' This table is a subset of the `sj_all_revenue` table from the **intendo**
#' data package. It's the first 2,000 rows from that table where revenue records
#' range from `2015-01-01` to `2015-01-21`.
#'
#' @format A tibble with 2,000 rows and 11 variables:
#' \describe{
#' \item{player_id}{A `character` column with unique identifiers for each
#' user/player.}
#' \item{session_id}{A `character` column that contains unique identifiers for
#' each player session.}
#' \item{session_start}{A date-time column that indicates when the session
#' (containing the revenue event) started.}
#' \item{time}{A date-time column that indicates exactly when the player
#' purchase (or revenue event) occurred.}
#' \item{item_type}{A `character` column that provides the class of the item
#' purchased.}
#' \item{item_name}{A `character` column that provides the name of the item
#' purchased.}
#' \item{item_revenue}{A `numeric` column with the revenue amounts per item
#' purchased.}
#' \item{session_duration}{A `numeric` column that states the length of the
#' session (in minutes) for which the purchase occurred.}
#' \item{start_day}{A `Date` column that provides the date of first login for
#' the player making a purchase.}
#' \item{acquisition}{A `character` column that provides the method of
#' acquisition for the player.}
#' \item{country}{A `character` column that provides the probable country of
#' residence for the player.}
#' }
#'
#' @examples
#' # Here is a glimpse at the data
#' # available in `game_revenue`
#' dplyr::glimpse(game_revenue)
#'
#' @family Datasets
#' @section Function ID:
#' 14-4
#'
"game_revenue"

#' A table with metadata for the `game_revenue` dataset
#'
#' This table contains metadata for the `game_revenue` table. The first column
#' (named `column`) provides the column names from `game_revenue`. The second
#' column (`info`) contains descriptions for each of the columns in that
#' dataset. This table is in the correct format for use in the
#' [info_columns_from_tbl()] function.
#'
#' @format A tibble with 11 rows and 2 variables:
#' \describe{
#' \item{column}{A `character` column with unique identifiers for each
#' user/player.}
#' \item{info}{A `character` column that contains unique identifiers for
#' each player session.}
#' }
#'
#' @examples
#' # Here is a glimpse at the data
#' # available in `game_revenue_info`
#' dplyr::glimpse(game_revenue_info)
#'
#' @family Datasets
#' @section Function ID:
#' 14-5
#'
"game_revenue_info"
