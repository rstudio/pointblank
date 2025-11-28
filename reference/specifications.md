# A table containing data pertaining to various specifications

The `specifications` dataset is useful for testing the
[`col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md),
[`test_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md),
and
[`expect_col_vals_within_spec()`](https://rstudio.github.io/pointblank/reference/col_vals_within_spec.md)
functions. For each column, holding character values for different
specifications, rows 1-5 contain valid values, the 6th row is an NA
value, and the final two values (rows 7 and 8) are invalid. Different
specification (`spec`) keywords apply to each of columns when validating
with any of the aforementioned functions.

## Usage

``` r
specifications
```

## Format

A tibble with 8 rows and 12 variables:

- isbn_numbers:

  ISBN-13 numbers; can be validated with the `"isbn"` specification.

- vin_numbers:

  VIN numbers (identifiers for motor vehicles); can be validated with
  the `"vin"` specification.

- zip_codes:

  Postal codes for the U.S.; can be validated with the `"postal[USA]"`
  specification or its `"zip"` alias.

- credit_card_numbers:

  Credit card numbers; can be validated with the `"credit_card"`
  specification or the `"cc"` alias.

- iban_austria:

  IBAN numbers for Austrian accounts; can be validated with the
  `"iban[AUT]"` specification.

- swift_numbers:

  Swift-BIC numbers; can be validated with the `"swift"` specification.

- phone_numbers:

  Phone numbers; can be validated with the `"phone"` specification.

- email_addresses:

  Email addresses; can be validated with the `"email"` specification.

- urls:

  URLs; can be validated with the `"url"` specification.

- ipv4_addresses:

  IPv4 addresses; can be validated with the `"ipv4"` specification

- ipv6_addresses:

  IPv6 addresses; can be validated with the `"ipv6"` specification

- mac_addresses:

  MAC addresses; can be validated with the `"mac"` specification

## Function ID

14-3

## See also

Other Datasets:
[`game_revenue`](https://rstudio.github.io/pointblank/reference/game_revenue.md),
[`game_revenue_info`](https://rstudio.github.io/pointblank/reference/game_revenue_info.md),
[`small_table`](https://rstudio.github.io/pointblank/reference/small_table.md),
[`small_table_sqlite()`](https://rstudio.github.io/pointblank/reference/small_table_sqlite.md)

## Examples

``` r
# Here is a glimpse at the data
# available in `specifications`
dplyr::glimpse(specifications)
#> Rows: 8
#> Columns: 12
#> $ isbn_numbers        <chr> "978 1 85715 201 2", "978-1-84159-362-3", "978 1 8…
#> $ vin_numbers         <chr> "4UZAANDH85CV12329", "JM1BL1S59A1134659", "1GCEK14…
#> $ zip_codes           <chr> "99553", "36264", "71660", "85225", "90309", NA, "…
#> $ credit_card_numbers <chr> "340000000000009", "378734493671000", "67034444444…
#> $ iban_austria        <chr> "AT582774098454337653", "AT220332087576467472", "A…
#> $ swift_numbers       <chr> "RBOSGGSX", "RZTIAT22263", "BCEELULL", "MARKDEFF",…
#> $ phone_numbers       <chr> "+5-555-555-5555", "+5 555 555 5555", "+5.555.555.…
#> $ email_addresses     <chr> "test@test.com", "mail+mail@example.com", "mail.em…
#> $ urls                <chr> "http://foo.com/blah_blah", "http://foo.com/blah_b…
#> $ ipv4_addresses      <chr> "93.184.220.20", "161.148.172.130", "161.148.172.1…
#> $ ipv6_addresses      <chr> "2001:0db8:0000:85a3:0000:0000:ac1f:8001", "2001:d…
#> $ mac_addresses       <chr> "01-2d-4c-ef-89-ab", "01-2D-4C-EF-89-AB", "01:2d:4…
```
