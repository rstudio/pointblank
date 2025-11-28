# A `fn` for `info_snippet()`: get a list of column categories

The `snip_list()` function can be used as an
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
function (i.e., provided to `fn`) to get a catalog list from a table
column. You can limit the of items in that list with the `limit` value.

## Usage

``` r
snip_list(
  column,
  limit = 5,
  sorting = c("inorder", "infreq", "inseq"),
  reverse = FALSE,
  sep = ",",
  and_or = NULL,
  oxford = TRUE,
  as_code = TRUE,
  quot_str = NULL,
  na_rm = FALSE,
  lang = NULL
)
```

## Arguments

- column:

  *The target column*

  `scalar<character>` // **required**

  The name of the column that contains the target values.

- limit:

  *Limit for list length*

  `scalar<integer>` // *default:* `5`

  A limit of items put into the generated list. The returned text will
  state the remaining number of items beyond the `limit`.

- sorting:

  *Type of sorting within list*

  `singl-kw:[inorder|infreq|inseq]` // *default:* `"inorder"`

  A keyword used to designate the type of sorting to use for the list.
  The three options are `"inorder"` (the default), `"infreq"`, and
  `"inseq"`. With `"inorder"`, distinct items are listed in the order in
  which they first appear. Using `"infreq"` orders the items by the
  decreasing frequency of each item. The `"inseq"` option applies an
  alphanumeric sorting to the distinct list items.

- reverse:

  *Reversal of list order*

  `scalar<logical>` // *default:* `FALSE`

  An option to reverse the ordering of list items. By default, this is
  `FALSE` but using `TRUE` will reverse the items before applying the
  `limit`.

- sep:

  *Separator text for list*

  `scalar<character>` // *default:* `","`

  The separator to use between list items. By default, this is a comma.

- and_or:

  *Use of 'and' or 'or' within list*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The type of conjunction to use between the final and penultimate list
  items (should the item length be below the `limit` value). If `NULL`
  (the default) is used, then the 'and' conjunction will be used.
  Alternatively, the following keywords can be used: `"and"`, `"or"`, or
  an empty string (for no conjunction at all).

- oxford:

  *Usage of oxford comma*

  `scalar<logical>` // *default:* `TRUE`

  Whether to use an Oxford comma under certain conditions.

- as_code:

  *Treat items as code*

  `scalar<logical>` // *default:* `TRUE`

  Should each list item appear in a 'code font' (i.e., as monospaced
  text)? By default this is `TRUE`. Using `FALSE` keeps all list items
  in the same font as the rest of the information report.

- quot_str:

  *Set items in double quotes*

  `scalar<logical>` // *default:* `NULL` (`optional`)

  An option for whether list items should be set in double quotes. If
  `NULL` (the default), the quotation marks are mainly associated with
  list items derived from `character` or `factor` values; numbers,
  dates, and logical values won't have quotation marks. We can
  explicitly use quotations (or not) with either `TRUE` or `FALSE` here.

- na_rm:

  *Remove NA values from list*

  `scalar<logical>` // *default:* `FALSE`

  An option for whether NA values should be counted as an item in the
  list.

- lang:

  *Reporting language*

  `scalar<character>` // *default:* `NULL` (`optional`)

  The language to use for any joining words (from the `and_or` option)
  or additional words in the generated list string. By default, `NULL`
  will use whichever `lang` setting is available in the parent
  *informant* object (this is settable in the
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  `lang` argument). If specified here as an override, the language
  options are English (`"en"`), French (`"fr"`), German (`"de"`),
  Italian (`"it"`), Spanish (`"es"`), Portuguese (`"pt"`), Turkish
  (`"tr"`), Chinese (`"zh"`), Russian (`"ru"`), Polish (`"pl"`), Danish
  (`"da"`), Swedish (`"sv"`), and Dutch (`"nl"`).

## Value

A formula needed for
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)'s
`fn` argument.

## Examples

Generate an informant object, add a snippet with
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
and `snip_list()` (giving us a method to get a distinct list of column
values for column `f`). Define a location for the snippet result in
[`{ }`](https://rdrr.io/r/base/Paren.html) and then
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
the snippet into the info text. Note here that the order of the
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md)
and
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
calls doesn't matter.

    informant <-
      create_informant(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "An example."
      ) %>%
      info_columns(
        columns = f,
        `Items` = "This column contains {values_f}."
      ) %>%
      info_snippet(
        snippet_name = "values_f",
        fn = snip_list(column = "f")
      ) %>%
      incorporate()

We can print the `informant` object to see the information report.

    informant

![This image was generated from the first code example in the
\`snip_list()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_snip_list_1.png)

## Function ID

3-6

## See also

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
[`info_tabular()`](https://rstudio.github.io/pointblank/reference/info_tabular.md),
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
