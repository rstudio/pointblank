# Add information that focuses on aspects of the data table as a whole

When an *informant* object is created with the
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
function, it has two starter sections: (1) 'table' and (2) 'columns'.
The 'table' section should contain a few properties upon creation, such
as the supplied table name (`name`) and table dimensions (as `_columns`
and `_rows`). We can add more table-based properties with the
`info_tabular()` function. By providing a series of named arguments (in
the form `entry_name = "The *info text*."`), we can add more information
that makes sense for describing the table as a whole.

## Usage

``` r
info_tabular(x, ...)
```

## Arguments

- x:

  *The pointblank informant object*

  `obj:<ptblank_informant>` // **required**

  A **pointblank** *informant* object that is commonly created through
  the use of the
  [`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md)
  function.

- ...:

  *Information entries*

  `<info-text expressions>` // **required**

  Information entries as a series of named arguments. The names refer to
  subsection titles within the `TABLE` section and the values are the
  *info text* (informational text that can be written as Markdown and
  further styled with *Text Tricks*).

## Value

A `ptblank_informant` object.

## Info Text

The *info text* that's used for any of the `info_*()` functions readily
accepts Markdown formatting, and, there are a few *Text Tricks* that can
be used to spice up the presentation. Markdown links written as
`< link url >` or `[ link text ]( link url )` will get nicely-styled
links. Any dates expressed in the ISO-8601 standard with parentheses,
`"(2004-12-01)"`, will be styled with a font variation (monospaced) and
underlined in purple. Spans of text can be converted to label-style text
by using: (1) double parentheses around text for a rectangular border as
in `((label text))`, or (2) triple parentheses around text for a
rounded-rectangular border like `(((label text)))`.

CSS style rules can be applied to spans of *info text* with the
following form:

`[[ info text ]]<< CSS style rules >>`

As an example of this in practice suppose you'd like to change the color
of some text to red and make the font appear somewhat thinner. A
variation on the following might be used:

`"This is a [[factor]]<<color: red; font-weight: 300;>> value."`

The are quite a few CSS style rules that can be used to great effect.
Here are a few you might like:

- `color: <a color value>;` (text color)

- `background-color: <a color value>;` (the text's background color)

- `text-decoration: (overline | line-through | underline);`

- `text-transform: (uppercase | lowercase | capitalize);`

- `letter-spacing: <a +/- length value>;`

- `word-spacing: <a +/- length value>;`

- `font-style: (normal | italic | oblique);`

- `font-weight: (normal | bold | 100-900);`

- `font-variant: (normal | bold | 100-900);`

- `border: <a color value> <a length value> (solid | dashed | dotted);`

In the above examples, 'length value' refers to a CSS length which can
be expressed in different units of measure (e.g., `12px`, `1em`, etc.).
Some lengths can be expressed as positive or negative values (e.g., for
`letter-spacing`). Color values can be expressed in a few ways, the most
common being in the form of hexadecimal color values or as CSS color
names.

## YAML

A **pointblank** informant can be written to YAML with
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
and the resulting YAML can be used to regenerate an informant (with
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md))
or perform the 'incorporate' action using the target table (via
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)).
When `info_tabular()` is represented in YAML, *info text* goes into
subsections of the top-level `table` key. Here is an example of how a
call of `info_tabular()` is expressed in R code and in the corresponding
YAML representation.

R statement:

    informant %>%
      info_tabular(
        section_1 = "*info text* 1.",
        `section 2` = "*info text* 2 and {snippet_1}"
      )

YAML representation:

    table:
      _columns: 23
      _rows: 205.0
      _type: tbl_df
      section_1: '*info text* 1.'
      section 2: '*info text* 2 and {snippet_1}'

Subsection titles as defined in `info_tabular()` can be set in backticks
if they are not syntactically correct as an argument name without them
(e.g., when using spaces, hyphens, etc.).

It's safest to use single quotation marks around any *info text* if
directly editing it in a YAML file. Note that Markdown formatting and
*info snippet* placeholders (shown here as `{snippet_1}`, see
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md)
for more information) are preserved in the YAML. The Markdown to HTML
conversion is done when printing an informant (or invoking
[`get_informant_report()`](https://rstudio.github.io/pointblank/reference/get_informant_report.md)
on an *informant*) and the processing of snippets (generation and
insertion) is done when using the
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
function. Thus, the source text is always maintained in the YAML
representation and is never written in processed form.

## Examples

Create a pointblank `informant` object with
[`create_informant()`](https://rstudio.github.io/pointblank/reference/create_informant.md).
We can specify a `tbl` with the `~` followed by a statement that gets
the `small_table` dataset.

    informant <-
      create_informant(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "An example."
      )

We can add *info text* to describe the table with the various `info_*()`
functions. In this example, we'll use `info_tabular()` to generally
describe the `small_table` dataset.

    informant <-
      informant %>%
      info_tabular(
        `Row Definition` = "A row has randomized values.",
        Source = c(
          "- From the **pointblank** package.",
          "- [https://rstudio.github.io/pointblank/]()"
         )
       )

Upon printing the `informant` object, we see the additions made to the
'Table' section of the report.

    informant

![This image was generated from the first code example in the
\`info_tabular()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_info_tabular_1.png)

## Function ID

3-1

## See also

Other Information Functions:
[`info_columns()`](https://rstudio.github.io/pointblank/reference/info_columns.md),
[`info_columns_from_tbl()`](https://rstudio.github.io/pointblank/reference/info_columns_from_tbl.md),
[`info_section()`](https://rstudio.github.io/pointblank/reference/info_section.md),
[`info_snippet()`](https://rstudio.github.io/pointblank/reference/info_snippet.md),
[`snip_highest()`](https://rstudio.github.io/pointblank/reference/snip_highest.md),
[`snip_list()`](https://rstudio.github.io/pointblank/reference/snip_list.md),
[`snip_lowest()`](https://rstudio.github.io/pointblank/reference/snip_lowest.md),
[`snip_stats()`](https://rstudio.github.io/pointblank/reference/snip_stats.md)
