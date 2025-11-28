# Read **pointblank** *agents* stored on disk as a *multiagent*

An *agent* or *informant* can be written to disk with the
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
function. While useful for later retrieving the stored agent with
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
it's also possible to read a series of on-disk agents with the
`read_disk_multiagent()` function, which creates a `ptblank_multiagent`
object. A *multiagent* object can also be generated via the
[`create_multiagent()`](https://rstudio.github.io/pointblank/reference/create_multiagent.md)
function but is less convenient to use if one is just using agents that
have been previously written to disk.

## Usage

``` r
read_disk_multiagent(filenames = NULL, pattern = NULL, path = NULL)
```

## Arguments

- filenames:

  *File names*

  `vector<character>` // *default:* `NULL` (`optional`)

  The names of files (holding *agent* objects) that were previously
  written by
  [`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md).

- pattern:

  *Regex pattern*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A regex pattern for accessing saved-to-disk *agent* files located in a
  directory (specified in the `path` argument).

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  A path to a collection of files. This is either optional in the case
  that files are specified in `filenames` (the `path` combined with all
  `filenames`), or, required when providing a `pattern` for file names.

## Value

A `ptblank_multiagent` object.

## Function ID

10-2

## See also

Other The multiagent:
[`create_multiagent()`](https://rstudio.github.io/pointblank/reference/create_multiagent.md),
[`get_multiagent_report()`](https://rstudio.github.io/pointblank/reference/get_multiagent_report.md)
