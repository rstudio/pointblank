# Get an *informant* from **pointblank** YAML and `incorporate()`

The `yaml_informant_incorporate()` function operates much like the
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
function (reading a **pointblank** YAML file and generating an
*informant* with all information in place). The key difference is that
this function takes things a step further and incorporates aspects from
the the target table (defined by table-prep formula that is required in
the YAML file). The additional auto-invocation of
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
uses the default options of that function. As with
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
the informant is returned except, this time, it has been updated with
the latest information from the target table.

## Usage

``` r
yaml_informant_incorporate(filename, path = NULL)
```

## Arguments

- filename:

  *File name*

  `scalar<character>` // **required**

  The name of the YAML file that contains fields related to an
  *informant*.

- path:

  *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional path to the YAML file (combined with `filename`).

## Value

A `ptblank_informant` object.

## Examples

There's a YAML file available in the **pointblank** package that's
called `"informant-small_table.yml"`. The path for it can be accessed
through [`system.file()`](https://rdrr.io/r/base/system.file.html):

    yml_file_path <-
      system.file(
        "yaml", "informant-small_table.yml",
        package = "pointblank"
      )

The YAML file can be read as an informant by using the
`yaml_informant_incorporate()` function. If you expect metadata to
change with time, it's best to use `yaml_informant_incorporate()`
instead of
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md)
since the former will go the extra mile and perform
[`incorporate()`](https://rstudio.github.io/pointblank/reference/incorporate.md)
in addition to the reading.

    informant <- yaml_informant_incorporate(filename = yml_file_path)

    informant

![This image was generated from the third code example in the
\`yaml_write()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_yaml_write_3.png)

As can be seen from the information report, the available table metadata
was restored and reported. If the metadata were to change with time,
that would be updated as well.

## Function ID

11-7

## See also

Other pointblank YAML:
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md),
[`yaml_agent_show_exprs()`](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md),
[`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md),
[`yaml_exec()`](https://rstudio.github.io/pointblank/reference/yaml_exec.md),
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md),
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md),
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
