# Get an *agent* from **pointblank** YAML and `interrogate()`

The `yaml_agent_interrogate()` function operates much like the
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
function (reading a **pointblank** YAML file and generating an *agent*
with a validation plan in place). The key difference is that this
function takes things a step further and interrogates the target table
(defined by table-prep formula that is required in the YAML file). The
additional auto-invocation of
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
uses the default options of that function. As with
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
the agent is returned except, this time, it has intel from the
interrogation.

## Usage

``` r
yaml_agent_interrogate(filename, path = NULL)
```

## Arguments

- filename:

  *File name*

  `scalar<character>` // **required**

  The name of the YAML file that contains fields related to an *agent*.

- path:

  \#' @param path *File path*

  `scalar<character>` // *default:* `NULL` (`optional`)

  An optional path to the YAML file (combined with `filename`).

## Value

A `ptblank_agent` object.

## Examples

There's a YAML file available in the **pointblank** package that's also
called `"agent-small_table.yml"`. The path for it can be accessed
through [`system.file()`](https://rdrr.io/r/base/system.file.html):

    yml_file_path <-
      system.file(
        "yaml", "agent-small_table.yml",
        package = "pointblank"
      )

The YAML file can be read as an agent with a pre-existing validation
plan by using the
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md)
function.

    agent <- yaml_read_agent(filename = yml_file_path)

    agent

![This image was generated from the first code example in the
\`yaml_write()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_yaml_write_1.png)

This particular agent is using
`~ tbl_source("small_table", "tbl_store.yml")` to source the table-prep
from a YAML file that holds a table store (can be seen using
`yaml_agent_string(agent = agent)`). Let's put that file in the working
directory (the **pointblank** package has the corresponding YAML file):

    yml_tbl_store_path <-
      system.file(
        "yaml", "tbl_store.yml",
        package = "pointblank"
      )

    file.copy(from = yml_tbl_store_path, to = ".")

As can be seen from the validation report, no interrogation was yet
performed. Saving an agent to YAML will remove any traces of
interrogation data and serve as a plan for a new interrogation on the
same target table. We can either follow this up with with
[`interrogate()`](https://rstudio.github.io/pointblank/reference/interrogate.md)
and get an agent with intel, or, we can interrogate directly from the
YAML file with `yaml_agent_interrogate()`:

    agent <- yaml_agent_interrogate(filename = yml_file_path)

    agent

![This image was generated from the second code example in the
\`yaml_write()\` help
file.](https://raw.githubusercontent.com/rstudio/pointblank/main/images/man_yaml_write_2.png)

## Function ID

11-4

## See also

Other pointblank YAML:
[`yaml_agent_show_exprs()`](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md),
[`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md),
[`yaml_exec()`](https://rstudio.github.io/pointblank/reference/yaml_exec.md),
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md),
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md),
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md),
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
