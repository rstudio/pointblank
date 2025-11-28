# Execute all agent and informant YAML tasks

The `yaml_exec()` function takes all relevant **pointblank** YAML files
in a directory and executes them. Execution involves interrogation of
agents for YAML agents and incorporation of informants for YAML
informants. Under the hood, this uses
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md)
and
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md)
and then
[`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
to save the processed objects to an output directory. These written
artifacts can be read in at any later time with the
[`x_read_disk()`](https://rstudio.github.io/pointblank/reference/x_read_disk.md)
function or the
[`read_disk_multiagent()`](https://rstudio.github.io/pointblank/reference/read_disk_multiagent.md)
function. This is useful when data in the target tables are changing and
the periodic testing of such tables is part of a data quality monitoring
plan.

The output RDS files are named according to the object type processed,
the target table, and the date-time of processing. For convenience and
modularity, this setup is ideal when a table store YAML file (typically
named `"tbl_store.yml"` and produced via the
[`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
and
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)
workflow) is available in the directory, and when table-prep formulas
are accessed by name through
[`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md).

A typical directory of files set up for execution in this way might have
the following contents:

- a `"tbl_store.yml"` file for holding table-prep formulas (created with
  [`tbl_store()`](https://rstudio.github.io/pointblank/reference/tbl_store.md)
  and written to YAML with
  [`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md))

- one or more YAML *agent* files to validate tables (ideally using
  [`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md))

- one or more YAML *informant* files to provide refreshed metadata on
  tables (again, using
  [`tbl_source()`](https://rstudio.github.io/pointblank/reference/tbl_source.md)
  to reference table preparations is ideal)

- an output folder (default is `"output"`) to save serialized versions
  of processed agents and informants

Minimal example files of the aforementioned types can be found in the
**pointblank** package through the following
[`system.file()`](https://rdrr.io/r/base/system.file.html) calls:

- `system.file("yaml", "agent-small_table.yml", package = "pointblank")`

- `system.file("yaml", "informant-small_table.yml", package = "pointblank")`

- `system.file("yaml", "tbl_store.yml", package = "pointblank")`

The directory itself can be accessed using
`system.file("yaml", package = "pointblank")`.

## Usage

``` r
yaml_exec(
  path = NULL,
  files = NULL,
  write_to_disk = TRUE,
  output_path = file.path(path, "output"),
  keep_tbl = FALSE,
  keep_extracts = FALSE
)
```

## Arguments

- path:

  The path that contains the YAML files for agents and informants.

- files:

  A vector of YAML files to use in the execution workflow. By default,
  `yaml_exec()` will attempt to process every valid YAML file in `path`
  but supplying a vector here limits the scope to the specified files.

- write_to_disk:

  Should the execution workflow include a step that writes output files
  to disk? This internally calls
  [`x_write_disk()`](https://rstudio.github.io/pointblank/reference/x_write_disk.md)
  to write RDS files and uses the base filename of the agent/informant
  YAML file as part of the output filename, appending the date-time to
  the basename.

- output_path:

  The output path for any generated output files. By default, this will
  be a subdirectory of the provided `path` called `"output"`.

- keep_tbl, keep_extracts:

  For agents, the table may be kept if it is a data frame object
  (databases tables will never be pulled for storage) and *extracts*,
  collections of table rows that failed a validation step, may also be
  stored. By default, both of these options are set to `FALSE`.

## Value

Invisibly returns a named vector of file paths for the input files that
were processed; file output paths (for wherever writing occurred) are
given as the names.

## Function ID

11-8

## See also

Other pointblank YAML:
[`yaml_agent_interrogate()`](https://rstudio.github.io/pointblank/reference/yaml_agent_interrogate.md),
[`yaml_agent_show_exprs()`](https://rstudio.github.io/pointblank/reference/yaml_agent_show_exprs.md),
[`yaml_agent_string()`](https://rstudio.github.io/pointblank/reference/yaml_agent_string.md),
[`yaml_informant_incorporate()`](https://rstudio.github.io/pointblank/reference/yaml_informant_incorporate.md),
[`yaml_read_agent()`](https://rstudio.github.io/pointblank/reference/yaml_read_agent.md),
[`yaml_read_informant()`](https://rstudio.github.io/pointblank/reference/yaml_read_informant.md),
[`yaml_write()`](https://rstudio.github.io/pointblank/reference/yaml_write.md)

## Examples

``` r
if (interactive()) {

# The 'yaml' directory that is
# accessible in the package through
# `system.file()` contains the files
# 1. `agent-small_table.yml`
# 2. `informant-small_table.yml`
# 3. `tbl_store.yml`

# There are references in YAML files
# 1 & 2 to the table store YAML file,
# so, they all work together cohesively

# Let's process the agent and the
# informant YAML files with `yaml_exec()`;
# and we'll specify the working directory
# as the place where the output RDS files
# are written

output_dir <- getwd()

yaml_exec(
  path = system.file(
    "yaml", package = "pointblank"
  ),
  output = output_dir
)

# This generates two RDS files in the
# working directory: one for the agent
# and the other for the informant; each
# of them are automatically time-stamped
# so that periodic execution can be
# safely carried out without risk of
# overwriting 

}
```
