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


#' Define a store of tables with table-prep formulas: a table store
#' 
#' @description
#' 
#' It can be useful to set up all the data sources you need and just draw from
#' them when necessary. This upfront configuration with `tbl_store()` lets us
#' define the methods for obtaining tabular data from mixed sources (e.g.,
#' database tables, tables generated from flat files, etc.) and provide
#' identifiers for these data preparation procedures.
#' 
#' What results from this work is a convenient way to materialize tables with
#' [tbl_get()]. We can also get any table-prep formula from the table store
#' with [tbl_source()]. The content of a table-prep formulas can involve reading
#' a table from a location, or, it can involve data transformation. One can
#' imagine scenarios where we might (1) procure several mutated variations of
#' the same source table, (2) generate a table using disparate data sources, or
#' (3) filter the rows of a database table according to the system time. Another
#' nice aspect of organizing table-prep formulas in a single object is supplying
#' it to the `tbl` argument of [create_agent()] or [create_informant()] via `$`
#' notation (e.g, `create_agent(tbl = <tbl_store>$<name>)`) or with
#' [tbl_source()] (e.g.,
#' `create_agent(tbl = ~ tbl_source("<name>", <tbl_store>))`).
#' 
#' @param ... Expressions that contain table-prep formulas and table names for
#'   data retrieval. Two-sided formulas (e.g, `<LHS> ~ <RHS>`) are to be used,
#'   where the left-hand side is an identifier and the right-hand contains a
#'   statement that obtains a table (i.e., the table-prep formula). If the LHS
#'   is omitted then an identifier will be generated for you.
#'   
#' @param .list Allows for the use of a list as an input alternative to `...`.
#' 
#' @param .init We can optionally provide an initialization statement (in a
#'   one-sided formula) that should be executed whenever *any* of tables in the
#'   table store are obtained. This is useful, for instance, for including a
#'   `library()` call that can be executed before any table-prep formulas in
#'   `...`.
#' 
#' @return A `tbl_store` object that contains table-prep formulas.
#' 
#' @section YAML:
#' 
#' A **pointblank** table store can be written to YAML with [yaml_write()] and
#' the resulting YAML can be used in several ways. The ideal scenario is to have
#' pointblank agents and informants also in YAML form. This way the agent and
#' informant can refer to the table store YAML (via [tbl_source()]), and, the
#' processing of both agents and informants can be performed with
#' [yaml_agent_interrogate()] and [yaml_informant_incorporate()]. With the
#' following R code, a table store with two table-prep formulas is generated and
#' written to YAML (if no filename is given then the YAML is written to
#' `"tbl_store.yml"`).
#' 
#' R statement for generating the `"tbl_store.yml"` file:
#' 
#' ```r
#' tbl_store(
#'   tbl_duckdb ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb"),
#'   sml_table_high ~ small_table %>% dplyr::filter(f == "high"),
#'   .init = ~ library(tidyverse)
#' ) %>%
#'   yaml_write()
#' ```
#' 
#' YAML representation (`"tbl_store.yml"`):
#' 
#' ```yaml
#' type: tbl_store
#' tbls:
#'   tbl_duckdb: ~ db_tbl(small_table, dbname = ":memory:", dbtype = "duckdb")
#'   sml_table_high: ~ small_table %>% dplyr::filter(f == "high")
#' init: ~library(tidyverse)
#' ```
#' 
#' This is useful when you want to get fresh pulls of prepared data from a
#' source materialized in an R session (with the [tbl_get()] function. For
#' example, the `sml_table_high` table can be obtained by using
#' `tbl_get("sml_table_high", "tbl_store.yml")`. To get an agent to check this
#' prepared data periodically, then the following example with [tbl_source()]
#' will be useful:
#' 
#' R code to generate agent that checks `sml_table_high` and writing the agent
#' to YAML:
#' 
#' ```r
#' create_agent(
#'   tbl = ~ tbl_source("sml_table_high", "tbl_store.yml"),
#'   label = "An example that uses a table store.",
#'   actions = action_levels(warn_at = 0.10)
#' ) %>% 
#'   col_exists(vars(date, date_time)) %>%
#'   write_yaml()
#' ```
#'   
#' The YAML representation (`"agent-sml_table_high.yml"`):
#' 
#' ```yaml
#' tbl: ~ tbl_source("sml_table_high", "tbl_store.yml")
#' tbl_name: sml_table_high
#' label: An example that uses a table store.
#' actions:
#'   warn_fraction: 0.1
#' locale: en
#' steps:
#'   - col_exists:
#'     columns: vars(date, date_time)
#' ```
#' 
#' Now, whenever the `sml_table_high` table needs to be validated, it can be
#' done with [yaml_agent_interrogate()] (e.g., 
#' `yaml_agent_interrogate("agent-sml_table_high.yml")`).
#' 
#' @section Examples:
#' 
#' ## Creating an in-memory table store and adding table-prep formulas
#' 
#' The table store provides a way to get the tables we need fairly easily. Think
#' of an identifier for the table you'd like and then provide the code necessary
#' to obtain that table. Then repeat as many times as you like!
#'
#' Here we'll define two tables that can be materialized later: `tbl_duckdb` (an
#' in-memory DuckDB database table with **pointblank**'s `small_table` dataset)
#' and `sml_table_high` (a filtered version of `tbl_duckdb`):
#' 
#' ```r
#' store_1 <-
#'   tbl_store(
#'     tbl_duckdb ~ 
#'       db_tbl(
#'         pointblank::small_table,
#'         dbname = ":memory:",
#'         dbtype = "duckdb"
#'       ),
#'     sml_table_high ~ 
#'       db_tbl(
#'         pointblank::small_table,
#'         dbname = ":memory:",
#'         dbtype = "duckdb"
#'       ) %>%
#'       dplyr::filter(f == "high")
#'   )
#' ```
#' 
#' We can see what's in the table store `store_1` by printing it out:
#' 
#' ```r
#' store_1
#' ```
#' 
#' \preformatted{## -- The `table_store` table-prep formulas
#' ## 1 tbl_duckdb // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
#' ## dbtype = "duckdb")
#' ## 2 sml_table_high // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
#' ## dbtype = "duckdb") \%>\% dplyr::filter(f == "high")
#' ## ----}
#' 
#' 
#' It's good to check that the tables can be obtained without error. We can do
#' this with the [tbl_get()] function. With that function, we need to supply the
#' given name of the table-prep formula (in quotes) and the table store object.
#' 
#' ```r
#' tbl_get(tbl = "tbl_duckdb", store = store_1)
#' ```
#' 
#' \preformatted{## # Source:   table<pointblank::small_table> [?? x 8]
#' ## # Database: duckdb_connection
#' ##    date_time           date           a b             c      d e     f    
#' ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## # … with more rows}
#' 
#' 
#' ```r
#' tbl_get(tbl = "sml_table_high", store = store_1)
#' ```
#' 
#' \preformatted{## # Source:   lazy query [?? x 8]
#' ## # Database: duckdb_connection
#' ##   date_time           date           a b             c     d e     f    
#' ##   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl> <chr>
#' ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high 
#' ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high 
#' ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high 
#' ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high}
#' 
#' 
#' We can shorten the `tbl_store()` statement with some syntax that
#' **pointblank** provides. The `sml_table_high` table-prep is simply a
#' transformation of `tbl_duckdb`, so, we can use `{{ tbl_duckdb }}` in place of
#' the repeated statement. Additionally, we can provide a `library()` call to
#' the `.init` argument of `tbl_store()` so that **dplyr** is available (thus
#' allowing us to use `filter(...)` instead of `dplyr::filter(...)`). Here is
#' the revised `tbl_store()` call:
#' 
#' ```r
#' store_2 <- 
#'   tbl_store(
#'     tbl_duckdb ~ 
#'       db_tbl(
#'         pointblank::small_table,
#'         dbname = ":memory:",
#'         dbtype = "duckdb"
#'       ),
#'     sml_table_high ~ 
#'       {{ tbl_duckdb }} %>%
#'       filter(f == "high"),
#'     .init = ~ library(tidyverse)
#'   )
#' ```
#' 
#' Printing the table store `store_2` now shows that we used an `.init`
#' statement:
#' 
#' ```r
#' store_2
#' ```
#' 
#' \preformatted{## -- The `table_store` table-prep formulas
#' ## 1 tbl_duckdb // ~ db_tbl(pointblank::small_table, dbname = ":memory:",
#' ## dbtype = "duckdb")
#' ## 2 sml_table_high // ~ \{\{tbl_duckdb\}\} \%>\% filter(f == "high") 
#' ## ----
#' ## INIT // ~library(tidyverse)
#' ## ----}
#' 
#' 
#' Checking again with [tbl_get()] should provide the same tables as before:
#' 
#' ```r
#' tbl_get(tbl = "tbl_duckdb", store = store_2)
#' ```
#' \preformatted{## # Source:   table<pointblank::small_table> [?? x 8]
#' ## # Database: duckdb_connection
#' ##    date_time           date           a b             c      d e     f    
#' ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## # … with more rows}
#' 
#' 
#' ```r
#' tbl_get(tbl = "sml_table_high", store = store_2)
#' ```
#' 
#' \preformatted{## # Source:   lazy query [?? x 8]
#' ## # Database: duckdb_connection
#' ##   date_time           date           a b             c     d e     f    
#' ##   <dttm>              <date>     <int> <chr>     <dbl> <dbl> <lgl> <chr>
#' ## 1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3 3423. TRUE  high 
#' ## 2 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3 2343. TRUE  high 
#' ## 3 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3  843. TRUE  high 
#' ## 4 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 5 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9  838. FALSE high 
#' ## 6 2016-01-30 11:23:00 2016-01-30     1 3-dka-303    NA 2230. TRUE  high}
#' 
#' 
#' ## Using a table store in a data validation workflow
#' 
#' Define a `tbl_store` object by adding table-prep formulas inside the
#' [tbl_store()] call.
#' 
#' ```r
#' store_3 <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     ~ db_tbl(
#'       table = "rna",
#'       dbname = "pfmegrnargs",
#'       dbtype = "postgres",
#'       host = "hh-pgsql-public.ebi.ac.uk",
#'       port = 5432,
#'       user = I("reader"),
#'       password = I("NWDMCE5xdipIjRrp")
#'     ),
#'     all_revenue ~ db_tbl(
#'       table = file_tbl(
#'         file = from_github(
#'           file = "sj_all_revenue_large.rds",
#'           repo = "rich-iannone/intendo",
#'           subdir = "data-large"
#'         )
#'       ),
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' ```
#' 
#' Let's get a summary of what's in the table store `store_3` through printing:
#' 
#' ```r
#' store_3
#' ```
#' 
#' \preformatted{## -- The `table_store` table-prep formulas
#' ## 1 small_table_duck // ~ db_tbl(table = small_table, dbname = ":memory:",
#' ## dbtype = "duckdb")
#' ## 2 rna // ~db_tbl(table = "rna", dbname = "pfmegrnargs", dbtype =
#' ## "postgres", host = "hh-pgsql-public.ebi.ac.uk", port = 5432, user =
#' ## I("reader"), password = I("NWDMCE5xdipIjRrp"))
#' ## 3 all_revenue // ~ db_tbl(table = file_tbl(file = from_github(file =
#' ## "sj_all_revenue_large.rds", repo = "rich-iannone/intendo", subdir =
#' ## "data-large")), dbname = ":memory:", dbtype = "duckdb")
#' ## 4 sml_table // ~ pointblank::small_table
#' ## ----}
#' 
#' 
#' Once this object is available, you can check that the table of interest is
#' produced to your specification with the [tbl_get()] function.
#' 
#' ```r
#' tbl_get(
#'   tbl = "small_table_duck",
#'   store = store_3
#' )
#' ```
#' 
#' \preformatted{## # Source:   table<small_table> [?? x 8]
#' ## # Database: duckdb_connection
#' ##    date_time           date           a b             c      d e     f    
#' ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## # … with more rows}
#' 
#' 
#' Another way to get the same table materialized is by using `$` to get the
#' entry of choice for [tbl_get()].
#' 
#' ```r
#' store_3$small_table_duck %>% tbl_get()
#' ```
#' 
#' \preformatted{## # Source:   table<small_table> [?? x 8]
#' ## # Database: duckdb_connection
#' ##    date_time           date           a b             c      d e     f    
#' ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## # … with more rows}
#' 
#' 
#' Creating an agent is easy when all table-prep formulas are encapsulated in a
#' `tbl_store` object. Use `$` notation to pass the appropriate procedure for
#' reading a table to the `tbl` argument.
#' 
#' ```r
#' agent_1 <-
#'   create_agent(
#'     tbl = store_3$small_table_duck
#'   )
#' ```
#'   
#' There are other ways to use the table store to assign a target table to an
#' agent, like using the [tbl_source()] function (which extracts the table-prep
#' formula from the table store).
#' 
#' ```r
#' agent_2 <-
#'   create_agent(
#'     tbl = ~ tbl_source(
#'       tbl = "small_table_duck",
#'       store = store_3
#'       )
#'   )
#' ```
#' 
#' ## Writing a table store to a YAML file
#' 
#' The table store can be moved to YAML with `yaml_write` and the [tbl_source()]
#' call could then refer to that on-disk table store. Let's do that YAML
#' conversion.
#' 
#' ```r
#' yaml_write(store_3)
#' ```
#' 
#' The above writes the `tbl_store.yml` file (by not providing a `filename` this
#' default filename is chosen).
#' 
#' It can be convenient to read table-prep formulas from a YAML file that's a
#' table store. To achieve this, we can modify the [tbl_source()] statement in
#' the [create_agent()] call so that `store` refers to the on-disk YAML file.
#' 
#' ```r
#' agent_3 <-
#'   create_agent(
#'     tbl = ~ tbl_source(
#'       tbl = "small_table_duck",
#'       store = "tbl_store.yml"
#'     )
#'   )
#' ```
#' 
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-8
#' 
#' @export
tbl_store <- function(
    ...,
    .list = list2(...),
    .init = NULL
) {
  
  # Collect a fully or partially named list of tables
  tbl_list <- .list
  
  # Check that every list element is a formula
  for (i in seq_along(tbl_list)) {
    
    if (!inherits(tbl_list[[i]], "formula")) {
      stop(
        "Each entry to `tbl_store()` must be a formula.",
        call. = FALSE
      )
    }
  }
  
  # If there is anything provided for `.init`, put that 
  # into an attribute of `tbl_list`
  if (!is.null(.init)) {
    attr(tbl_list, which = "pb_init") <- .init
  }
  
  # Get names for every entry in the list
  name_list <- c()
  has_given_name <- c()
  for (i in seq_along(tbl_list)) {
    
    if (is.null(rlang::f_lhs(tbl_list[[i]]))) {
      
      # Get RHS of formula and attempt to get the table name if there
      # is only a single `db_tbl()` or `file_tbl()` call
      rhs <- capture_formula(tbl_list[[i]])[2]
      
      if (grepl("~\\s*?(db_tbl|file_tbl)\\(", rhs) &&
          grepl("table\\s*?=\\s*?\".*?\"", rhs)) {
        
        tbl_name <- gsub(".*table\\s*?=\\s*?\"(.*?)\".*$", "\\1", rhs)
        
        if (!is.null(tbl_name) && length(tbl_name) == 1 && nzchar(tbl_name)) {
          name_list <- add_to_name_list(name_list, tbl_name, "stop")
          has_given_name <- c(has_given_name, TRUE)
        } else {
          tbl_name <- paste0("tbl_", formatC(i, width = 3, flag = "0"))
          name_list <- add_to_name_list(name_list, tbl_name, "rename")
          has_given_name <- c(has_given_name, FALSE)
        }
        
      } else {
        
        # If the table name isn't provided and isn't recoverable, 
        # use the index number formatted as string
        tbl_name <- paste0("tbl_", formatC(i, width = 3, flag = "0"))
        name_list <- add_to_name_list(name_list, tbl_name, "rename")
        has_given_name <- c(has_given_name, FALSE)
      }
      
    } else if (inherits(rlang::f_lhs(tbl_list[[i]]), "name")) {
      name_list <- 
        add_to_name_list(name_list, as.character(rlang::f_lhs(tbl_list[[i]])))
      has_given_name <- c(has_given_name, TRUE)
    }
  }
  
  tbl_list <- rlang::set_names(tbl_list, name_list)
  
  for (i in seq_along(tbl_list)) {
    if (has_given_name[i]) {
      class(tbl_list[[i]]) <- c("with_tbl_name", "read_fn")
    } else {
      class(tbl_list[[i]]) <- "read_fn"  
    }
  }
  
  class(tbl_list) <- "tbl_store"
  
  tbl_list
}

add_to_name_list <- function(
    name_list,
    tbl_name,
    duplicate_strategy = c("stop", "rename")
) {

  duplicate_strategy <- match.arg(duplicate_strategy)
  
  # Determine if name is a duplicate in the `name_list` and employ
  # the chosen strategy
  if (tbl_name %in% name_list) {
    
    if (duplicate_strategy == "stop") {
      # Stop function if duplicate `tbl_name` seen
      stop(
        "The table name `", tbl_name, "` is a duplicate name:\n",
        "* Please choose another name since all table names must be unique",
        call. = FALSE 
      )
      
    } else {
      # Rename `tbl_name` with suffix of random numbers
      tbl_name <- paste0(tbl_name, paste(sample(0:9, 2), collapse = ""))
    }
  }
  
  c(name_list, tbl_name)
}

#' Obtain a table-prep formula from a table store
#' 
#' @description
#' 
#' The `tbl_source()` function provides a convenient means to access a
#' table-prep formula from either a `tbl_store` object or a table store YAML
#' file (which can be created with the [yaml_write()] function). A call to
#' `tbl_source()` is most useful as an input to the `tbl` argument of
#' [create_agent()], [create_informant()], or [set_tbl()].
#'
#' Should you need to obtain the table itself (that is generated via the
#' table-prep formula), then the [tbl_get()] function should be used for that.
#' 
#' @param tbl The table name associated with a table-prep formula. This is part
#'   of the table `store`. This table could be identified by its name (e.g.,
#'   `tbl = "large_table"`) or by supplying a reference using a subset (with
#'   `$`) of the `tbl_store` object (e.g., `tbl = store$large_table`). If using
#'   the latter method then nothing needs to be supplied to `store`.
#'   
#' @param store Either a table store object created by the [tbl_store()]
#'   function or a path to a table store YAML file created by [yaml_write()].
#' 
#' @return A table-prep formula.
#' 
#' @section Examples:
#' 
#' Let's create a `tbl_store` object by giving two table-prep formulas to
#' [tbl_store()].
#' 
#' ```r
#' store <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' ```
#' 
#' We can pass a table-prep formula to [create_agent()] via [tbl_source()], add
#' some validation steps, and interrogate the table shortly thereafter.
#' 
#' ```r
#' agent_1 <- 
#'   create_agent(
#'     tbl = ~ tbl_source("sml_table", store),
#'     label = "`tbl_source()` example",
#'     actions = action_levels(warn_at = 0.10)
#'   ) %>% 
#'   col_exists(columns = vars(date, date_time)) %>%
#'   interrogate()
#' ```
#' 
#' The `agent_1` object can be printed to see the validation report in the
#' Viewer.
#' 
#' ```r
#' agent_1
#' ```
#' 
#' \if{html}{
#' 
#' \out{
#' `r pb_get_image_tag(file = "man_tbl_source_1.png")`
#' }
#' }
#' 
#' The `tbl_store` object can be transformed to YAML with the [yaml_write()]
#' function. The following statement writes the `tbl_store.yml` file by default
#' (but a different name could be used with the `filename` argument):
#' 
#' ```r
#' yaml_write(store)
#' ```
#' 
#' Let's modify the agent's target to point to the table labeled as
#' `"sml_table"` in the YAML representation of the `tbl_store`.
#' 
#' ```r
#' agent_2 <-
#'   agent_1 %>% 
#'   set_tbl(
#'     ~ tbl_source(
#'         tbl = "sml_table",
#'         store = "tbl_store.yml"
#'       )
#'   )
#' ```
#' 
#' We can likewise write the agent to a YAML file with [yaml_write()] (writes to
#' `agent-sml_table.yml` by default but the `filename` allows for any filename
#' you want).
#' 
#' ```r
#' yaml_write(agent_2)
#' ```
#' 
#' Now that both the agent and the associated table store are present as on-disk
#' YAML, interrogations can be done by using [yaml_agent_interrogate()].
#' 
#' ```r
#' agent <- yaml_agent_interrogate(filename = "agent-sml_table.yml")
#' ```
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-9
#' 
#' @export
tbl_source <- function(
    tbl,
    store = NULL
) {
  
  # If `store` is supplied as a character vector,
  # assume it is a file path to a YAML file
  if (is.character(store)) {
    store <- yaml_read_tbl_store(filename = store)
  }
  
  if (is.character(tbl) && tbl %in% names(store)) {
    tbl_entry <- store[[tbl]]
  } else if (inherits(tbl, "read_fn")) {
    tbl_entry <- tbl
  }
  
  tbl_entry_str <- capture_formula(formula = tbl_entry)[2]
  
  Sys.sleep(0.25)
  
  this_entry_idx <- which(names(store) == tbl)

  if (has_substitutions(tbl_entry_str)) {
    
    if (this_entry_idx == 1) {
      stop(
        "There cannot be a substitution with other table-prep formulas since ",
        "this is the first entry.",
        call. = FALSE
      )
    }
    
    prev_tbl_entries <- names(store)[1:(this_entry_idx - 1)]
    
    # Make substitutions where specified in `tbl_entry_str`; use only
    # previous entries to perform the replacement
    for (i in seq_along(prev_tbl_entries)) {
      
      pattern <- paste0("\\{\\{\\s*?", prev_tbl_entries[i], "\\s*?\\}\\}")
      
      replacement <- 
        gsub(
          "^~\\s+", "",
          capture_formula(formula = store[[prev_tbl_entries[i]]])[2]
        )
      
      Sys.sleep(0.25)
      
      tbl_entry_str <-
        gsub(pattern = pattern, replacement = replacement, tbl_entry_str)
      
    }
  }
  
  tbl_entry <- 
    tbl_store(
      .list = list(
        stats::as.formula(
          paste0(names(store)[this_entry_idx], " ", tbl_entry_str)
        )
      )
    )[[1]]
  
  tbl_entry
}

has_substitutions <- function(x) {
  grepl("\\{\\{\\s*?[a-zA-Z0-9_\\.]*?\\s*?\\}\\}", x)
}

#' Obtain a materialized table via a table store
#' 
#' @description
#' 
#' The `tbl_get()` function gives us the means to materialize a table that has
#' an entry in a table store (i.e., has a table-prep formula with a unique
#' name). The table store that is used for this can be in the form of a
#' `tbl_store` object (created with the [tbl_store()] function) or an on-disk
#' YAML representation of a table store (created by using [yaml_write()] with a
#' `tbl_store` object).
#'
#' Should you want a table-prep formula from a table store to use as a value for
#' `tbl` (in [create_agent()], [create_informant()], or [set_tbl()]), then have
#' a look at the [tbl_source()] function.
#'
#' @param tbl The table to retrieve from a table `store`. This table could be
#'   identified by its name (e.g., `tbl = "large_table"`) or by supplying a
#'   reference using a subset (with `$`) of the `tbl_store` object (e.g., `tbl =
#'   store$large_table`). If using the latter method then nothing needs to be
#'   supplied to `store`.
#'   
#' @param store Either a table store object created by the [tbl_store()]
#'   function or a path to a table store YAML file created by [yaml_write()].
#' 
#' @return A table object.
#' 
#' @section Examples:
#' 
#' Define a `tbl_store` object by adding several table-prep formulas in
#' [tbl_store()].
#' 
#' ```r
#' store <- 
#'   tbl_store(
#'     small_table_duck ~ db_tbl(
#'       table = small_table,
#'       dbname = ":memory:",
#'       dbtype = "duckdb"
#'     ),
#'     ~ db_tbl(
#'       table = "rna",
#'       dbname = "pfmegrnargs",
#'       dbtype = "postgres",
#'       host = "hh-pgsql-public.ebi.ac.uk",
#'       port = 5432,
#'       user = I("reader"),
#'       password = I("NWDMCE5xdipIjRrp")
#'     ),
#'     sml_table ~ pointblank::small_table
#'   )
#' ```
#' 
#' Once this object is available, we can access the tables named:
#' `"small_table_duck"`, `"rna"`, and `"sml_table"`. Let's check that the
#' `"rna"` table is accessible through [tbl_get()]:
#' 
#' ```r
#' tbl_get(
#'   tbl = "rna",
#'   store = store
#' )
#' ```
#' 
#' \preformatted{## # Source:   table<rna> [?? x 9]
#' ## # Database: postgres [reader@hh-pgsql-public.ebi.ac.uk:5432/pfmegrnargs]
#' ##          id upi        timestamp           userstamp crc64   len seq_short
#' ##     <int64> <chr>      <dttm>              <chr>     <chr> <int> <chr>    
#' ##  1 24583872 URS000177… 2019-12-02 13:26:08 rnacen    C380…   511 ATTGAACG…
#' ##  2 24583873 URS000177… 2019-12-02 13:26:08 rnacen    BC42…   390 ATGGGCGA…
#' ##  3 24583874 URS000177… 2019-12-02 13:26:08 rnacen    19A5…   422 CTACGGGA…
#' ##  4 24583875 URS000177… 2019-12-02 13:26:08 rnacen    66E1…   534 AGGGTTCG…
#' ##  5 24583876 URS000177… 2019-12-02 13:26:08 rnacen    CC8F…   252 TACGTAGG…
#' ##  6 24583877 URS000177… 2019-12-02 13:26:08 rnacen    19E4…   413 ATGGGCGA…
#' ##  7 24583878 URS000177… 2019-12-02 13:26:08 rnacen    AE91…   253 TACGAAGG…
#' ##  8 24583879 URS000177… 2019-12-02 13:26:08 rnacen    E21A…   304 CAGCAGTA…
#' ##  9 24583880 URS000177… 2019-12-02 13:26:08 rnacen    1AA7…   460 CCTACGGG…
#' ## 10 24583881 URS000177… 2019-12-02 13:26:08 rnacen    2046…   440 CCTACGGG…
#' ## # … with more rows, and 2 more variables: seq_long <chr>, md5 <chr>}
#' 
#' 
#' An alternative method for getting the same table materialized is by using `$`
#' to get the formula of choice from `tbls` and passing that to `tbl_get()`. The
#' benefit of this is that we can use autocompletion to show us what's available
#' in the table store (i.e., appears after typing the `$`).
#' 
#' ```r
#' store$small_table_duck %>% tbl_get()
#' ```
#' 
#' \preformatted{## # Source:   table<small_table> [?? x 8]
#' ## # Database: duckdb_connection
#' ##    date_time           date           a b             c      d e     f    
#' ##    <dttm>              <date>     <int> <chr>     <dbl>  <dbl> <lgl> <chr>
#' ##  1 2016-01-04 11:00:00 2016-01-04     2 1-bcd-345     3  3423. TRUE  high 
#' ##  2 2016-01-04 00:32:00 2016-01-04     3 5-egh-163     8 10000. TRUE  low  
#' ##  3 2016-01-05 13:32:00 2016-01-05     6 8-kdg-938     3  2343. TRUE  high 
#' ##  4 2016-01-06 17:23:00 2016-01-06     2 5-jdo-903    NA  3892. FALSE mid  
#' ##  5 2016-01-09 12:36:00 2016-01-09     8 3-ldm-038     7   284. TRUE  low  
#' ##  6 2016-01-11 06:15:00 2016-01-11     4 2-dhe-923     4  3291. TRUE  mid  
#' ##  7 2016-01-15 18:46:00 2016-01-15     7 1-knw-093     3   843. TRUE  high 
#' ##  8 2016-01-17 11:27:00 2016-01-17     4 5-boe-639     2  1036. FALSE low  
#' ##  9 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## 10 2016-01-20 04:30:00 2016-01-20     3 5-bce-642     9   838. FALSE high 
#' ## # … with more rows}
#' 
#' 
#' @family Planning and Prep
#' @section Function ID:
#' 1-10
#' 
#' @export
tbl_get <- function(
    tbl,
    store = NULL
) {
  
  # Get the table-prep formula with the `tbl_source()` function
  tbl_entry <- tbl_source(tbl = tbl, store = store)
  
  if (!is.null(attr(store, which = "pb_init", exact = TRUE))) {
    
    init_stmt <- attr(store, which = "pb_init", exact = TRUE)
    
    rlang::eval_tidy(rlang::f_rhs(init_stmt))
  }

  # Obtain the table object
  tbl_obj <- 
    rlang::f_rhs(tbl_entry) %>%
    rlang::eval_tidy()
  
  # Add the in-store table name to the `pb_tbl_name` attribute
  # of the retrieved table
  if (
    !is.null(rlang::f_lhs(tbl_entry)) &&
    is.null(attr(tbl, "pb_tbl_name", exact = TRUE))
  ) {
    table_name <- as.character(rlang::f_lhs(tbl_entry))
    attr(tbl_obj, "pb_tbl_name") <- table_name
  }
  
  # Add the retrieval time to the `pb_tbl_name` attribute
  # of the table if it isn't present
  if (is.null(attr(tbl, "pb_access_time", exact = TRUE))) {
    
    access_time <- Sys.time()
    attr(tbl_obj, "pb_access_time") <- access_time
  }
  
  suppressWarnings(tbl_obj)
}

yaml_read_tbl_store <- function(filename) {
  
  # Read the YAML file with `yaml::read_yaml()`
  y <- yaml::read_yaml(file = filename)
  
  table_names <- names(y$tbls)
  table_formulas <- unlist(y$tbls, recursive = FALSE, use.names = FALSE)
  
  statements <- paste(table_names, table_formulas)
  
  # If there is an init statement, obtain that
  if ("init" %in% names(y)) {
    init <- y$init
  } else {
    init <- NULL
  }
  
  # Generate the expression string
  expr_str <-
    paste0(
      "tbl_store(\n",
      paste(paste0("  ", statements), collapse = ",\n"),
      if (is.null(init)) "\n)" else paste0(",\n  .init = ", init, "\n)")
    )

  tbl_store <- 
    expr_str %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()
    
  tbl_store
}
