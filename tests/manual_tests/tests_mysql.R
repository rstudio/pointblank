library(tidyverse)
library(pointblank)
library(DBI)
library(RMariaDB)

# Create a connection to the `aedes_aegypti_core_55_1d`
# database hosted publicly at "ensembldb.ensembl.org"
con <- 
  DBI::dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "aedes_aegypti_core_55_1d",
  username = "anonymous",
  password = "",
  host = "ensembldb.ensembl.org",
  port = 3306
)

##
## Data Quality Analysis
##

# Create an `action_levels` object, setting
# *warn*, *stop*, and *notify* thresholds at
# 0.02, 0.05, and 0.10
al <- action_levels(warn_at = 0.02, stop_at = 0.05, notify_at = 0.10)

# Validate the `assembly` table in the `aedes_aegypti_core_55_1d` DB
agent <- 
  dplyr::tbl(con, "assembly") %>%
  create_agent(
    name = "aedes_aegypti_core_55_1d: 'assembly' table",
    actions = al
  ) %>%
  col_vals_equal(vars(cmp_start), 1) %>%
  col_vals_equal(vars(ori), 1) %>%
  col_vals_gt(vars(asm_seq_region_id), 1) %>%
  col_vals_gt(vars(cmp_seq_region_id), 1) %>%
  col_vals_gt(vars(asm_end), vars(asm_start)) %>%
  col_vals_gt(vars(cmp_end), vars(cmp_start)) %>%
  col_schema_match(
    schema = col_schema(
      asm_seq_region_id = "integer",
      cmp_seq_region_id = "integer",
      asm_start = "integer",
      asm_end = "integer",
      cmp_start = "integer",
      cmp_end = "integer",
      ori = "integer"
    )
  ) %>%
  col_schema_match(
    schema = col_schema(
      asm_seq_region_id = "INT",
      cmp_seq_region_id = "INT",
      asm_start = "INT",
      asm_end = "INT",
      cmp_start = "INT",
      cmp_end = "INT",
      ori = "TINYINT",
      .db_col_types = "sql"
    )
  ) %>%
  interrogate()

DBI::dbDisconnect(con)

# Get a report from the `agent`
agent

##
## Table Scan
##

assembly_tbl <- dplyr::tbl(con, "assembly")
scan_data(assembly_tbl)
