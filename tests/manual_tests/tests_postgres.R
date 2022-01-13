library(tidyverse)
library(pointblank)
library(DBI)
library(RPostgres)

# Create a connection to the `pfmegrnargs` database hosted
# publicly at "hh-pgsql-public.ebi.ac.uk"; this is an
# RNAcentral joint (https://rnacentral.org/help/public-database)
con <- 
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "pfmegrnargs",
    user = "reader",
    password = "NWDMCE5xdipIjRrp",
    host = "hh-pgsql-public.ebi.ac.uk",
    port = 5432
  )

# Set failure thresholds and functions that are
# actioned from exceeding certain error levels
al <- action_levels(warn_at = 0.02, stop_at = 0.05, notify_at = 0.10)

# Validate the `rna` table in the `pfmegrnargs` DB (contains 27M rows)
agent <- 
  dplyr::tbl(con, "rna") %>%
  create_agent(
    label = "pfmegrnargs: 'rna' table",
    actions = al
  ) %>%
  col_vals_regex(vars(seq_short), "[GTCA]*", na_pass = TRUE) %>%
  col_is_character(vars(upi, userstamp, crc64, seq_short, seq_long, md5)) %>%
  col_vals_gte(vars(len), 1) %>%
  col_schema_match(
    schema = col_schema(
      id = "integer64",
      upi = "character",
      timestamp = c("POSIXct", "POSIXt"),
      userstamp = "character",
      crc64 = "character",
      len = "integer",
      seq_short = "character",
      seq_long = "character",
      md5 = "character"
    )
  ) %>%
  col_schema_match(
    schema = col_schema(
      id = "bigint",
      upi = "character varying",
      timestamp = "timestamp without time zone",
      userstamp = "character varying",
      crc64 = "character",
      len = "integer",
      seq_short = "character varying",
      seq_long = "text",
      md5 = "character varying",
      .db_col_types = "sql"
    )
  ) %>%
  interrogate()

DBI::dbDisconnect(con)

# Get a report from the `agent`
agent
