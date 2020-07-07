library(tidyverse)
library(pointblank)
library(sparklyr)
library(intendo)

# Set an environment variable for the location of
# Java 8 JDK; this library was obtained by using
# `brew cask install homebrew/cask-versions/adoptopenjdk8`
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home")

# Install Spark if necessary
# spark_install(version = "2.4.3", hadoop_version = "2.7")

# Generate a Spark table with `intendo::intendo_revenue`
sc <- spark_connect(master = "local")
intendo_revenue_sp <- dplyr::copy_to(sc, intendo::intendo_revenue)

##
## Data Quality Analysis
##

# Make a vector of known product types
intendo_products <-
  c(
    "gems1", "gems2", "gems3", "gems4", "gems5",
    "gold1", "gold2", "gold3", "gold4", "gold5", "gold6", "gold7",
    "offer1", "offer2", "offer3", "offer4", "offer5",
    "ad_5sec", "pass"
  )

# Generate an expected schema for the table
revenue_tbl_schema <- 
  col_schema(
    user_id = "StringType",
    session_id = "StringType",
    time = "TimestampType",
    name = "StringType",
    size = "StringType",
    type = "StringType",
    price = "DoubleType",
    revenue = "DoubleType", 
    .db_col_types = "sql"
  )

# Create an `action_levels` object, setting
# *warn* and *stop* thresholds at 0.1 and 0.10
al <- action_levels(warn_at = 0.01, stop_at = 0.10)

agent <-
  create_agent(tbl = intendo_revenue_sp, actions = al) %>%
  col_vals_between(
    vars(revenue),
    left = 0.01, right = 150
  ) %>%
  col_vals_not_null(vars(user_id, session_id, time, name, type, revenue)) %>%
  col_vals_between(
    vars(time),
    left = "2015-01-01", right = "2016-01-01"
  ) %>%
  col_vals_lt(
    vars(revenue),
    value = vars(price),
    preconditions = ~ . %>% filter(type != "ad")
  ) %>%
  col_vals_in_set(
    vars(type),
    set = c(
      "ad", "currency", "season_pass",
      "offer_agent", NA
    ),
  ) %>%
  col_vals_in_set(
    vars(name),
    set = intendo_products
  ) %>%
  col_vals_regex(
    vars(user_id),
    regex = "[A-Y]{12}"
  ) %>%
  col_vals_regex(
    vars(session_id),
    regex = "[A-Z]{5}_[a-z]{8}"
  ) %>%
  col_is_character(vars(user_id, session_id)) %>%
  col_schema_match(schema = revenue_tbl_schema) %>%
  interrogate()

agent

##
## Table Scan
##

# Use a smaller version of the `intendo::intendo_revenue` table
intendo_revenue_small <- dplyr::copy_to(sc, intendo::intendo_revenue[1:2000, ])

# Use `scan_data()` to get a Table Scan
scan_data(intendo_revenue_small)
