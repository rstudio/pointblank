library(tidyverse)
library(pointblank)
library(DBI)
library(RMariaDB)

# Create a connection to the `Rfam`
# database hosted publicly at "mysql-rfam-public.ebi.ac.uk"
# Connection info at: https://docs.rfam.org/en/latest/database.html
con <- 
  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = "Rfam",
    username = "rfamro",
    password = "",
    host = "mysql-rfam-public.ebi.ac.uk",
    port = 4497
  )

# Tables and Descriptions
# 
# family: a list of all Rfam families and family specific information
#   (family accession, family name, description etc.)
# rfamseq: a list of all analysed sequences including INSDC accessions,
#   taxonomy id etc.
# full_region: a list of all sequences annotated with Rfam families
#   including INSDC accessions, start and end coordinates, bit scores etc.
# clan: description of all Rfam clans
# clan_membership: a list of all Rfam families per clan
# taxonomy: NCBI taxonomy identifiers

# Access the `full_region` table
full_region <- dplyr::tbl(con, "full_region")

# Use the `scan_data()` function
scan_data(full_region)
