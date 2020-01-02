library(pointblank)
library(tidyverse)
library(RMariaDB)

# Public access MySQL database from `ensembl.org`
server <- "ensembldb.ensembl.org"
username <- "anonymous"
port <- 3306L

con <- 
  dbConnect(
    drv = RMariaDB::MariaDB(),
    dbname = "homo_sapiens_core_98_38",
    username = username,
    host = server,
    port = port
  )
  
tbl_exon <- tbl(con, "exon")

agent_plan <-
  create_agent(tbl = tbl_exon) %>%
  col_vals_in_set(vars(is_current), c(0, 1)) %>%
  col_vals_gt(vars(seq_region_start, seq_region_end), 10000)

agent_plan %>% get_agent_report()

agent_intel <- agent_plan %>% interrogate()

agent_intel

agent_report <- agent_intel %>% get_agent_report()

agent_report

extract_1 <- agent_intel %>% get_data_extracts(i = 2)

extract_1
