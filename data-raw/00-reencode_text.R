library(yaml)

y <- yaml::read_yaml(here::here("data-raw/translations_source.yml"), fileEncoding = "UTF-8")

saveRDS(y, file = here::here("inst/text/translations_built"))
