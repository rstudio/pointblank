library(tidyverse)
library(yaml)

file_list_countries <-
  list.files(
    path = "data-raw/zzz-data-01-countries",
    full.names = TRUE,
    pattern = ".yaml"
  )

country_tbl <-
  dplyr::tibble(
    name = character(0),
    alpha_2 = character(0),
    alpha_3 = character(0),
    alpha_n = character(0),
    region = character(0),
    subregion = character(0),
    world_region = character(0),
    postal_code_format = character(0),
    lat_dec = numeric(0),
    lon_dec = numeric(0),
    currency_a = character(0)
  )

for (i in seq_along(file_list_countries)) {

  y <-
    yaml::read_yaml(
      file = file_list_countries[i]
    )[[1]]

  if (y$postal_code) {
    postal_code_format <- y$postal_code_format
  } else {
    postal_code_format <- NA_character_
  }

  country_tbl <-
    dplyr::bind_rows(
      country_tbl,
      dplyr::tibble(
        name = y$name,
        alpha_2 = y$alpha2,
        alpha_3 = y$alpha3,
        alpha_n = y$country_code,
        region = y$region,
        subregion = y$subregion,
        world_region = y$world_region,
        postal_code_format = postal_code_format,
        lat_dec = as.numeric(y$geo$latitude_dec),
        lon_dec = as.numeric(y$geo$longitude_dec),
        currency_a = y$currency_code
      )
    )
}

currency_tbl <-
  readr::read_csv(file = "data-raw/currency_codes.csv", col_types = "cccccc") %>%
  dplyr::select(AlphabeticCode, NumericCode) %>%
  dplyr::distinct() %>%
  tidyr::drop_na() %>%
  dplyr::rename(currency_a = AlphabeticCode, currency_n = NumericCode) %>%
  dplyr::arrange(currency_a)

country_tbl <-
  dplyr::left_join(
    country_tbl,
    currency_tbl,
    by = "currency_a"
  ) %>%
  dplyr::mutate(region = ifelse(region == "", NA_character_, region)) %>%
  dplyr::mutate(subregion = ifelse(subregion == "", NA_character_, subregion))


file_list_subdivisions <-
  list.files(
    path = "data-raw/zzz-data-02-subdivisions",
    full.names = TRUE,
    pattern = ".yaml"
  )

countries <-
  gsub(".yaml", "", basename(file_list_subdivisions))

subdivision_tbl <-
  dplyr::tibble(
    country_alpha_2 = character(0),
    subd_name = character(0),
    name_en = character(0),
    lat_dec = numeric(0),
    lon_dec = numeric(0)
  )

for (i in seq_along(file_list_countries)) {

  y <-
    try(
      yaml::read_yaml(
        file = file_list_subdivisions[i]
      ), silent = TRUE
    )

  if (!inherits(y, "try-error")) {

    sub_names <- names(y)

    for (j in seq_along(sub_names)) {

      z <- y[[sub_names[j]]]

      if (!is.null(z$translations$en)) {
        name_en <- z$translations$en[1]
      } else {
        name_en <- NA_character_
      }

      if (!is.null(z$geo$latitude)) {
        lat_dec <- z$geo$latitude[1]
      } else {
        lat_dec <- NA_real_
      }

      if (!is.null(z$geo$longitude)) {
        lon_dec <- z$geo$longitude[1]
      } else {
        lon_dec <- NA_real_
      }

      subdivision_tbl <-
        dplyr::bind_rows(
          subdivision_tbl,
          dplyr::tibble(
            country_alpha_2 = countries[i],
            subd_name = sub_names[j],
            name_en = name_en,
            lat_dec = lat_dec,
            lon_dec = lon_dec
          )
        )
    }
  }
}

subdivision_tbl <-
  dplyr::left_join(
    subdivision_tbl,
    country_tbl %>% dplyr::select(name, alpha_2, alpha_3, alpha_n),
    by = c("country_alpha_2" = "alpha_2")
  ) %>%
  dplyr::select(
    country_name = name,
    country_alpha_2,
    country_alpha_3 = alpha_3,
    dplyr::everything()
  )

countries <- country_tbl
subdivisions <- subdivision_tbl
currencies <- currency_tbl

get_subdivision_list <- function(subdivisions, countries) {

  subd_list <- list()

  for (co in countries) {

    subd_country <-
      subdivisions %>%
      dplyr::filter(country_alpha_3 == {{ co }}) %>%
      dplyr::filter(!(subd_name %in% c("FALSE"))) %>%
      dplyr::filter(!(grepl("[0-9]", subd_name))) %>%
      dplyr::pull(subd_name)

    subd_country_list <- list(subd_country)
    names(subd_country_list) <- co

    subd_list <- append(subd_list, subd_country_list)
  }

  subd_list
}

subd_list_main <-
  get_subdivision_list(
    subdivisions = subdivisions,
    countries = c(
      "USA", "CAN", "AUS", "GBR", "NZL", "IND", "RUS",
      "ZAF", "BRA", "MEX", "DEU", "ITA", "CHN", "IDN"
    )
  )
