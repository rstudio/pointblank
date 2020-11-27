library(pointblank)

create_informant(
  read_fn = ~ palmerpenguins::penguins,
  tbl_name = "penguins",
  label = "Les données `penguins` du paquet **palmerpenguins**.",
  lang = "fr"
) %>% 
  info_columns(
    columns = "species",
    `ℹ️` = "Un facteur désignant les espèces de manchots ({species_snippet})."
  ) %>%
  info_columns(
    columns = "island",
    `ℹ️` = "L'île de l'archipel de Palmer, Antarctique
    ({island_snippet})."
  ) %>%
  info_columns(
    columns = "year",
    `ℹ️` = "L'année d'étude: {year_snippet}."
  ) %>%
  info_snippet(
    snippet_name = "species_snippet",
    fn = snip_list(column = "species", and_or = "and")
  ) %>%
  info_snippet(
    snippet_name = "island_snippet",
    fn = snip_list(column = "island", and_or = "or")
  ) %>%
  info_snippet(
    snippet_name = "year_snippet",
    fn = snip_list(column = "year", limit = 1)
  ) %>%
  incorporate()

