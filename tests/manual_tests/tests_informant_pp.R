library(pointblank)

informant_pp <- 
  create_informant(
    read_fn = ~ palmerpenguins::penguins,
    tbl_name = "penguins",
    label = "The `penguins` dataset from the **palmerpenguins** 📦."
  ) %>% 
  info_columns(
    columns = "species",
    `ℹ️` = "A factor denoting penguin species ({species_snippet})."
  ) %>%
  info_columns(
    columns = "island",
    `ℹ️` = "A factor denoting island in Palmer Archipelago, Antarctica
    ({island_snippet})."
  ) %>%
  info_snippet(
    snippet_name = "species_snippet",
    fn = snip_list(column = "species")
  ) %>%
  info_snippet(
    snippet_name = "island_snippet",
    fn = snip_list(column = "island")
  ) %>%
  info_columns(
    columns = "year",
    `ℹ️` = "The study year ({year_snippet})."
  ) %>%
  info_snippet(
    snippet_name = "year_snippet",
    fn = snip_list(column = "year")
  ) %>%
  info_columns(
    columns = "bill_length_mm",
    `ℹ️` = "A number denoting bill length"
  ) %>%
  info_columns(
    columns = "bill_depth_mm",
    `ℹ️` = "A number denoting bill depth (in the range of
    {min_depth} to {max_depth} millimeters)."
  ) %>%
  info_columns(
    columns = "flipper_length_mm",
    `ℹ️` = "An integer denoting flipper length"
  ) %>%
  info_columns(
    columns = matches("length"),
    `ℹ️` = "(in units of millimeters)."
  ) %>%
  info_columns(
    columns = "flipper_length_mm",
    `ℹ️` = "Largest observed is {largest_flipper_length} mm."
  ) %>%
  info_snippet(
    snippet_name = "min_depth",
    fn = snip_lowest(column = "bill_depth_mm")
  ) %>%
  info_snippet(
    snippet_name = "max_depth",
    fn = snip_highest(column = "bill_depth_mm")
  ) %>%
  info_snippet(
    snippet_name = "largest_flipper_length",
    fn = snip_highest(column = "flipper_length_mm")
  ) %>%
  info_columns(
    columns = vars(body_mass_g), 
    `ℹ️` = "An integer denoting body mass."
  ) %>%
  info_columns(
    columns = c(ends_with("mm"), ends_with("g")),
    `ℹ️` = "((measured))"    
  ) %>%
  info_section(
    section_name = "additional notes",
    `data types` = "(((factor))) (((numeric))) (((integer)))"
  ) %>%
  info_tabular(
    `R dataset` = "The goal of `palmerpenguins` is to provide a great dataset
    for data exploration & visualization, as an alternative to `iris`. The
    latest CRAN release was published on (2020-07-25).",
    `data collection` = "Data were collected and made available by Dr. Kristen
    Gorman and the [Palmer Station, Antarctica LTER](https://pal.lternet.edu),
    a member of the [Long Term Ecological Research Network](https://lternet.edu).",
    citation = "Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer 
    Archipelago (Antarctica) penguin data. R package version 0.1.0.
    <https://allisonhorst.github.io/palmerpenguins/>. 
    doi: 10.5281/zenodo.3960218."
  ) %>%
  info_columns(
    columns = vars(sex), 
    `ℹ️` = "A [[factor]]<<text-decoration: underline;>> 
    denoting penguin sex (female or male)."
  ) %>%
  info_section(
    section_name = "additional notes",
    keywords = "
    [[((penguins))]]<<border-color: platinum; background-color: #F0F8FF;>>
     [[((Antarctica))]]<<border-color: #800080; background-color: #F2F2F2;>>
     [[((measurements))]]<<border-color: #FFB3B3; background-color: #FFFEF4;>>
    "
  ) %>%
  info_section(
    section_name = "source",
    "References" = "
- Adélie penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural 
size measurements and isotopic signatures of foraging among adult male and female 
Adélie penguins (Pygoscelis adeliae) nesting along the Palmer Archipelago near
Palmer Station, 2007-2009 ver 5. Environmental Data Initiative
<https://doi.org/10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f>
- Gentoo penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural
size measurements and isotopic signatures of foraging among adult male and female
Gentoo penguin (Pygoscelis papua) nesting along the Palmer Archipelago near Palmer
Station, 2007-2009 ver 5. Environmental Data Initiative
<https://doi.org/10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689>
- Chinstrap penguins: Palmer Station Antarctica LTER and K. Gorman. 2020.
Structural size measurements and isotopic signatures of foraging among adult male
and female Chinstrap penguin (Pygoscelis antarcticus) nesting along the Palmer
Archipelago near Palmer Station, 2007-2009 ver 6. Environmental Data Initiative
<https://doi.org/10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e>
",
"Note" = 
  "Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual
Dimorphism and Environmental Variability within a Community of Antarctic Penguins
(Genus Pygoscelis). PLoS ONE 9(3): e90081. doi:10.1371/journal.pone.0090081
"
  ) %>%
  incorporate()

informant_pp
