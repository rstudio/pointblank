skip_on_os(os = "windows")
local_edition(3)

work_path <- "./generated_r_files"

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

fs::dir_create(path = work_path)

write_draft_snapshot_test <- function(
    dataset,
    filename,
    tbl_name = NULL,
    path = work_path,
    lang = NULL,
    output_type = "R",
    add_comments = TRUE
) {

  tbl <- dataset
  
  suppressMessages(
    expect_true(
      draft_validation(
        tbl = tbl,
        tbl_name = tbl_name,
        filename = filename,
        path = work_path,
        lang = lang,
        output_type = output_type,
        add_comments = add_comments,
        overwrite = TRUE
      )
    )
  )
  
  path <- list.files(path = work_path, pattern = filename, full.names = TRUE)
  expect_snapshot(readLines(con = path) %>% paste0(collapse = "\n"))
}

test_that("draft validations for data tables can be generated", {
  
  #write_draft_snapshot_test(dataset = gt::countrypops, filename = "countrypops")
  write_draft_snapshot_test(dataset = gt::sza, filename = "sza")
  write_draft_snapshot_test(dataset = gt::gtcars, filename = "gtcars")
  write_draft_snapshot_test(dataset = gt::sp500, filename = "sp500")
  write_draft_snapshot_test(dataset = gt::pizzaplace, filename = "pizzaplace")
  write_draft_snapshot_test(dataset = gt::exibble, filename = "exibble")
  
  write_draft_snapshot_test(dataset = ggplot2::diamonds, filename = "diamonds")
  write_draft_snapshot_test(dataset = ggplot2::economics_long, filename = "economics_long")
  write_draft_snapshot_test(dataset = ggplot2::faithfuld, filename = "faithfuld")
  write_draft_snapshot_test(dataset = ggplot2::luv_colours, filename = "luv_colours")
  write_draft_snapshot_test(dataset = ggplot2::midwest, filename = "midwest")
  write_draft_snapshot_test(dataset = ggplot2::mpg, filename = "mpg")
  write_draft_snapshot_test(dataset = ggplot2::msleep, filename = "msleep")
  write_draft_snapshot_test(dataset = ggplot2::presidential, filename = "presidential")
  write_draft_snapshot_test(dataset = ggplot2::seals, filename = "seals")
  write_draft_snapshot_test(dataset = ggplot2::txhousing, filename = "txhousing")
  
  write_draft_snapshot_test(dataset = dplyr::band_instruments, filename = "band_instruments")
  write_draft_snapshot_test(dataset = dplyr::band_members, filename = "band_members")
  write_draft_snapshot_test(dataset = dplyr::starwars, filename = "starwars")
  # write_draft_snapshot_test(dataset = dplyr::storms, filename = "storms")
  
  write_draft_snapshot_test(dataset = tidyr::billboard, filename = "billboard")
  write_draft_snapshot_test(dataset = tidyr::construction, filename = "construction")
  write_draft_snapshot_test(dataset = tidyr::fish_encounters, filename = "fish_encounters")
  #write_draft_snapshot_test(dataset = tidyr::population, filename = "population")
  write_draft_snapshot_test(dataset = tidyr::relig_income, filename = "relig_income")
  write_draft_snapshot_test(dataset = tidyr::smiths, filename = "smiths")
  write_draft_snapshot_test(dataset = tidyr::us_rent_income, filename = "us_rent_income")
  #write_draft_snapshot_test(dataset = tidyr::who, filename = "who")
  write_draft_snapshot_test(dataset = tidyr::world_bank_pop, filename = "world_bank_pop")
  
  write_draft_snapshot_test(dataset = lubridate::lakers, filename = "lakers")
  
  write_draft_snapshot_test(dataset = datasets::airquality, filename = "airquality")
  write_draft_snapshot_test(dataset = datasets::chickwts, filename = "chickwts")
  write_draft_snapshot_test(dataset = datasets::iris, filename = "iris")
  write_draft_snapshot_test(dataset = datasets::LifeCycleSavings, filename = "LifeCycleSavings")
  write_draft_snapshot_test(dataset = datasets::longley, filename = "longley")
  write_draft_snapshot_test(dataset = datasets::morley, filename = "morley")
  write_draft_snapshot_test(dataset = datasets::mtcars, filename = "mtcars")
  write_draft_snapshot_test(dataset = datasets::Orange, filename = "Orange")
  write_draft_snapshot_test(dataset = datasets::pressure, filename = "pressure")
  write_draft_snapshot_test(dataset = datasets::quakes, filename = "quakes")
  write_draft_snapshot_test(dataset = datasets::rock, filename = "rock")
  write_draft_snapshot_test(dataset = datasets::swiss, filename = "swiss")
  write_draft_snapshot_test(dataset = datasets::USJudgeRatings, filename = "USJudgeRatings")
})

test_that("draft validations for data tables can be generated in different languages", {

  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_en", lang = "en")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_fr", lang = "fr")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_de", lang = "de")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_it", lang = "it")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_es", lang = "es")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_pt", lang = "pt")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_tr", lang = "tr")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_zh", lang = "zh")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_ru", lang = "ru")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_pl", lang = "pl")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_da", lang = "da")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_sv", lang = "sv")
  write_draft_snapshot_test(dataset = pointblank::small_table, filename = "st_nl", lang = "nl")
})

test_that("draft validations for data tables can be generated in .Rmd format", {
  
  write_draft_snapshot_test(dataset = gt::countrypops, filename = "countrypops_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::sza, filename = "sza_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::gtcars, filename = "gtcars_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::sp500, filename = "sp500_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::pizzaplace, filename = "pizzaplace_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::exibble, filename = "exibble_rmd", output_type = "Rmd")
})

test_that("draft validations for data tables can be generated without comments", {
  
  write_draft_snapshot_test(dataset = gt::countrypops, filename = "countrypops_no_comments", add_comments = FALSE)
})

test_that("an invalid path used in `draft_validation()` will result in an error", {
  expect_error(draft_validation(tbl = gt::countrypops, filename = "countrypops", path = "invalid/path"))
})

test_that("a file created with `draft_validation()` cannot be overwritten by default", {

  suppressMessages(
    draft_validation(tbl = gt::countrypops, filename = "countrypops_new", path = work_path)
  )
  
  expect_error(
    suppressMessages(
      draft_validation(tbl = gt::countrypops, filename = "countrypops_new", path = work_path)
    )
  )
  
  expect_no_error(
    suppressMessages(
      draft_validation(tbl = gt::countrypops, filename = "countrypops_new", path = work_path, overwrite = TRUE)
    )
  )
})

test_that("messages emitted by `draft_validation()` can be quieted", {
  
  expect_message(
    draft_validation(tbl = gt::countrypops, filename = "countrypops", path = work_path, overwrite = TRUE)
  )
  
  expect_no_message(
    draft_validation(tbl = gt::countrypops, filename = "countrypops", path = work_path, overwrite = TRUE, quiet = TRUE)
  )
})

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}
