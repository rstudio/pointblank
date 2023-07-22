skip_on_os(os = "windows")
local_edition(3)

work_path <- "./generated_r_files"

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}

fs::dir_create(path = work_path)

write_draft_snapshot_test <- function(
    dataset,
    file_name,
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
        file_name = file_name,
        path = work_path,
        lang = lang,
        output_type = output_type,
        add_comments = add_comments,
        overwrite = TRUE
      )
    )
  )
  
  path <- list.files(path = work_path, pattern = file_name, full.names = TRUE)
  expect_snapshot(readLines(con = path) %>% paste0(collapse = "\n"))
}

test_that("draft validations for data tables can be generated", {
  
  #write_draft_snapshot_test(dataset = gt::countrypops, file_name = "countrypops")
  write_draft_snapshot_test(dataset = gt::sza, file_name = "sza")
  write_draft_snapshot_test(dataset = gt::gtcars, file_name = "gtcars")
  write_draft_snapshot_test(dataset = gt::sp500, file_name = "sp500")
  write_draft_snapshot_test(dataset = gt::pizzaplace, file_name = "pizzaplace")
  write_draft_snapshot_test(dataset = gt::exibble, file_name = "exibble")
  
  write_draft_snapshot_test(dataset = ggplot2::diamonds, file_name = "diamonds")
  write_draft_snapshot_test(dataset = ggplot2::economics_long, file_name = "economics_long")
  write_draft_snapshot_test(dataset = ggplot2::faithfuld, file_name = "faithfuld")
  write_draft_snapshot_test(dataset = ggplot2::luv_colours, file_name = "luv_colours")
  write_draft_snapshot_test(dataset = ggplot2::midwest, file_name = "midwest")
  write_draft_snapshot_test(dataset = ggplot2::mpg, file_name = "mpg")
  write_draft_snapshot_test(dataset = ggplot2::msleep, file_name = "msleep")
  write_draft_snapshot_test(dataset = ggplot2::presidential, file_name = "presidential")
  write_draft_snapshot_test(dataset = ggplot2::seals, file_name = "seals")
  write_draft_snapshot_test(dataset = ggplot2::txhousing, file_name = "txhousing")
  
  write_draft_snapshot_test(dataset = dplyr::band_instruments, file_name = "band_instruments")
  write_draft_snapshot_test(dataset = dplyr::band_members, file_name = "band_members")
  write_draft_snapshot_test(dataset = dplyr::starwars, file_name = "starwars")
  write_draft_snapshot_test(dataset = dplyr::storms, file_name = "storms")
  
  write_draft_snapshot_test(dataset = tidyr::billboard, file_name = "billboard")
  write_draft_snapshot_test(dataset = tidyr::construction, file_name = "construction")
  write_draft_snapshot_test(dataset = tidyr::fish_encounters, file_name = "fish_encounters")
  #write_draft_snapshot_test(dataset = tidyr::population, file_name = "population")
  write_draft_snapshot_test(dataset = tidyr::relig_income, file_name = "relig_income")
  write_draft_snapshot_test(dataset = tidyr::smiths, file_name = "smiths")
  write_draft_snapshot_test(dataset = tidyr::us_rent_income, file_name = "us_rent_income")
  #write_draft_snapshot_test(dataset = tidyr::who, file_name = "who")
  write_draft_snapshot_test(dataset = tidyr::world_bank_pop, file_name = "world_bank_pop")
  
  write_draft_snapshot_test(dataset = lubridate::lakers, file_name = "lakers")
  
  write_draft_snapshot_test(dataset = datasets::airquality, file_name = "airquality")
  write_draft_snapshot_test(dataset = datasets::chickwts, file_name = "chickwts")
  write_draft_snapshot_test(dataset = datasets::iris, file_name = "iris")
  write_draft_snapshot_test(dataset = datasets::LifeCycleSavings, file_name = "LifeCycleSavings")
  write_draft_snapshot_test(dataset = datasets::longley, file_name = "longley")
  write_draft_snapshot_test(dataset = datasets::morley, file_name = "morley")
  write_draft_snapshot_test(dataset = datasets::mtcars, file_name = "mtcars")
  write_draft_snapshot_test(dataset = datasets::Orange, file_name = "Orange")
  write_draft_snapshot_test(dataset = datasets::pressure, file_name = "pressure")
  write_draft_snapshot_test(dataset = datasets::quakes, file_name = "quakes")
  write_draft_snapshot_test(dataset = datasets::rock, file_name = "rock")
  write_draft_snapshot_test(dataset = datasets::swiss, file_name = "swiss")
  write_draft_snapshot_test(dataset = datasets::USJudgeRatings, file_name = "USJudgeRatings")
})

test_that("draft validations for data tables can be generated in different languages", {

  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_en", lang = "en")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_fr", lang = "fr")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_de", lang = "de")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_it", lang = "it")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_es", lang = "es")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_pt", lang = "pt")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_tr", lang = "tr")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_zh", lang = "zh")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_ru", lang = "ru")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_pl", lang = "pl")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_da", lang = "da")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_sv", lang = "sv")
  write_draft_snapshot_test(dataset = pointblank::small_table, file_name = "st_nl", lang = "nl")
})

test_that("draft validations for data tables can be generated in .Rmd format", {
  
  write_draft_snapshot_test(dataset = gt::countrypops, file_name = "countrypops_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::sza, file_name = "sza_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::gtcars, file_name = "gtcars_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::sp500, file_name = "sp500_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::pizzaplace, file_name = "pizzaplace_rmd", output_type = "Rmd")
  write_draft_snapshot_test(dataset = gt::exibble, file_name = "exibble_rmd", output_type = "Rmd")
})

test_that("draft validations for data tables can be generated without comments", {
  
  write_draft_snapshot_test(dataset = gt::countrypops, file_name = "countrypops_no_comments", add_comments = FALSE)
})

test_that("an invalid path used in `draft_validation()` will result in an error", {
  expect_error(draft_validation(tbl = gt::countrypops, file_name = "countrypops", path = "invalid/path"))
})

test_that("a file created with `draft_validation()` cannot be overwritten by default", {

  suppressMessages(
    draft_validation(tbl = gt::countrypops, file_name = "countrypops_new", path = work_path)
  )
  
  expect_error(
    suppressMessages(
      draft_validation(tbl = gt::countrypops, file_name = "countrypops_new", path = work_path)
    )
  )
  
  expect_error(
    regexp = NA,
    suppressMessages(
      draft_validation(tbl = gt::countrypops, file_name = "countrypops_new", path = work_path, overwrite = TRUE)
    )
  )
})

test_that("messages emitted by `draft_validation()` can be quieted", {
  
  expect_message(
    draft_validation(tbl = gt::countrypops, file_name = "countrypops", path = work_path, overwrite = TRUE)
  )
  
  expect_message(
    regexp = NA,
    draft_validation(tbl = gt::countrypops, file_name = "countrypops", path = work_path, overwrite = TRUE, quiet = TRUE)
  )
})

if (fs::dir_exists(path = work_path)) {
  fs::dir_delete(path = work_path)
}
