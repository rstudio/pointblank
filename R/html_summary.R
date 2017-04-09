#' Create an HTML summary of the validation 
#' @description Create an HTML summary of the validation.
#' @param reporting_object a pointblank summary object.
#' @importFrom rmarkdown render
#' @export html_summary

html_summary <- function(reporting_object) {
  
  saveRDS(reporting_object, "summary.rds")
  
  file.copy(from = system.file("report_rmd", "validation_report.Rmd", package = "pointblank"),
            to = "./validation_report.Rmd")
  
  rmarkdown::render("validation_report.Rmd")
}
