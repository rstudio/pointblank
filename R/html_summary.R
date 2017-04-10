#' Create an HTML summary file for the
#' interrogation
#' @description Get the essential information
#' from an agent object after an interrogation
#' is complete and then generates an HTML file
#' for a visual summary.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @importFrom rmarkdown render
#' @export html_summary

html_summary <- function(agent) {
  
  summary <- get_summary(agent)
  
  saveRDS(summary, "summary.rds")
  
  file.copy(from = system.file("report_rmd", "validation_report.Rmd", package = "pointblank"),
            to = "./validation_report.Rmd")
  
  rmarkdown::render("validation_report.Rmd")
}
