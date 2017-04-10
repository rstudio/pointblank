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
  
  if (!inherits(agent, "ptblank_agent")) {
    stop("The object provided must be a valid `ptblank_agent` object.")
  }
  
  saveRDS(agent, "agent.rds")
  
  file.copy(from = system.file("report_rmd", "validation_report.Rmd", package = "pointblank"),
            to = "./validation_report.Rmd",
            overwrite = TRUE)
  
  rmarkdown::render("validation_report.Rmd")
}
