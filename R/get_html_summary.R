#' Create an HTML summary file for the
#' interrogation
#' @description Get the essential
#' information from an agent object
#' after an interrogation is complete
#' and then generates an HTML file for
#' a visual summary.
#' @param agent an agent object of
#' class \code{ptblank_agent}.
#' @param output_file an optional filename
#' to use for the output HTML file. If
#' not provided, the filename
#' \code{validation_report.html} will
#' be used.
#' @param output_dir an optional path
#' to place the output HTML file and
#' the subfolder of CSV data (if row
#' sample data has been collected). If
#' the path does not exist, the
#' directory will be created.
#' @param intro_text HTML text to be
#' placed at the top of the summary.
#' Can be provided as plaintext or as
#' markdown text.
#' @param pre_results_text HTML text
#' to be placed just before the tables
#' of validation results. Can be
#' provided as plaintext or as markdown
#' text.
#' @param footer_text HTML text to be
#' placed in the footer of the summary.
#' Can be provided as plaintext or as
#' markdown text.
#' @return an agent object.
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace_all str_detect
#' @importFrom commonmark markdown_html
#' @export
get_html_summary <- function(agent,
                             output_file = NULL,
                             output_dir = NULL,
                             intro_text = NULL,
                             pre_results_text = NULL,
                             footer_text = NULL) {
  
  if (!inherits(agent, "ptblank_agent")) {
    stop("The object provided must be a valid `ptblank_agent` object.")
  }
  
  #
  # Clean up remnants of directory before
  # writing any work files
  #
  
  if (file.exists("validation_report.Rmd")) {
    invisible(file.remove("validation_report.Rmd"))
  }
  
  if (file.exists("include_intro.html")) {
    invisible(file.remove("include_intro.html"))
  }
  
  if (file.exists("include_pre_results.html")) {
    invisible(file.remove("include_pre_results.html"))
  }
  
  if (file.exists("include_footer.html")) {
    invisible(file.remove("include_footer.html"))
  }
  
  if (file.exists("agent.rds")) {
    invisible(file.remove("agent.rds"))
  }
  
  if (is.null(output_file)) {
    output_file <- "validation_report.html"
  }
  
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  
  # If the `output_file` file does not have
  # an `.html` extension, then append that
  # to the file name
  if (stringr::str_detect(string = output_file, pattern = "\\.html$") == FALSE) {
    output_file <- paste0(output_file, ".html")
  }
    
  # Generate the CSV subfolder name based
  # on `agent$validation_name`
  csv_subfolder_name <-
    gsub("(-|:)", "_", agent$validation_name)
  
  # Remove any previously generated subfolder
  # directory if one exists
  if (dir.exists(csv_subfolder_name)) {
    invisible(
      unlink(
        x = csv_subfolder_name,
        recursive = TRUE,
        force = TRUE))
  }
  
  # Save the `agent` object as an RDS file
  saveRDS(agent, "agent.rds")
  
  # Copy the validation report .Rmd file from
  # `/inst/report_rmd` to the working directory
  file.copy(
    from = system.file(
      "report_rmd", "validation_report.Rmd",
      package = "pointblank"),
    to = "./validation_report.Rmd",
    overwrite = TRUE)
  
  # If `intro_text` is provided, write the
  # `include_intro.txt` file to the working directory
  if (!is.null(intro_text)) {
    if (inherits(intro_text, "character")) {
      cat(
        stringr::str_replace_all(
          commonmark::markdown_html(intro_text),
          "\n", ""),
        file = "include_intro.html")
    }
  }
  
  # If `pre_results_text` is provided, write the
  # `include_pre_results.html` file to the working directory
  if (!is.null(pre_results_text)) {
    if (inherits(pre_results_text, "character")) {
      cat(
        stringr::str_replace_all(
          commonmark::markdown_html(pre_results_text),
          "\n", ""),
        file = "include_pre_results.html")
    }
  }
  
  # If `footer_text` is provided, write the
  # `include_footer.html` file to the working directory
  if (!is.null(footer_text)) {
    if (inherits(footer_text, "character")) {
      cat(
        stringr::str_replace_all(
          commonmark::markdown_html(footer_text),
          "\n", ""),
        file = "include_footer.html")
    }
  } else {
    cat(paste0("<p></p>"), file = "include_footer.html")
  }
  
  # Render the RMarkdown file to the working directory
    rmarkdown::render(
      input = "validation_report.Rmd",
      output_format = "html_document",
      output_dir = output_dir,
      output_file = output_file,
      quiet = TRUE)
  
  #
  # Perform clean up of working directory
  #
  
  if (file.exists("validation_report.Rmd")) {
    invisible(file.remove("validation_report.Rmd"))
  }
  
  if (file.exists("include_intro.html")) {
    invisible(file.remove("include_intro.html"))
  }
  
  if (file.exists("include_pre_results.html")) {
    invisible(file.remove("include_pre_results.html"))
  }
  
  if (file.exists("include_footer.html")) {
    invisible(file.remove("include_footer.html"))
  }
  
  if (file.exists("agent.rds")) {
    invisible(file.remove("agent.rds"))
  }
  
  if (dir.exists("temporary_images")) {
    temp_images_files <- 
      list.files(
        path = "./temporary_images",
        full.names = TRUE)
    
    # Remove the image files
    invisible(file.remove(temp_images_files))
    
    # Remove the empty parent directory
    invisible(file.remove("temporary_images"))
  }
  
  if (dir.exists("temporary_images_plan")) {
    temp_images_files <- 
      list.files(
        path = "./temporary_images_plan",
        full.names = TRUE)
    
    # Remove the image files
    invisible(file.remove(temp_images_files))
    
    # Remove the empty parent directory
    invisible(file.remove("temporary_images_plan"))
  }
  
  # Move the subfolder of CSV data if
  # it has been made
  if (dir.exists(csv_subfolder_name)) {
    
    file.rename(
      from = csv_subfolder_name,
      to = gsub("//", "/", paste0(output_dir, "/", csv_subfolder_name)))
  }
  
  agent
}
