#' Create an HTML summary file for the
#' interrogation
#' @description Get the essential information
#' from an agent object after an interrogation
#' is complete and then generates an HTML file
#' for a visual summary.
#' @param agent an agent object of class
#' \code{ptblank_agent}.
#' @param filename an optional filename to use
#' for the output HTML file. If not provided,
#' the filename \code{validation_report.html}
#' will be used.
#' @param intro_text HTML text to be placed at
#' the top of the summary.
#' @param footer_text HTML text to be placed in
#' the footer of the summary.
#' @param output_dir an optional path to place
#' the output HTML file.
#' @importFrom rmarkdown render
#' @export get_html_summary

get_html_summary <- function(agent,
                             filename = NULL,
                             intro_text = NULL,
                             footer_text = NULL,
                             output_dir = NULL) {
  
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
  
  if (file.exists("include_intro.txt")) {
    invisible(file.remove("include_intro.txt"))
  }
  
  if (file.exists("include_footer.html")) {
    invisible(file.remove("include_footer.html"))
  }
  
  if (file.exists("agent.rds")) {
    invisible(file.remove("agent.rds"))
  }
  
  if (dir.exists("_csv_data")) {
    invisible(
      unlink(
        x = "_csv_data",
        recursive = TRUE,
        force = TRUE))
  }
  
  # Save the `agent` object as an RDS file
  saveRDS(agent, "agent.rds")
  
  # Copy the validation report .Rmd file from
  # `/inst/report_rmd` to the working directory
  file.copy(
    from = system.file("report_rmd", "validation_report.Rmd", package = "pointblank"),
    to = "./validation_report.Rmd",
    overwrite = TRUE)
  
  # If `intro_text` is provided, write the
  # `include_intro.txt` file to the working directory
  if (!is.null(intro_text)) {
    if (inherits(intro_text, "character")) {
      cat(intro_text, file = "include_intro.txt")
    }
  }
  
  # If `footer_text` is provided, write the
  # `include_footer.html` file to the working directory
  if (!is.null(footer_text)) {
    if (inherits(footer_text, "character")) {
      cat(paste0("<p>", footer_text, "</p>"), file = "include_footer.html")
    }
  } else {
    cat(paste0("<p></p>"), file = "include_footer.html")
  }
  
  # Render the RMarkdown file to the working directory
  suppressMessages(rmarkdown::render("validation_report.Rmd"))
  
  # Get the default output filename
  output_file <- "validation_report.html"
  
  #
  # Perform clean up of working directory
  #
  
  if (file.exists("validation_report.Rmd")) {
    invisible(file.remove("validation_report.Rmd"))
  }
  
  if (file.exists("include_intro.txt")) {
    invisible(file.remove("include_intro.txt"))
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
  
  # Save file with a specified file name
  if (!is.null(filename)) {
    if (inherits(filename, "character")) {
      invisible(
        file.rename(
          from = "validation_report.html",
          to = filename))
      
      output_file <- filename
    }
  }
  
  # Move file to a specified directory
  if (!is.null(output_dir)) {
    
    if (dir.exists(output_dir)) {
      invisible(
        file.copy(
          from = gsub("//", "/", paste0(getwd(), "/", output_file)),
          to = gsub("//", "/", paste0(output_dir, "/", output_file)),
          overwrite = TRUE))
      
      # Remove the file from the working directory
      invisible(file.remove(output_file))
      
      # Move the subfolder of CSV data if
      # it has been made
      if (dir.exists("_csv_data")) {
        
        if (!dir.exists(output_dir)) {
          dir.create(output_dir)
        }
        
        file.rename(
          from = "_csv_data",
          to = gsub("//", "/", paste0(output_dir, "/_csv_data")))
        
      }
    }
  }
}
