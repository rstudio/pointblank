#' Process multiple validation files
#' @description Run multiple validation
#' scripts in a single function call. This
#' is useful when validation scripts are
#' developed over time since this function
#' could be simply pointed to a directory
#' containing validation script files.
#' @param input_dir a path to where the
#' validation scripts reside. Any files that
#' match the pattern specified in
#' \code{file_pattern} are included in the
#' file list for processing.
#' @param file_pattern a file pattern to
#' distinguish which files are to be included
#' in the file list for processing. If no
#' pattern is provided, the default pattern
#' will caputure any R script files in
#' \code{input_dir}. 
#' @param input_files a vector of input files
#' that are to be processed. If filenames
#' are provided here, any values provided to
#' \code{input_dir} and \code{file_pattern}
#' are ignored.
#' @importFrom purrr walk
#' @importFrom glue glue
#' @export
run_validation_files <- function(input_dir = NULL,
                                 file_pattern = NULL,
                                 input_files = NULL) {
  
  if (is.null(input_files)) {
    
    if (is.null(input_dir)) {
      input_dir <- getwd()
    }
    
    if (is.null(file_pattern)) {
      file_pattern <- "^.*\\.R$"
    }
    
    # Get all validation R scripts
    validation_script_files <-
      list.files(
        path = input_dir,
        pattern = file_pattern)
    
  } else {
    
    validation_script_files <- input_files
  }
  
  validation_script_files %>%
    purrr::walk(
      .f = function(x) {
        
        message(glue::glue("Validation script `{x}` is processing now."))
        
        try(source(x))
      })
}
