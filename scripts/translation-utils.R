# Get all roxygen entries for exported functions into a single file
get_r_files_with_exports <- function(pkg_root = ".") {

  path_resolved <- fs::as_fs_path(path.expand(paste0(pkg_root, "/R")))
  
  # Get a list of paths for all .R files in the R dir
  r_files <- 
    list.files(
      path = path_resolved,
      pattern = ".R$",
      full.names = TRUE
    )
  
  has_exports <- rep(FALSE, length(r_files))
  
  # Subset this to only those files that contain roxygen2 @export tags
  for (i in seq_along(r_files)) {
    
    lines <- readLines(r_files[i])
    
    has_exports[i] <- any(grepl("^#' @export", lines))
  }
  
  r_files[has_exports]
}

get_documentation_tbl <- function(pkg_root = ".") {
  
  r_files <- get_r_files_with_exports(pkg_root = pkg_root)
  
  docs_tbl <-
    dplyr::tibble(
      file = character(0),
      rd_name = character(0),
      line_start = integer(0),
      line_end = integer(0),
      line_n = integer(0),
      has_name = logical(0),
      has_export = logical(0),
      has_no_rd = logical(0)
    )
  
  for (i in seq_along(r_files)) {
    
    lines <- readLines(r_files[i])
    
    are_roxygen_lines <- grepl("^#'", lines)
    
    rle_lines <- rle(are_roxygen_lines)
    
    rle_lengths <- rle_lines$lengths
    rle_values <- rle_lines$values
    cumsum_lengths <- cumsum(rle_lines$lengths)
  
    change_idx <- c(1, (cumsum_lengths + 1))
    change_idx <- change_idx[-length(change_idx)]
    roxygen_start_idx <- change_idx[rle_values]
    roxygen_end_idx <- roxygen_start_idx + rle_lengths[rle_values] - 1
    
    rd_name <- rep(NA_character_, length(roxygen_start_idx))
    has_name <- has_export <- has_no_rd <- rep(NA, length(roxygen_start_idx))
    
    for (j in seq_along(roxygen_start_idx)) {
      
      roxygen_part <- lines[roxygen_start_idx[j]:roxygen_end_idx[j]]
      line_n <- length(roxygen_part)
      
      # Check for @name directive
      if (any(grepl("^#' @name", roxygen_part))) {
        has_name[j] <- TRUE
        rd_name[j] <- gsub("^#' @name ", "", roxygen_part[grepl("^#' @name", roxygen_part)])
      } else {
        has_name[j] <- FALSE
      }
      
      # Check for @export directive
      if (any(grepl("^#' @export", roxygen_part))) {
        has_export[j] <- TRUE
        rd_name[j] <- gsub(" <-.*$", "", lines[roxygen_end_idx[j] + 1])
      } else {
        
        has_export[j] <- FALSE
        
        # Handle `datasets.R` specially
        if (grepl("datasets.R", r_files[i])) {
          rd_name[j] <- gsub("\"", "", lines[roxygen_end_idx[j] + 1])
        }
      }
      
      # Check for @noRd directive
      if (any(grepl("^#' @noRd", roxygen_part))) {
        has_no_rd[j] <- TRUE
      } else {
        has_no_rd[j] <- FALSE
      }
      
      docs_tbl_row <-
        dplyr::tibble(
          file = r_files[i],
          rd_name = rd_name[j],
          line_start = roxygen_start_idx[j],
          line_end = roxygen_end_idx[j],
          line_n = as.integer(line_n),
          has_name = has_name[j],
          has_export = has_export[j],
          has_no_rd = has_no_rd[j]
        )
      
      docs_tbl <- dplyr::bind_rows(docs_tbl, docs_tbl_row)
    }
  }

  docs_tbl %>%
    dplyr::filter(!has_no_rd) %>%
    dplyr::filter(rd_name != "NULL") %>%
    dplyr::filter(line_n > 5)
}

write_r_file <- function(lines, file_path) {
  
  # Create a file connection  
  file_connection <- file(file_path, open = "wb", encoding = "utf-8")
  on.exit(close(file_connection))
  
  # Obtain the appropriate line ending based on the platform
  if (.Platform$OS.type == "windows") {
    line_ending <- "\r\n"
  } else {
    line_ending <- "\n"
  }
  
  lines <- gsub("\r?\n", line_ending, lines)
  
  writeLines(
    text = enc2utf8(lines),
    con = file_connection,
    sep = line_ending, 
    useBytes = TRUE
  )
  
  invisible(TRUE)
}


make_man_translation_file <- function(pkg_root = ".",
                                      lang_code = "es",
                                      name = paste0("man-", toupper(lang_code), ".R"),
                                      path = "inst/translations",
                                      overwrite = FALSE) {
  
  # Create path that contains the testthat test file name
  file_path <- as.character(fs::path_norm(fs::path(pkg_root, path, name)))
  
  # Check if the file to write already exists; if it does, don't
  # write the new file if `overwrite` is FALSE
  if (fs::file_exists(file_path) && !overwrite) {
    stop(
      "The file of the same name already exists:\n",
      "* set `overwrite` to `TRUE`, or\n",
      "* choose a different `name`, or\n",
      "* define another `path` for the file",
      call. = FALSE
    )
  }
  
  # Get the roxygen documentation tbl
  docs_tbl <- get_documentation_tbl(pkg_root = pkg_root)
  
  docs_tbl <-
    docs_tbl %>%
    dplyr::arrange(rd_name)
  
  lines <- 
    c(
      "# Generated file",
      paste0("# ", toupper(lang_code), " translations for man pages"),
      ""
    )
  
  for (i in seq_len(nrow(docs_tbl))) {
    
    docs_list <- as.list(docs_tbl[i, ])
    
    lines_file <- readLines(con = docs_list$file)
    lines_doc <-
      c(
        "",
        paste0(
          "# ",
          docs_list$rd_name,
          paste(rep("-", 76 - nchar(docs_list$rd_name) - 3), collapse = "")
        ),
        lines_file[(docs_list$line_start):(docs_list$line_end)]
      )
    
    lines <- c(lines, lines_doc)
  }
  
  write_r_file(lines = lines, file_path = file_path)
}

translate_with_file <- function(pkg_root = ".",
                                lang_code = "es",
                                name = paste0("man-", toupper(lang_code), ".R"),
                                subdir = "inst/translations",
                                article_names = NULL) {
  
  # Create file path
  file_path <- as.character(fs::path_norm(fs::path(pkg_root, subdir, name)))
  
  # Check if the file to use exists
  if (!fs::file_exists(file_path)) {
    stop("The translation file doesn't exist", call. = FALSE)
  }
  
  translation_lines <- readLines(con = file_path)
  translation_lines <- translation_lines[5:length(translation_lines)]
  
  article_headers <- which(grepl("^# [a-zA-Z0-9_\\.]*-*?$", translation_lines))
  article_footers <- c(article_headers[(2:length(article_headers))] - 1, length(translation_lines))
  article_names_all <- gsub("(^# |-*|\\s+)", "", translation_lines[article_headers])
  
  if (!is.null(article_names)) {
    article_names <- base::intersect(article_names_all, article_names)
  } else {
    article_names <- article_names_all
  }
  
  if (length(article_names) < 1) {
    return(NULL)
  }
  
  for (i in seq_along(article_names)) {
    
    # Get the roxygen documentation tbl
    docs_tbl <- get_documentation_tbl(pkg_root = pkg_root)
    
    if (!(article_names[i] %in% docs_tbl$rd_name)) {
      next
    }
    
    # Get information about the existing article doc in '/R'
    docs_list <- as.list(dplyr::filter(docs_tbl, rd_name == article_names[i]))
    
    # Get the index for the translated article doc
    idx <- which(article_names_all == docs_list$rd_name)
    
    roxygen_lines <- translation_lines[article_headers[idx]:article_footers[idx]]
    roxygen_lines <- roxygen_lines[grepl("^#'", roxygen_lines)]
    
    r_file_lines <- readLines(con = docs_list$file)
  
    r_file_lines_top <- r_file_lines[0:(docs_list$line_start - 1)]
    r_file_lines_bottom <- r_file_lines[(docs_list$line_end + 1):length(r_file_lines)]
    
    r_file_lines_replaced <-
      c(
        r_file_lines_top, roxygen_lines, r_file_lines_bottom
      )
  
    # Write the file anew
    write_r_file(lines = r_file_lines_replaced, file_path = docs_list$file)
    
    # Write message to console
    cat(
      paste0(
        "The `", article_names[i], "` article was replaced.\n"
      )
    )
  }
}
