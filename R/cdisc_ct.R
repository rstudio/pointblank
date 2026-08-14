#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#

# Internal: path to bundled CT directory
.cdisc_ct_dir <- function() {
  system.file("conformance", "ct", package = "pointblank")
}

# Internal: return slugs for all bundled CT packages, sorted chronologically
cdisc_ct_available <- function() {
  ct_dir <- .cdisc_ct_dir()
  if (!nzchar(ct_dir) || !dir.exists(ct_dir)) {
    return(character(0))
  }
  files <- sort(list.files(ct_dir, pattern = "\\.json$"))
  tools::file_path_sans_ext(files)
}

# Internal: load one or more bundled CT packages
cdisc_ct_load <- function(packages) {

  rlang::check_installed(
    "jsonlite",
    "to load bundled CDISC controlled terminology."
  )

  codelists <- list()

  for (pkg in packages) {
    path <- file.path(.cdisc_ct_dir(), paste0(pkg, ".json"))
    if (!file.exists(path)) {
      rlang::abort(
        c(
          paste0("No bundled CT package '", pkg, "'."),
          i = paste0(
            "Available: ",
            paste(cdisc_ct_available(), collapse = ", "),
            "."
          )
        )
      )
    }
    data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    for (name in names(data$codelists)) {
      terms <- as.character(unlist(data$codelists[[name]], use.names = FALSE))
      codelists[[toupper(name)]] <- terms
    }
  }

  structure(
    list(codelists = codelists, packages = packages),
    class = "cdisc_ct"
  )
}

# Internal: load the most recent bundled CT package automatically
cdisc_ct_load_default <- function() {
  avail <- cdisc_ct_available()
  if (length(avail) == 0L) {
    return(
      structure(
        list(codelists = list(), packages = character(0)),
        class = "cdisc_ct"
      )
    )
  }
  cdisc_ct_load(avail[length(avail)])
}

# Internal: get the set of permitted values for a codelist, or NULL if unknown
cdisc_ct_get_codelist <- function(ct, name) {
  ct$codelists[[toupper(name)]]
}

#' @export
print.cdisc_ct <- function(x, ...) {
  cat(
    sprintf(
      "<cdisc_ct> packages=%s, n_codelists=%d\n",
      paste(x$packages, collapse = ", "),
      length(x$codelists)
    )
  )
  invisible(x)
}
