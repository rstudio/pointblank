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

# Internal: path to bundled conformance rules directory
.cdisc_rules_dir <- function() {
  system.file("conformance", "rules", package = "pointblank")
}

# Internal: build on-disk catalog file path for a standard/version pair
cdisc_catalog_path <- function(standard, version) {
  slug <- paste0(tolower(standard), "-", gsub("\\.", "-", version))
  file.path(.cdisc_rules_dir(), paste0(slug, ".json"))
}

# Internal: list (standard, version) pairs for all bundled catalogs
cdisc_rules_available <- function() {

  rlang::check_installed("jsonlite", "to load bundled CDISC rule catalogs.")

  rules_dir <- .cdisc_rules_dir()

  if (!nzchar(rules_dir) || !dir.exists(rules_dir)) {
    return(list())
  }

  files <- sort(list.files(rules_dir, pattern = "\\.json$", full.names = TRUE))

  pairs <- lapply(files, function(f) {
    tryCatch(
      {
        data <- jsonlite::fromJSON(f, simplifyVector = FALSE)
        std <- data$standard %||% ""
        ver <- data$version %||% ""
        if (nzchar(std) && nzchar(ver)) {
          list(standard = std, version = ver)
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
  })

  Filter(Negate(is.null), pairs)
}

# Internal: load rules for a standard/version, optionally filtered by rule_types
cdisc_load_rules <- function(standard, version, rule_types = NULL) {

  rlang::check_installed("jsonlite", "to load bundled CDISC rule catalogs.")

  path <- cdisc_catalog_path(standard, version)

  if (!file.exists(path)) {

    available <- cdisc_rules_available()
    avail_str <- if (length(available) > 0) {
      paste(
        vapply(available, function(p) paste(p$standard, p$version), character(1)),
        collapse = ", "
      )
    } else {
      "(none)"
    }

    rlang::abort(
      c(
        paste0("No bundled rule catalog for ", standard, " ", version, "."),
        i = paste0("Available: ", avail_str, ".")
      )
    )
  }

  data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rules <- lapply(data$rules, cdisc_rule_from_list)

  if (!is.null(rule_types)) {
    rules <- Filter(function(r) r$rule_type %in% rule_types, rules)
  }

  rules
}

# Internal: fill defaults for one rule entry (mirrors Python's NativeRule.from_dict)
cdisc_rule_from_list <- function(d) {
  list(
    core_id       = d$core_id,
    rule_type     = d$rule_type,
    executability = d$executability %||% "Fully Executable",
    sensitivity   = d$sensitivity %||% "Error",
    description   = d$description %||% "",
    authority     = d$authority %||% "CDISC",
    standards     = d$standards %||% list(),
    classes       = d$classes %||% list(),
    domains       = d$domains %||% list(),
    datasets      = d$datasets %||% list(),
    operations    = d$operations %||% list(),
    conditions    = d$conditions %||% list(),
    actions       = d$actions %||% list()
  )
}

# Internal: extract the message from a rule's actions
cdisc_rule_message <- function(rule) {
  msg <- rule$actions$params$message
  if (!is.null(msg) && nzchar(msg)) msg else rule$description
}

# Internal: catalog header metadata without loading the rules array
cdisc_catalog_metadata <- function(standard, version) {

  rlang::check_installed("jsonlite", "to load bundled CDISC rule catalogs.")

  path <- cdisc_catalog_path(standard, version)

  if (!file.exists(path)) {
    rlang::abort(paste0("No catalog for ", standard, " ", version, "."))
  }

  data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  data[setdiff(names(data), "rules")]
}
