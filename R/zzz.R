#nocov start

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

utils::globalVariables(
  c(
    ".",
    "__diff__",
    "__mean__",
    "__var__",
    "a",
    "action",
    "actions",
    "active",
    "agent",
    "::all_na::",
    "assertion_type",
    "bin_num",
    "brief",
    "color",
    "column",
    "columns",
    "col_name",
    "condition",
    "count",
    "Count",
    "::cut_group::",
    "data_type",
    "DATA_TYPE",
    "desc",
    "duplicates",
    "extract",
    "eval_error",
    "eval_pts",
    "eval_sym",
    "eval_warning",
    "f_fail",
    "f_pass",
    "f_passed",
    "Freq",
    "frequency",
    "Frequency",
    "i",
    "item",
    "label",
    "::labels::",
    "n",
    "n_fail",
    "n_pass",
    "na_pass",
    "N",
    "N_pts",
    "N_val",
    "notify",
    ".panel_x",
    ".panel_y",
    "pb_is_good_",
    "pct",
    "precon",
    "preconditions",
    "q_1",
    "q_3",
    "%REGEXP%",
    "RLIKE",
    "S",
    "schema",
    "sd",
    "S_pts",
    "S_val",
    "status_color",
    "tbl_checked",
    "total_pts",
    "type",
    "value",
    "values",
    "Var1",
    "Var2",
    "W",
    "W_pts",
    "W_val",
    "warn",
    "x"
  )
)

.onLoad <- function(libname, pkgname, ...) {
  
  register_s3_method("knitr", "knit_print", "ptblank_agent")
  
  if ("knitr" %in% loadedNamespaces()) {
    validate_rmd_setup()
  }
  
  setHook(
    packageEvent("knitr", "onLoad"),
    function(...) validate_rmd_setup()
  )
  
  invisible()
}

#nocov end
