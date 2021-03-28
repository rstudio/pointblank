#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


# nocov start
# nolint start

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
    "a__",
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
    "columns_expr",
    "col_element",
    "col_name",
    "col_num",
    "condition",
    "count",
    "Count",
    "country_alpha_3",
    "::cut_group::",
    "data_type",
    "DATA_TYPE",
    "desc",
    "duplicates",
    "extract",
    "eval_active",
    "eval_error",
    "eval_pts",
    "eval_sym",
    "eval_warning",
    "f_fail",
    "f_failed",
    "f_pass",
    "f_passed",
    "file_name",
    "Freq",
    "frequency",
    "Frequency",
    "head.ref",
    "i",
    "item",
    "i_o",
    "label",
    "::labels::",
    "merge_commit_sha",
    "n",
    "n_fail",
    "n_pass",
    "na_pass",
    "name_en",
    "N",
    "N_pts",
    "N_val",
    "notify",
    "number",
    ".panel_x",
    ".panel_y",
    "pb_is_good_",
    "pb_is_good_1_",
    "pb_is_good_2_",
    "pb_lagged_difference_",
    "pct",
    "precon",
    "preconditions",
    "q_1",
    "q_3",
    "%REGEXP%",
    "regexp_matches",
    "RLIKE",
    "rowid",
    "S",
    "schema",
    "sd",
    "set_element",
    "S_pts",
    "S_val",
    "sha1",
    "status_color",
    "step_id",
    "str_length__",
    "subd_name",
    "tbl_checked",
    "time_str",
    "total_pts",
    "type",
    "value",
    "values",
    "Var1",
    "Var2",
    "view",
    "W",
    "W_pts",
    "W_val",
    "warn",
    "x"
  )
)

.onLoad <- function(libname, pkgname, ...) {
  
  register_s3_method("knitr", "knit_print", "ptblank_agent")
  register_s3_method("knitr", "knit_print", "ptblank_informant")
  register_s3_method("knitr", "knit_print", "table_scan")
  
  if ("knitr" %in% loadedNamespaces()) {
    validate_rmd_setup()
  }
  
  setHook(
    packageEvent("knitr", "onLoad"),
    function(...) validate_rmd_setup()
  )
  
  invisible()
}

# nocov end
# nolint end
