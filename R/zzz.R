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
    "alpha_2",
    "alpha_3",
    "::all_na::",
    "assertion_type",
    "b",
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
    "info",
    "item",
    "i_o",
    "interrogation_notes",
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
    ".param.",
    "pb_is_good_",
    "pb_is_good_1_",
    "pb_is_good_2_",
    "pb_lagged_difference_",
    "pb_vin_001_",
    "pb_vin_001_w",
    "pb_vin_002_",
    "pb_vin_002_w",
    "pb_vin_003_",
    "pb_vin_003_w",
    "pb_vin_004_",
    "pb_vin_004_w",
    "pb_vin_005_",
    "pb_vin_005_w",
    "pb_vin_006_",
    "pb_vin_006_w",
    "pb_vin_007_",
    "pb_vin_007_w",
    "pb_vin_008_",
    "pb_vin_008_w",
    "pb_vin_009_",
    "pb_vin_009_w",
    "pb_vin_010_",
    "pb_vin_010_w",
    "pb_vin_011_",
    "pb_vin_011_w",
    "pb_vin_012_",
    "pb_vin_012_w",
    "pb_vin_013_",
    "pb_vin_013_w",
    "pb_vin_014_",
    "pb_vin_014_w",
    "pb_vin_015_",
    "pb_vin_015_w",
    "pb_vin_016_",
    "pb_vin_016_w",
    "pb_vin_017_",
    "pb_vin_017_w",
    "pb_vin_all_",
    "pb_vin_chk_",
    "pb_vin_mod_",
    "pb_vin_nch_",
    "pb_vin_sum_",
    "pb_vin_sum_uw",
    "pct",
    "postal_code_format",
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
    "seg_expr",
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
  register_s3_method("knitr", "knit_print", "ptblank_multiagent")
  register_s3_method("knitr", "knit_print", "ptblank_multiagent_report.long")
  register_s3_method("knitr", "knit_print", "ptblank_tbl_scan")
  register_s3_method("knitr", "knit_print", "x_list_i")
  register_s3_method("knitr", "knit_print", "x_list_n")
  register_s3_method("knitr", "knit_print", "tbl_store")
  register_s3_method("knitr", "knit_print", "action_levels")
  
  invisible()
}

# nocov end
# nolint end
