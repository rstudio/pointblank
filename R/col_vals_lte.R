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


#' @rdname col_vals_le
#' @export
col_vals_lte <- function(
    x,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    segments = NULL,
    actions = NULL,
    step_id = NULL,
    label = NULL,
    brief = NULL,
    active = TRUE
) {

  rlang::warn(
    c(
      "`col_vals_lte()` is deprecated.",
      "i" = "Please use `col_vals_le()` instead."
    ),
    class = "pointblank_soft_deprecated",
    .frequency = "once",
    .frequency_id = "pointblank-col_vals_lte-deprecation"
  )

  col_vals_le(
    x = x,
    columns = {{ columns }},
    value = value,
    na_pass = na_pass,
    preconditions = preconditions,
    segments = segments,
    actions = actions,
    step_id = step_id,
    label = label,
    brief = brief,
    active = active
  )
}

#' @rdname col_vals_le
#' @export
expect_col_vals_lte <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  rlang::warn(
    c(
      "`expect_col_vals_lte()` is deprecated.",
      "i" = "Please use `expect_col_vals_le()` instead."
    ),
    class = "pointblank_soft_deprecated",
    .frequency = "once",
    .frequency_id = "pointblank-expect_col_vals_lte-deprecation"
  )

  expect_col_vals_le(
    object = object,
    columns = {{ columns }},
    value = {{ value }},
    na_pass = na_pass,
    preconditions = {{ preconditions }},
    threshold = threshold
  )
}

#' @rdname col_vals_le
#' @export
test_col_vals_lte <- function(
    object,
    columns,
    value,
    na_pass = FALSE,
    preconditions = NULL,
    threshold = 1
) {

  rlang::warn(
    c(
      "`test_col_vals_lte()` is deprecated.",
      "i" = "Please use `test_col_vals_le()` instead."
    ),
    class = "pointblank_soft_deprecated",
    .frequency = "once",
    .frequency_id = "pointblank-test_col_vals_lte-deprecation"
  )

  test_col_vals_le(
    object = object,
    columns = {{ columns }},
    value = {{ value }},
    na_pass = na_pass,
    preconditions = {{ preconditions }},
    threshold = threshold
  )
}
