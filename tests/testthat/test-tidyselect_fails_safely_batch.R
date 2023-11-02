agent <- create_agent(tbl = small_table[, c("a", "b", "c")])
mixed_cols <- c("a", "z")

# Column selection expressions to test
select_exprs <- rlang::quos(
  empty            = ,
  null             = NULL,
  exists           = a,
  nonexistent      = z,
  mixed            = c(a, z),
  mixed_all        = all_of(mixed_cols),
  mixed_any        = any_of(mixed_cols),
  empty_tidyselect = starts_with("z")
)

# Properties of `$validation_set` to test
get_behaviors <- function(vs) {
  list(
    n_steps = nrow(vs),
    column = unlist(vs$column),
    eval_error = any(vs$eval_error)
  )
}

# Dimnames for `expected_behaviors` matrix
behaviors_dimnames <- list(
  property = c("n_steps", "column", "eval_error"),
  col_select = names(select_exprs)
)

test_that("`col_*()`s show expected column selection failure/success behavior", {

  expected_behaviors <- matrix(
    c(
      list(1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L),
      list(NA_character_, NA_character_, "a", "z", c("a", "z"), c("a", "z"), "a", NA_character_),
      list(T, T, F, T, T, T, F, T)
    ),
    ncol = length(select_exprs), byrow = TRUE, dimnames = behaviors_dimnames
  )
  expected_behaviors
  # |           |empty/null |exists |nonexistent |mixed       |mixed_all   |mixed_any |empty_tidyselect |
  # |:----------|:----------|:------|:-----------|:-----------|:-----------|:---------|:----------------|
  # |n_steps    |1          |1      |1           |2           |2           |1         |1                |
  # |column     |NA         |a      |z           |c("a", "z") |c("a", "z") |a         |NA               |
  # |eval_error |TRUE       |FALSE  |TRUE        |TRUE        |TRUE        |FALSE     |TRUE             |
  
  check_behaviors <- function(agent, expr_name) {
    x <- suppressMessages(interrogate(agent))
    behaviors <- get_behaviors(x$validation_set)
    expect_identical(behaviors[["n_steps"]], expected_behaviors[["n_steps", expr_name]])
    expect_identical(behaviors[["column"]], expected_behaviors[["column", expr_name]])
    expect_identical(behaviors[["eval_error"]], expected_behaviors[["eval_error", expr_name]])
  }

  for (expr_name in names(select_exprs)) {

    select_expr <- select_exprs[[expr_name]]
    
    agent %>% col_vals_lt({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_lte({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_equal({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_not_equal({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_gte({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_gt({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_between({{ select_expr }}, 2, 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_not_between({{ select_expr }}, 2, 5) %>% check_behaviors(expr_name)
    agent %>% col_vals_in_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
    agent %>% col_vals_not_in_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
    agent %>% col_vals_make_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
    agent %>% col_vals_make_subset({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
    agent %>% col_vals_null({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_vals_not_null({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_vals_increasing({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_vals_decreasing({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_vals_regex({{ select_expr }}, regex = "abc") %>% check_behaviors(expr_name)
    agent %>% col_vals_within_spec({{ select_expr }}, spec = "email") %>% check_behaviors(expr_name)
    agent %>% col_is_character({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_numeric({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_integer({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_logical({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_date({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_posix({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% col_is_factor({{ select_expr }}) %>% check_behaviors(expr_name)
  }
  
})

test_that("`rows_*()`s show expected column selection failure/success behavior", {
  
  all_cols <- toString(colnames(agent$tbl))
  
  expected_behaviors <- matrix(
    c(
      list(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
      list(all_cols, all_cols, "a", "z", "a, z", "a, z", "a", NA_character_),
      list(F, F, F, T, T, T, F, T)
    ),
    ncol = length(select_exprs), byrow = TRUE, dimnames = behaviors_dimnames
  )
  expected_behaviors
  # |           |empty/null |exists |nonexistent |mixed |mixed_all |mixed_any |empty_tidyselect |
  # |:----------|:----------|:------|:-----------|:-----|:---------|:---------|:----------------|
  # |n_steps    |1          |1      |1           |1     |1         |1         |1                |
  # |column     |a, b, c    |a      |z           |a, z  |a, z      |a         |NA               |
  # |eval_error |FALSE      |FALSE  |TRUE        |TRUE  |TRUE      |FALSE     |TRUE             |
  
  check_behaviors <- function(agent, expr_name) {
    x <- suppressMessages(interrogate(agent))
    behaviors <- get_behaviors(x$validation_set)
    expect_identical(behaviors[["n_steps"]], expected_behaviors[["n_steps", expr_name]])
    expect_identical(behaviors[["column"]], expected_behaviors[["column", expr_name]])
    expect_identical(behaviors[["eval_error"]], expected_behaviors[["eval_error", expr_name]])
  }
  
  for (expr_name in names(select_exprs)) {
    
    select_expr <- select_exprs[[expr_name]]
    
    agent %>% rows_distinct({{ select_expr }}) %>% check_behaviors(expr_name)
    agent %>% rows_complete({{ select_expr }}) %>% check_behaviors(expr_name)
    
  }
  
})

test_that("`col_exists()`s show expected column selection failure/success behavior", {
  
  expected_behaviors <- matrix(
    c(
      list(1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L),
      list(NA_character_, NA_character_, "a", "z", c("a", "z"), c("a", "z"), "a", NA_character_),
      list(T, T, F, F, F, F, F, F)
    ),
    ncol = length(select_exprs), byrow = TRUE, dimnames = behaviors_dimnames
  )
  expected_behaviors
  # |           |empty/null |exists |nonexistent |mixed       |mixed_all   |mixed_any |empty_tidyselect |
  # |:----------|:----------|:------|:-----------|:-----------|:-----------|:---------|:----------------|
  # |n_steps    |1          |1      |1           |2           |2           |1         |1                |
  # |column     |NA         |a      |z           |c("a", "z") |c("a", "z") |a         |NA               |
  # |eval_error |TRUE       |FALSE  |FALSE       |FALSE       |FALSE       |FALSE     |FALSE            |
  
  check_behaviors <- function(agent, expr_name) {
    x <- suppressMessages(interrogate(agent))
    behaviors <- get_behaviors(x$validation_set)
    expect_identical(behaviors[["n_steps"]], expected_behaviors[["n_steps", expr_name]])
    expect_identical(behaviors[["column"]], expected_behaviors[["column", expr_name]])
    expect_identical(behaviors[["eval_error"]], expected_behaviors[["eval_error", expr_name]])
  }
  
  for (expr_name in names(select_exprs)) {
    
    select_expr <- select_exprs[[expr_name]]
    
    agent %>% col_exists({{ select_expr }}) %>% check_behaviors(expr_name)
    
  }
  
})

test_that("Genuine evaluation errors are rethrown immediately (tested on a sample)", {
  
  errs <- rlang::quos(
    "Oh no!" = stop("Oh no!"),
    "not found" = all_of(I_dont_exist)
  )
  simple_err1 <- rlang::quo(stop("Oh no!"))
  simple_err2 <- rlang::quo(all_of(I_dont_exist))
  
  for (err_regex in names(errs)) {
    
    err_expr <- errs[[err_regex]]
  
    expect_error(agent %>% col_vals_between({{ err_expr }}, 2, 5), err_regex)
    expect_error(agent %>% rows_distinct({{ err_expr }}), err_regex)
    expect_error(agent %>% col_exists({{ err_expr }}), err_regex)
  
  }
  
})
