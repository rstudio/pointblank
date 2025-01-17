test_that("`brief` recycles when possible", {

  agent <- create_agent(small_table)
  get_briefs <- function(x) x$validation_set$brief

  expect_length(
    agent %>%
      col_exists(c("a", "b"), brief = NULL) %>%
      get_briefs(),
    2L
  )

  expect_equal(
    agent %>%
      col_exists(c("a", "b"), brief = "one") %>%
      get_briefs(),
    c("one", "one")
  )

  expect_equal(
    agent %>%
      col_exists(c("a", "b"), brief = c("one", "two")) %>%
      get_briefs(),
    c("one", "two")
  )

  expect_error(
    agent %>%
      col_vals_equal(
        c("a", "b"), 0,
        segments = vars(f),
        brief = c("one", "two", "three")
    ),
    "must be length 1 or 6, not 3"
  )

  expect_error(
    agent %>%
      col_exists("a", brief = c("one", "two")),
    "must be length 1, not 2"
  )

})

test_that("Briefs batch tests", {

  validation_fns <- all_validations_fns_vec()
  validation_fns <- setdiff(validation_fns, c("serially", "conjointly", "specially"))
  validation_fns <- mget(validation_fns, asNamespace("pointblank"))
  validation_fn_args <- sapply(validation_fns, function(x) {
    paste(intersect(names(formals(x)), c("segments", "columns")), collapse = "+")
  })
  validation_fn_args <- validation_fn_args[validation_fn_args != ""]
  validation_fn_args[grepl(x = names(validation_fn_args), "^rows_")] <- "rows_*"

  test_multi_briefs <- function(f, ...) {
    x <- create_agent(small_table)
    test <- validation_fn_args[[f]]
    f <- get(f, asNamespace("pointblank"))
    cols <- c("a", "b")
    segs <- c("high", "low", "mid")
    switch(test,
      "columns" = {
        out <- cols
        x <- x %>%
          f(columns = c("a", "b"), ..., brief = cols) # 2-steps
      },
      "segments" = {
        out <- segs
        x <- x %>%
          f(segments = vars(f), ..., brief = segs) # 3-steps
      }, "columns+segments" = {
        out <- as.vector(outer(segs, cols, function(s, c) paste0(c, ":", s)))
        x <- x %>%
          f(columns = c("a", "b"), segments = vars(f), ..., brief = out) # 6-steps
      }, "rows_*" = {
        out <- segs
        x <- x %>%
          f(columns = c("a", "b"), segments = vars(f), ..., brief = segs) # 3-steps
      }
    )
    expect_identical(out, x$validation_set$brief)
  }

  # TODO: The rest of this + fix iteration over `brief = brief` in validation functions
  test_multi_briefs("col_vals_lt", value = 5)
  # agent %>% col_vals_lt({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_lte({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_equal({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_not_equal({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_gte({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_gt({{ select_expr }}, value = 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_between({{ select_expr }}, 2, 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_not_between({{ select_expr }}, 2, 5) %>% check_behaviors(expr_name)
  # agent %>% col_vals_in_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
  # agent %>% col_vals_not_in_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
  # agent %>% col_vals_make_set({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
  # agent %>% col_vals_make_subset({{ select_expr }}, c(2, 5)) %>% check_behaviors(expr_name)
  # agent %>% col_vals_null({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_vals_not_null({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_vals_increasing({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_vals_decreasing({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_vals_regex({{ select_expr }}, regex = "abc") %>% check_behaviors(expr_name)
  # agent %>% col_vals_within_spec({{ select_expr }}, spec = "email") %>% check_behaviors(expr_name)
  # agent %>% col_is_character({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_numeric({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_integer({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_logical({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_date({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_posix({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_is_factor({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% rows_distinct({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% rows_complete({{ select_expr }}) %>% check_behaviors(expr_name)
  # agent %>% col_exists({{ select_expr }}) %>% check_behaviors(expr_name)

})

test_that("Briefs batch tests: special validations", {

  agent <- create_agent(small_table)
  get_briefs <- function(x) x$validation_set$brief

  # Special: serially
  expect_identical(
    agent %>%
      serially(
        ~ test_col_vals_lt(., columns = a, value = 8),
        ~ test_col_vals_gt(., columns = c, value = vars(a)),
        ~ col_vals_not_null(., columns = b),
        preconditions = ~ . %>% dplyr::filter(a < 10),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `serially()` step.",
        active = FALSE,
        brief = "custom brief"
      ) %>%
      get_briefs()
    ,
    "custom brief"
  )

  # Special: specially
  expect_identical(
    agent %>%
      specially(
        fn = function(x) { ... },
        preconditions = ~ . %>% dplyr::filter(a < 10),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `specially()` step.",
        active = FALSE,
        brief = "custom brief"
      ) %>%
      get_briefs()
    ,
    "custom brief"
  )

  # Special: conjointly (no segments)
  expect_identical(
    agent %>%
      conjointly(
        ~ col_vals_lt(., columns = a, value = 8),
        ~ col_vals_gt(., columns = c, value = vars(a)),
        ~ col_vals_not_null(., columns = b),
        preconditions = ~ . %>% dplyr::filter(a < 10),
        # segments = b ~ c("group_1", "group_2"),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `conjointly()` step.",
        active = FALSE,
        brief = "custom brief"
      ) %>%
      get_briefs(),
    "custom brief"
  )

  # Special: conjointly (expanded with segments)
  expect_identical(
    agent %>%
      conjointly(
        ~ col_vals_lt(., columns = a, value = 8),
        ~ col_vals_gt(., columns = c, value = vars(a)),
        ~ col_vals_not_null(., columns = b),
        preconditions = ~ . %>% dplyr::filter(a < 10),
        segments = b ~ c("group_1", "group_2"),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `conjointly()` step.",
        active = FALSE,
        brief = "custom brief constant"
      ) %>%
      conjointly(
        ~ col_vals_lt(., columns = a, value = 8),
        ~ col_vals_gt(., columns = c, value = vars(a)),
        ~ col_vals_not_null(., columns = b),
        preconditions = ~ . %>% dplyr::filter(a < 10),
        segments = b ~ c("group_1", "group_2"),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2),
        label = "The `conjointly()` step.",
        active = FALSE,
        brief = c("custom brief multi1", "custom brief multi2")
      ) %>%
      get_briefs(),
    c("custom brief constant", "custom brief constant",
      "custom brief multi1", "custom brief multi2")
  )

})
