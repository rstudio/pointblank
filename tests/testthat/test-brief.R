test_that("`brief` recycles when possible", {

  agent <- create_agent(small_table)
  get_briefs <- function(x) x$validation_set$brief

  expect_length(
    agent %>%
      col_exists(c("a", "b"), brief = NULL) %>%
      get_briefs(),
    2L
  )

  expect_identical(
    agent %>%
      col_exists(c("a", "b"), brief = NA) %>%
      get_briefs(),
    c(NA_character_, NA_character_)
  )

  expect_identical(
    agent %>%
      col_exists(c("a", "b"), brief = c("x", NA)) %>%
      get_briefs(),
    c("x", NA_character_)
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
  table(validation_fn_args)

  agent <- create_agent(small_table)
  test_multi_briefs <- function(f, ...) {
    test <- validation_fn_args[[f]]
    f <- validation_fns[[f]]
    cols <- c("a", "b")
    segs <- c("high", "low", "mid")
    switch(test,
      "columns" = {
        out <- cols
        agent <- agent %>%
          f(columns = c("a", "b"), ..., brief = cols) # 2-steps
      },
      "segments" = {
        out <- segs
        agent <- agent %>%
          f(segments = vars(f), ..., brief = segs) # 3-steps
      }, "columns+segments" = {
        out <- as.vector(outer(segs, cols, function(s, c) paste0(c, ":", s)))
        agent <- agent %>%
          f(columns = c("a", "b"), segments = vars(f), ..., brief = out) # 6-steps
      }, "rows_*" = {
        out <- segs
        agent <- agent %>%
          f(columns = c("a", "b"), segments = vars(f), ..., brief = segs) # 3-steps
      }
    )
    if (rlang::is_interactive()) print(agent)
    expect_identical(out, agent$validation_set$brief)
  }

  # columns
  test_multi_briefs("col_is_character")
  test_multi_briefs("col_is_numeric")
  test_multi_briefs("col_is_integer")
  test_multi_briefs("col_is_logical")
  test_multi_briefs("col_is_date")
  test_multi_briefs("col_is_posix")
  test_multi_briefs("col_is_factor")
  test_multi_briefs("col_exists")

  # columns + segments
  test_multi_briefs("col_vals_lt", value = 5)
  test_multi_briefs("col_vals_lte", value = 5)
  test_multi_briefs("col_vals_equal", value = 5)
  test_multi_briefs("col_vals_not_equal", value = 5)
  test_multi_briefs("col_vals_gte", value = 5)
  test_multi_briefs("col_vals_gt", value = 5)
  test_multi_briefs("col_vals_between", left = 2, right = 5)
  test_multi_briefs("col_vals_not_between", left = 2, right = 5)
  test_multi_briefs("col_vals_in_set", set = c(2, 5))
  test_multi_briefs("col_vals_not_in_set", set = c(2, 5))
  test_multi_briefs("col_vals_make_set", set = c(2, 5))
  test_multi_briefs("col_vals_make_subset", set = c(2, 5))
  test_multi_briefs("col_vals_null")
  test_multi_briefs("col_vals_not_null")
  test_multi_briefs("col_vals_increasing")
  test_multi_briefs("col_vals_decreasing")
  test_multi_briefs("col_vals_regex", regex = "abc")
  test_multi_briefs("col_vals_within_spec", spec = "email")

  # segments
  test_multi_briefs("col_vals_expr", expr = expr(a %% 1 == 0))
  test_multi_briefs("row_count_match", count = small_table)
  test_multi_briefs("tbl_match", tbl_compare = small_table)

  # rows_*
  test_multi_briefs("rows_distinct")
  test_multi_briefs("rows_complete")

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

test_that("brief_cls hosts information about class", {

  chr <- "chr"

  autobrief <- NULL

  foobar <- structure("foobar", class = "foobar")

  verbatim_yaml <- structure(
    yaml::as.yaml(list(author = "June", id = 123)),
    class = "verbatim"
  )

  get_brief_cls <- function(brief1, brief2) {
    create_agent(~ small_table) %>%
      col_vals_gt(c(a, c), 5, brief = brief1) %>%
      col_vals_gt(c(a, c), 5, brief = brief2) %>%
      el("validation_set") %>%
      el("brief_cls")
  }

  get_brief_cls(chr, chr)
  get_brief_cls(chr, autobrief)
  get_brief_cls(chr, foobar)


})
