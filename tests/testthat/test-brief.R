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

test_that("Batch tests: special validations", {
  
  agent <- create_agent(small_table)
  get_briefs <- function(x) x$validation_set$brief
  
  validation_fns <- all_validations_fns_vec()
  validation_fns <- setdiff(validation_fns, c("serially", "conjointly", "specially"))
  
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
        brief = "x"
      ) %>% 
      get_briefs()
    ,
    "x"
  )
  
  # Special: 
  expect_identical(
    agent %>% 
      specially(
        fn = function(x) { ... },
        preconditions = ~ . %>% dplyr::filter(a < 10),
        actions = action_levels(warn_at = 0.1, stop_at = 0.2), 
        label = "The `specially()` step.",
        active = FALSE,
        brief = "x"
      ) %>% 
      get_briefs()
    ,
    "x"
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
        brief = "x"
      ) %>% 
      get_briefs(),
    "x"
  )
  
  # Special: conjointly (with segments)
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
        brief = "x"
      ) %>% 
      get_briefs(),
    c("x", "x")
  )

})
