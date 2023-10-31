tbl <- data.frame(x = 1:2, y = 1:2, nonunique = "A")
exist_col <- "y"
nonunique_col <- "nonunique"
nonexist_col <- "z"

test_that("Backwards compatibility with `vars()`", {
  
  # Bare symbol selects column(s)
  expect_success(expect_rows_distinct(tbl, vars(x)))
  expect_success(expect_rows_distinct(tbl, vars(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, vars(nonunique)))
  
  # Bare character selects column(s)
  expect_success(expect_rows_distinct(tbl, vars("x")))
  expect_success(expect_rows_distinct(tbl, vars("x", "nonunique")))
  expect_failure(expect_rows_distinct(tbl, vars("nonunique")))
  
  # Bang-bang in-lines value
  expect_success(expect_rows_distinct(tbl, vars(!!exist_col)))
  expect_failure(expect_rows_distinct(tbl, vars(!!nonunique_col)))
  
  # `vars()` wrapping tidyselect expressions is redundant but continues to work
  expect_success(expect_rows_distinct(tbl, vars(all_of("x"))))
  
  # `vars()` selection of 0-columns errors *only* in non-validation-planning contexts
  expect_error(rows_distinct(tbl, vars("z")))
  expect_error(expect_rows_distinct(tbl, vars("z")))
  expect_error(test_rows_distinct(tbl, vars("z")))
  expect_no_error(tbl %>% create_agent() %>% rows_distinct(vars("z")))
  expect_no_error(tbl %>% create_agent() %>% rows_distinct(vars("z")) %>% interrogate())
  
})

test_that("Full range of tidyselect features available in column selection", {
  
  # Single symbol
  expect_success(expect_rows_distinct(tbl, x))
  expect_failure(expect_rows_distinct(tbl, nonunique))
  
  # Preferred {tidyselect}-style `c()` syntax
  expect_success(expect_rows_distinct(tbl, c(x)))
  expect_success(expect_rows_distinct(tbl, c(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, c(nonunique)))
  
  # {tidyselect} functions
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of("x")))
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of(c("x", "nonunique"))))
  expect_failure(expect_rows_distinct(tbl, tidyselect::all_of("nonunique")))
  
  # NEW: {tidyselect} integer indexing
  expect_success(expect_rows_distinct(tbl, 1))
  expect_success(expect_rows_distinct(tbl, c(1, 3)))
  expect_failure(expect_rows_distinct(tbl, 3))
  
  # NEW: {tidyselect} negative indexing
  expect_success(expect_rows_distinct(tbl, -(2:3)))
  expect_success(expect_rows_distinct(tbl, -2))
  expect_failure(expect_rows_distinct(tbl, -(1:2)))
  
  # NEW: {tidyselect} `where()` predicate:
  expect_success(expect_rows_distinct(tbl, !tidyselect::where(is.character)))
  expect_success(expect_rows_distinct(tbl, tidyselect::where(is.numeric)))
  expect_failure(expect_rows_distinct(tbl, tidyselect::where(is.character)))
  
  # NEW: {tidyselect} functions in complex expressions
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::all_of(exist_col))))
  expect_error(expect_rows_distinct(tbl, c(x, tidyselect::all_of(nonexist_col))))
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::any_of(nonexist_col))))
  
  # Supplying a character vector variable still works, but signals deprecation:
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_success(expect_warning(
    expect_rows_distinct(tbl, exist_col),
    "Using an external vector in selections was deprecated in tidyselect 1.1.0."
  ))
  
})

test_that("'NULL = select everything' behavior in rows_*() validation functions", {

  # For `rows_*()` functions specifically, empty/NULL = "select everything" behavior:
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2)))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2)))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1))))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA))))
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2), columns = NULL))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2), columns = NULL))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1)), columns = NULL))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA)), columns = NULL))
  
  # Report shows all column names with empty `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct() %>% 
      rows_complete() %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
  
  # Report shows all column names with explicit NULL `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct(columns = NULL) %>% 
      rows_complete(columns = NULL) %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
    
})

test_that("tidyselect coverage for `col_exists()`", {
  
  # Reprex from (#433)
  df <- tibble::tibble(
    id.x = 1:3,
    id.y = 1:3,
    stuff = 1:3
  )
  expect_success({
    df %>% 
      expect_col_exists(
        columns = vars(ends_with(".x"))
      )
  })
  expect_equal({
    df %>% 
      col_exists(
        columns = vars(ends_with(".x"))
      )
  }, df)
  
  # Multiple column selection produces multiple steps
  expect_no_error({
    df_interrogated <- df %>% 
      create_agent() %>% 
      col_exists(starts_with("id")) %>% 
      interrogate()
  })
  expect_equal(nrow(df_interrogated$validation_set), 2L)
  
})

test_that("error/failure patterns for `col_exists`", {
  
  # Selecting non-existent columns signals failure
  expect_error(expect_failure({
    small_table %>% 
      col_exists("z")
  }))
  expect_failure({
    small_table %>% 
      expect_col_exists("z")
  })
  
  # 0-column tidyselect selection signals failure
  expect_error(expect_failure({
    small_table %>% 
      col_exists(starts_with("z"))
  }))
  expect_failure({
    small_table %>% 
      expect_col_exists("z")
  })
  
  # Unrelated evaluation errors should be chained and rethrown
  expect_error({
    small_table %>% 
      col_exists(stop("Error!"))
  }, "Error!")
  expect_error({
    small_table %>% 
      expect_col_exists(stop("Error!"))
  }, "Error!")
  expect_error({
    small_table %>% 
      test_col_exists(stop("Error!"))
  }, "Error!")
  
  # Test should return FALSE for 0-column and non-existent column
  expect_false({
    small_table %>% 
      test_col_exists("z")
  })
  expect_false({
    small_table %>% 
      test_col_exists("z")
  })
  
  # No failure/error during validation
  expect_no_error({
    agent_nonexist_col <- create_agent(small_table) %>% 
      col_exists("z") %>% 
      interrogate()
  })
  expect_false(all_passed(agent_nonexist_col))
  expect_no_error({
    agent_tidyselect_0col <- create_agent(small_table) %>% 
      col_exists(starts_with("z")) %>% 
      interrogate()
  })
  expect_false(all_passed(agent_tidyselect_0col))
  
})

test_that("c()-expr works for serially", {
  
  # Example from `serially()` docs
  tbl <-
    dplyr::tibble(
      a = c(5, 2, 6),
      b = c(6, 4, 9),
      c = c(1, 2, 3)
    )
  agent_1 <-
    create_agent(tbl = tbl) %>%
    serially(
      ~ test_col_is_numeric(., columns = vars(a, b)),
      ~ test_col_vals_not_null(., columns = vars(a, b)),
      ~ col_vals_gt(., columns = vars(b), value = vars(a))
    ) %>%
    interrogate()
  expect_no_error({
    agent_1_c <-
      create_agent(tbl = tbl) %>%
      serially(
        ~ test_col_is_numeric(., columns = c(a, b)),
        ~ test_col_vals_not_null(., columns = c(a, b)),
        ~ col_vals_gt(., columns = b, value = vars(a))
      ) %>%
      interrogate()
  })
  expect_identical(
    get_agent_report(agent_1, display_table = FALSE)$n_pass,
    get_agent_report(agent_1_c, display_table = FALSE)$n_pass
  )
  
})
