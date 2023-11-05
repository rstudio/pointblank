skip_on_covr()

test_that("col_vals_expr() extracts columns (moved from `test-interrogate_with_agent_segments.R`)", {
  expr_tbl <-
    dplyr::tibble(
      a = c(
        rep("group_1", 2), rep("group_2", 2), rep(NA_character_, 4)
      ),
      b = c(specifications$zip_codes[1:4], rep("2308", 4))
    )
  
  validation <-
    create_agent(tbl = expr_tbl) %>%
    col_vals_expr(
      expr = expr(grepl("[0-9]{5}", b)),
      segments = a ~ c("group_1", "group_2")) %>%
    interrogate()
  
  expect_equivalent(validation$validation_set$column %>% unlist(), rep("b", 2))
})
