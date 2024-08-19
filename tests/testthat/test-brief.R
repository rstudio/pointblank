test_that("`brief` recycles when possible", {

  expect_equal(
    create_agent(small_table) %>% 
      col_exists(c("a", "b"), brief = "one") %>% 
      el("validation_set") %>% 
      el("brief"),
    c("one", "one")
  )

  expect_equal(
    create_agent(small_table) %>% 
      col_exists(c("a", "b"), brief = c("one", "two")) %>% 
      el("validation_set") %>% 
      el("brief"),
    c("one", "two")
  )

  expect_error(
    create_agent(small_table) %>% 
      col_vals_equal(
        c("a", "b"), 0,
        segments = vars(f),
        brief = c("one", "two")
    ),
    "must be length 1 or 6, not 2"
  )

})
