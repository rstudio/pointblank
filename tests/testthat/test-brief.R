test_that("`brief` recycles when possible", {

  agent <- create_agent(small_table)
  
  expect_length(
    agent %>% 
      col_exists(c("a", "b"), brief = NULL) %>% 
      el("validation_set") %>% 
      el("brief"),
    2L
  )
  
  expect_equal(
    agent %>% 
      col_exists(c("a", "b"), brief = "one") %>% 
      el("validation_set") %>% 
      el("brief"),
    c("one", "one")
  )

  expect_equal(
    agent %>% 
      col_exists(c("a", "b"), brief = c("one", "two")) %>% 
      el("validation_set") %>% 
      el("brief"),
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
      col_exists("a", brief = c("one", "two")) %>% 
      el("validation_set") %>% 
      el("brief"),
    "must be length 1, not 2"
  )

})
