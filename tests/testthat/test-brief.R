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
