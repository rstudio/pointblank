test_that("label supports glue syntax for {.seg_val} {.seg_col} {.step} {.col}", {

  # Reprex from (#451) for {.seg_val}
  agent1 <- small_table %>%
    create_agent() %>%
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "The `col_vals_lt()` step for group '{.seg_val}'"
    ) %>%
    interrogate()
  expect_identical(
    gsub(".*'(high|low|mid)'.*", "\\1", agent1$validation_set$label),
    c("high", "low", "mid")
  )

  # {.seg_col}
  agent2 <- small_table %>%
    create_agent() %>%
    col_vals_lt(
      c, 8,
      segments = vars(e, f),
      label = "The `col_vals_lt()` step for group '{.seg_val}' from col '{.seg_col}'"
    ) %>%
    interrogate()
  expect_identical(
    gsub(".*'(TRUE|FALSE|high|low|mid)'.*", "\\1", agent2$validation_set$label),
    c("TRUE", "FALSE", "high", "low", "mid")
  )
  expect_identical(
    gsub(".*'(e|f)'.*", "\\1", agent2$validation_set$label),
    c("e", "e", "f", "f", "f")
  )

  # {.step}
  agent3 <- small_table %>%
    create_agent() %>%
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "{.step}"
    ) %>%
    interrogate()
  expect_true(all(agent3$validation_set$label == "col_vals_lt"))

  # {.col}
  agent4 <- small_table %>%
    create_agent() %>%
    col_vals_lt(
      columns = matches("^[acd]$"),
      value = 8,
      label = "{.col}"
    ) %>%
    interrogate()
  expect_identical(agent4$validation_set$label, c("a", "c", "d"))

  # Only those internal values are available inside the glue mask
  agent_all <- create_agent(small_table) %>%
    col_vals_lt(
      c, 8,
      label = "{toString(sort(ls(all.names = TRUE)))}"
    )
  expect_identical(agent_all$validation_set$label, c(".col, .seg_col, .seg_val, .step"))

})

test_that("glue scope doesn't expose internal variables", {

  # Ex: should not be able to access `columns` local variable in `col_vals_lt()`
  expect_error(create_agent(small_table) %>% col_vals_lt(c, 8, label = "{columns}"))
  # Ex: should not be able to access `i` local variable in `create_validation_step()`
  expect_error(create_agent(small_table) %>% col_vals_lt(c, 8, label = "{i}"))

  # Should be able to access global vars/fns
  expect_equal(
    create_agent(small_table) %>%
      col_vals_lt(c, 8, label = "{match(.col, letters)}") %>%
      {.$validation_set$label},
    "3"
  )

})

test_that("glue env searches from the caller env of the validation function", {

  to_upper <- function(x) stop("Oh no!")

  expect_error(
    create_agent(small_table) %>%
      col_vals_lt(c, 8, label = "{to_upper(.col)}") %>%
      {.$validation_set$label},
    "Oh no!"
  )

  expect_equal(
    local({
      to_upper <- function(x) toupper(x)
      create_agent(small_table) %>%
        col_vals_lt(c, 8, label = "{to_upper(.col)}") %>%
        {.$validation_set$label}
    }),
    "C"
  )

})

test_that("materialized multi-length glue labels make the yaml roundtrip", {

  agent_pre <- create_agent(~ small_table) %>%
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "The `col_vals_lt()` step for group '{.seg_val}'"
    )
  # yaml_agent_string(agent_pre, expanded = FALSE)

  agent_yaml <- tempfile()
  yaml_write(agent_pre, expanded = FALSE, filename = agent_yaml)

  agent_post <- yaml_read_agent(agent_yaml)
  # yaml_agent_string(agent_post, expanded = FALSE)

  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = FALSE),
    as_agent_yaml_list(agent_post, expanded = FALSE)
  )
  expect_identical(
    agent_pre %>% interrogate() %>% get_agent_report(display_table = FALSE),
    agent_post %>% interrogate() %>% get_agent_report(display_table = FALSE)
  )

})


test_that("multi-length label collapses when possible in yaml representation", {

  agent_pre <- create_agent(~ small_table) %>%
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = "{nchar(.seg_val) * 0}"
    )

  expect_identical(
    as_agent_yaml_list(agent_pre, expanded = FALSE)$steps[[1]]$col_vals_lt$label,
    c("0")
  )

})

test_that("glue syntax works for many segments, many columns", {

  agent <- create_agent(~ small_table) %>%
    col_vals_lt(
      columns = vars(a, c),
      value = 8,
      segments = f ~ c("high", "low"),
      label = "{.col},{.seg_val}"
    )
  expect_identical(
    strsplit(agent$validation_set$label, ","),
    list(
      c("a", "high"),
      c("a", "low"),
      c("c", "high"),
      c("c", "low")
    )
  )

})

test_that("glue syntax works for custom vector of labels", {

  # Custom labels show up in order
  many_labels <- strsplit("it's a feature not a bug", " ")[[1]]
  agent_many_labels <- create_agent(~ small_table) %>%
    col_vals_lt(
      columns = vars(a, c),
      value = 8,
      segments = vars(f),
      label = paste(many_labels, "({.col}, {.seg_val})")
    )
  many_labels_out <- agent_many_labels$validation_set$label
  # Loose test on set equality
  expect_setequal(gsub(" \\(.*\\)", "", many_labels_out), many_labels)
  # Stricter test on order
  expect_identical(gsub(" \\(.*\\)", "", many_labels_out), many_labels)
  # `resolve_label()` fills matrix by row bc validation functions iterate by row
  expect_identical(
    pointblank:::resolve_label(many_labels, c("a", "c"), unique(small_table$f)),
    matrix(many_labels, nrow = 2, ncol = 3, byrow = TRUE)
  )

  # Labels show up in the order supplied, for multi-column * multi-segment step
  agent_many_many_labels <- create_agent(~ small_table) %>%
    col_vals_lt(
      columns = vars(a, c),
      value = 8,
      segments = vars(f, e),
      label = 1:10
    )
  expect_identical(agent_many_many_labels$validation_set$label, as.character(1:10))
  # Order preserved in the yaml round trip
  agent_yaml <- tempfile()
  yaml_write(agent_many_many_labels, expanded = FALSE, filename = agent_yaml)
  agent_many_many_labels2 <- yaml_read_agent(agent_yaml)
  expect_identical(
    as_agent_yaml_list(agent_many_many_labels2, expanded = FALSE),
    as_agent_yaml_list(agent_many_many_labels2, expanded = FALSE)
  )

  # Errors on length mismatch
  expect_error({
    create_agent(~ small_table) %>%
      col_vals_lt(
        c, 8,
        segments = vars(f),
        label = c("label 1/3", "label 2/3")
      )
  }, "must be length 1 or 3, not 2")
  expect_error({
    create_agent(~ small_table) %>%
      col_vals_lt(
        c, 8,
        segments = vars(f),
        label = c("label 1/3", "label 2/3", "label 3/3", "label 4/3")
      )
  }, "must be length 1 or 3, not 4")

  # NA elements in `label` passed down
  some_empty <- c("{.seg_val} is 1 of 3", "{.seg_val} is 2 of 3", NA)
  agent_some_empty <- create_agent(~ small_table) %>%
    col_vals_lt(
      c, 8,
      segments = vars(f),
      label = some_empty
    )
  expect_identical(
    agent_some_empty$validation_set$label,
    c("high is 1 of 3", "low is 2 of 3", NA_character_)
  )

})

test_that("glue works for brief too", {

  # Basic usage (recycled)
  agent <- create_agent(~ small_table) %>%
    col_vals_lt(
      c(a, c), 5, segments = vars(f),
      label = "{.col} {.seg_val}",
      brief = "{.col} {.seg_val}"
    )
  expect_identical(
    agent$validation_set$label,
    agent$validation_set$brief
  )

  # NAs (recycled)
  agent_NA <- create_agent(~ small_table) %>%
    col_vals_lt(
      c(a, c), 5, segments = vars(f),
      label = NA,
      brief = NA
    )
  expect_identical(
    agent_NA$validation_set$label,
    agent_NA$validation_set$brief
  )

  # Mix (non-recycled)
  agent_mix <- create_agent(~ small_table) %>%
    col_vals_lt(
      c(a, c), 5, segments = vars(f),
      label = replace(rep("{.col} {.seg_val}", 6), 3, NA),
      brief = replace(rep("{.col} {.seg_val}", 6), 3, NA)
    )
  expect_identical(
    agent_mix$validation_set$label,
    agent_mix$validation_set$brief
  )

})
