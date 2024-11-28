al <- action_levels(stop_at = 1)

test_that("The `cols_schema_match()` function works with an agent", {

  # [#1] Check that a user-defined schema works in the most
  # stringent case of `col_schema_match()`
  agent_1 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = c("POSIXct", "POSIXt"),
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric",
        e = "logical",
        f = "character"
      )
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_1))
  expect_true(has_agent_intel(agent_1))
  expect_true(all_passed(agent_1))

  # [#2] Check that a schema based on a table works
  # in the most stringent case of `col_schema_match()`
  agent_2 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(.tbl = small_table)
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_2))
  expect_true(has_agent_intel(agent_2))
  expect_true(all_passed(agent_2))

  # [#3] Check that a user-defined schema (incomplete)
  # works with `complete = FALSE`
  agent_3 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric"
      ),
      complete = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_3))
  expect_true(has_agent_intel(agent_3))
  expect_true(all_passed(agent_3))

  # [#4] Check that a user-defined schema (incomplete)
  # doesn't work when `complete = TRUE` (the default)
  agent_4 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        a = "integer",
        b = "character",
        c = "numeric",
        d = "numeric"
      )
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_4))
  expect_true(has_agent_intel(agent_4))
  expect_false(all_passed(agent_4))

  # [#5] Check that a user-defined schema works with
  # out-of-order column definitions when `in_order = FALSE`
  agent_5 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date = "Date",
        date_time = c("POSIXct", "POSIXt"),
        a = "integer",
        c = "numeric",
        b = "character",
        d = "numeric",
        f = "character",
        e = "logical"
      ),
      in_order = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_5))
  expect_true(has_agent_intel(agent_5))
  expect_true(all_passed(agent_5))

  # [#6] Check that a user-defined schema works does not
  # work with an out-of-order column definition
  # when `in_order = TRUE` (the default)
  agent_6 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date = "Date",
        date_time = c("POSIXct", "POSIXt"),
        a = "integer",
        c = "numeric",
        b = "character",
        d = "numeric",
        f = "character",
        e = "logical"
      )
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_6))
  expect_true(has_agent_intel(agent_6))
  expect_false(all_passed(agent_6))

  # [#7] Check that a user-defined schema works with
  # less defined or not defined column types when
  # `is_exact = FALSE`
  agent_7 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = "POSIXt",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = NULL,
        e = "logical",
        f = NULL
      ),
      is_exact = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_7))
  expect_true(has_agent_intel(agent_7))
  expect_true(all_passed(agent_7))

  # [#8] Check that a user-defined schema does not
  # work with less defined or not defined column
  # types when `is_exact = TRUE` (the default)
  agent_8 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = "POSIXt",
        date = "Date",
        a = "integer",
        b = "character",
        c = "numeric",
        d = NULL,
        e = "logical",
        f = NULL
      )
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_8))
  expect_true(has_agent_intel(agent_8))
  expect_false(all_passed(agent_8))

  # [#9] Check where two stringency options are
  # `FALSE`: `complete` and `in_order`
  agent_9 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date = "Date",
        date_time = c("POSIXct", "POSIXt"),
        c = "numeric",
        b = "character",
        d = "numeric",
        e = "logical"
      ),
      complete = FALSE,
      in_order = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_9))
  expect_true(has_agent_intel(agent_9))
  expect_true(all_passed(agent_9))

  # [#10] Check where two stringency options are
  # `FALSE`: `complete` and `is_exact`
  agent_10 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = "POSIXt",
        date = "Date",
        a = "integer",
        b = NULL,
        c = "numeric",
        d = "numeric"
      ),
      complete = FALSE,
      is_exact = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_10))
  expect_true(has_agent_intel(agent_10))
  expect_true(all_passed(agent_10))

  # [#11] Check where two stringency options are
  # `FALSE`: `in_order` and `is_exact`
  agent_11 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = NULL,
        b = NULL,
        d = "numeric",
        c = "numeric",
        f = "character",
        e = "logical"
      ),
      in_order = FALSE,
      is_exact = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_11))
  expect_true(has_agent_intel(agent_11))
  expect_true(all_passed(agent_11))

  # [#12] Check where all three stringency options
  # are `FALSE`
  agent_12 <-
    create_agent(
      tbl = small_table,
      actions = al
    ) %>%
    col_schema_match(
      schema = col_schema(
        date_time = "POSIXct",
        date = "Date",
        a = NULL,
        b = NULL,
        f = "character",
        e = "logical"
      ),
      complete = FALSE,
      in_order = FALSE,
      is_exact = FALSE
    ) %>%
    interrogate()

  expect_true(is_ptblank_agent(agent_12))
  expect_true(has_agent_intel(agent_12))
  expect_true(all_passed(agent_12))
})

test_that("The `expect_cols_schema_match()` function works", {

  # [#1] Check that a user-defined schema works in the most
  # stringent case of `col_schema_match()`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = c("POSIXct", "POSIXt"),
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric",
          e = "logical",
          f = "character"
        )
      )
  )

  # [#2] Check that a schema based on a table works
  # in the most stringent case of `col_schema_match()`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(.tbl = small_table)
      )
  )

  # [#3] Check that a user-defined schema (incomplete)
  # works with `complete = FALSE`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric"
        ),
        complete = FALSE
      )
  )

  # [#4] Check that a user-defined schema (incomplete)
  # doesn't work when `complete = TRUE` (the default)
  expect_failure(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric"
        )
      )
  )

  # [#5] Check that a user-defined schema works with
  # out-of-order column definitions when `in_order = FALSE`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          a = "integer",
          c = "numeric",
          b = "character",
          d = "numeric",
          f = "character",
          e = "logical"
        ),
        in_order = FALSE
      )
  )

  # [#6] Check that a user-defined schema works does not
  # work with an out-of-order column definition
  # when `in_order = TRUE` (the default)
  expect_failure(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          a = "integer",
          c = "numeric",
          b = "character",
          d = "numeric",
          f = "character",
          e = "logical"
        )
      )
  )

  # [#7] Check that a user-defined schema works with
  # less defined or not defined column types when
  # `is_exact = FALSE`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = NULL,
          e = "logical",
          f = NULL
        ),
        is_exact = FALSE
      )
  )

  # [#8] Check that a user-defined schema does not
  # work with less defined or not defined column
  # types when `is_exact = TRUE` (the default)
  expect_failure(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = NULL,
          e = "logical",
          f = NULL
        )
      )
  )

  # [#9] Check where two stringency options are
  # `FALSE`: `complete` and `in_order`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          c = "numeric",
          b = "character",
          d = "numeric",
          e = "logical"
        ),
        complete = FALSE,
        in_order = FALSE
      )
  )

  # [#10] Check where two stringency options are
  # `FALSE`: `complete` and `is_exact`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = NULL,
          c = "numeric",
          d = "numeric"
        ),
        complete = FALSE,
        is_exact = FALSE
      )
  )

  # [#11] Check where two stringency options are
  # `FALSE`: `in_order` and `is_exact`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = "POSIXct",
          date = "Date",
          a = NULL,
          b = NULL,
          d = "numeric",
          c = "numeric",
          f = "character",
          e = "logical"
        ),
        in_order = FALSE,
        is_exact = FALSE
      )
  )

  # [#12] Check where all three stringency options
  # are `FALSE`
  expect_success(
    small_table %>%
      expect_col_schema_match(
        schema = col_schema(
          date_time = "POSIXct",
          date = "Date",
          a = NULL,
          b = NULL,
          f = "character",
          e = "logical"
        ),
        complete = FALSE,
        in_order = FALSE,
        is_exact = FALSE
      )
  )
})

test_that("The `test_cols_schema_match()` function works", {

  # [#1] Check that a user-defined schema works in the most
  # stringent case of `col_schema_match()`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = c("POSIXct", "POSIXt"),
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric",
          e = "logical",
          f = "character"
        )
      )
  )

  # [#2] Check that a schema based on a table works
  # in the most stringent case of `col_schema_match()`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(.tbl = small_table)
      )
  )

  # [#3] Check that a user-defined schema (incomplete)
  # works with `complete = FALSE`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric"
        ),
        complete = FALSE
      )
  )

  # [#4] Check that a user-defined schema (incomplete)
  # doesn't work when `complete = TRUE` (the default)
  expect_false(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          a = "integer",
          b = "character",
          c = "numeric",
          d = "numeric"
        )
      )
  )

  # [#5] Check that a user-defined schema works with
  # out-of-order column definitions when `in_order = FALSE`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          a = "integer",
          c = "numeric",
          b = "character",
          d = "numeric",
          f = "character",
          e = "logical"
        ),
        in_order = FALSE
      )
  )

  # [#6] Check that a user-defined schema works does not
  # work with an out-of-order column definition
  # when `in_order = TRUE` (the default)
  expect_false(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          a = "integer",
          c = "numeric",
          b = "character",
          d = "numeric",
          f = "character",
          e = "logical"
        )
      )
  )

  # [#7] Check that a user-defined schema works with
  # less defined or not defined column types when
  # `is_exact = FALSE`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = NULL,
          e = "logical",
          f = NULL
        ),
        is_exact = FALSE
      )
  )

  # [#8] Check that a user-defined schema does not
  # work with less defined or not defined column
  # types when `is_exact = TRUE` (the default)
  expect_false(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = "character",
          c = "numeric",
          d = NULL,
          e = "logical",
          f = NULL
        )
      )
  )

  # [#9] Check where two stringency options are
  # `FALSE`: `complete` and `in_order`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date = "Date",
          date_time = c("POSIXct", "POSIXt"),
          c = "numeric",
          b = "character",
          d = "numeric",
          e = "logical"
        ),
        complete = FALSE,
        in_order = FALSE
      )
  )

  # [#10] Check where two stringency options are
  # `FALSE`: `complete` and `is_exact`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = "POSIXt",
          date = "Date",
          a = "integer",
          b = NULL,
          c = "numeric",
          d = "numeric"
        ),
        complete = FALSE,
        is_exact = FALSE
      )
  )

  # [#11] Check where two stringency options are
  # `FALSE`: `in_order` and `is_exact`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = "POSIXct",
          date = "Date",
          a = NULL,
          b = NULL,
          d = "numeric",
          c = "numeric",
          f = "character",
          e = "logical"
        ),
        in_order = FALSE,
        is_exact = FALSE
      )
  )

  # [#12] Check where all three stringency options
  # are `FALSE`
  expect_true(
    small_table %>%
      test_col_schema_match(
        schema = col_schema(
          date_time = "POSIXct",
          date = "Date",
          a = NULL,
          b = NULL,
          f = "character",
          e = "logical"
        ),
        complete = FALSE,
        in_order = FALSE,
        is_exact = FALSE
      )
  )
})
