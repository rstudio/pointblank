tbl <-
  dplyr::tibble(
    a = 1:5,
    b = letters[1:5],
    c = c(3.5, 8.3, 6.7, 9.1, NA_real_)
  )

test_that("`col_schema_match()` works properly", {

  # Incorrect schema (column `d` doesn't exist)
  schema_obj_i_1 <-
    col_schema(a = "integer", b = "character", d = "numeric")

  # Incorrect schema (class in column `c` doesn't match)
  schema_obj_i_2 <-
    col_schema(a = "integer", b = "character", c = "character")

  # Incorrect schema (too many columns in column `c` doesn't match)
  schema_obj_i_3 <-
    col_schema(a = "integer", b = "character", c = "numeric", d = "numeric")

  # Schema with correct classes, complete columns, in order
  schema_obj_1_1 <-
    col_schema(a = "integer", b = "character", c = "numeric")

  # Schema with incorrect classes, complete columns, in order
  schema_obj_1_1_i <-
    col_schema(a = "integer_i", b = "character_i", c = "numeric_i")

  # Schemas with correct classes, complete columns, out of order
  schema_obj_2_1 <-
    col_schema(b = "character", c = "numeric", a = "integer")
  schema_obj_2_2 <-
    col_schema(c = "numeric", a = "integer", b = "character")

  # Schemas with incorrect classes, complete columns, out of order
  schema_obj_2_1_i <-
    col_schema(b = "character_i", c = "numeric_i", a = "integer_i")
  schema_obj_2_2_i <-
    col_schema(c = "numeric_i", a = "integer_i", b = "character_i")

  # Schemas with correct classes, incomplete columns, in order
  schema_obj_3_1 <- col_schema(b = "character", c = "numeric")
  schema_obj_3_2 <- col_schema(a = "integer", b = "character")
  schema_obj_3_3 <- col_schema(a = "integer", c = "numeric")
  schema_obj_3_4 <- col_schema(a = "integer")
  schema_obj_3_5 <- col_schema(b = "character")
  schema_obj_3_6 <- col_schema(c = "numeric")

  # Schemas with incorrect classes, incomplete columns, in order
  schema_obj_3_1_i <- col_schema(b = "character_i", c = "numeric_i")
  schema_obj_3_2_i <- col_schema(a = "integer_i", b = "character_i")
  schema_obj_3_3_i <- col_schema(a = "integer_i", c = "numeric_i")
  schema_obj_3_4_i <- col_schema(a = "integer_i")
  schema_obj_3_5_i <- col_schema(b = "character_i")
  schema_obj_3_6_i <- col_schema(c = "numeric_i")

  # Schemas with correct classes, incomplete columns, out of order
  schema_obj_4_1 <- col_schema(b = "character", c = "numeric")
  schema_obj_4_2 <- col_schema(c = "numeric", b = "character")
  schema_obj_4_3 <- col_schema(a = "integer", b = "character")
  schema_obj_4_4 <- col_schema(b = "character", a = "integer")
  schema_obj_4_5 <- col_schema(a = "integer", c = "numeric")
  schema_obj_4_6 <- col_schema(c = "numeric", a = "integer")

  # Schemas with incorrect classes, incomplete columns, out of order
  schema_obj_4_1_i <- col_schema(b = "character_i", c = "numeric_i")
  schema_obj_4_2_i <- col_schema(c = "numeric_i", b = "character_i")
  schema_obj_4_3_i <- col_schema(a = "integer_i", b = "character_i")
  schema_obj_4_4_i <- col_schema(b = "character_i", a = "integer_i")
  schema_obj_4_5_i <- col_schema(a = "integer_i", c = "numeric_i")
  schema_obj_4_6_i <- col_schema(c = "integer_i", a = "numeric_i")

  #
  # Case I (Default) (`complete = TRUE`, `in_order = TRUE`)
  #

  expect_no_error(tbl %>% col_schema_match(schema_obj_1_1, complete = TRUE, in_order = TRUE))

  # Error here but not necessarily in other cases
  expect_error(tbl %>% col_schema_match(schema_obj_1_1_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_1, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_2, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_1_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_2_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_1, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_1_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5_i, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6_i, complete = TRUE, in_order = TRUE))

  # Certain error cases
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_1, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_2, complete = TRUE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_3, complete = TRUE, in_order = TRUE))

  #
  # Case II (`complete = TRUE`, `in_order = FALSE`)
  #

  expect_no_error(tbl %>% col_schema_match(schema_obj_2_1, complete = TRUE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_2_2, complete = TRUE, in_order = FALSE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_1_1, complete = TRUE, in_order = FALSE))


  # Error here but not necessarily in other cases
  expect_error(tbl %>% col_schema_match(schema_obj_2_1_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_2_i, complete = TRUE, in_order = FALSE))

  expect_error(tbl %>% col_schema_match(schema_obj_3_1, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_1_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5_i, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6_i, complete = TRUE, in_order = FALSE))

  # Certain error cases
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_1, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_2, complete = TRUE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_3, complete = TRUE, in_order = FALSE))

  #
  # Case III (`complete = FALSE`, `in_order = TRUE`)
  #

  expect_no_error(tbl %>% col_schema_match(schema_obj_3_1, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_2, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_3, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_4, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_5, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_6, complete = FALSE, in_order = TRUE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_1_1, complete = FALSE, in_order = TRUE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_4_1, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_3, complete = FALSE, in_order = TRUE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_5, complete = FALSE, in_order = TRUE))

  # Error here but not necessarily in other cases
  expect_error(tbl %>% col_schema_match(schema_obj_3_1_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_1, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_2, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5_i, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6_i, complete = FALSE, in_order = TRUE))

  # Certain error cases
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_1, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_2, complete = FALSE, in_order = TRUE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_3, complete = FALSE, in_order = TRUE))

  # Case IV (`complete = FALSE`, `in_order = FALSE`)

  expect_no_error(tbl %>% col_schema_match(schema_obj_4_1, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_2, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_3, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_4, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_5, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_4_6, complete = FALSE, in_order = FALSE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_1_1, complete = FALSE, in_order = FALSE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_2_1, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_2_2, complete = FALSE, in_order = FALSE))

  expect_no_error(tbl %>% col_schema_match(schema_obj_3_1, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_2, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_3, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_4, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_5, complete = FALSE, in_order = FALSE))
  expect_no_error(tbl %>% col_schema_match(schema_obj_3_6, complete = FALSE, in_order = FALSE))


  # Error here but not necessarily in other cases
  expect_error(tbl %>% col_schema_match(schema_obj_2_1_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_2_2_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_1_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_2_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_3_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_4_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_5_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_3_6_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_1_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_2_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_3_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_4_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_5_i, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_4_6_i, complete = FALSE, in_order = FALSE))

  # Certain error cases
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_1, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_2, complete = FALSE, in_order = FALSE))
  expect_error(tbl %>% col_schema_match(schema_obj_cnc_3, complete = FALSE, in_order = FALSE))
})
