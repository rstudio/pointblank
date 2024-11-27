skip_on_os(os = "windows")

test_that("A table store object can be created and used effectively", {
  
  # Create a table store object with 2 table-prep formulas
  tbls <- 
    tbl_store(
      small_table_high ~ small_table %>% dplyr::filter(f == "high"),
      some_specifications ~ specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses)
    )
  
  # Expect the length of the object to be `2`
  expect_equal(length(tbls), 2)
  
  # Verify that this is a `tbl_store` object, which is a list
  expect_s3_class(tbls, "tbl_store")
  expect_type(tbls, "list")
  
  # Expect the names of the list to be the LHS values
  expect_equal(
    names(tbls),
    c("small_table_high", "some_specifications")
  )
  
  # Expect each component to be a list that contains two classes
  expect_type(tbls[1], "list")
  expect_s3_class(tbls[[1]], "read_fn")
  expect_s3_class(tbls[[1]], "with_tbl_name")
  expect_type(tbls[2], "list")
  expect_s3_class(tbls[[2]], "read_fn")
  expect_s3_class(tbls[[2]], "with_tbl_name")
  
  # Create a table store object with 2 table-prep formulas but, this
  # time, no names are provided
  tbls_2 <- 
    tbl_store(
      ~ small_table %>% dplyr::filter(f == "high"),
      ~ specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses)
    )
  
  # Expect the length of the object to be `2`
  expect_equal(length(tbls_2), 2)
  
  # Verify that this is a `tbl_store` object, which is a list
  expect_s3_class(tbls_2, "tbl_store")
  expect_type(tbls_2, "list")
  
  # Expect the names of the list to be generated and given the
  # form `tbl_<###>`
  expect_equal(
    names(tbls_2),
    c("tbl_001", "tbl_002")
  )
  
  # Expect each component to be a list that contains two classes
  expect_type(tbls_2[1], "list")
  expect_s3_class(tbls_2[[1]], "read_fn")
  expect_type(tbls_2[2], "list")
  expect_s3_class(tbls_2[[2]], "read_fn")
  
  # Expect an error if not providing a formula
  expect_error(
    tbl_store(small_table %>% dplyr::filter(f == "high"))
  )
  
  # Expect an error if giving duplicate names
  expect_error(
    tbl_store(
      one ~ small_table %>% dplyr::filter(f == "high"),
      one ~ specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses)
    )
  )
  
  # Use a list to provide tables to the `tbl_store` object
  tbls_3 <- 
    tbl_store(
      .list = list(
        one = small_table_high ~ small_table %>% dplyr::filter(f == "high"),
        two = some_specifications ~ specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses)
      )
    )
  
  # Expect that the table store objects `tbls` and `tbls_3` are the same
  expect_equal(tbls, tbls_3)
  
  # Obtain a table prep formula by using the `tbl_source()` function
  tbl_formula <- tbl_source(tbl = "some_specifications", store = tbls)
  
  # Expect this to contain two classes
  expect_s3_class(tbl_formula, "read_fn")
  expect_s3_class(tbl_formula, "with_tbl_name")
  
  # Expect the character components to contain the `~` and the LHS/RHS text
  tbl_formula_chr <- as.character(tbl_formula)
  expect_equal(tbl_formula_chr[1], "~")
  expect_equal(tbl_formula_chr[2], "some_specifications")
  expect_equal(tbl_formula_chr[3], "specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses)")
  
  # Expect the `tbl_formula` to be compatible with the `tbl` argument in
  # `create_agent()` and `create_informant()`
  expect_no_error(create_agent(tbl = tbl_formula))
  expect_no_error(create_informant(tbl = tbl_formula))
  
  # Materialize all tables from the `tbls` table store with `tbl_get()`
  # tbl_small_tbl_duckdb <- tbl_get(tbl = "small_tbl_duckdb", store = tbls)
  tbl_small_table_high <- tbl_get(tbl = "small_table_high", store = tbls)
  tbl_some_specifications <- tbl_get(tbl = "some_specifications", store = tbls)

  # Ensure that these are data tables
  # expect_s3_class(tbl_small_tbl_duckdb, "tbl_dbi")
  expect_s3_class(tbl_small_table_high, "tbl_df")
  expect_s3_class(tbl_some_specifications, "tbl_df")
  
  # Ensure that the `tbl_df` tables are generally the same tables as
  # defined in the table store object
  expect_equal(
    tbl_some_specifications, specifications %>% dplyr::select(zip_codes, credit_card_numbers, email_addresses),
    ignore_attr = TRUE
  )
  
  # Expect certain attributes to be available in the tables accessed
  # from the table store
  expect_true(
    all(c("pb_tbl_name", "pb_access_time") %in% names(attributes(tbl_small_table_high)))
  )
  expect_true(
    all(c("pb_tbl_name", "pb_access_time") %in% names(attributes(tbl_some_specifications)))
  )
})
