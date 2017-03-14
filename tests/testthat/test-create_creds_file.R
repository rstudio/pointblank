context("Creating a credentials file")

test_that("Creating a credentials file is possible", {
  
  # Create a crendentials file
  create_creds_file(
    file = "creds.rds",
    dbname = "db",
    host = "host",
    port = "31",
    user = "user",
    password = "password")
  
  # Read in the credentials file
  creds <- readRDS("creds.rds")
  
  # Expect the same input values in the credentials file vector
  expect_equivalent(creds[1], "db")
  expect_equivalent(creds[2], "host")
  expect_equivalent(creds[3], "31")
  expect_equivalent(creds[4], "user")
  expect_equivalent(creds[5], "password")
})
