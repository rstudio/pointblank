context("Creating a credentials file")

test_that("Creating a database credentials file is possible", {
  
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

test_that("Creating an email credentials file is possible", {
  
  # Create a crendentials file
  create_email_creds_file(
    file = "email_creds_file.rds",
    sender = "point@blank.org",
    host = "smtp.blank.org",
    port = 465,
    user = "point@blank.org",
    password = "password")
  
  # Read in the credentials file
  email_creds <- readRDS("email_creds_file.rds")
  
  # Expect the same input values in the credentials file vector
  expect_equivalent(email_creds[1], "point@blank.org")
  expect_equivalent(email_creds[3], "465")
  expect_equivalent(email_creds[4], "point@blank.org")
  expect_equivalent(email_creds[5], "password")
  expect_equivalent(email_creds[6], "TRUE")
  expect_equivalent(email_creds[7], "TRUE")
})
