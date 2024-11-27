library(tidyverse)

specifications <-
  dplyr::tibble(
    isbn_numbers = c(
      "978 1 85715 201 2",
      "978-1-84159-362-3",
      "978 1 84159 329 6",
      "978 1 85715 202 9",
      "978 1 85715 198 5",
      NA_character_,
      "978 1 84159 312 9",
      "97a1857150469"
    ),
    vin_numbers = c(
      "4UZAANDH85CV12329",
      "JM1BL1S59A1134659",
      "1GCEK14R3WZ274764",
      "2B7JB21Y0XK524370",
      "4UZAANDH85CV12329",
      NA_character_,
      "2B7JB21Y0XK52437!",
      "2B7JB21Y0XK52437"
    ),
    zip_codes = c(
      "99553",
      "36264",
      "71660",
      "85225",
      "90309",
      NA_character_,
      "8208",
      "5416a"
    ),
    credit_card_numbers = c(
      "340000000000009",
      "378734493671000",
      "6703444444444449",
      "6703000000000000003",
      "4035501000000008",
      NA_character_,
      "5019717010103743",
      "60110000000000040"
    ),
    iban_austria = c(
      "AT582774098454337653",
      "AT220332087576467472",
      "AT328650112318219886",
      "AT193357281080332578",
      "AT535755326448639816",
      NA_character_,
      "AT22033208757646747A",
      "AI535755326448639816"
    ),
    swift_numbers = c(
      "RBOSGGSX",
      "RZTIAT22263",
      "BCEELULL",
      "MARKDEFF",
      "GENODEF1JEV",
      NA_character_,
      "CE1EL2LLFFF",
      "E31DCLLFFF"
    ),
    phone_numbers = c(
      "+5-555-555-5555",
      "+5 555 555 5555",
      "+5.555.555.5555",
      "5-555-555-5555",
      "5.555.555.5555",
      NA_character_,
      "(11- 97777-7777",
      "-11) 97777-7777"
    ),
    email_addresses = c(
      "test@test.com",
      "mail+mail@example.com",
      "mail.email@e.test.com",
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@letters-in-local.org",
      "01234567890@numbers-in-local.net",
      NA_character_,
      "invalid-characters-in-sld@! \"#$%(),/;<>_[]`|.org",
      "(),:;`|@more-invalid-characters-in-local.org"
    ),
    urls = c(
      "http://foo.com/blah_blah",
      "http://foo.com/blah_blah/",
      "http://foo.com/blah_blah_(wikipedia)",
      "http://\u2605foo.com/blah_blah_(wikipedia)_(again)",
      "http://www.example.com/wpstyle/?p=364",
      NA_character_,
      "ftps://foo.bar/",
      "http://-error-.invalid/"
    ),
    ipv4_addresses = c(
      "93.184.220.20",
      "161.148.172.130",
      "161.148.172.130",
      "73.194.66.94",
      "60.92.167.193",
      NA_character_,
      "000.000.000.000",
      "256.255.255.255"
    ),
    ipv6_addresses = c(
      "2001:0db8:0000:85a3:0000:0000:ac1f:8001",
      "2001:db8:0:85a3:0:0:ac1f:8001",
      "::",
      "2001:db8::1234:5678",
      "2001:db8:1::ab9:C0A8:102",
      NA_character_,
      "161.148.172.130",
      "161.148.172.130"
    ),
    mac_addresses = c(
      "01-2d-4c-ef-89-ab",
      "01-2D-4C-EF-89-AB",
      "01:2d:4c:ef:89:ab",
      "01:2D:4C:EF:89:AB",
      "01-2d-4c-ef-89-59",
      NA_character_,
      "01-2d-4c-ef-89-ab-06",
      "01-2d:4c-ef:89-ab"
    )
  )
