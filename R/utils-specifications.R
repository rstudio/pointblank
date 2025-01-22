#------------------------------------------------------------------------------#
#
#                 _         _    _      _                _
#                (_)       | |  | |    | |              | |
#   _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
#  | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
#  | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   <
#  | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
#  | |
#  |_|
#
#  This file is part of the 'rstudio/pointblank' project.
#
#  Copyright (c) 2017-2025 pointblank authors
#
#  For full copyright and license information, please look at
#  https://rstudio.github.io/pointblank/LICENSE.html
#
#------------------------------------------------------------------------------#


is_isbn_10 <- function(x) {

  x <- remove_hyphens(x)
  x <- remove_punctuation(x)
  x <- tolower(x)
  x <- remove_spaces(x)

  if (!grepl("\\d{9}[0-9x]", x)) {
    return(FALSE)
  }

  x <- unlist(strsplit(x, ""))

  # If the check digit is "x" then substitute that for "10"
  if (x[10] == "x") x[10] <- "10"

  # Recast as integer values
  x <- as.integer(x)

  # The sum of vector multiplication of `x` by the digit
  # weights (10 to 1 across the `x` digits) should be
  # divided evenly by 11 for this to be a valid ISBN-10
  sum(x * seq(10, 1, -1)) %% 11 == 0
}

is_isbn_13 <- function(x) {

  x <- remove_hyphens(x)

  if (!grepl("\\d{13}", x)) {
    return(FALSE)
  }

  x <- as.integer(unlist(strsplit(x, "")))

  check <- x[13]

  remainder <- sum(x[1:12] * rep(c(1, 3), 6)) %% 10

  remainder == 0 && check == 0 || 10 - remainder == check
}

remove_hyphens <- function(x, replacement = "") {
  gsub("-", replacement, x, fixed = TRUE)
}

remove_spaces <- function(x, replacement = "") {
  gsub(" ", replacement, x, fixed = TRUE)
}

remove_letters <- function(x, replacement = "") {
  gsub("[a-zA-Z]", replacement, x)
}

remove_punctuation <- function(x, replacement = " ") {
  gsub("[[:punct:]]", replacement, x)
}

check_vin <- function(x) {

  x <- remove_hyphens(x)
  x <- remove_punctuation(x)
  x <- tolower(x)
  x <- remove_spaces(x)

  vapply(
    seq_along(x),
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE,
    FUN = function(i) {
      if (is.na(x[i])) {
        return(FALSE)
      }
      is_vin(x[i])
    }
  )
}

is_vin <- function(x) {

  if (!grepl(regex_vin(), tolower(x))) {
    return(FALSE)
  }

  x <- unlist(strsplit(x, ""))

  weights <- c(8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2)

  letter_vals <-
    c(
      "a" = 1, "b" = 2, "c" = 3, "d" = 4,
      "e" = 5, "f" = 6, "g" = 7, "h" = 8,
      "j" = 1, "k" = 2, "l" = 3, "m" = 4,
      "n" = 5, "p" = 7, "r" = 9, "s" = 2,
      "t" = 3, "u" = 4, "v" = 5, "w" = 6,
      "x" = 7, "y" = 8, "z" = 9,
      "A" = 1, "B" = 2, "C" = 3, "D" = 4,
      "E" = 5, "F" = 6, "G" = 7, "H" = 8,
      "J" = 1, "K" = 2, "L" = 3, "M" = 4,
      "N" = 5, "P" = 7, "R" = 9, "S" = 2,
      "T" = 3, "U" = 4, "V" = 5, "W" = 6,
      "X" = 7, "Y" = 8, "Z" = 9
    )

  sum <- 0

  for (i in 1:17) {
    if (!grepl("[0-9]", x[i])) {
      sum <- sum + letter_vals[x[i]] * weights[i]
    } else {
      sum <- sum + as.integer(x[i]) * weights[i]
    }
  }

  check <- unname(sum) %% 11

  if (check == 10) {
    check <- "x"
  }

  check == x[9]
}

is_credit_card <- function(x) {

  if (!grepl(regex_credit_card_1(), x)) {
    return(FALSE)
  }

  if (!grepl(regex_credit_card_2(), x)) {
    return(FALSE)
  }

  x <- remove_hyphens(x)
  x <- remove_punctuation(x)
  x <- remove_spaces(x)

  luhn(x)
}

luhn <- function(x) {

  x <- rev(as.integer(unlist(strsplit(x, ""))))

  idx_odd  <- seq_along(x) %% 2 == 1
  idx_even <- seq_along(x) %% 2 == 0

  x[idx_even] <- x[idx_even] * 2
  x[idx_even] <- ifelse(x[idx_even] > 9, x[idx_even] - 9, x[idx_even])

  sum_odd  <- sum(x[idx_odd])
  sum_even <- sum(x[idx_even])

  sum_x <- sum_odd + sum_even

  sum_x %% 10 == 0
}

check_credit_card <- function(x) {

  vapply(
    seq_along(x),
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE,
    FUN = function(i) {
      is_credit_card(x[i])
    }
  )
}

check_isbn <- function(x) {

  x <- remove_hyphens(x)
  x <- remove_punctuation(x)
  x <- tolower(x)
  x <- remove_spaces(x)

  isbn_str_length <- as.character(nchar(x))

  vapply(
    seq_along(x),
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE,
    FUN = function(i) {
      if (is.na(isbn_str_length[i])) {
        FALSE
      } else if (isbn_str_length[i] == 10) {
        is_isbn_10(x[i])
      } else if (isbn_str_length[i] == 13) {
        is_isbn_13(x[i])
      } else {
        FALSE
      }
    }
  )
}

check_iban <- function(x, country = NULL) {
  grepl(regex_iban(country = country), x)
}

check_postal_code <- function(x, country) {

  if (length(country) == length(x)) {
    res <-
      vapply(
        seq_along(country),
        FUN.VALUE = logical(1),
        USE.NAMES = FALSE,
        FUN = function(i) {
          grepl(regex_postal_code(country = country[i]), toupper(x[i]))
        }
      )
  } else {
    res <- grepl(regex_postal_code(country = country), toupper(x))
  }

  res
}

check_url <- function(x) {
  grepl(regex_url(), x, perl = TRUE)
}

check_ipv4_address <- function(x) {
  grepl(regex_ipv4_address(), x, perl = TRUE)
}

check_ipv6_address <- function(x) {
  grepl(regex_ipv6_address(), x, perl = TRUE)
}

check_email <- function(x) {
  grepl(regex_email(), x, perl = TRUE)
}

check_phone <- function(x) {
  grepl(regex_phone(), x, perl = TRUE)
}

check_mac <- function(x) {
  grepl(regex_mac(), x)
}

check_swift_bic <- function(x) {
  grepl(regex_swift_bic(), x)
}

# nolint start

check_vin_db <- function(table,
                         column) {

  tbl_colnames <- get_table_column_names(data = table)

  table <-
    table %>%
    dplyr::mutate(
      pb_vin_all_ = {{ column }},
      pb_vin_all_ = tolower(as.character((pb_vin_all_))),
      pb_vin_nch_ = nchar(pb_vin_all_) == 17,
      pb_vin_001_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 1, 1), "8"),
      pb_vin_002_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 2, 2), "8"),
      pb_vin_003_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 3, 3), "8"),
      pb_vin_004_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 4, 4), "8"),
      pb_vin_005_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 5, 5), "8"),
      pb_vin_006_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 6, 6), "8"),
      pb_vin_007_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 7, 7), "8"),
      pb_vin_008_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 8, 8), "8"),
      pb_vin_009_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 9, 9), "8"),
      pb_vin_010_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 10, 10), "8"),
      pb_vin_011_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 11, 11), "8"),
      pb_vin_012_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 12, 12), "8"),
      pb_vin_013_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 13, 13), "8"),
      pb_vin_014_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 14, 14), "8"),
      pb_vin_015_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 15, 15), "8"),
      pb_vin_016_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 16, 16), "8"),
      pb_vin_017_ = ifelse(pb_vin_nch_, substr(pb_vin_all_, 17, 17), "8"),
      dplyr::across(
        .cols = dplyr::matches("pb_vin_[0-9]{3}_"),
        function(x) {
          dplyr::case_match(x,
            c("a", "j") ~ "1",
            c("b", "k", "s") ~ "2",
            c("c", "l", "t") ~ "3",
            c("d", "m", "u") ~ "4",
            c("e", "n", "v") ~ "5",
            c("f", "w") ~ "6",
            c("g", "p", "x") ~ "7",
            c("h", "y") ~ "8",
            c("r", "z") ~ "9",
            c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") ~ "100000",
            .default = x
          )
      }),
      pb_vin_chk_ = pb_vin_009_,
      dplyr::across(dplyr::matches("pb_vin_[0-9]{3}_"), as.integer),
      pb_vin_001_w = pb_vin_001_ * 8L,
      pb_vin_002_w = pb_vin_002_ * 7L,
      pb_vin_003_w = pb_vin_003_ * 6L,
      pb_vin_004_w = pb_vin_004_ * 5L,
      pb_vin_005_w = pb_vin_005_ * 4L,
      pb_vin_006_w = pb_vin_006_ * 3L,
      pb_vin_007_w = pb_vin_007_ * 2L,
      pb_vin_008_w = pb_vin_008_ * 10L,
      pb_vin_009_w = pb_vin_009_ * 0L,
      pb_vin_010_w = pb_vin_010_ * 9L,
      pb_vin_011_w = pb_vin_011_ * 8L,
      pb_vin_012_w = pb_vin_012_ * 7L,
      pb_vin_013_w = pb_vin_013_ * 6L,
      pb_vin_014_w = pb_vin_014_ * 5L,
      pb_vin_015_w = pb_vin_015_ * 4L,
      pb_vin_016_w = pb_vin_016_ * 3L,
      pb_vin_017_w = pb_vin_017_ * 2L,
      pb_vin_sum_uw =
        pb_vin_001_ + pb_vin_002_ + pb_vin_003_ + pb_vin_004_ +
        pb_vin_005_ + pb_vin_006_ + pb_vin_007_ + pb_vin_008_ +
        pb_vin_009_ + pb_vin_010_ + pb_vin_011_ + pb_vin_012_ +
        pb_vin_013_ + pb_vin_014_ + pb_vin_015_ + pb_vin_016_ +
        pb_vin_017_,
      pb_vin_sum_ =
        pb_vin_001_w + pb_vin_002_w + pb_vin_003_w + pb_vin_004_w +
        pb_vin_005_w + pb_vin_006_w + pb_vin_007_w + pb_vin_008_w +
        pb_vin_009_w + pb_vin_010_w + pb_vin_011_w + pb_vin_012_w +
        pb_vin_013_w + pb_vin_014_w + pb_vin_015_w + pb_vin_016_w +
        pb_vin_017_w,
      pb_vin_mod_ = as.character(pb_vin_sum_ %% 11L),
      pb_vin_mod_ = ifelse(pb_vin_mod_ == "10", "x", pb_vin_mod_),
      pb_is_good_ = pb_vin_mod_ == pb_vin_chk_,
      pb_is_good_ = ifelse(!pb_vin_nch_, FALSE, pb_vin_nch_),
      pb_is_good_ = ifelse(pb_vin_sum_uw >= 100000, FALSE, pb_is_good_)
    )

  table <-
    table %>% dplyr::select(dplyr::all_of(c(tbl_colnames, "pb_is_good_")))

  table
}

# nolint end
