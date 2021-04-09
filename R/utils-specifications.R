#
#                _         _    _      _                _    
#               (_)       | |  | |    | |              | |   
#  _ __    ___   _  _ __  | |_ | |__  | |  __ _  _ __  | | __
# | '_ \  / _ \ | || '_ \ | __|| '_ \ | | / _` || '_ \ | |/ /
# | |_) || (_) || || | | || |_ | |_) || || (_| || | | ||   < 
# | .__/  \___/ |_||_| |_| \__||_.__/ |_| \__,_||_| |_||_|\_\
# | |                                                        
# |_|                                                        
# 
# This file is part of the 'rich-iannone/pointblank' package.
# 
# (c) Richard Iannone <riannone@me.com>
# 
# For full copyright and license information, please look at
# https://rich-iannone.github.io/pointblank/LICENSE.html
#


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
      if (isbn_str_length[i] == 10) {
        is_isbn_10(x[i])
      } else if (isbn_str_length[i] == 13) {
        is_isbn_13(x[i])
      } else {
        FALSE
      }
    }
  )
}

is_isbn_10 <- function(x) {
  
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
  
  if (!grepl("\\d{13}", x)) {
    return(FALSE)
  }
  
  x <- as.integer(unlist(strsplit(x, "")))
  
  check <- x[13]
  
  remainder <- sum(x[1:12] * rep(c(1, 3), 6)) %% 10

  10 - remainder == check
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
      is_vin(x[i])
    }
  )
}

is_vin <- function(x) {
  
  if (!grepl("^[^\\Wioq]{17}$", x)) {
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
      "x" = 7, "y" = 8, "z" = 9
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
  
  if (!grepl(".*[1-9].*", x)) {
    return(FALSE)
  }
  
  if (!grepl("^[0-9]*$", x)) {
    return(FALSE)
  }
  
  x <- remove_hyphens(x)
  x <- remove_punctuation(x)
  x <- remove_spaces(x)
  
  luhn(x)
}
