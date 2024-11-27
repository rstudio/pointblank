library(tidyverse)
library(here)
library(yaml)
library(stringi)

reencode_utf8 <- function(x) {

  # Ensure that we encode non-UTF-8 strings to UTF-8 in a
  # two-step process: (1) to native encoding, and then
  # (2) to UTF-8
  if (Encoding(x) != 'UTF-8') {
    x <- enc2utf8(x)
  }

  # Use `iconv()` to convert to UTF-32 (big endian) as
  # raw bytes and convert again to integer (crucial here
  # to set the base to 16 for this conversion)
  raw_bytes <-
    iconv(x, "UTF-8", "UTF-32BE", toRaw = TRUE) %>%
    unlist() %>%
    strtoi(base = 16L)

  # Split into a list of four bytes per element
  chars <- split(raw_bytes, ceiling(seq_along(raw_bytes) / 4))

  x <-
    vapply(
      chars,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE,
      FUN = function(x) {

        bytes_nz <- x[min(which(x > 0)):length(x)]

        if (length(bytes_nz) > 2) {
          out <- paste("\\U", paste(as.hexmode(x), collapse = ""), sep = "")
        } else if (length(bytes_nz) > 1) {
          out <- paste("\\u", paste(as.hexmode(bytes_nz), collapse = ""), sep = "")
        } else if (length(bytes_nz) == 1 && bytes_nz > 127) {
          out <- paste("\\u", sprintf("%04s", paste(as.hexmode(bytes_nz)), collapse = ""), sep = "")
        } else {
          out <- rawToChar(as.raw(bytes_nz))
        }
        out
      }
    ) %>%
    paste(collapse = "")

  x
}

y <- yaml::read_yaml(here::here("data-raw/translations_source.yml"))

# Obtain a list that reencodes all translation text to Unicode
# code points
translations_list <-
  lapply(y, lapply, lapply, function(x) {
    reencode_utf8(x) %>% stringi::stri_unescape_unicode()
    })

saveRDS(translations_list, file = here("inst/text/translations_built"))
