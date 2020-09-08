reporting_languages <- c("en", "fr", "de", "it", "es")

normalize_reporting_language <- function(lang) {
  
  if (is.null(lang)) return("en")
  
  if (!(tolower(lang) %in% reporting_languages)) {
    stop("The text ", lang, " doesn't correspond to a pointblank reporting language",
         call. = FALSE)
  }
  
  tolower(lang)
}

get_lsv <- function(text,
                    yaml_file = system.file("text", "translations", package = "pointblank")) {
  
  x <- yaml::read_yaml(yaml_file)
  
  if (length(text) == 2) {
    x <- x[[text[1]]]
    return(unlist(x[[text[2]]]))
  } else if (length(text) == 1) {
    if (grepl("/", text)) {
      text <- unlist(strsplit(text, "/"))
      x <- x[[text[1]]]
      return(unlist(x[[text[2]]]))
    } else {
      return(unlist(x[[text]]))
    }
  } else {
    stop("The length of `text` must be either 1 or 2.")
  }
}
