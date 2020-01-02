#' Set action levels for reacting to exceeding thresholds
#' 
#' This helper function works with the `actions` argument that is present in
#' every validation step function.
#' 
#' @param warn_at,stop_at,notify_at The threshold number for validation units
#'   returning a `FALSE` result before applying the `warn`, `stop`, or `notify`
#'   flags.
#'
#' @export
action_levels <- function(warn_at = NULL,
                          stop_at = NULL,
                          notify_at = NULL) {
  
  
  warn_list <- normalize_fraction_count(warn_at)
  stop_list <- normalize_fraction_count(stop_at)
  notify_list <- normalize_fraction_count(notify_at)
  
  action_levels_list <-
    list(
      warn_fraction = warn_list$fraction,
      warn_count = warn_list$count,
      stop_fraction = stop_list$fraction,
      stop_count = stop_list$count,
      notify_fraction = notify_list$fraction,
      notify_count = notify_list$count
    )
}

normalize_fraction_count <- function(x) {

  if (!is.null(x) && !any(c(inherits(x, "numeric"), inherits(x, "integer")))) {
    stop("All values provided to `action_levels()` must be either `numeric` or `integer` types.",
         call. = FALSE)
  }
  
  if (!is.null(x) && x <= 0) {
    stop("All values provided to `action_levels()` must be `>=0`.", call. = FALSE)
  }
  
  if (!is.null(x)) {
    if (x < 1) {
      fraction <- x
      count <- NULL
    } else if (x >= 1) {
      count <- floor(x) %>% as.numeric()
      fraction <- NULL
    }
  } else {
    fraction <- NULL
    count <- NULL
  }
  
  list(fraction = fraction, count = count)
}
