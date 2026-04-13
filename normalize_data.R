#' Normalize Data Using Min-Max Scaling
#'
#' Rescales a numeric vector stored inside a \code{SummaryStats} object
#' to a specified target range using min-max normalization.
#'
#' @param stats_obj An object of class \code{SummaryStats} (output of \code{compute_summary}).
#' @param new_min Numeric. The lower bound of the target range. Default is 0.
#' @param new_max Numeric. The upper bound of the target range. Default is 1.
#'
#' @return An object of class \code{SummaryStats} with normalized data and
#' an additional \code{normalized_range} attribute.
#'
#' @examples
#' x <- c(10, 20, 30, 40, 50)
#' s <- compute_summary(x)
#' s_norm <- normalize_data(s)
#' s_norm$data  # values between 0 and 1
#'
#' s_norm2 <- normalize_data(s, new_min = -1, new_max = 1)
#' s_norm2$data  # values between -1 and 1
#'
#' @export
normalize_data <- function(stats_obj, new_min = 0, new_max = 1) {

  stopifnot(inherits(stats_obj, "SummaryStats"))
  stopifnot(is.numeric(new_min), length(new_min) == 1)
  stopifnot(is.numeric(new_max), length(new_max) == 1)
  stopifnot(new_min < new_max)

  x <- stats_obj$data

  x_range <- max(x) - min(x)
  if (x_range == 0) {
    stop("Cannot normalize: all values in data are identical (range = 0).")
  }

  x_normalized <- (x - min(x)) / x_range * (new_max - new_min) + new_min

  result <- tryCatch({
    compute_summary(x_normalized,
                    na.rm          = FALSE,
                    iqr_multiplier = stats_obj$iqr_multiplier)
  }, error = function(e) {
    stop("Error recomputing summary after normalization: ", e$message)
  })

  result$normalized_range <- c(new_min, new_max)
  return(result)
}
