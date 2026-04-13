#' Compute Summary Statistics with Outlier Detection
#'
#' Computes descriptive statistics for a numeric vector and identifies
#' outliers using the IQR-based fence method (Tukey's rule).
#'
#' @param x A numeric vector of observations.
#' @param na.rm Logical. If TRUE (default), NA values are removed before computation.
#' @param iqr_multiplier Numeric. Multiplier applied to the IQR to define outlier fences. Default is 1.5.
#'
#' @return An object of class \code{SummaryStats}, which is a list containing:
#' \itemize{
#'   \item \code{mean}: Mean of x.
#'   \item \code{sd}: Standard deviation of x.
#'   \item \code{median}: Median of x.
#'   \item \code{Q1}: First quartile.
#'   \item \code{Q3}: Third quartile.
#'   \item \code{IQR}: Interquartile range.
#'   \item \code{lower_fence}: Lower outlier fence.
#'   \item \code{upper_fence}: Upper outlier fence.
#'   \item \code{outliers}: Values outside the fences.
#'   \item \code{n_outliers}: Number of outliers detected.
#'   \item \code{data}: The cleaned input data.
#'   \item \code{iqr_multiplier}: The multiplier used.
#' }
#'
#' @examples
#' x <- c(2, 4, 4, 4, 5, 5, 7, 9, 100)
#' result <- compute_summary(x)
#' result$outliers
#' result$mean
#'
#' @export
compute_summary <- function(x, na.rm = TRUE, iqr_multiplier = 1.5) {

  stopifnot(is.numeric(x))
  stopifnot(is.logical(na.rm))
  stopifnot(is.numeric(iqr_multiplier), length(iqr_multiplier) == 1,
            iqr_multiplier > 0)

  if (na.rm) x <- x[!is.na(x)]

  stopifnot(length(x) >= 2)
  stopifnot(!any(is.infinite(x)))

  mean_val   <- mean(x)
  sd_val     <- sd(x)
  median_val <- median(x)
  Q1_val     <- as.numeric(quantile(x, probs = 0.25))
  Q3_val     <- as.numeric(quantile(x, probs = 0.75))
  IQR_val    <- IQR(x)

  lower_fence <- Q1_val - iqr_multiplier * IQR_val
  upper_fence <- Q3_val + iqr_multiplier * IQR_val

  outliers   <- x[x < lower_fence | x > upper_fence]
  n_outliers <- length(outliers)

  result <- list(
    mean           = mean_val,
    sd             = sd_val,
    median         = median_val,
    Q1             = Q1_val,
    Q3             = Q3_val,
    IQR            = IQR_val,
    lower_fence    = lower_fence,
    upper_fence    = upper_fence,
    outliers       = outliers,
    n_outliers     = n_outliers,
    data           = x,
    iqr_multiplier = iqr_multiplier
  )

  class(result) <- "SummaryStats"
  return(result)
}
