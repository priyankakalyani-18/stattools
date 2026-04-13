#' Plot Summary Statistics with Outlier Highlighting
#'
#' Creates a dot plot of the data stored in a \code{SummaryStats} object,
#' highlighting outliers in red and showing the IQR fences as horizontal lines.
#' This function uses the \code{ggplot2} package for visualization.
#'
#' @param stats_obj An object of class \code{SummaryStats} (output of \code{compute_summary}).
#' @param title Character. Title of the plot. Default is "Data with Outliers".
#'
#' @return A \code{ggplot2} plot object.
#'
#' @import ggplot2
#'
#' @examples
#' x <- c(2, 4, 4, 4, 5, 5, 7, 9, 100)
#' s <- compute_summary(x)
#' plot_summary(s)
#' plot_summary(s, title = "My Measurements")
#'
#' @export
plot_summary <- function(stats_obj, title = "Data with Outliers") {

  stopifnot(inherits(stats_obj, "SummaryStats"))
  stopifnot(is.character(title), length(title) == 1)

  x <- stats_obj$data
  is_outlier <- x < stats_obj$lower_fence | x > stats_obj$upper_fence

  df <- data.frame(
    index    = seq_along(x),
    value    = x,
    outlier  = ifelse(is_outlier, "Outlier", "Normal")
  )

  ggplot(df, aes(x = index, y = value, color = outlier)) +
    geom_point(size = 3) +
    geom_hline(yintercept = stats_obj$lower_fence, linetype = "dashed",
               color = "blue", linewidth = 0.8) +
    geom_hline(yintercept = stats_obj$upper_fence, linetype = "dashed",
               color = "blue", linewidth = 0.8) +
    scale_color_manual(values = c("Normal" = "steelblue", "Outlier" = "red")) +
    labs(
      title  = title,
      x      = "Index",
      y      = "Value",
      color  = "Type",
      caption = "Dashed lines indicate IQR fences"
    ) +
    theme_minimal()
}
