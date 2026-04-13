#' Synthetic Sensor Measurements Dataset
#'
#' A synthetic dataset representing simulated sensor readings from a
#' materials testing experiment. The data includes temperature, strain,
#' and stress measurements with some injected outliers to demonstrate
#' the outlier detection capabilities of this package.
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{sample_id}{Integer. Unique identifier for each measurement sample.}
#'   \item{temperature}{Numeric. Simulated temperature readings in degrees Celsius.}
#'   \item{strain}{Numeric. Simulated strain measurements (dimensionless).}
#'   \item{stress}{Numeric. Simulated stress measurements in MPa.}
#' }
#'
#' @examples
#' data(sensor_data)
#' head(sensor_data)
#' s <- compute_summary(sensor_data$temperature)
#' s$outliers
#'
#' @source Synthetically generated for demonstration purposes.
"sensor_data"
