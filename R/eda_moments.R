#' @export
#'
#' @title
#'  Compute moments of a dataset
#'
#' @description
#'  Computes the mean, variance, skewness, and excess kurtosis of a
#'  given numeric vector.
#'
#' @param x A numeric vector representing the dataset.
#'
#' @return A named vector with the count, mean, variance, skewness,
#'  and excess kurtosis of the data.
#'

eda_moments <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / n / (sd_x^3)
  kurt <- sum((x - mean_x)^4) / n / (sd_x^4) - 3 # Excess kurtosis
  return(c(n = n ,mean = mean_x, var = sd_x^2, skew = skew, kurt = kurt))
}
