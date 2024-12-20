#' @export
#' @title Simulate Data Using Fleishman Transformation (experimental)
#'
#' @description Generates simulated data with the same mean, standard deviation,
#' skewness, and kurtosis as the input data.
#'
#' @param x A numeric vector representing the dataset to match moments.
#' @param n An integer specifying the number of simulated data points to generate.
#'
#' @return A numeric vector of simulated data.
#'
#' @examples
#' simulated_data <- simulate_fleishman_data(rnorm(1000, mean = 10, sd = 5), 1000)
#'
#' @references
#'
#'  \itemize{
#'  \item Fleishman, A. I. (1978). A method for simulating non-normal
#'  distributions. Psychometrika, 43, 521–532.
#'  \item Wicklin, R. (2013). Simulating Data with SAS (Appendix D: Functions
#'  for Simulating Data by Using Fleishman’s Transformation). Cary, NC: SAS
#'  Institute Inc. Retrieved from  https://tinyurl.com/4tustnph }
#'


simulate_fleishman_data <- function(x, n) {
  # Compute moments of the input data
  moments <- compute_moments(x)
  mean_x <- moments["mean"]
  sd_x <- moments["sd"]
  skew <- moments["skew"]
  kurt <- moments["kurt"]

  # Compute Fleishman coefficients
  coeffs <- compute_fleishman_coeffs(skew, kurt)
  c0 <- coeffs[1]; c1 <- coeffs[2]; c2 <- coeffs[3]; c3 <- coeffs[4]

  # Generate standard normal data
  z <- rnorm(n)

  # Transform to specified distribution using Fleishman coefficients
  y <- c0 + c1 * z + c2 * z^2 + c3 * z^3

  # Scale and shift the simulated data to match mean and sd of input data
  y <- mean_x + sd_x * y
  return(y)
}



#'  Compute Moments of a Dataset (ex)
#'
#'
#'  Computes the mean, standard deviation, skewness, and excess kurtosis of a
#'  given numeric vector.
#'
#'
#' @param x A numeric vector representing the dataset.
#' @return A named vector with the mean, standard deviation, skewness, and excess kurtosis of the data.
#' @examples
#' compute_moments(rnorm(1000, mean = 10, sd = 5))
compute_moments <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / n / (sd_x^3)
  kurt <- sum((x - mean_x)^4) / n / (sd_x^4) - 3 # Excess kurtosis
  return(c(mean = mean_x, sd = sd_x, skew = skew, kurt = kurt))
}

#' Fit Fleishman Model to Data
#'
#' Computes the Fleishman coefficients for transforming standard normal data to approximate the skewness and kurtosis of the input data.
#'
#' @param x A numeric vector representing the dataset.
#' @return A named vector of Fleishman coefficients: c0, c1, c2, and c3.
#' @examples
#' fit_fleishman(rnorm(1000, mean = 10, sd = 5))
fit_fleishman <- function(x) {
  # Compute skewness and kurtosis of the data
  moments <- compute_moments(x)
  skew <- moments["skew"]
  kurt <- moments["kurt"]

  # Compute Fleishman coefficients
  coeffs <- compute_fleishman_coeffs(skew, kurt)
  return(coeffs)
}



#' Compute Fleishman Coefficients
#'
#' Computes the Fleishman coefficients required to generate a distribution with given skewness and kurtosis using a numerical optimization approach.
#'
#' @param skew A numeric value specifying the desired skewness.
#' @param kurt A numeric value specifying the desired excess kurtosis.
#' @return A numeric vector of Fleishman coefficients: c0, c1, c2, and c3.
#' @examples
#' compute_fleishman_coeffs(1.15, 2)
compute_fleishman_coeffs <- function(skew, kurt) {
  # Initialize coefficients
  initial_guess <- c(
    0.95357 - 0.05679 * kurt + 0.03520 * skew^2 + 0.00133 * kurt^2, # c1
    0.10007 * skew + 0.00844 * skew^3,                              # c2
    0.30978 - 0.31655 * (0.95357 - 0.05679 * kurt + 0.03520 * skew^2 + 0.00133 * kurt^2) # c3
  )

  # Objective function
  fleishman_function <- function(c) {
    b <- c[1]; c2 <- c[2]; d <- c[3]
    var <- b^2 + 6 * b * d + 2 * c2^2 + 15 * d^2
    skewness <- 2 * c2 * (b^2 + 24 * b * d + 105 * d^2 + 2)
    kurtosis <- 24 * (b * d + c2^2 * (1 + b^2 + 28 * b * d) + d^2 * (12 + 48 * b * d + 141 * c2^2 + 225 * d^2))
    c(var - 1, skewness - skew, kurtosis - kurt)
  }

  # Solve using Newton-Raphson method
  result <- optim(initial_guess, function(c) sum(fleishman_function(c)^2), method = "BFGS")
  return(c(-result$par[2], result$par))
}
