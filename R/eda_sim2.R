#' Simulate Data Using Fleishman Transformation (experimental)
#'
#' Generates random data with the specified skewness and kurtosis using the Fleishman transformation method.
#'
#' @param n An integer specifying the number of random data points to generate.
#' @param skew A numeric value specifying the desired skewness of the simulated data.
#' @param kurt A numeric value specifying the desired excess kurtosis of the simulated data.
#'
#' @return A numeric vector of simulated data points.
#'
#' @details
#' The function is valid for a skewness range of -3 to 3 and a kurtosis
#' greater than -1.13168 + 1.58837 * skew ^ 2 (usually between -2 and 10)
#'
#'
#' @examples
#' # Simulate 1000 random data points with skewness = 1.15 and kurtosis = 2
#' simulated_data <- sim(1000, skew = 1.15, kurt = 2)
#'
#' # Verify skewness and kurtosis of the simulated data
#' e1071::skewness(simulated_data)
#' moments::kurtosis(simulated_data)
#'
#' # Visualize the simulated data
#' hist(simulated_data, breaks = 30, main = "Simulated Data", xlab = "Value")
#'
#'  \itemize{
#'  \item Fleishman, A. I. (1978). A method for simulating non-normal
#'  distributions. Psychometrika, 43, 521–532.
#'  \item Wicklin, R. (2013). Simulating Data with SAS (Appendix D: Functions
#'  for Simulating Data by Using Fleishman’s Transformation). Cary, NC: SAS
#'  Institute Inc. Retrieved from  https://tinyurl.com/4tustnph }
#'
#' # Example Usage
# set.seed(123)
# simulated_data <- sim(10000, skew = 3, kurt = 0)
# e1071::skewness(simulated_data)
# qqnorm(simulated_data)
# qqline(simulated_data)


sim <- function(n, skew, kurt) {
  # Get Fleishman coefficients
  coeffs <- compute_fleishman_coeffs(skew, kurt)
  c0 <- coeffs[1]; c1 <- coeffs[2]; c2 <- coeffs[3]; c3 <- coeffs[4]

  # Generate standard normal data
  z <- rnorm(n)

  # Transform to specified distribution
  y <- c0 + c1 * z + c2 * z^2 + c3 * z^3
  return(y)
}


# Function to check validity of skewness and kurtosis
is_valid_skew_kurt <- function(skew, kurt) {
  min_kurt <- -1.13168 + 1.58837 * skew^2
  if (kurt >= min_kurt) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Compute Fleishman Coefficients
#'
#' Calculates the Fleishman coefficients required to generate a distribution with the specified skewness
#' and kurtosis using the Newton-Raphson optimization method.
#'
#' @param skew A numeric value specifying the desired skewness of the distribution.
#' @param kurt A numeric value specifying the desired excess kurtosis of the distribution.
#' @return A numeric vector of Fleishman coefficients: c0, c1, c2, and c3.
#' @examples
#' # Compute coefficients for a distribution with skewness = 1.15 and kurtosis = 2
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

  # Solve using quasi-Newton method
  result <- stats::optim(initial_guess, function(c) sum(fleishman_function(c)^2), method = "BFGS")
  return(c(-result$par[2], result$par))
}


