#' @export
#' @title
#'  Simulate Data Using Fleishman Transformation (experimental)
#'
#' @description
#' Generates random data with the specified skewness and excess kurtosis using the
#' Fleishman transformation method.
#'
#' @param n An integer specifying the number of random data points to generate.
#' @param skew A numeric value specifying the desired skewness of the simulated data.
#' @param kurt A numeric value specifying the desired excess kurtosis of the simulated data.
#' @param check Boolean determining if the combination of skewness and kurtosis are valid.
#' @param coefout Boolean determining if the Fleishman coefficients should be
#'  outputted instead of the simulated values.
#' @param coefin Vector of the four coefficients to be used in Fleishman's equation.
#'  This bypasses the need to solve for the parameters.
#'
#' @return A numeric vector of simulated data points.
#'
#' @details
#' The function uses Fleishman's polynomial transformation of the form:
#'
#' \deqn{Y = a + bX + cX^2 + dX^3}
#'
#' where \code{a}, \code{b}, \code{c}, and \code{d} are coefficients determined
#' to approximate the specified skewness and excess kurtosis, and \code{X} is a
#' standard normal variable. The coefficients are solved using a numerical
#' optimization approach based on minimizing the residuals of Fleishman's
#' equations. An excess kurtosis is defined as the kurtosis of a Normal
#' distribution (k=3) minus 3. \cr\cr
#'
#' The function is valid for a skewness range of -3 to 3 and an excess kurtosis
#' greater than \code{-1.13168 + 1.58837 * skew ^ 2}. If \code{check = TRUE},
#' the function will warn the user if an invalid combination of skewness and
#' kurtosis are passed to the function. Deviation from the recommended combination
#' will result in a distribution that may not reflect the desired skewness and
#' kurtosis values. \cr \cr
#'
#' If the proper combination of skewness and kurtosis parameters are passed to the
#' function, the output distribution will have a mean of around \code{0} and a
#' variance of around \code{1}. But note that a strongly skewed distribution will
#' require a large \code{n} to reflect the desired properties due to the
#' disproportionate influence of the tail's extreme values on the various moments
#' of the distribution, particularly higher-order moments like skewness and kurtosis.
#'
#'  \itemize{
#'  \item Fleishman, A. I. (1978). A method for simulating non-normal
#'  distributions. Psychometrika, 43, 521–532.
#'  \item Wicklin, R. (2013). Simulating Data with SAS (Appendix D: Functions
#'  for Simulating Data by Using Fleishman’s Transformation). Cary, NC: SAS
#'  Institute Inc. Retrieved from  https://tinyurl.com/4tustnph }
#'
#' @examples
#'
#' # Generate a normal distribution
#' set.seed(321)
#' x <- eda_sim(1000, skew = 0, kurt = 0)
#' eda_theo(x) # Check for normality
#'
#' # Simulate distribution with skewness = 1.15 and kurtosis = 2
#' # A larger sample size is more likely to reflect the desired parameters
#' set.seed(653)
#' x <- eda_sim(500000, skew = 1.15, kurt = 2)
#'
#' # Verify skewness and excess kurtosis of the simulated data
#' skewness <- sum((x - mean(x))^3) / (length(x) - 1) * sd(x)^3
#' excess_kurtosis <- (sum((x - mean(x))^4) / (length(x) - 1)) / (sd(x)^2) - 3
#'
#' # Mean and variance should be close to 0 and 1 respectively
#' mean(x)
#' var(x)
#'
#' # Visualize the simulated data
#' hist(x, breaks = 30, main = "Simulated Data", xlab = "Value")


eda_sim <- function(n, skew, kurt, check = TRUE, coefout = FALSE, coefin = NULL) {
  # Check for valid skew/kurtosis combination
  if (check == TRUE) {
    min_kurt <- -1.13168 + 1.58837 * skew^2
    if (!is.null(coefin)){
      print("Skew/kurtosis arguments are ignored given that Fleishman coeffcients are provided.")
    } else if (kurt >= min_kurt) {
      print("Skew/kurtosis combination is valid.")
    } else {
      warning(cat("Excess kurtosis is below the recommended value of ",min_kurt,
                  "for a skew of ",skew,".\n This may result in a distribution",
                  "that does not reflect the desired \nskewness/kurtosis combination.\n"))
    }
  }


  # Get Fleishman coefficients
  if(is.null(coefin)) {
    coeffs <- compute_fleishman_coeffs(skew, kurt)
    c0 <- coeffs[1]; c1 <- coeffs[2]; c2 <- coeffs[3]; c3 <- coeffs[4]
  } else {
    c0 <- coefin[1]; c1 <- coefin[2]; c2 <- coefin[3]; c3 <- coefin[4]
  }


  # Generate standard normal data
  z <- rnorm(n)

  # Transform to specified distribution
  y <- c0 + c1 * z + c2 * z^2 + c3 * z^3

  # Output
  if (coefout == TRUE){
    return(paste(c0, c1, c2, c3))
  } else {
    return(y)
  }
}


#' Compute Fleishman Coefficients
#'
#' Calculates the Fleishman coefficients required to generate a distribution with the specified skewness
#' and kurtosis using the quasi-Newton optimization method.
#'
#' @param skew A numeric value specifying the desired skewness of the distribution.
#' @param kurt A numeric value specifying the desired excess kurtosis of the distribution.
#' @return A numeric vector of Fleishman coefficients: c0, c1, c2, and c3.
#'
#' @noRd

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


