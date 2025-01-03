#' @export
#' @title
#'  Simulate data using Fleishman transformation
#'
#' @description
#' `r lifecycle::badge("experimental")` \cr\cr
#' Generates random data with the specified skewness and excess kurtosis using the
#' Fleishman transformation method.
#'
#' @param n An integer specifying the number of random data points to generate.
#' @param skew A numeric value specifying the desired skewness of the simulated data.
#' @param kurt A numeric value specifying the desired excess kurtosis of the simulated data.
#'  A \code{NULL} value will have the function compute the minimum kurtosis value
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
#' References suggest that the function is valid for a skewness range of -3 to 3
#' and an excess kurtosis greater than \code{-1.13168 + 1.58837 * skew ^ 2}.
#' However, the suggested cutoff  fails for a skewness beyond the range \code{-2,2}
#' in this function's implementation of Fleishman's routine. Instead, a cutoff
#' of \code{-1.13168 + 0.9 + 1.58837 * skew ^ 2} is implemented here.
#' \cr \cr
#' If \code{check = TRUE}, the function will warn the user if an invalid
#' combination of skewness and excess kurtosis are passed to the function. If
#' \code{kurt = NULL} , the function will generate the minimum valid excess kurtosis
#' value given the input skewness.   \cr \cr
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
#' # Mean and variance should be close to 0 and 1 respectively
#' eda_moments(x)
#'
#' # Visualize the simulated data
#' hist(x, breaks = 30, main = "Simulated Data", xlab = "Value")
#'
#' # Check skewness/kurtosis output
#' set.seed(123)
#' skew <- kurt <- z <- vector()
#' y <- seq(-3.5,3.5, by = 0.5)
#' for (i in 1:length(y)){
#'  z[i] <- -1.13168 + 0.9 + 1.58837 * y[i]^2 # Compute within range kurtosis
#'  x <- eda_sim(199999, skew = y[i], kurt = z[i], check = FALSE)
#'  skew[i] <- eda_moments(x)[4]
#'  kurt[i] <- eda_moments(x)[5]
#' }
#'
#' eda_qq(y, skew)
#' eda_qq(z,kurt)

eda_sim <- function(n = 1, skew = 0, kurt = NULL, check = TRUE,
                    coefout = FALSE, coefin = NULL) {
  # Check for valid skew/kurtosis combination
  min_kurt <- -0.23168 + 1.58837 * skew^2

  if (check == TRUE & !is.null(kurt)) {
    if (!is.null(coefin)){
      message("Skew/kurtosis arguments are ignored given that Fleishman coefficients are provided.")
    } else if (kurt >= min_kurt) {
      message("Skew/kurtosis combination is valid.")
    } else {
      warning(cat("Excess kurtosis is below the recommended value of ",min_kurt,
                  "for a skew of ",skew,".\n This may result in a distribution",
                  "that does not reflect the desired \nskewness/kurtosis combination.\n"))
    }
  }

  # Check if minimum kurtosis value should be computed
  if (is.null(kurt)){
    kurt <- min_kurt + min_kurt * 0.0001
  }

  # Get Fleishman coefficients
  if(is.null(coefin)) {
    coeffs <- FitFleishmanFromSK(skew, kurt)
    c0 <- coeffs[1]; c1 <- coeffs[2]; c2 <- coeffs[3]; c3 <- coeffs[4]
  } else {
    c0 <- coefin[1]; c1 <- coefin[2]; c2 <- coefin[3]; c3 <- coefin[4]
  }

  # Generate standard normal data
  if(coefout == FALSE){
    z <- rnorm(n)
    # Transform to specified distribution
    y <- c0 + c1 * z + c2 * z^2 + c3 * z^3
    return(y)
  } else {
    return(c(c0, c1, c2, c3)) # Return coefficients
  }
}


#' Define the Fleishman coefficients function
#'
#' @noRd

Fleishman <- function(coef) {
  b <- coef[1]
  c <- coef[2]
  d <- coef[3]

  b2 <- b^2
  c2 <- c^2
  d2 <- d^2
  bd <- b * d

  # Variance
  var <- b2 + 6 * bd + 2 * c2 + 15 * d2
  # Skewness
  skew <- 2 * c * (b2 + 24 * bd + 105 * d2 + 2)
  # Excess kurtosis
  kurt <- 24 * (bd + c2 * (1 + b2 + 28 * bd) + d2 * (12 + 48 * bd + 141 * c2 + 225 * d2))

  return(c(var, skew, kurt))
}


#' Define the root function
#'
#' @noRd

FlFunc <- function(x, target) {
  Fleishman(x) - c(1, target[1], target[2])
}

#' Define derivatives of the Fleishman function
#'
#' @noRd

FlDeriv <- function(x) {
  b <- x[1]
  c <- x[2]
  d <- x[3]

  b2 <- b^2
  c2 <- c^2
  d2 <- d^2
  bd <- b * d

  df1db <- 2 * b + 6 * d
  df1dc <- 4 * c
  df1dd <- 6 * b + 30 * d

  df2db <- 4 * c * (b + 12 * d)
  df2dc <- 2 * (b2 + 24 * bd + 105 * d2 + 2)
  df2dd <- 4 * c * (12 * b + 105 * d)

  df3db <- 24 * (d + c2 * (2 * b + 28 * d) + 48 * d^3)
  df3dc <- 48 * c * (1 + b2 + 28 * bd + 141 * d2)
  df3dd <- 24 * (b + 28 * b * c2 + 2 * d * (12 + 48 * bd + 141 * c2 + 225 * d2) + d2 * (48 * b + 450 * d))

  J <- matrix(c(df1db, df1dc, df1dd,
                df2db, df2dc, df2dd,
                df3db, df3dc, df3dd), nrow = 3, byrow = TRUE)
  return(J)
}

#' Newton's method
#'
#' @noRd

Newton <- function(x0, func, deriv, target, max_iter = 25, tol = 1e-5) {
  x <- x0
  f <- func(x, target)

  iter <- 1
  while (max(abs(f)) > tol && iter <= max_iter) {
    J <- deriv(x)
    delta <- solve(J, -f) # Solve for correction vector
    x <- x + delta
    f <- func(x, target)
    iter <- iter + 1
  }

  if (iter > max_iter) {
    return(rep(NA, length(x0))) # Return NA if no convergence
  }

  return(x)
}

#' Initial guess for the Fleishman coefficients
#'
#' @noRd

FleishmanIC <- function(skew, kurt) {
  c1 <- 0.95357 - 0.05679 * kurt + 0.03520 * skew^2 + 0.00133 * kurt^2
  c2 <- 0.10007 * skew + 0.00844 * skew^3
  c3 <- 0.30978 - 0.31655 * c1
  return(c(c1, c2, c3))
}

#' Fit Fleishman coefficients based on skewness and kurtosis
#'
#' @noRd

FitFleishmanFromSK <- function(skew, kurt) {
  # Initial guess for nonlinear root finding
  x0 <- FleishmanIC(skew, kurt)

  # Find cubic coefficients (c1, c2, c3)
  coef <- Newton(x0, FlFunc, FlDeriv, target = c(skew, kurt))

  if (any(is.na(coef))) {
    return(rep(NA, 4)) # Return NA if Newton's method failed
  }

  return(c(-coef[2], coef))
}
