#' @export
#' @title Generate Data with Specific Skewness and Kurtosis
#'
#' @description
#'  This function generates a dataset with a specified skewness and
#'  kurtosis using Fleishman's polynomial transformation.
#'
#' @param n Integer. Number of data points to generate.
#' @param skewness Numeric. Desired skewness of the generated data.
#'  Default is 0 (symmetric distribution).
#' @param kurtosis Numeric. Desired kurtosis of the generated data.
#'  Default is 0 (normal distribution).
#' @param ... Not used.
#'
#' @return A numeric vector of length \code{n} containing the generated data.
#'
#' @details
#' The function uses Fleishman's polynomial transformation of the form:
#' \deqn{Y = a + bX + cX^2 + dX^3}
#' where \code{a}, \code{b}, \code{c}, and \code{d} are coefficients determined
#' to approximate the specified skewness and kurtosis. The coefficients are
#' solved using a numerical optimization approach based on minimizing the
#' residuals of Fleishman's equations.
#'
#'
#' @references
#'
#'  \itemize{
#'  \item Fleishman, A. I. (1978). A method for simulating non-normal
#'  distributions. Psychometrika, 43, 521–532.}
#'
#' @examples
#'
#'  # A normal distribution
#'  x <- eda_sim(5000, 0, 0)
#'  hist(x)
#'
#'  # A right-skewed distribution
#'  x <- eda_sim(5000, 2, 0)
#'  hist(x)
#'
#'  # A left-skewed distribution
#'  x <- eda_sim(5000, -2, 0)
#'  hist(x)
#'
#'  # A uniform distribution
#'  # Note that this is unbounded which may result in outliers
#'  set.seed(21)
#'  x <- eda_sim(5000, 0, -10)
#'  hist(x, breaks = 20)
#'
#'  # A "peaky" distribution
#'   set.seed(12)
#'   x <- eda_sim(5000, 0, 10)
#'   hist(x, breaks = 20)
#'
#'   # Distributions can be modified so as to share the same mean and
#'   # variances. For example, to have x1 (a right skewed distribution) share
#'   # the  same mean and variance as x2 (a left skewed distribution), type:
#'   set.seed(432)
#'   x1 <- eda_sim(10000, 0.5, 0)
#'   x2 <- eda_sim(10000, -0.5, 0)
#'
#'   x1_u <- mean(x1)
#'   x1_sd <- sd(x1)
#'   x2_sd <- sqrt(var(x2))
#'   x1b <- (x1 - x1_u) / x1_sd * x2_sd + mean(x2)
#'   mean(x1b)
#'   mean(x2)
#'   var(x1b)
#'   var(x2)
#'
#' eda_dens(x1b, x2)

# Generate data with specific skewness and kurtosis
eda_sim <- function(n, skewness = 0, kurtosis = 0, ...) {
  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Initial guess for parameters
  initial_guess <- c(1, 0, 0)

  # Solve for coefficients
  params <- solve_equations(initial_guess, skewness, kurtosis)
  b <- params[1]
  c <- params[2]
  d <- params[3]
  a <- -c

  # Generate standard normal data
  X <- rnorm(n)

  # Apply Fleishman transformation
  Y <- a + b * X + c * X^2 + d * X^3
  return(Y)
}

#' Fleishman's Polynomial Equations
#'
#' Computes the residuals of Fleishman's equations given parameters \code{b},
#'   \code{c}, and \code{d}.
#'
#' @param params Numeric vector. Parameters \code{b}, \code{c}, and \code{d}.
#' @param skewness Numeric. Desired skewness of the resulting distribution.
#' @param kurtosis Numeric. Desired kurtosis of the resulting distribution.
#' @return A numeric vector containing the residuals of the three equations.
#'
#' @examples
#' # Compute residuals for a test parameter set
#' params <- c(1, 0, 0)
#' residuals <- equations(params, skewness = 0, kurtosis = 3)
#' print(residuals)
#'
#' @noRd

# Define the equations function
equations <- function(params, skewness = 0, kurtosis = 3) {
  b <- params[1]
  c <- params[2]
  d <- params[3]

  # Equations derived from Fleishman's method
  eq1 <- b^2 + 6*b*d + 2*c^2 - 1
  eq2 <- 2*c*(b^2 + 24*b*d + 105*d^2 + 2) - skewness
  eq3 <- 24*(b*d + c^2*(1 + b^2 + 28*b*d) + d^2*(12 + 48*b*d + 141*d^2)) - kurtosis
  c(eq1, eq2, eq3)
}

#' Solve Fleishman's Equations for Coefficients
#'
#' This function solves Fleishman's equations to determine the coefficients
#' \code{b}, \code{c}, and \code{d} for the polynomial transformation.
#'
#' @param initial_guess Numeric vector. Initial guess for the parameters
#'  \code{b}, \code{c}, and \code{d}.
#' @param skewness Numeric. Desired skewness of the resulting distribution.
#' @param kurtosis Numeric. Desired kurtosis of the resulting distribution.
#' @return A numeric vector containing the coefficients \code{b}, \code{c},
#'  and \code{d}.
#' @details
#' The function minimizes the sum of squared residuals of Fleishman's equations
#' to determine the optimal coefficients.
#'
#' @examples
#' # Solve equations for normal distribution (skewness = 0, kurtosis = 3)
#' initial_guess <- c(1, 0, 0)
#' params <- solve_equations(initial_guess, skewness = 0, kurtosis = 3)
#' print(params)
#'
#' @noRd


# Function to solve the equations using optim as a root-finder
solve_equations <- function(initial_guess, skewness = 0, kurtosis = 3) {
  # Define a wrapper function to pass skewness and kurtosis
  root_function <- function(params) {
    residuals <- equations(params, skewness, kurtosis)
    sum(residuals^2) # Sum of squared residuals
  }

  # Use optim to minimize the sum of squared residuals
  solution <- stats::optim(par = initial_guess,
                           fn = root_function, method = "BFGS")

  # Return solution
  if (solution$convergence == 0) {
    return(solution$par) # Parameters b, c, d
  } else {
    stop("Solution did not converge!")
  }
}

