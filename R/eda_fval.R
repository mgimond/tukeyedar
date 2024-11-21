#' @export
#' @importFrom utils modifyList
#' @title Calculate Quantile f-values
#'
#' @description \code{eda_qq} Generates an empirical,  Normal,
#'  symmetry or Tukey mean-difference plot
#'
#' @param x  Vector whose f-values are to be computed.
#' @param q.type  Quantile algorithm.
#'
#' @details
#' For each value \eqn{i} in \code{x}, \code{eda_fval} computes the
#' cumulative probability associated with \eqn{i}. It provides a measure of the
#' position of a data point relative to the rest of the data scaled to the
#' range [0,1]. This is essentially the fraction, \eqn{f-value},  of the dataset
#' that lies below \eqn{i}. \cr
#' \cr
#' Different algorithms are used to compute the probability/f-value. This
#' function offers as option algorithm type \code{4} through \code{6} documented
#' in \code{stats::quantile}. The algorithms used are: \cr \cr
#'   \itemize{
#'     \item{\code{4}: \deqn{f = i / n,}}
#'     \item{\code{5}: \deqn{f = (i - 0.5) / n,}}
#'     \item{\code{6}: \deqn{f = i / (n + 1),}}
#'     \item{\code{7}: \deqn{f = (i  - 1) / (n - 1),}}
#'     \item{\code{8}: \deqn{f = (i - 1/3) / (n + 1/3),}}
#'     \item{\code{9}: \deqn{f = (i - 3/8) / (n + 1/4).}}
#'   }
#'   Where \eqn{f} is the fraction of values that lies below \eqn{i}, and
#'   \eqn{n} is the total number values.
#'
#' @returns A vector of \eqn{f-values} in the same order a vector \code{x}.
#'
#' @references
#' \itemize{
#'   \item{\link[stats]{quantile}}
#'   \item{\link[stats]{ppoints}}}
#'
#' @examples
#'
#'  set.seed(321)
#'  z <- runif(10, 1, 20)
#'  eda_fval(z)  # Cleveland's f-values
#'  eda_fval(z, q.type = 7)  # Algorithm used by the quantile() function.

eda_fval <- function(x, q.type = 5) {
  # Check if algorithm is between 4 and 9
  if (!q.type %in% 4:9) {
    stop("Algorithm must be a number between 4 and 9.")
  }

  # Check if x is numeric
  if (!is.numeric(x)) {
    stop("Data must be a numeric vector.")
  }

  # Sort the x
  n <- length(x)
  ord <- order(x)
  # Calculate the probabilities based on the algorithm
  fval <- switch(as.character(q.type),
                  `4` = (1:n) / n,
                  `5` = (1:n - 0.5) / n,
                  `6` = (1:n) / (n + 1),
                  `7` = ((1:n) - 1)/  (n - 1),
                  `8` = (1:n - 1/3) / (n + 1/3),
                  `9` = (1:n - 3/8) / (n + 1/4)
  )

  # Return a vector corresponding fractions
  return(fval[ord])
}

