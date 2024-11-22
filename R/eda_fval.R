#' @export
#' @importFrom utils modifyList
#' @title Calculate Quantile f-values
#'
#' @description \code{eda_fval} Generates f-values for quantile functions
#'
#' @param x  Vector whose f-values are to be computed.
#' @param q.type  Quantile algorithm.
#'
#' @details  For each value i in \code{x}, \code{eda_fval} computes the
#'  cumulative probability associated with i. It provides a measure of the
#'  position of a data point relative to the rest of the data scaled to the
#'  range [0,1]. This is essentially the fraction, f-value,  of the dataset
#'  that lies below i. This fraction is sometimes reported as the
#'  probability or cumulative frequency. \cr
#'  \cr
#'  Different algorithms are used to compute the f-value. \code{eda_fval}
#'  offers as optional algorithm type \code{4} through \code{6} documented
#'  in \code{stats::quantile}. The algorithms used are: \cr \cr
#'   \itemize{
#'     \item \code{4}: f = i / n,
#'     \item \code{5}: f = (i - 0.5) / n,
#'     \item \code{6}: f = i / (n + 1),
#'     \item \code{7}: f = (i  - 1) / (n - 1),
#'     \item \code{8}: f = (i - 1/3) / (n + 1/3),
#'     \item \code{9}: f = (i - 3/8) / (n + 1/4).}
#'
#'   Where f is the fraction of values that lies below index i, and
#'   n is the total number values.
#'
#' @returns A vector of f-values in the same order as vector \code{x}.
#'
#' @references
#'
#'  \itemize{
#'  \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)}
#'
#' @examples
#'
#'  set.seed(321)
#'  z <- runif(10, 1, 20)
#'
#'  # William Cleveland's f-values algorithm
#'  eda_fval(z)
#'
#'  # Algorithm used by the stats::quantile() function
#'  eda_fval(z, q.type = 7)

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

