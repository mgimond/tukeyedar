#' @export
#' @import stats
#' @title Tukey's resistant line
#'
#' @description \code{eda_rline} is an R implementation of Hoaglin, Mosteller
#'   and Tukey's resistant line technique outlined in chapter 5 of
#'   "Understanding Robust and Exploratory Data Analysis" (Wiley, 1983).
#'
#' @param dat Data frame.
#' @param x   Column assigned to the x axis.
#' @param y   Column assigned to the y axis.
#' @param px  Power transformation to apply to the x-variable.
#' @param py  Power transformation to apply to the y-variable.
#' @param tukey Logical; determining if a Tukey transformation should be adopted.
#'    (FALSE adopts a Box-Cox transformation).
#' @param base Base used with the log() function if \code{px} or
#'  \code{py} is \code{0}.
#' @param maxiter Maximum number of iterations to run.
#'
#'
#' @return Returns a list of class \code{eda_rline} with the following named
#'   components:
#'
#' \itemize{
#'   \item \code{data}: Input data table with residuals
#'   \item \code{a}: Intercept
#'   \item \code{b}: Slope
#'   \item \code{residuals}: Residuals sorted on x-values
#'   \item \code{x}: Sorted x values
#'   \item \code{y}: y values following sorted x-values
#'   \item \code{xmed}: Median x values for each third
#'   \item \code{ymed}: Median y values for each third
#'   \item \code{index}: Index of sorted x values defining upper boundaries of
#'                      each thirds
#'   \item \code{xlab}: X label name
#'   \item \code{ylab}: Y label name
#'   \item \code{iter}: Number of iterations
#'   \item \code{fitted.values}: Fitted values}
#'
#' @details  This is an R implementation of the \code{RLIN.F} FORTRAN code in
#'   Velleman et. al's book. This function fits a robust line using a
#'   three-point summary strategy whereby the data are split into three equal
#'   length groups along the x-axis and a line is fitted to the medians defining
#'   each group via an iterative process. This function should mirror the
#'   built-in \code{stat::line} function in its fitting strategy but it outputs
#'   additional parameters.
#'
#'   \cr See the accompanying \href{../articles/RLine.html}{resistant line
#'   article} for a detailed breakdown of the resistant line technique.
#'
#' @references
#'   \itemize{
#'    \item Velleman, P. F., and D. C. Hoaglin. 1981. Applications, Basics and Computing of Exploratory Data Analysis. Boston: Duxbury Press.
#'    \item D. C. Hoaglin, F. Mosteller, and J. W. Tukey. 1983. Understanding Robust and Exploratory Data Analysis. Wiley.}
#'
#' @seealso \code{[plot.eda_rline]}
#'
#' @examples
#'
#' # This first example uses breast cancer data from "ABC's of EDA" page 127.
#' # The output model's  parameters should closely match:  Y = -46.19 + 2.89X
#' # The plots shows the original data with a fitted resistant line (red)
#' # and a regular lm fitted line (dashed line), and the modeled residuals.
#' # The 3-point summary dots are shown in red.
#'
#' M <- eda_rline(neoplasms, Temp, Mortality)
#' M
#'
#' # Plot the output (red line is the resistant line)
#' plot(M)
#'
#' # Add a traditional OLS regression line (dashed blue line)
#' plot(M, reg = TRUE)
#'
#' # Plot the residuals
#' plot(M, plot = "residuals")
#'
#' # This next example uses Andrew Siegel's pathological 9-point dataset to test
#' # for model stability when convergence cannot be reached.
#' M <- eda_rline(nine_point, X, Y)
#' plot(M)

eda_rline <- function (dat, x, y, px = 1, py = 1, tukey = FALSE, maxiter = 20,
                       base = exp(1))
{
  if (!missing(dat)) {
    xlab <- deparse(substitute(x))
    ylab <- deparse(substitute(y))
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }
  nodata <- c(which(is.na(x)), which(is.na(y)))
  if (length(nodata > 0)) {
    x <- x[-nodata]
    y <- y[-nodata]
    cat(length(nodata), " rows had missing values. These were removed from the plot.")
  }
  x <- eda_re(x, p = px, tukey = tukey, base = base)
  x.nan <- is.na(x)
  y <- eda_re(y, p = py, tukey = tukey, base = base)
  y.nan <- is.na(y)
  if (any(x.nan, y.nan)) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
    bad <- x.nan | y.nan
    x <- x[!bad]
    y <- y[!bad]
  }
  m <- thirds(x, y)
  xmed <- m$xmed
  ymed <- m$ymed
  index <- m$index
  x <- m$x
  y <- m$y
  deltax <- xmed[3] - xmed[1]
  b <- (ymed[3] - ymed[1])/(xmed[3] - xmed[1])
  a0 <- sum(ymed - b * xmed)/3
  res <- y - (a0 + b * x)
  mr <- thirds(x, res)
  br <- (mr$ymed[3] - mr$ymed[1])/(mr$xmed[3] - mr$xmed[1])
  cutoff <- abs(0.001 * b)
  del <- Delta.r(x, y, index, xmed, b)/deltax
  iter <- 1
  if (maxiter > 1) {
    sgn <- 0
    while (iter <= maxiter & abs(del) > cutoff) {
      if (sgn > -1 | iter == 1) {
        b_old <- b
        b <- b + br
      }
      else {
        b_old1 <- b
        b <- b - br * ((b - b_old)/(br - br_old))
        b_old <- b_old1
      }
      res <- y - (a0 + b * x)
      mr <- thirds(x, res)
      br_old <- br
      br <- (mr$ymed[3] - mr$ymed[1])/(mr$xmed[3] - mr$xmed[1])
      sgn <- sign(br_old) * sign(br)
      del <- Delta.r(x, mr$y, index, xmed, br)/deltax
      iter <- iter + 1
    }
  }
  a <- sum(mr$ymed)/3 + a0
  fitted.values <- a + b * (x)
  residuals <- y - fitted.values
  data <- data.frame(x,y,residuals)
  names(data) <- c(xlab, ylab, "residuals")
  out <- list(data = data, b = b, a = a, residuals = residuals, x = x,
              y = y, xmed = xmed, ymed = ymed, index = index, xlab = xlab,
              ylab = ylab, px = px, py = py, tukey=tukey, base = base,
              iter = iter, fitted.values = fitted.values)
  class(out) <- "eda_rline"
  return(out)
}


thirds <- function(x,y){
  # find unique values and add index to dataframe
  x.unique <- unique(x)
  # We need at least three unique x values
  if (length(x.unique) < 3) stop("You need at least 3 unique x-values.")
  # Find the three thirds
  dat2 <- data.frame(x=x, y =y, index = match(x, x.unique[order(x.unique)] ) )
  n <- length( unique(x) ) # Get the number of unique values
  span <- floor(n/3)
  r    <- n %% 3
  # Compute the max index of each third
  if( r == 0) d <- c(span, span *2, span *3)
  if( r == 1) d <- c(span, span *2 + 1, span *3 + 1)
  if( r == 2) d <- c(span + 1, span * 2 + 1, span *3 + 2)
  # Get X medians
  xmed <- vector(length = 3)
  xmed[1] <- median( dat2$x[dat2$index %in% (1 : d[1]) ] )
  xmed[2] <- median( dat2$x[dat2$index %in% ( (d[1] + 1) : d[2]) ])
  xmed[3] <- median( dat2$x[dat2$index %in% ( (d[2] + 1) : d[3]) ])
  # Get Y medians
  ymed <- vector(length = 3)
  ymed[1] <- median( dat2$y[dat2$index %in% (1 : d[1]) ] )
  ymed[2] <- median( dat2$y[dat2$index %in% ( (d[1] + 1) : d[2]) ])
  ymed[3] <- median( dat2$y[dat2$index %in% ( (d[2] + 1) : d[3]) ])
  # Get each third's boundary record number (this may not match index if there are ties)
  dat2 <- dat2[with(dat2, order(index)),] #sort table
  xb <- vector(length=3)
  xb[1] <- max(which( dat2$index %in% (1 : d[1])))
  xb[2] <- max( which( dat2$index %in% ( (d[1] + 1) : d[2]) ) )
  xb[3] <- max( which( dat2$index %in% ( (d[2] + 1) : d[3]) ))
  # Output x medians, y medians, x indices and y indices
  out <- list(xmed = xmed, ymed = ymed, index = xb,
              x = dat2$x, y = dat2$y)
  return(out)
}

# See page 132 of ABC of EDA for approach to computing
# delta R

Delta.r <- function(x, y, d, xmed, b){
  rl <- median(y[1:d[1]] - b * (x[1:d[1]] - xmed[2]) )
  rr <- median(y[(d[2]+1):d[3]] - b * (x[(d[2]+1):d[3]] - xmed[2]) )
  D  <-  rr - rl
  # print(sprintf("b=%f,  rl=%f, rr=%f",b,rl,rr)) # For debugging
  return(D)
}


