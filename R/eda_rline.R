#' @export
#' @import stats
#' @title Tukey's resistant line
#'
#' @description \code{eda_rline} is an R implementation of Hoaglin, Mosteller
#'   and Tukey's resistant line technique outlined in chapter 5 of
#'   "Understanding Robust and Exploratory Data Analysis" (Wiley, 1983).
#'
#' @param dat Data frame
#' @param x   Column assigned to the x axis
#' @param y   Column assigned to the y axis
#' @param px  Power transformation to apply to the x-variable
#' @param py  Power transformation to apply to the y-variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#'
#' @return Returns a list of class \code{eda_rline}with the following named
#'   components:
#'
#' \itemize{
#'   \item \code{a}: Intercept
#'   \item \code{b}: Slope
#'   \item \code{res}: Residuals sorted on x-values
#'   \item \code{x}: Sorted x values
#'   \item \code{y}: y values following sorted x-values
#'   \item \code{xmed}: Median x values for each third
#'   \item \code{ymed}: Median y values for each third
#'   \item \code{index}: Index of sorted x values defining upper boundaries of
#'                      each thirds
#'   \item \code{xlab}: X label name
#'   \item \code{ylab}: Y label name
#'   \item \code{iter}: Number of iterations}
#'
#' @details  This is an R implementation of the \code{RLIN.F} FORTRAN code in
#'   Velleman et. al's book. This function fits a robust line using a
#'   three-point summary strategy whereby the data are split into three equal
#'   length groups along the x-axis and a line is fitted to the medians defining
#'   each group via an iterative process. This function should mirror the
#'   built-in \code{stat::line} function in its fitting strategy but it outputs
#'   additional parameters.
#'
#'   \cr See the accompanying vignette \code{Resistant Line} for a detailed
#'   breakdown of the resistant line technique.
#'
#' @references
#'   \itemize{
#'    \item Velleman, P. F., and D. C. Hoaglin. 1981. Applications, Basics and Computing of Exploratory Data Analysis. Boston: Duxbury Press.
#'    \item D. C. Hoaglin, F. Mosteller, and J. W. Tukey. 1983. Understanding Robust and Exploratory Data Analysis. Wiley.}
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
#' # Plot the output
#' plot(M)
#' abline(lm(Mortality ~ Temp, neoplasms), lty = 3)
#'
#' # Plot the residuals
#' plot(M, type = "residuals")
#'
#' # This next example models gas consumption as a function of engine displacement.
#' # It applies a transformation to both variables via the px and py arguments.
#' eda_3pt(mtcars, disp, mpg,  px = -1/3, py = -1,
#'        ylab = "gal/mi", xlab = expression("Displacement"^{-1/3}))
#'
#' # This next example uses Andrew Siegel's pathological 9-point dataset to test
#' # for model stability when convergence cannot be reached.
#' M <- eda_rline(nine_point, X, Y)
#' plot(M)
#'
#'
eda_rline <- function(dat, x, y, px = 1, py = 1, tukey = TRUE){

  if(!missing(dat))
  {
    xlab <- deparse(substitute(x))
    ylab <- deparse(substitute(y))
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  # Re-express data if required
  x <- eda_re(x, p = px, tukey = tukey)
  x.nan <- is.na(x)
  y <- eda_re(y, p = py, tukey = tukey)
  y.nan <- is.na(y)


  # Re-expression may produce NaN values. Output warning if TRUE
  if( any(x.nan, y.nan) ) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
    bad <- x.nan | y.nan
    x <- x[!bad]
    y <- y[!bad]

  }

  # Get medians and sorted dataset
  m     <- thirds(x,y)
  xmed  <- m$xmed
  ymed  <- m$ymed
  index <- m$index

  x <- m$x
  y <- m$y

  # Compute delta x (a constant throughout the code)
  deltax <- xmed[3] - xmed[1]

  # Step 1
  # Compute the first slope
  b0 <- (ymed[3] - ymed[1]) / (xmed[3] - xmed[1])

  # Find the cuttoff, this is where the final diff between D0 and D1 is 0.1% of b0
  cutoff <- abs(0.001 * b0)

  # Compute Delta r and del r for first iteration
  D0  <-  Delta.r(x,y,index,xmed,b0)
  del <- D0 / deltax

  # Step 2
  # Add del r to b0
  b1 <- b0 + del

  # Compute Delta r and del r for second iteration
  D1 <-  Delta.r(x,y,index,xmed,b1)

  # print(sprintf("D0=%f, D1=%f, b0=%f, b1=%f",D0, D1, b0,b1)) # For debugging
  # print(sprintf("D0=%f, D1=%f",D0,D1)) # For debugging
  count <- 0

  # If D0 or D1 are 0, then we already have a robust line, if not, proceed
  if (round(D1,8) != 0 ) {
    # D0 and D1 should have opposite signs, if not, add another delta r
    control <- 0
    while ( sign(D0) == sign(D1) && (control < 20)) {
      b0 <- b1
      D0 <- D1
      b1 <- b1 + del
      del <- del + del
      D1  <-  Delta.r(x,y,index,xmed,b1)
      control <- control +1
    }

    # Interpolate between b0 and b1
    b2 <- b1 - D1 * (b1 - b0) / (D1 - D0)

    # Compute Delta r and del r for 3rd iteration
    D2 <-  Delta.r(x,y,index,xmed,b2)
    del <- D2 / deltax
    # print(sprintf("del=%f, b2=%f, D2=%f",del,b2,D2)) # For debugging

    # Now repeat the last iteration until Delta r is less than 0.1% of b0
    count <- 0

    while ( (abs(del) > cutoff) && (count < 20)){
      # Narrow the interval, assign b2 to b0 or b1 depending on sign of D2
      if( sign(D2) == sign(D1)) {
        b1 <- b2
        D1 <- D2
      }else{
        b0 <- b2
        D0 <- D2
      }

      b2 <- b1 - D1 * (b1 - b0) / (D1 - D0)
      D2 <-  Delta.r(x,y,index,xmed,b2)
      del <- D2 / deltax
      # print(sprintf("Count=%i, b2=%f, D2=%f, del=%f",count, b2,D2,del)) # For debugging
      count <- count + 1
    }
  }else{
    b2 <- b1
  }

  # Compute the new intercept (following procedure outlined
  # on page 158 of ABC of EDA)
   a   <- median( y - b2 * x )

  # Compute final residuals
  res <- y - (a + b2 * x)

  # Output (include sorted y's and x's)
  out <- list(b=b2, a=a, res=res, x=x, y=y, xmed=xmed, ymed=ymed,
              index = index, xlab = xlab, ylab=ylab, px= px, py=py,
              iter = count + 3)
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


