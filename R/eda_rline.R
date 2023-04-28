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
#'
#' @return Returns a list of class \code{eda_rline}with the following named components:
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
#'                      each thirds}
#'
#' @details  This is an R implementation of the \code{RLIN.F} FORTRAN code in
#'   Velleman et. al's book. This function fits a robust line using a
#'   three-point summary strategy whereby the data are split into three equal
#'   length groups along the x-axis and a line is fitted to the medians defining
#'   each group via an iterative process.
#'
#'   \cr See the accompanying vignette \code{Resistant Line} for a detailed
#'   breakdown of the resistant line technique.
#'
#' @references
#'   \itemize{
#'    \item Velleman, P. F., and D. C. Hoaglin. 1981. Applications, Basics and Computing of Exploratory Data Analysis. Boston: Duxbury Press.
#'    \item D. C. Hoaglin, F. Mosteller, and J. W. Tukey. 1983. Understanding Robust and Exploratory Data Analysis. Wiley.}

#
#' @examples
#'
#' # This first example fits a resistant line to the neoplasms data
#'
#' M <- eda_rline(neoplasms, Temp, Mortality)
#' plot(Mortality~Temp, neoplasms, pch=20)
#' abline(a=M$a, b=M$b, col=rgb(1,0,0,0.5))
#'
#' # This second example uses breast cancer data from "ABC's of EDA" page 127.
#' # The output model's  parameters should closely match:  Y = -46.19 + 2.89X
#' # The plots shows the original data with a fitted resistant line (red)
#' # and a regular lm fitted line (dashed grey), and the modeled residuals.
#' # The 3-point summary dots are shown in red.
#'
#' r.lm <- eda_rline(neoplasms, Temp, Mortality)
#' r.lm
#'
#' # Check output
#' OP <- par( mfrow = c(2,1))
#'   plot(Mortality ~ Temp, neoplasms)
#'   mtext(sprintf("y = %f + (%f)x", r.lm$a, r.lm$b ))
#'   abline(a = r.lm$a, b = r.lm$b, col="red")
#'   abline( lm(Mortality ~ Temp, neoplasms), col="grey", lty=3)
#'   points(cbind(r.lm$xmed,r.lm$ymed), pch =16, col="red")
#'   abline(v= r.lm$x[r.lm$index],lty=3)
#'   plot(r.lm$res ~ r.lm$x)
#'   abline( h = 0, lty=3)
#' par(OP)
#'
#' # This next example compares children height to age.
#' # The plots shows the original data with a fitted resistant line (red)
#' # and a regular lm fitted line (dashed grey), and the modeled residuals.
#' # The 3-point summary dots are shown in red.
#' r.lm    <- eda_rline(age_height, Months, Height)
#'
# Now plot the data
#'OP <- par( mfrow = c(2,1))
#'  plot(Height ~ Months, age_height, xlab="Age (months)", ylab="Height (cm)")
#'  mtext(sprintf("y = %f + (%f)x", r.lm$a, r.lm$b ))
#'  abline(a = r.lm$a, b = r.lm$b, col="red")
#'  abline( lm(Height ~ Months, age_height), col="grey", lty=3)
#'  points(cbind(r.lm$xmed,r.lm$ymed), pch =16, col="red")
#'  abline(v= r.lm$x[r.lm$index],lty=3)
#'  plot(r.lm$res ~ r.lm$x)
#'  abline( h = 0, lty=3)
#'par(OP)
#'
#' # Andrew Siegel's pathological 9-point data set
#' r.lm <- eda_rline(nine_point, X, Y)
#'
#' OP <- par( mfrow = c(2,1))
#' plot(Y ~ X, nine_point, xlab="Age (months)", ylab="Height (cm)")
#'    mtext(sprintf("y = %f + (%f)x", r.lm$a, r.lm$b ))
#'    abline(a = r.lm$a, b = r.lm$b, col="red")
#'    abline( lm(Y ~ X, nine_point), col="grey", lty=3)
#'    points(cbind(r.lm$xmed,r.lm$ymed), pch =16, col="red")
#'    abline(v= r.lm$x[r.lm$index],lty=3)
#'    plot(r.lm$res ~ r.lm$x)
#'    abline( h = 0, lty=3)
#' par(OP)
#'
eda_rline <- function(dat,x,y){

  if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
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

  #print(sprintf("b0=%f, b1=%f",b0,b1)) # For debugging
  #print(sprintf("D0=%f, D1=%f",D0,D1)) # For debugging

  # If D0 or D1 are 0, then we already have a robust line, if not, proceed
  if (D1 !=0) {
    # D0 and D1 should have opposite signs, if not, add another delta r
    control <- 0
    while ( sign(D0) == sign(D1) && (control < 20)) {
      b0 <- b1
      D0 <- D1
      b1 <- b1 + del
      del <- del + del
      D1  <-  Delta.r(x,y,index,xmed,b1)
      #print(sprintf("D's with equal sign: b0=%f, b1=%f, D1=%f",b0,b1,D1)) # For debugging
      control <- control +1
    }

    # Interpolate between b0 and b1
    b2 <- b1 - D1 * (b1 - b0) / (D1 - D0)

    # Compute Delta r and del r for 3rd iteration
    D2 <-  Delta.r(x,y,index,xmed,b2)
    del <- D2 / deltax
    #print(sprintf("b2=%f, D2=%f",b2,D2)) # For debugging

    # Now repeat the last iteration until Delta r is less than 1% of b0
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
  out <- list(b2, a, res,x,y,xmed,ymed,index)
  names(out) <- c("b", "a", "res", "x", "y","xmed","ymed","index")
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
  out <- list(xmed, ymed, xb, dat2$x, dat2$y)
  names(out) <- c("xmed", "ymed", "index", "x","y")
  class(out) <- "eda_rline"
  return(out)
}

# See page 132 of ABC of EDA for approach to computing
# delta R

Delta.r <- function(x,y,d,xmed,b){
  rl <- median(y[1:d[1]] - b * (x[1:d[1]] - xmed[2]) )
  rr <- median(y[(d[2]+1):d[3]] - b * (x[(d[2]+1):d[3]] - xmed[2]) )
  D  <-  rr - rl
  return(D)
}


