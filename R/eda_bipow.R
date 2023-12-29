#' @export
#' @title Ladder of powers transformation on bivariate data with three-point summary plot
#'
#' @description
#'  Re-expresses a vector on the ladder of powers.
#'  Requires \code{eda_3pt()} function.
#'
#' @param dat Data frame
#' @param x   Column name assigned the x axis
#' @param y   Column name assigned the y axis
#' @param p   Vector of powers
#' @param tukey If set to TRUE, then adopt Tukey's power transformation. If FALSE,
#' adopt Box-Cox transformation.
#' @param ... Other parameters passed to the graphics::plot function.
#'
#' @details
#' Generates a matrix of scatter plots and boxplots of various
#' re-expressions of both x and y values. The 3-point summary and
#' associated half-slopes are also plotted (this function makes use
#' of the eda_3pt function). The values are re-expressed using either
#' the Tukey power transformation (default) or the Box-Cox transformation
#' (see eda_re for more information on these transformation techniques). Axes
#' labels are omitted to reduce plot clutter.
#'
#' @return {No return value}
#'
#' @references
#'   \itemize{
#'    \item Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley.}
#'
#' @examples
#'
#' data(cars)
#' # Example 1
#' eda_bipow(dat = cars, x = speed, y = dist)
#' # Custom powers
#' eda_bipow(dat = cars, x = speed, y = dist, p = c(-1, -0.5, 0, 0.5, 1))
#' # Adopt box-cox transformation
#' eda_bipow(dat = cars, x = speed, y = dist, tukey = FALSE, p = c(-1, -0.5, 0, 0.5, 1))
#'

eda_bipow <- function(dat, x, y, p = c(-1, 0, 0.5, 1, 2), tukey=TRUE, ...) {

  if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  if (length(p) != 5) stop("You must have exactly 5 power transformations in parameter p.")
  # Define the plot layout
  nRows <- 5
  nCols <- 5

  # Plot data for each transformation
  M <- matrix( 1: ((nRows + 2)*(nCols + 2)),nrow=nRows + 2, ncol = nCols + 2, byrow=TRUE)

  .pardef <- par(mar = c(0,0,0,0))
#  on.exit(par(.pardef), add = TRUE)
  on.exit(par(.pardef))

 # par(mar = c(0,0,0,0))
  lHeight <- c(0.3, rep(1, nCols),0.3 )
  lWidth <- c(0.3, rep(1, nCols),0.3 )
  layout(mat=M, widths = lWidth , heights= lHeight)
  # Add x powers to first row of panels (pad both ends of rows with no data)
  for (i in c(NA,p,NA)){
    plot(NA,NA,axes=FALSE,xlim=c(0,1),ylim=c(0,1))
    text(.5,.5,labels=as.character(i), cex=1.5,col="#888888")
  }
  # Loop through each power
  for (j in p) {
    # Plot y power
    plot(NA,NA,axes=FALSE,xlim=c(0,1),ylim=c(0,1))
    text(.5,.5,labels=as.character(j), cex=1.5,col="#888888")
    # Transform the y variable
    yj <- eda_re(y, j, tukey=tukey)
    for (i in p) {
      # Transform the x variable
      xi <-  eda_re(x,i,tukey=tukey)

     # Check that all values are finite
      if ( FALSE %in% is.finite(xi) |
           FALSE %in% is.finite(yj)) stop("\nOne or more values did not return a
finite value. Check that the selected
powers are valid for your dataset. For
example, a value of 0 will return this
error if a log transformation is
chosen.")

      datij <- data.frame(x = xi,y = yj)
      par( mar = c(0.5,0.5,0.5,0.5))
      eda_3pt(datij, x, y, dir = FALSE, axes=FALSE, xlab=NA, ylab=NA,
             p.col="#999999", size = 0.5, pch = 20, grey = 0.65,...)
   #   axis(1,labels=NA,tick = 0.01)
   #   axis(2,labels=NA,tick = 0.01)
   #   box(col="grey65")
    }
    # Plot y histogram
    #par( mar = c(0.5,0.5,0.5,0.5))
    boxplot(yj, horizontal = FALSE, col="bisque", boxwex =1.5,
            frame.plot=FALSE, xlab=NA, axes=FALSE, border="#777777")
  }
  # Plot x histograms
  for(k in c(NA,p,NA)){
    if (is.na(k)) {plot(NA,NA,axes=FALSE,xlim=c(0,1),ylim=c(0,1))}
    else{
      if(k == 0) {
        xk <- log(x)
      } else {
        xk <- (x^k - 1)/k
      }

      # Check that all values are finite
      if ( FALSE %in% is.finite(xk) ) stop("\nOne or more values did not return a
finite value. Check that the selected
powers are valid for your dataset. For
example, a value of 0 will return this
error if a log transformation is
chosen.")

      par( mar = c(0.5,0.5,0.5,0.5))
      boxplot(xk, horizontal = TRUE, col="bisque", boxwex =1.5,
              frame.plot=FALSE, xlab=NA, axes=FALSE, border="#777777")
    }
  }
  par(.pardef)
}

