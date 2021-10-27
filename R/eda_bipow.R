#' @export
#' @title Ladder of powers transformation on bivariate data with three-point summary plot
#'
#' @description
#'  To re-express a vector on the ladder of powers.
#'  Requires eda_3pt() function.
#'
#' @param dat data frame
#' @param x   column name assigned the x axis
#' @param y   column name assigned the y axis
#' @param p   vector of powers
#' @param tukey if set to TRUE then adopt Tukey's power transformation, if FALSE,
#' adopt Box-Cox transformation technique
#' @param ... other parameters passed to the graphics::plot function.
#'
#' @references
#'   Applications, Basics and Computing of Exploratory Data Analysis,
#'    by P.F. Velleman and D.C. Hoaglin
#'   Understanding robust and exploratory data analysis, by D.C. Hoaglin,
#'    F. Mosteller and J.W. Tukey
#'   Exploratory Data Analysis, by John Tukey
#'

eda_bipow <- function(x,y,dat, p = c(3, 2, 1, .5, 0),tukey=TRUE, ...) {

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
  .pardef <- par(no.readonly = T)
  par(mar = c(0,0,0,0))
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
    yj <- eda_re(y,j,tukey=tukey)
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
      eda_3pt(datij,x,y, dir=FALSE,axes=FALSE,x.lab=NA, y.lab=NA,
             col="#999999",cex=0.5,...)
      axis(1,labels=NA,tick = 0.01)
      axis(2,labels=NA,tick = 0.01)
      box(col="grey65")
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
