#' @export
#' @import grDevices
#' @title Least Squares regression plot (with optional LOESS fit)
#'
#' @description
#'  \code{eda_lm} generates a scatter plot with a fitted regression line.
#'  A loess line can  also be added to the plot
#'  for model comparison. The axes are scaled such that their respective
#'  standard  deviations match axes unit length.
#'
#' @param dat Data frame
#' @param x   Column assigned to the x axis
#' @param y   Column assigned to the y axis
#' @param reg Boolean indicating whether a least squares regression line
#'            should be plotted
#' @param loe Boolean indicating if a loess curve should be fitted
#' @param lm.col Regression line color
#' @param loe.col LOESS curve color
#' @param stats Boolean indicating if regression summary statistics should be
#'              displayed
#' @param plot.d A list of parameters passed to the plot function
#' @param loess.d  A list of parameters passed to the loess.smooth function
#' @param x.lab X label for output plot
#' @param y.lab Y label for output plot
#' @param ... Not used
#'
#' @seealso \code{\link[graphics]{plot}} and \code{\link[stats]{loess.smooth}} functions
#'
#' @examples
#'
#' # Add a regular (OLS) regression model and loess smooth to the data
#' eda_lm(mtcars, wt, mpg, plot.d = list(pch=16, col="blue"), loe=TRUE)
#'
#' # Modify the loess smooth by adopting a robust fit and adjusting its
#' # span and polynomial order
#' eda_lm(mtcars, wt, mpg, plot.d = list(pch=16, col="black"), loe=TRUE,
#'       loess.d=list(family = "symmetric", span=0.5, degree=2))
#'


eda_lm <- function(dat, x, y, x.lab = NULL, y.lab = NULL, reg = TRUE,
                   loe = FALSE, lm.col = rgb(1, 0.5, 0.5, 0.8),
                   loe.col = rgb(.73, .73, 1, 1), stats=FALSE,
                   plot.d=list(pch=20,col="grey40"), ..., loess.d=NULL) {

  if(is.null(x.lab)){
    x.lab = as.character(substitute(x))
  }
  if(is.null(y.lab)){
    y.lab = as.character(substitute(y))
  }

    if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  plot.l  <- as.list(plot.d)
  loess.l <- as.list(loess.d)

  M <- lm(y ~ x)

  # Plot data
  .pardef <- par(pty = "s")
  on.exit(par(.pardef))

  sd.x = sd(x,na.rm=T); sd.y = sd(y,na.rm=T)
  do.call( "plot", c( list( x=x, y=y , asp=sd.x/sd.y, ylab=NA, las=1,
                            xlab=x.lab, col.lab="grey50"),plot.l) )
  box(col="grey80")
  axis(1,col="grey80", col.axis="grey80", labels=TRUE)
  axis(2,col="grey80", col.axis="grey80", labels=TRUE, las=1)
  mtext(y.lab, side=3, adj= -0.1 , col="grey80")
  if(reg == TRUE)  abline(M, lw = 2, col = lm.col )
  abline(v=mean(x),lty=1,col="grey70")
  abline(h=mean(y), lty=1, col="grey70")
  abline(v= mean(x) + c(-sd(x),sd(x)) ,lty=2,col="grey90")
  abline(h=mean(y) + c(-sd(y),sd(y)), lty=2, col="grey90")
  if(loe == TRUE)  lines( do.call( "loess.smooth",c( list(x=x,y=y), loess.l)),
                          col=loe.col,lw=1 )
  if(stats == TRUE){
    st <- summary(M)
    mtext( sprintf("R-sq = %0.2f  Beta= %g P(beta) = %0.3f", st$r.sq,
                   st$coef[2,1] , st$coef[2,4] ), side=3, col="blue"  )
  }
  par(.pardef)
}
