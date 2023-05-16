#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Least Squares regression plot (with optional LOESS fit)
#'
#' @description \code{eda_lm} generates a scatter plot with a fitted regression
#' line. A loess line can  also be added to the plot for model comparison. The
#' axes are scaled such that their respective standard  deviations match axes
#' unit length.
#'
#' @param dat Data frame
#' @param x   Column assigned to the x axis
#' @param y   Column assigned to the y axis
#' @param px  Power transformation to apply to the x-variable
#' @param py  Power transformation to apply to the y-variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param reg Boolean indicating whether a least squares regression line should
#'   be plotted
#' @param w Weight to pass to regression model
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param loe Boolean indicating if a loess curve should be fitted
#' @param lm.col Regression line color
#' @param loe.col LOESS curve color
#' @param stats Boolean indicating if regression summary statistics should be
#'   displayed
#' @param plot.d A list of parameters passed to the plot function
#' @param loess.d  A list of parameters passed to the loess.smooth function
#' @param x.lab X label for output plot
#' @param y.lab Y label for output plot
#' @param ... Not used
#'
#' @return Returns the intercept and the slope of the fitted `lm` model.
#'
#' @seealso \code{\link[graphics]{plot}} and \code{\link[stats]{loess.smooth}}
#'   functions
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
#' # Apply a log transformation to the x an y axes
#' df2 <- read.csv("https://mgimond.github.io/ES218/Data/Income_education.csv")
#' eda_lm(df2, x=B20004013, y=B20004007, plot.d = list(col=rgb(0,0,0,0.1)),
#'             loe = TRUE, px = 1/3, py = 0)


eda_lm <- function(dat, x, y, x.lab = NULL, y.lab = NULL, px = 1, py = 1,
                   tukey = FALSE, reg = TRUE, w=NULL, grey = 0.5,
                   loe = FALSE, lm.col = rgb(1, 0.5, 0.5, 0.8),
                   loe.col = rgb(.73, .73, 1, 1), stats=FALSE,
                   plot.d=list(pch=20, col=rgb(0,0,0,0.4)), ...,
                   loess.d=list(span=0.5)) {

  if(is.null(x.lab)){
    x.lab = as.character(substitute(x))
  }
  if(is.null(y.lab)){
    y.lab = as.character(substitute(y))
  }

    if(!missing(dat))
  {
    x <- eda_re(eval(substitute(x), dat), p = px, tukey = TRUE)
    y <- eda_re(eval(substitute(y), dat), p = py, tukey = TRUE)
  }

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Add to default plot list parameters
  plot.l   <- modifyList(list(pch=20, col=rgb(0,0,0,0.4)), plot.d)
  loess.l  <- modifyList(list(span = 0.5), loess.d)

  # Run regression model
  M <- lm(y ~ x, weights = w)

  # Plot data
  .pardef <- par(pty = "s")
  on.exit(par(.pardef))

  sd.x = sd(x,na.rm=T); sd.y = sd(y,na.rm=T)
  do.call( "plot", c( list( x=x, y=y , asp=sd.x/sd.y, ylab=NA, las=1, yaxt='n',
                           xaxt='n', xlab=NA, col.lab=plotcol), plot.l) )
  box(col=plotcol)
  axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
  mtext(y.lab, side=3, adj= -0.1 , col=plotcol, padj = -1)
  sq <- par("usr") # get plot corners
  ysd1 <- (mean(y) + sd(y))
  ysd2 <- (mean(y) - sd(y))
  text( label="+1sd", x= sq[2] - diff(sq[1:2]) * 0.03, y= ysd1 + diff(sq[3:4])*0.02,
        srt=0, col="grey70",  cex=0.7)
  text( label="-1sd", x= sq[2] - diff(sq[1:2]) * 0.03, y= ysd2 + diff(sq[3:4])*0.02,
        srt=0, col="grey70",  cex=0.7)
  text( label="+1sd", y= sq[4] - diff(sq[3:4]) * 0.01, x= (mean(x) + sd(x)) ,
        srt=0, col="grey70",  cex=0.7)
  text( label="-1sd", y= sq[4] - diff(sq[3:4]) * 0.01, x= (mean(x) - sd(x)) ,
        srt=0, col="grey70",  cex=0.7)
  title(xlab = x.lab, line =1.7, col.lab=plotcol)
  if(reg == TRUE)  abline(M, lw = 2, col = lm.col )
  abline(v=mean(x),lty=1,col="grey70")
  abline(h=mean(y), lty=1, col="grey70")
  abline(v= mean(x) + c(-sd(x),sd(x)) ,lty=2,col="grey80")
  abline(h=mean(y) + c(-sd(y),sd(y)), lty=2, col="grey80")
  if(loe == TRUE)  lines( do.call( "loess.smooth",c( list(x=x,y=y), loess.l)),
                          col=loe.col,lw=2 , lty=2)
  if(stats == TRUE){
    st <- summary(M)
    mtext( sprintf("R-sq = %0.2f  Beta= %g P(beta) = %0.3f", st$r.sq,
                   st$coef[2,1] , st$coef[2,4] ), side=3, col="blue"  )
  }
  par(.pardef)
  return(coef(M))
}
