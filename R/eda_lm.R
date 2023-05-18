#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Least Squares regression plot (with optional LOESS fit)
#'
#' @description \code{eda_lm} generates a scatter plot with a fitted regression
#'   line. A loess line can  also be added to the plot for model comparison. The
#'   axes are scaled such that their respective standard  deviations match axes
#'   unit length.
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
#' @param sd Boolean determining if standard deviation lines should be plotted
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param pch Point symbol type
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if grey quantile boxes should be plotted
#' @param q.val F-values to use to define the quantile box parameters. Defaults
#'   to mid 68% of values. If more than 2 f-values are defined, the first two
#'   are used to generate the box.
#' @param q.type Quantile type. Defaults to 5 (Cleveland's f-quantile
#'   definition)
#' @param loe Boolean indicating if a loess curve should be fitted
#' @param lm.col Regression line color
#' @param loe.col LOESS curve color
#' @param stats Boolean indicating if regression summary statistics should be
#'   displayed
#' @param loess.d  A list of parameters passed to the \code{loess.smooth}
#'   function. A robust loess is used by default.
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param ... Not used
#'
#' @return Returns a list from the  `lm` output.
#'
#' \itemize{
#'   \item \code{a}: Intercept
#'   \item \code{b}: Slope
#'   \item \code{residuals}: Regression model residuals}
#'
#' @seealso \code{\link[graphics]{plot}} and \code{\link[stats]{loess.smooth}}
#'   functions
#'
#' @examples
#'
#' # Add a regular (OLS) regression model and loess smooth to the data
#' eda_lm(mtcars, wt, mpg, loe = TRUE)
#'
#' # Add the inner 68% quantile to compare the true 68% of data to the SD
#' eda_lm(mtcars, wt, mpg, loe = TRUE, q = TRUE)
#'
#' # Show the IQR box
#' eda_lm(mtcars, wt, mpg, loe = TRUE, q = TRUE, sd = FALSE, q.val = c(0.25,0.75))
#'
#' # Fit an OLS to the Income for Female vs Male
#' df2 <- read.csv("https://mgimond.github.io/ES218/Data/Income_education.csv")
#' eda_lm(df2, x=B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             loe = TRUE)
#'
#' # Add the inner 68% quantile to compare the true 68% of data to the SD
#' eda_lm(df2, x = B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             q = TRUE)
#'
#' # Apply a transformation to x and y axes: x -> 1/3 and y -> log
#' eda_lm(df2, x = B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             px = 1/3, py = 0, q = TRUE, loe = TRUE)
#'


eda_lm <- function(dat, x, y, xlab = NULL, ylab = NULL, px = 1, py = 1,
                   tukey = FALSE, reg = TRUE, w=NULL, sd = TRUE, grey = 0.6,
                   pch = 21, p.col = "grey50", p.fill = "grey80", size = 0.8,
                   alpha = 0.8, q = FALSE, q.val = c(0.16,0.84), q.type = 5,
                   loe = FALSE, lm.col = rgb(1, 0.5, 0.5, 0.8),
                   loe.col = rgb(.3, .3, 1, 1), stats=FALSE,
                   loess.d=list(family = "symmetric", span=0.7, degree=1), ...) {

  if(is.null(xlab)){
    xlab = as.character(substitute(x))
  }
  if(is.null(ylab)){
    ylab = as.character(substitute(y))
  }

  # Re-express data if required
    if(!missing(dat))
  {
    x <- eda_re(eval(substitute(x), dat), p = px, tukey = tukey)
    y <- eda_re(eval(substitute(y), dat), p = py, tukey = tukey)
  }

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Add to default plot list parameters
  loess.l  <- modifyList(list(span = 0.5), loess.d)

  # Run regression model
  M <- lm(y ~ x, weights = w)

  # Plot data
  .pardef <- par(pty = "s", col = plotcol)
  on.exit(par(.pardef))

  sd.x = sd(x,na.rm=T); sd.y = sd(y,na.rm=T)
  plot( x=x, y=y , asp=sd.x/sd.y, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
        col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
  box(col=plotcol)
  axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
  mtext(ylab, side=3, adj= -0.1 , col=plotcol, padj = -1)
  sq <- par("usr") # get plot corners
  if (sd == TRUE){
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
  }
  title(xlab = xlab, line =1.8, col.lab=plotcol)
  if(reg == TRUE)  abline(M, lw = 2, col = lm.col )
  abline(v=mean(x),lty=1,col="grey70")
  abline(h=mean(y), lty=1, col="grey70")
  if (sd == TRUE){
    abline(v= mean(x) + c(-sd(x),sd(x)) ,lty=2,col="grey80")
    abline(h=mean(y) + c(-sd(y),sd(y)), lty=2, col="grey80")
  }
  if(loe == TRUE)  lines( do.call( "loess.smooth",c( list(x=x,y=y), loess.l)),
                          col=loe.col,lw=2 , lty=2)
  if(stats == TRUE){
    st <- summary(M)
    mtext( sprintf("R-sq = %0.2f  Beta= %g P(beta) = %0.3f", st$r.sq,
                   st$coef[2,1] , st$coef[2,4] ), side=3, col="blue"  )
  }

  if(q == TRUE){
  #  st <- summary(M)
    qy <- quantile(y, q.val, type = q.type)
    qx <- quantile(x, q.val, type = q.type)
    rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
         col = rgb(0,0,0,0.1), border = NA)
    rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
         col = rgb(0,0,0,0.1), border = NA)
  }

  par(.pardef)
  print(coef(M))
  invisible(list(residuals = residuals(M), a = coef(M)[1], b = coef(M)[2]))
}
