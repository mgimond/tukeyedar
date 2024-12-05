#' @export
#' @import MASS
#' @importFrom utils modifyList
#' @title Regression plot (with optional LOESS fit)
#'
#' @description \code{eda_lm} generates a scatter plot with a fitted regression
#'   line. A loess line can  also be added to the plot for model comparison. The
#'   axes are scaled such that their respective standard  deviations match axes
#'   unit length.
#'
#' @param dat Dataframe.
#' @param x   Column assigned to the x axis.
#' @param y   Column assigned to the y axis.
#' @param px  Power transformation to apply to the x-variable.
#' @param py  Power transformation to apply to the y-variable.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param show.par Boolean determining if power transformation should be
#'   displayed in the plot.
#' @param reg Boolean indicating whether a least squares regression line should
#'   be plotted.
#' @param poly Polynomial order.
#' @param robust Boolean indicating if robust regression should be used.
#' @param w Weight to pass to regression model.
#' @param sd Boolean determining if standard deviation lines should be plotted.
#' @param mean.l Boolean determining if the x and y mean lines should be added
#'   to the plot.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1).
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if shaded region showing the mid-portion of the
#' data should be added to the plot.
#' @param q.val Upper and lower bounds of the shaded region shown by \code{q}.
#'   Defaults to the mid 68% of values. Upper and lower values are defined by
#'   fractions ranging from 0 to 1 and are passed to the argument via a
#'   \code{c()} function. Defaults to \code{c(0.16,0.84)}.
#' @param q.type Quantile type. Defaults to 5 (Cleveland's f-quantile
#'   definition).
#' @param loe Boolean indicating if a loess curve should be fitted.
#' @param lm.col Regression line color.
#' @param loe.col LOESS curve color.
#' @param stats Boolean indicating if regression summary statistics should be
#'   displayed.
#' @param stat.size Text size of stats output in plot.
#' @param loess.d  A list of arguments passed to the \code{loess.smooth}
#'   function. A robust loess is used by default.
#' @param rlm.d A list of arguments passed to the \code{MASS::rlm}
#'   function.
#' @param asp Boolean determining if the plot aspect ratio should equal the
#'   ratio of the x and y standard deviations. A value of \code{FALSE} defaults
#'   to the base plot's default aspect ratio. A value of \code{TRUE} uses the
#'   aspect ratio \code{sd(x)/sd(y)}.
#' @param xlab X label for output plot.
#' @param ylab Y label for output plot.
#' @param ... Not used.
#'
#' @details The function will plot a regression line and, if requested, a loess
#'   fit. The function adopts the least squares fitting technique by default. It
#'   defaults to a first order polynomial fit. The polynomial order can be
#'   specified via the \code{poly} argument.
#'   \cr\cr
#'   The plot displays the +/- 1 standard deviations as dashed lines. In
#'   theory, if both x and y values follow a perfectly Normal distribution,
#'   roughly 68 percent of the points should fall in between these lines.
#'   \cr \cr
#'   The true 68 percent of values can be displayed as a shaded region by
#'   setting \code{q=TRUE}. It uses the \code{quantile} function to compute
#'   the upper and lower bounds defining the inner 68 percent of values. If the
#'   data follow a Normal distribution, the grey rectangle edges should coincide
#'   with the +/- 1SD dashed lines.
#'   If you wish to show the interquartile ranges (IQR) instead of the inner
#'   68 percent of values, simply set \code{q.val = c(0.25,0.75)}.
#'   \cr \cr
#'   The plot has the option to re-express the values via the \code{px} and
#'   \code{py} arguments. But note that if the re-expression produces \code{NaN}
#'    values (such as if a negative value is logged) those points will be
#'    removed from the plot. This will result in fewer observations being
#'    plotted. If observations are removed as result of a re-expression a
#'    warning message will be displayed in the console.
#'    The re-expression powers are shown in the upper right side of the plot. To
#'   suppress the display of the re-expressions set \code{show.par = FALSE}.
#'   \cr\cr
#'   If the \code{robust} argument is set to TRUE, \code{MASS}'s
#'   built-in robust fitting model, \code{rlm}, is used to fit the regression
#'   line to the data. \code{rlm} arguments can be passed as a list via the
#'   \code{rlm.d} argument.
#'
#' @return Returns a list of class \code{eda_lm}. Output includes the following
#'    if \code{reg = TRUE}. Returns \code{NULL} otherwise.
#'
#' \itemize{
#'   \item \code{residuals}: Regression model residuals
#'   \item \code{a}: Intercept
#'   \item \code{b}: Polynomial coefficient(s)
#'   \item \code{fitted.values}: Fitted values}
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
#' # Fit an OLS to income for Female vs Male
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
#' # Fit a second order polynomial
#' eda_lm(mtcars, hp, mpg, poly = 2)
#'
#' # Fit a robust regression model
#' eda_lm(mtcars, hp, mpg, robust = TRUE, poly = 2)


eda_lm <- function(dat, x, y, xlab = NULL, ylab = NULL, px = 1, py = 1,
                   tukey = FALSE, show.par = TRUE, reg = TRUE, poly = 1,
                   robust = FALSE,  w=NULL, sd = TRUE, mean.l = TRUE,asp = TRUE,
                   grey = 0.6, pch = 21, p.col = "grey50", p.fill = "grey80",
                   size = 0.8, alpha = 0.8, q = FALSE, q.val = c(0.16,0.84),
                   q.type = 5, loe = FALSE, lm.col = rgb(1, 0.5, 0.5, 0.8),
                   loe.col = rgb(.3, .3, 1, 1), stats=FALSE, stat.size = 0.8,
                   loess.d=list(family = "symmetric", span=0.7, degree=1),
                   rlm.d = list(psi = "psi.bisquare"), ...){

  if(is.null(xlab)){
    xlab = as.character(substitute(x))
  }
  if(is.null(ylab)){
    ylab = as.character(substitute(y))
  }

  if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  # Remove missing rows
  nodata <- unique(c(which(is.na(x)), which(is.na(y))))
  if(length(nodata > 0)){
    x <-  x[-nodata]
    y <-  y[-nodata]
    cat(length(nodata), " rows had missing values. These were removed from the plot.\n")
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
  if(robust == FALSE) {
    #M <- lm(y ~ x, weights = w)
    if (poly > 0){
      M <- lm(y ~ poly(x, degree = poly, raw = TRUE), weights = w)
    } else {
      M <- lm(y ~ 1, weights = w)
    }
  } else {
    if (poly > 0){
      rlm.d <- modifyList(list(formula = y ~ poly(x, degree=poly, raw=TRUE)), rlm.d)
      M <- do.call(MASS::rlm, rlm.d)
    } else
    rlm.d <- modifyList(list(formula = y ~ 1), rlm.d)
    M <- do.call(MASS::rlm, rlm.d)
  }

  # Modify ylim as needed to ensure that the full extent of the fitted model
  # fits in the plot window
  ylim = range(y, predict(M))

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL, ylim = ylim)
  #y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Compute the margin width (returned in inches before converting to lines)
  # y.wid <- max( strwidth( y.labs[1], units="inches"),
  #               strwidth( y.labs[2], units="inches")) * in2line + 1

  .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
  on.exit(par(.pardef))

  sd.x <- sd(x, na.rm=T)
  sd.y <- sd(y, na.rm=T)
  mean.x <- mean(x, na.rm=T)
  mean.y <- mean(y, na.rm=T)

  asp_val <- ifelse(asp == TRUE, sd.x/sd.y, NA) # See if x and y SDs need to match

  plot( x=x, y=y , asp=asp_val, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
        col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
        ylim = ylim)
  box(col=plotcol)
  axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
       tck = -0.02)
  mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2, cex = par("cex"))

  sq <- par("usr") # get plot corners
  if (sd == TRUE){
    ysd1 <- (mean.y + sd.y)
    ysd2 <- (mean.y - sd.y)
    text( label="+1sd", x= sq[2] - diff(sq[1:2]) * 0.03, y= ysd1 + diff(sq[3:4])*0.02,
          srt=0, col="grey70",  cex=0.7)
    text( label="-1sd", x= sq[2] - diff(sq[1:2]) * 0.03, y= ysd2 + diff(sq[3:4])*0.02,
          srt=0, col="grey70",  cex=0.7)
    text( label="+1sd", y= sq[4] - diff(sq[3:4]) * 0.01, x= (mean.x + sd.x) ,
          srt=0, col="grey70",  cex=0.7)
    text( label="-1sd", y= sq[4] - diff(sq[3:4]) * 0.01, x= (mean.x - sd.x) ,
          srt=0, col="grey70",  cex=0.7)
  }
  title(xlab = xlab, line =1.8, col.lab=plotcol)
  if(reg == TRUE) {
    #abline(M, lw = 2, col = lm.col )
    line_x <- seq(min(x), max(x), length.out=300)
    line_y <- predict(M, newdata = data.frame(x = line_x))
    lines(line_x, line_y, lw = 2, col = lm.col)
  }
  if (mean.l == TRUE){
    abline(v=mean.x,lty=1,col="grey70")
    abline(h=mean.y, lty=1, col="grey70")
  }
  if (sd == TRUE){
    abline(v= mean.x + c(-sd.x,sd.x) ,lty=2,col="grey80")
    abline(h=mean.y + c(-sd.y,sd.y), lty=2, col="grey80")
  }
  if(loe == TRUE)  lines( do.call( "loess.smooth",c( list(x=x,y=y), loess.l)),
                          col=loe.col,lw=2 , lty=2)
  if(stats == TRUE){
    st <- summary(M)
    mtext( sprintf("R-sq = %0.2f  Beta= %g P(beta) = %0.3f", st$r.sq,
                   st$coef[2,1] , st$coef[2,4] ),
           side=3, col="blue", cex=stat.size  )
  }

  if(q == TRUE){
  #  st <- summary(M)
    qy <- quantile(y, q.val, type = q.type, na.rm=TRUE)
    qx <- quantile(x, q.val, type = q.type, na.rm=TRUE)
    rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
         col = rgb(0,0,0,0.1), border = NA)
    rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
         col = rgb(0,0,0,0.1), border = NA)
  }

  if(show.par == TRUE){
    params <- paste0("px=",round(px,2),"\n py=",round(py,2))
    mtext(side = 3, text=params, adj=1, cex = 0.65)
  }

  # Reset plot parameters
  par(.pardef)

  # Output residuals and coefficients if regression is set to TRUE
  if(reg == TRUE) {
    out_coef <- coef(M)
    if (poly > 0){
      names(out_coef) <- c("int", paste0(xlab, "^", 1:poly))
    } else {
      names(out_coef) <- c("int")
    }

    print(out_coef)
    lst <- list(residuals = residuals(M),
                a = out_coef[1],
                b = out_coef[-1],
                fitted.values = predict(M))
    class(lst) <- "eda_lm"
    invisible(lst)
  } else {
    print(NULL)
  }
}
