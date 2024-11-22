#' @export
#' @title Jitter plot
#'
#' @description \code{eda_jitter} creates a jitter plot from a continuous
#'  variable conditioned on a categorical variable.
#'
#' @param dat  Dataframe.
#' @param x    Continuous variable.
#' @param fac  Categorical variable.
#' @param jitter Jittering parameter. A fraction of the group width
#'   (\code{0} to \code{1}).
#' @param p  Power transformation to apply to variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (\code{FALSE} adopts a Box-Cox transformation).
#' @param horiz  Plot horizontally (\code{TRUE}) or vertically (\code{FALSE}).
#' @param show.stat Boolean determining if a summary statistic should be added
#'   to jitter plot.
#' @param stat Choice of summary statistic to use when fitting a central value
#'   to the data. \code{stat} can be either \code{mean} or \code{median}. DO
#'   NOT wrap stat parameter in quotes.
#' @param stat.type Symbol to use to display statistical summary. Can be either
#'   a point, \code{"p"}, or a line, \code{"l"}.
#' @param stat.col Symbol color to use to display statistical summary. If
#'   \code{stat.type} is a point, then the color will be passed to its outline
#'   if the point symbol type, \code{stat.pch}, is 21 through 25.
#' @param stat.fill Fill color to use for the point statistical summary if the
#'   point symbol type, \code{stat.pch}, is 21 through 25. Ignored if
#'   \code{stat.type = "l"}.
#' @param stat.size Size of point statistical summary if \code{stat.type = "p"},
#'   or width of line if \code{stat.type = "l"}.
#' @param stat.pch Point statistical summary type (1 through 25). Ignored if
#'   \code{stat.type = "l"}.
#' @param stat.pad Fraction to add to the length of the line statistical
#'   summary. Ignored if \code{stat.type = "p"}.
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param pch Point symbol type
#' @param p.col Color for point symbol
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1).
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param reorder Boolean determining if factors have to be reordered based
#'   on \code{stat}.
#' @param show.par Boolean determining if power transformation should be
#'   displayed in the plot.
#' @param ylim Define custom y axis limits (or x limits if \code{horiz=TRUE}).
#'
#' @return {No values are returned}
#'
#' @details
#' Function generates jitter plot from a univariate dataset. If no categorical
#' variable is passed to the function, a single jitter plot is created from the
#' data. \cr
#' \cr
#' By default, the plots are ordered based on the statistic, \code{stat}, used to generate
#' the central value.
#'
#' @examples
#'
#' # A basic jitter plot for a single variable where the mean is automatically
#' # fitted to the data
#' eda_jitter(iris, Sepal.Width)
#'
#' # A basic jitter plot conditioned on a categorical variable
#' eda_jitter(iris, Sepal.Width, Species)
#'
#' # Use line instead of point for summary statistic
#' eda_jitter(iris, Sepal.Width, Species, stat.type = "l")
#'
#' # You can apply a transformation to the data. The summary statistic will be
#' # computed after the data are transformed.
#' eda_jitter(mtcars, hp, cyl, stat.type = "l", p = 0)

eda_jitter <- function(dat, x, fac=NULL , jitter = 0.05, p = 1, tukey = FALSE,
                      horiz=FALSE, stat = mean, show.stat = TRUE, stat.type = "p",
                      stat.col = "firebrick", stat.fill = "bisque", stat.size = 2,
                      stat.pch = 21, stat.pad = 1, xlab = NULL, ylab = NULL,
                      grey = 0.6, pch = 21, p.col = "grey50", p.fill = "grey80",
                      size = 0.8, alpha = 0.8, reorder=TRUE,
                      show.par = TRUE, ylim = NULL){

  # Parameters check
  if (!as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Argument \"stat\"  must be either the mean or the median (without quotes).")
  if (!stat.type %in% c("p", "l"))
    stop("Stat symbol \"stat.type\" must be \"l\" or \"p\"")


  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Check if factor is NULL
  chk_fac <- (eval(substitute(fac), dat))

  # Get axes labels
  if(is.null(xlab)){
    if(!is.null(chk_fac)){
      xlab = substitute(fac)
    } else {
      xlab = ""
    }
  }

  if(is.null(ylab)){
    ylab = substitute(x)
  }

  # Get values and factors
  x   <- eval(substitute(x), dat)

  if(!is.null(chk_fac)){
    fac <- as.factor(eval(substitute(fac), dat))
  } else {
    fac <- as.factor(rep(" ", length(x)))
  }

  # Reorder levels if requested
  if(reorder == TRUE && length(levels(fac)) > 1){
    if (!as.character(substitute(stat)) == "mean"){
      ord.stat <- tapply(x, fac, mean )
    } else {
      ord.stat <- tapply(x, fac, median )
    }
    new_levels <- names(sort(ord.stat))
    fac <- factor(fac, levels = new_levels)
  }

  # Get rank number for factor
  fac.order <- as.numeric(fac)

  # Re-express data if required
  x <- eda_re(x, p = p, tukey = tukey)
  x.nan <- is.na(x)
  if( any(x.nan)){
    x <- x[!x.nan]
    fac <- fac[!x.nan]
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
  }

  # Get axis limits
  if(is.null(ylim)){
    ylim <- range(x)
  }

  xlim <- c(0.5, length(levels(fac)) + 0.5)

  # Create stat point symbol list
  xp <-  fac.order
  yp <-  ave(x, fac, FUN=stat)
  if(horiz == TRUE){
    stat.point <- list(x = yp, y = xp, col = stat.col, bg = stat.fill,
                       pch = stat.pch, cex = stat.size)
  } else {
    stat.point <- list(x = xp, y = yp, col = stat.col, bg = stat.fill,
                       pch = stat.pch, cex = stat.size)
  }

  # Create stat line segment list
  x0   <-  fac.order - jitter
  x1   <-  fac.order + jitter
  x0x1 <- abs(x1-x0)
  x0   <- x0 - stat.pad * x0x1 # Pad line segment
  x1   <- x1 + stat.pad * x0x1 # Pad line segment
  y0 <-  ave(x, fac, FUN=stat)
  y1 <-  y0
  if(horiz == TRUE){
    stat.line <- list(x0 = y0, x1 = y1, y0 = x0, y1 = x1,
                      lwd = stat.size * 2, col = stat.col)
  } else {
    stat.line <- list(x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                      lwd = stat.size * 2, col = stat.col)
  }

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  if(horiz == TRUE){
    plot(x = x, y = fac.order, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt='n', main = NULL, ylim = xlim, xlim = ylim)
    y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  } else {
    plot(x = fac.order, y = x, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt='n', main = NULL, ylim = ylim, xlim = xlim)
    y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  }
  dev.off()

  .pardef <- par(pty = "m", col = plotcol, mar = c(3,y.wid,3.2,1))
  on.exit(par(.pardef), add = TRUE)

  # Generate jitter plot
  if (horiz == TRUE){
    plot(x=x, y=jitter(fac.order, amount = jitter) , ylab=NA, las=1,
         yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol, pch = pch, col = p.col,
         bg = p.fill, cex = size, ylim = xlim, xlim = ylim)
  } else {
    plot(y=x, x=jitter(fac.order, amount = jitter) ,  ylab=NA, las=1,
           yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol, pch = pch, col = p.col,
          bg = p.fill, cex = size, ylim = ylim, xlim = xlim)
  }

  if(show.stat == TRUE){
    if(stat.type == "p"){
        do.call(points, stat.point)
    } else {
      do.call(segments, stat.line)
    }
  }

  if (horiz == TRUE){
    xlab2 <- xlab
    xlab <- ylab
    ylab <- xlab2
  }

  if (horiz == TRUE){
    axis(1, col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2, at=1:length(levels(fac)), col=plotcol, col.axis=plotcol,
         labels=levels(fac), las=1, hadj = 0.9, tck = -0.02)
  } else {
    axis(1, at=1:length(levels(fac)), col=plotcol, col.axis=plotcol,
         labels=levels(fac), padj = -0.5)
    axis(2, col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)
  }

#  mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2)
  mtext(ylab, side=3, adj= -0.01 ,col=plotcol,  padj = -1.1, cex = par("cex"))
  title(xlab = xlab, line =1.8, col.lab=plotcol)

  if(show.par == TRUE){
    params <- paste0("p=",round(p,2))
    mtext(side = 3, text=params, adj=1, cex = 0.65)
  }

  par(.pardef)

}




