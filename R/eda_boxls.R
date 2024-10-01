#' @export
#' @import lattice
#' @title Boxplots equalized by level and spread
#'
#' @description \code{eda_boxls} creates boxplots conditioned on one variable
#'   while providing the option to level the data or equalize the spreads.
#'
#' @param dat  Data frame
#' @param x    Column name assigned to the values
#' @param fac  Column name assigned to the factor the values are to be
#'   conditioned on
#' @param p  Power transformation to apply to variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param outlier Boolean indicating if outliers should be plotted
#' @param out.txt Column whose values are to be used to label outliers. If
#'   set to \code{NULL} (the default), the record number is displayed.
#' @param type Plot type. "none" = no equalization ; "l" = equalize by level;
#'   "ls" = equalize by both level and spread
#' @param notch Boolean determining if notches should be added.
#' @param horiz  plot horizontally (TRUE) or vertically (FALSE)
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param fill Boxplot fill color
#' @param boxcol Boxplot outline color
#' @param outcol Outlier color
#' @param whiskcol Whisker line color
#' @param medcol Median line color
#' @param reorder Boolean determining if factors have to be reordered based
#'   on median, upper quartile or lower quartile (set in \code{reorder.type}).
#' @param reorder.stat Statistic to reorder level by if \code{reorder} is set to
#'   \code{TRUE}. Either \code{"median"}, \code{"upper"} (for upper quartile) or
#'   \code{"lower"} (for lower quartile). If \code{type} is set to a value other
#'   than \code{"none"}, the this argument is ignored and the stat defaults to
#'   \code{"median"}.
#' @param show.par Boolean determining if power transformation should be
#'   displayed in the plot.
#'
#' @return {No values are returned}
#'
#' @details
#'  \itemize{
#'   \item By default, the boxplots are re-ordered by their median values.
#'   \item If the outlier text to be displayed is its own value, it will not be
#'    modified if the data are equalized by level or spread.
#'   \item Note that the notch offers a 95 percent test of the null that the true
#'   medians are equal assuming that the distribution of each batch is
#'   approximately normal. If the notches do not overlap, we can assume that
#'   medians are significantly different at a 0.05 level. Note that the notches
#'   do not correct for multiple comparison issues when three or more batches are
#'   plotted.}
#'
#' @examples
#'
#' # A basic boxplot. The outlier is labeled with the row number by default.
#' eda_boxls(mtcars,mpg, cyl, type="none")
#'
#' # A basic boxplot. The outlier is labeled with its own value.
#' eda_boxls(mtcars,mpg, cyl, type="none", out.txt=mpg )
#'
#' # Boxplot equalized by level. Note that the outlier text is labeled with its
#' # original value.
#' eda_boxls(mtcars,mpg, cyl, type="l", out.txt=mpg )
#'
#' # Boxplots equalized by level and spread
#' eda_boxls(mtcars,mpg, cyl, type="ls", out.txt=mpg )
#'
#' # Hide outlier
#' eda_boxls(mtcars,mpg, cyl, type="ls", out.txt=mpg , outlier=FALSE)
#'
#' # Equalizing level helps visualize increasing spread with increasing
#' # median value
#' eda_boxls(iris, Sepal.Length, Species, type = "l",  out.txt=Sepal.Length)
#'
#' # For long factor level names, flip plot
#' eda_boxls(iris, Sepal.Length, Species, out.txt=Sepal.Length , horiz = TRUE)
#'
#' # By default, plots are ordered by their medians.
#' singer <- lattice::singer
#' eda_boxls(singer, height, voice.part, out.txt=height, horiz = TRUE)
#'
#' # To order by top quartile, set reorder.stat to "upper"
#' eda_boxls(singer, height, voice.part, out.txt=height, horiz = TRUE,
#'           reorder.stat = "upper")

eda_boxls <- function(dat, x, fac, p = 1, tukey = FALSE, outlier=TRUE,
                      out.txt = NULL, type="none", notch = FALSE, horiz=FALSE,
                      xlab = NULL, ylab = NULL, grey = 0.6, fill = "grey70",
                      boxcol = NULL, outcol="grey40", whiskcol="grey40",
                      medcol="grey40", reorder=TRUE, reorder.stat="median",
                      show.par = TRUE){

  # Parameters check
  if (!type %in% c("none", "l" , "ls"))
    stop("Argument \"type\" must be one of \"none\", \"l\" or \"ls\".")
  if (!reorder.stat %in% c("median", "upper" , "lower"))
    stop("Argument \"reorder.stat\" must be one of \"median\", \"upper\" or
         \"lower\".")

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Get axes labels
  if(is.null(xlab)){
    xlab = substitute(fac)
  }
  if(is.null(ylab)){
    ylab = substitute(x)
  }

  # Get values and factors
  x   <- eval(substitute(x), dat)
  fac <- as.factor(eval(substitute(fac), dat))

  # Reorder levels if requested
  if(reorder == TRUE){
    if(reorder.stat == "lower") {
      stat <- tapply(x, fac, function(x) quantile(x,probs=0.25))
    } else if (reorder.stat == "upper"){
      stat <- tapply(x, fac, function(x) quantile(x,probs=0.75))
    } else {
      stat <- tapply(x, fac, median )
    }
    new_levels <- names(sort(stat))
    fac <- factor(fac, levels = new_levels)
  }

  # If custom outlier label is desired, grab text
  if(!missing(out.txt)) {out.txt <- eval(substitute(out.txt), dat)}

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

  # Extract boxplot parameters
  bx  <- boxplot(x ~ fac, outline=outlier, plot=FALSE)

  # Which rows have outliers
  if( outlier == TRUE){
    # pst.dat <- paste(fac, x, sep=",")
    # pst.out <- paste(bx$names[bx$group], bx$out, sep=",")
    # out.row <- which(pst.dat %in% pst.out)
    out.row <- sapply(seq_along(bx$out), function(i)
      which(x == bx$out[i] & as.integer(fac) == bx$group[i]))
  }

  # Equalize levels
  if (type =="l" | type == "ls"){
    med <- bx$stats[3,]
    bx$stats <- sweep(bx$stats, 2, med, FUN="-")
    bx$conf  <- sweep(bx$conf, 2, med, FUN="-")
    bx$out <- bx$out - med[bx$group]
    if (type == "l"){
      ylab <- paste(ylab, "(equalized by level)")
    }
  }

  # Equalize levels and spreads (standardize using IQR)
  if (type == "ls"){
    sprd <- bx$stats[4,] - bx$stats[2,]
    bx$stats <- sweep(bx$stats, 2, sprd, FUN="/")
    bx$conf  <- sweep(bx$conf, 2, sprd, FUN="/")
    bx$out <- bx$out / sprd[bx$group]
    ylab <- paste(ylab, "(equalized by level/spread)")
  }

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  bxp(bx, pch=20, outline=outlier, horizontal=horiz)
  if(horiz == TRUE){
    fac.names <- unique(as.character(fac))
    fac.min <- which.min(nchar(fac.names))
    fac.max <- which.max(nchar(fac.names))
    y.labs <- c(fac.names[fac.min], fac.names[fac.max])
    y.wid <- max(strwidth( y.labs[1], units="inches"),
                 strwidth( y.labs[2], units="inches")) * in2line + 1.2
  } else {
    # y.labs <- range(axTicks(2))
    y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  }
  dev.off()

  # Compute the margin width (returned in inches before converting to lines)
  # y.wid <- max( strwidth( y.labs[1], units="inches"),
  #               strwidth( y.labs[2], units="inches")) * in2line + 1.2


 # .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3.2,1))
  .pardef <- par(pty = "m", col = plotcol, mar = c(3,y.wid,3.2,1))
  on.exit(par(.pardef), add = TRUE)


  # Generate boxplot
  bxp(bx, pch=20, outline=outlier, horizontal=horiz, border=NA, notch = notch,
      boxfill=fill, boxcol = boxcol, whiskcol=whiskcol, whisklty=1,
      staplecol="grey40", medcol=medcol,medlwd=4,outcol=outcol,outpch=20,
      yaxt='n', xaxt='n', pars=list(las=1, col.axis =plotcol,  col.lab="grey50"))
  box(col=plotcol)

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

  # Add 0 line
  if(type != "none"){
    if(horiz == FALSE){
      abline(h=0,col="grey90",lty=2,lw=0.5)
    } else {
      abline(v=0,col="grey90",lty=2,lw=0.5)
    }
  }

  # Add outlier name if desired
  if(outlier == TRUE & length(bx$out) > 0 ){
    if (missing(out.txt)){
      if(horiz == FALSE){
        text(bx$group, bx$out, as.character(out.row),pos=4, cex=0.7)
      } else {
        text(bx$out, bx$group, as.character(out.row),pos=3, cex=0.7)
      }
    } else{
      if(horiz == FALSE){
        text(bx$group, bx$out, as.character(out.txt[out.row]), pos=4, cex=0.7)
      } else {
        text(bx$out, bx$group, as.character(out.txt[out.row]), pos=3, cex=0.7)
      }
    }
  }

  if(show.par == TRUE){
    params <- paste0("p=",round(p,2))
    mtext(side = 3, text=params, adj=1, cex = 0.65)
  }

  par(.pardef)

  if(type != "none"){
    message(paste0("========================\n",
                   "Note that the data have been equalized with \"type\" set ",
                   "to \"",type,"\".\n",
                   "========================\n"))
  }
}




