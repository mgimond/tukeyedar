#' @export
#' @title Create boxplots equalized by level and spread
#'
#' @description \code{eda_boxls} creates boxplots conditioned on one variable
#'   while providing the option to spreads levels and/or levels.
#'
#' @param dat  Data frame
#' @param x    Column name assigned to the values
#' @param fac  Column name assigned to the factor the values are to be
#'   conditioned on
#' @param p  Power transformation to apply to variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param outlier Boolean indicating if outliers should be plotted
#' @param out.txt Column whose values are to be used to label outliers
#' @param type Plot type. "none" = no equalization ; "l" = equalize by level;
#'   "ls" = equalize by both level and spread
#' @param notch Boolean determining if notches should be added.
#' @param  horiz  plot horizontally (TRUE) or vertically (FALSE)
#' @param  outliers  plot outliers (TRUE) or not (FALSE)
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#'
#' @return {No return value}
#'
#'   #' @details
#'  \itemize{
#'   \item Note that the notch offers a 95% test of the null that the true
#'   medians are equal assuming that the distribution of each batch is
#'   approximately normal. If the notches do not overlap, we can assuming that
#'   medians are significantly different at a 0.05 level. Note that the notches
#'   do not correct for multiple comparison issues and three or more batches are
#'    plotted.}
#'
#' @examples
#'
#' # A basic boxplot (no equalization)
#' eda_boxls(mtcars,mpg, cyl, type="none", out.txt=mpg )
#'
#' # Boxplots equalized by level
#' eda_boxls(mtcars,mpg, cyl, type="l", out.txt=mpg )
#'
#' # Boxplots equalized by level and spread
#' eda_boxls(mtcars,mpg, cyl, type="ls", out.txt=mpg )
#'
#' # Hide outlier
#' eda_boxls(mtcars,mpg, cyl, type="ls", out.txt=mpg , outlier=FALSE)
#'
#' # For long factor level names, flip plot
#' eda_boxls(iris, Sepal.Length, Species, out.txt=Sepal.Length , horiz = TRUE)

eda_boxls <- function(dat, x, fac, p = 1, tukey = FALSE, outlier=TRUE,
                      out.txt = NULL, type="none", notch = FALSE, horiz=FALSE,
                      outliers=TRUE, grey = 0.6){

  # Parameters check
  if (!type %in% c("none", "l" , "ls")) stop("Argument \"type\" must be one of \"none\", \"l\" or \"ls\".")

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Get values and factors
  x <- eval(substitute(x), dat)
  fac <- eval(substitute(fac), dat)

  # If custom outlier label is desired, grab text
  if(!missing(out.txt)) {out.txt <- eval(substitute(out.txt), dat)}

  # Re-express data if required
  x <- eda_re(x, p = p, tukey = tukey)

  # Extract boxplot parameters
  bx  <- boxplot(x ~ fac, outline=outlier, plot=FALSE)
  ord <- order(bx$stats[3,])  # Grab median order for later use

  # Which rows have outliers
  if( outlier == TRUE){
    pst.dat <- paste(fac, x, sep=",")
    pst.out <- paste(bx$names[bx$group], bx$out, sep=",")
    out.row <- which(pst.dat %in% pst.out)
  }

  # Equalize levels
  if (type =="l" | type == "ls"){
    med <- bx$stats[3,]
    bx$stats <- sweep(bx$stats, 2, med, FUN="-")
    bx$conf  <- sweep(bx$conf, 2, med, FUN="-")
    bx$out <- bx$out - med[bx$group]
  }

  # Equalize levels and spreads (standardize using IQR)
  if (type == "ls"){
    sprd <- bx$stats[4,] - bx$stats[2,]
    bx$stats <- sweep(bx$stats, 2, sprd, FUN="/")
    bx$conf  <- sweep(bx$conf, 2, sprd, FUN="/")
    bx$out <- bx$out / sprd[bx$group]
  }

  # Order output by median value
  bx$stats <- bx$stats[,ord]
  bx$n <- bx$n[ord]
  bx$conf <- bx$conf[,ord]
  bx$group <- match( bx$group, ord)
  bx$names <- bx$names[ord]

  # Expand margin to accommodate row names if requested
  if( horiz==TRUE){
    .pardef <- par(mar = c(3, max(nchar(as.character(bx$names) ))/2.5 +2 ,3 , 1.5),
                   col = plotcol)
  } else {
    .pardef <- par(mar = c(3, 3 ,1.5 , 1.5), col = plotcol )
  }
  on.exit(par(.pardef), add = TRUE)


  # Generate boxplot
  bxp(bx, pch=20, outline=outlier, horizontal=horiz, border="white", notch = notch,
      boxfill="grey70", whiskcol="grey40", whisklty=1, staplecol="grey40",
      medcol="white",medlwd=3,outcol="grey40",outpch=20,
      pars=list(las=1, col.axis =plotcol,  col.lab="grey50"))
  box(col=plotcol)
#  axis(1,col=plotcol,labels=FALSE, at= bx$names)
#  axis(2,col=plotcol,labels=FALSE)

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
  par(.pardef)
}

