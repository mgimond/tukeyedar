#' @export
#' @title Create boxplots equalized by level and spread
#'
#' @description
#' \code{eda_boxls} creates boxplots conditioned on one variable while providing the
#' option to equalize levels and/or spreads.
#'
#' @param dat  Data frame
#' @param x    Column name assigned to the values
#' @param fac  Column name assigned to the factor the values are to be
#'              conditioned on
#' @param outlier Boolean indicating if outliers should be plotted
#' @param out.txt Column whose values are to be used to label outliers
#' @param type Plot type. "none" = no equalization ; "l" = equalize by level;
#'              "ls" = equalize by both level and spread
#' @param  horiz  plot horizontally (TRUE) or vertically (FALSE)
#' @param  outliers  plot outliers (TRUE) or not (FALSE)
#'
#' @keywords boxplot spread level
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

eda_boxls <- function(dat, x, fac, outlier=TRUE, out.txt, type="l", horiz=FALSE,
                      outliers=TRUE){

  # Get values and factors
  x <- eval(substitute(x), dat)
  fac <- eval(substitute(fac), dat)

  # If custom outlier label is desired, grab text
  if(!missing(out.txt)) {out.txt <- eval(substitute(out.txt), dat)}

  # EXtract boxplot parameters
  bx <- boxplot(x ~ fac, outline=outlier, plot=FALSE)
  ord <- order(bx$stats[3,])  # Grab median order for later use

  # Wich rows have outliers
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

  # Equalize levels and spreads (standardize)
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

  # Generate boxplot
  bxp(bx, pch=20, outline=outlier, horizontal=horiz,border="white",
      boxfill="bisque2", whiskcol="grey40", whisklty=1,staplecol="grey40",
      medcol="white",medlwd=3,outcol="grey40",outpch=20,
      pars=list(las=1, col.axis ="grey50",  col.lab="grey50"))
  box(col="grey80")
  axis(1,col="grey80",labels=FALSE)
  axis(2,col="grey80",labels=FALSE)

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
}
