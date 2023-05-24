#' @export
#' @title Tukey's spread-level function
#'
#' @description The \code{eda_sl} function generates a spread-level table from a
#' univariate dataset.
#'
#' @param dat Dataframe
#' @param x Continuous variable column
#' @param fac Categorical variable column
#' @param p  Power transformation to apply to variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param sprd Choice of spreads. Either interquartile, \code{sprd = "IQR"} or
#'   fourth-spread, \code{sprd = "frth"} (default).
#' @param plot Boolean determining if plot should be generated.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#'
#' @return Returns a dataframe of level and spreads.
#'
#' @details
#'  \itemize{
#'   \item Note that this function is not to be confused with William Cleveland's
#'   spread-location function.\cr
#'   \item If \code{fac} is not categorical, the output will produce many or all NA's.
#'   \item On page 59, Hoaglan et. al define the fourth-spread as the the range
#'   defined by the upper fourth and lower fourth. The \code{eda_lsum} function is used
#'   to compute the upper/lower fourths.
#'   }
#'
#' @references Understanding Robust and Exploratory Data Analysis, Hoaglin,
#' David C., Frederick Mosteller, and John W. Tukey, 1983.
#' @examples
#' eda_sl(iris, Sepal.Length, Species)


eda_sl <- function(dat, x, fac, p = 1, tukey = FALSE, sprd = "frth",
                   plot = TRUE, grey = 0.6, pch = 21, p.col = "grey50",
                   p.fill = "grey80", size = 1,  alpha = 0.8) {

  # Parameters check
  if (!sprd %in% c("frth", "IQR")) stop("Argument \"sprd\" must be one of \"frth\" or \"IQR\".",
                                        call. = FALSE)

  # Get values
  x   <- eval(substitute(x), dat)
  fac <- eval(substitute(fac), dat)

  # Split data into groups
  x_fac <- split(x, fac)

  # Re-express data if required
  x <- eda_re(x, p = p, tukey = tukey)

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Custom function
  frth_sprd <- function(x) {
    lsum <- eda_lsum(x, l=2)
    return(lsum[2,5] - lsum[2,3])
  }

  level <- log(unlist(lapply(x_fac, median)))

  if( sprd == "frth"){
    spread <- log(unlist(lapply(x_fac, frth_sprd)))
  } else if(sprd == "IQR") {
    spread <- log(unlist(lapply(x_fac, IQR)))
  }

  df4 <- data.frame(level, spread)

  # Plot if requested
  if(plot == TRUE){
    .pardef <- par(pty = "s", col = plotcol)
    on.exit(par(.pardef))

    plot( x=level, y=spread , ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext("Spread", side=3, adj= -0.1 , col=plotcol, padj = -1)
    title(xlab = "Level", line = 1.8, col.lab=plotcol)
    par(.pardef)
  }

  return(df4)
}
