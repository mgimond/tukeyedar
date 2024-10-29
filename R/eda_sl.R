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
#'   spread-location function. A descripion of this plot can be found on page
#'   77 of *Hoaglan et. al's* book.\cr
#'   \item If \code{fac} is not categorical, the output will produce many or all NA's.
#'   \item On page 59, Hoaglan et. al define the fourth-spread as the the range
#'   defined by the upper fourth and lower fourth. The \code{eda_lsum} function is used
#'   to compute the upper/lower fourths.
#'   }
#'
#' @references Understanding Robust and Exploratory Data Analysis, Hoaglin,
#' David C., Frederick Mosteller, and John W. Tukey, 1983.
#' @examples
#' sl <- eda_sl(iris, Petal.Length, Species)
#'
#' # The output can be passed to a model fitting function like eda_lm
#' # The output slope can be used to help identify a power transformation
#' # The suggested power transformation is 1 - slope.
#' eda_lm(sl, Level, Spread)
#'


eda_sl <- function(dat, x, fac, p = 1, tukey = FALSE, sprd = "frth",
                   plot = TRUE, grey = 0.6, pch = 21, p.col = "grey50",
                   p.fill = "grey80", size = 1,  alpha = 0.8) {

  # Parameters check
  if (!sprd %in% c("frth", "IQR")) stop("Argument \"sprd\" must be one of \"frth\" or \"IQR\".",
                                        call. = FALSE)

  # Get values
  x   <- eval(substitute(x), dat)
  fac <- eval(substitute(fac), dat)

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


  # Split data into groups
  x_fac <- split(x, fac)

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

  df4 <- data.frame(Level = level, Spread = spread)

  # Generated plot (if requested)
    if(plot == TRUE){

    # Get lines-to-inches ratio
    in2line <- ( par("mar") / par("mai") )[2]

    # Create a dummy plot to extract y-axis labels
    pdf(NULL)
    plot(x = level, y = spread, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt='n', main = NULL)
    # y.labs <- range(axTicks(2))
    y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
    dev.off()

    # Compute the margin width (returned in inches before converting to lines)
    # y.wid <- max( strwidth( y.labs[1], units="inches"),
    #               strwidth( y.labs[2], units="inches")) * in2line + 1

    .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
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
