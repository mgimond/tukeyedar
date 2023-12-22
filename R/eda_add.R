#' @export
#' @import graphics
#' @title Add graphical EDA elements to existing plot
#'
#' @description \code{eda_add}  adds graphical EDA elements to a scatter plot.
#'   Currently only adds \code{eda_rline} fit and points.
#'
#' @param x Object of class \code{eda_rline}
#' @param pch Point symbol type
#' @param p.col Point color passed to \code{col}
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25)
#' @param lty Line type
#' @param l.col Line color
#'
#' @return Returns the \code{eda_rline} intercept and slope.
#'
#' \itemize{
#'   \item \code{a}: Intercept
#'   \item \code{b}: Slope}
#'
#' @details  This function adds an eda_rline slope and 3-pt summary points to an
#'   existing scatter plot.
#'
#'   \cr See the accompanying vignette \code{Resistant Line} for a detailed
#'   breakdown of the resistant line technique.
#'
#' @examples
#'
#' eda_lm(mtcars, x = wt, y = mpg)
#' Mr <- eda_rline(mtcars, x=wt, y=mpg)
#' eda_add(Mr, l.col = "blue")

eda_add <- function(x, pch = 24, p.col = "darkred", p.fill = "yellow",
                    lty = 1, l.col = "darkred"){
  if (!inherits(x,"eda_rline")) stop("The input object must of class eda_rline.")
  if(is.null(dev.list())) stop("A plot window is not present")
  abline(a = x$a, b = x$b, col = l.col)
  points(x$xmed, x$ymed, col = p.col , bg = p.fill, pch = 24)
    return(list(a = x$a, b = x$b))
}
