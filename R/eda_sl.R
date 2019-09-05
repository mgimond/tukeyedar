#' @export
#' @title Tukey's spread-level function
#'
#' @description
#'  \code{eda_sl} The spread-level function generates a spread-level
#'    table from a univariate dataset
#'
#' @param dat Dataframe
#' @param x Categorical variable
#' @param y Continuous variable
#' @param sprd Choice of spreads. Either interquartile, `sprd = "IQR"` or
#'    fourth-spread, `sprd = "frth"` (default).
#'
#' @details
#'  \itemize{
#'   \item Note that this function is not to be confused with Bill Cleveland's
#'   spread-location function.\cr
#'   \item If x is not categorical, the output will produce many or all NA's.
#'   \item On page 59, Hoaglan et. al define the fourth-spread as the the range
#'   defined by the upper fourth and lower fourth. The `eda_lsum` function is used
#'   to compute the upper/lower fourths.
#'   }
#'
#' @references
#'    Understanding Robust and Exploratory Data Analysis, Hoaglin, David C., Frederick Mosteller, and John W. Tukey, 1983.
#' @examples
#' sl <- eda_sl(iris, Species, Sepal.Length)
#' plot(spread ~ level, sl, pch=16)

eda_sl <- function(dat, x, y, sprd = "frth") {
  xx <- eval(substitute(x), dat)
  yy <- eval(substitute(y), dat)

  y_x <- split(yy, xx)

  frth_sprd <- function(x) {
    lsum <- eda_lsum(x, l=2)
    return(lsum[2,5] - lsum[2,3])
  }

  level <- log(unlist(lapply(y_x, median)))

  if( sprd == "frth"){
    spread <- log(unlist(lapply(y_x, frth_sprd)))
  } else if(sprd == "IQR") {
    spread <- log(unlist(lapply(y_x, IQR)))
  } else {
    stop(paste(sprd, " is an invalid argument to sprd. Choose \"IQR\" or \"frth\"."),
         call. = FALSE)
  }

  df4 <- data.frame(level, spread)
  return(df4)
}
