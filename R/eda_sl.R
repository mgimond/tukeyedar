#' @export
#' @title Tukey's spread-level function
#'
#' @description
#'  \code{eda_sl} The spread-level function generates a spread-level
#'    table from a univariate dataset
#'
#' @param x Categorical variable
#' @param y Continuous variable
#' @param all generate upper, lower and mid summaries if TRUE or
#             only generate mid summaries if FALSE
#'
#' @details
#'  \itemize{
#'   \item This function depends on the dplyr package.
#'   \item Note that this function is not to be confused with Bill Cleveland's
#'   spread-location function.\cr
#'   \item If x is not categorical, the output will produce many or all NA's.
#'   }
#'
#' @references
#'    Exploratory Data Analysis, John Tukey, 1973.
#' @examples
#' sl <- eda_sl(iris, Species, Sepal.Length)
#' plot(sprd ~ med, sl, pch=16)

eda_sl <- function(dat,x,y) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)}
  xx <- eval(substitute(x), dat)
  yy <- eval(substitute(y), dat)

  df1 <- data.frame(grp=xx,y=yy)
  df2 <- dplyr::arrange(df1, grp , y)
  df2 <- dplyr::group_by(df2, grp)
  df3 <- dplyr::summarize(df2, n = dplyr::n(),
                M = (n - 1) / 2 ,
                H = ( floor(M) - 1 ) / 2,
                med = log(dplyr::nth(y,M)),
                Hlo = dplyr::nth(y,floor(H)),
                Hhi = dplyr::nth(y,ceiling(dplyr::n() + 1 - H)),
                sprd = log(Hhi - Hlo) )
  df4 <- dplyr::select(df3, grp, med, sprd)
  return(data.frame(df4))
}
