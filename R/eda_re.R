#' @export
#' @title Re-expression function
#'
#' @description
#'  \code{eda_re} re-expresses a vector following the Tukey or box-cox transformation.
#'
#' @param x Vector
#' @param p Power transformation
#' @param tukey If set to TRUE then adopt Tukey's power transformation, if FALSE,
#' adopt Box-Cox transformation technique
#'
#' @details
#'  The `eda_re` function is used to re-express data using one of two
#'  transformation techniques: Box-Cox transformation (tukey=FALSE)or
#'  Tukey's power transformation (tukey=TRUE).
#'
#' @examples
#' x <- c(15, 28, 17, 73,  8, 83,  2)
#' eda_re(x, p=-1/3)

eda_re <- function(x, p=0, tukey=TRUE){
  if(p == 0) {
    z <- ifelse(!is.na(x), log(x), NA)
  } else if(tukey == FALSE) {
    z <- ifelse(!is.na(x), (x^p - 1)/p , NA)
  } else {
    z <- ifelse(!is.na(x), x^p , NA)
  }
  return(z)
}
