#' @export
#' @name eda_trim
#'
#' @title Trims vector and dataframe objects
#'
#' @description Removes records from either tail-ends of a sorted dataset.
#' Trimming can be performed by number of records (specify the \code{num =}
#' option) or by quantiles (specify the \code{prop=} option). \cr\cr
#' \code{eda_trim} Trims a vector \cr \code{eda_trim_df} Trims a data frame \cr
#' \code{eda_ltrim} Left-trims a vector \cr \code{eda_rtrim} Right-trims a
#' vector \cr \code{eda_ltrim_df} Left-trims a dataframe \cr \code{eda_rtrim_df}
#' Right-trims a dataframe \cr
#'
#' @return Returns the same data type as the input (i.e. vector or dataframe)
#'
#' @details
#'  \itemize{
#'  \item The input dataset does not need to be sorted (sorting is performed in the
#'  functions).
#'  \item If \code{num} is set to zero, then the function will assume that the
#'  trimming is to be done by fraction (defined by the \code{prop} parameter).
#'  \item If \code{eda_trim} or \code{eda_trim_df} functions are called, the
#'  \code{num} and \code{prop} values apply to each tail. For example, if
#'  \code{num = 5} then the 5 smallest AND 5 largest values are removed from the
#'  data.
#'  \item \code{NA} values must be stripped from the input vector or
#'  column elements before running the trim functions.
#'  \item Elements are returned sorted on the trimmed elements.
#'  }
#'
#' @param  dat   Dataframe (applies to \code{*_df} functions only)
#' @param  x     Vector of values (if trimming a vector) or the column whose
#'   values are used to trim a dataframe (applies to \code{*_df} functions only)
#' @param  prop  Fraction of values to trim
#' @param  num   Number of values to trim
#'
#' @keywords trim
#'
#' @examples
#'
#' # Trim a vector by 10% (i.e. 10% of the smallest and 10% of the largest
#' # values)
#' eda_trim( mtcars[,1], prop=0.1)
#'
#' # Trim a data frame by 10% using the mpg column(i.e. 10% of the smallest
#' # and 10% of the largest mpg values)
#' eda_trim_df( mtcars, mpg, prop=0.1)
#' @rdname eda_trim
#' @export
eda_trim <- function(x,prop=.05, num = 0) {
  stopifnot( num * 2 <= length(x), prop < 0.5)
  if ( num > 0){
    x     <- sort(x)
    trimx <- x[ (1+num):(length(x)-num)]
  } else{
    lprop <- quantile(x, prob=prop, na.rm = TRUE)
    rprop <- quantile(x, prob=1-prop, na.rm = TRUE)
    trimx <- x[ (x >= lprop) & (x <= rprop)  ]
    trimx <- sort(trimx)
  }
  return(trimx)
}

#' @rdname eda_trim
#' @export
eda_trim_df <- function(dat, x, prop=.05, num = 0) {
  x <- eval(substitute(x), dat)
  stopifnot( num * 2 <= length(x), prop < 0.5)
  if ( num > 0){
    dat   <- dat[order(x),] # sort by column
    trimx <- dat[(1+num):(length(x)-num), ]
  } else{
    dat   <- dat[order(x),]
    x <- sort(x)
    lprop <- quantile(x, prob=prop, na.rm = TRUE)
    rprop <- quantile(x, prob=1-prop, na.rm = TRUE)
    trimx <- dat[ (x >= lprop) & (x <= rprop),  ]
  }
  return(trimx)
}

#' @rdname eda_trim
#' @export
eda_ltrim <- function(x, prop=.05, num = 0) {
  stopifnot( num <= length(x))
  if ( num > 0){
    x <- sort(x)
    trimx <- x[-(1:num)]
  } else{
    trimx <- x[x > quantile(x,prob=prop, na.rm = TRUE)]
    trimx <- sort(trimx)
  }
  return(trimx)
}

#' @rdname eda_trim
#' @export
eda_ltrim_df <- function(dat, x, prop=.05, num = 0) {
  x <- eval(substitute(x), dat)
  stopifnot( num <= length(x))
  if ( num > 0){
    dat   <- dat[order(x),]  # sort by column
    trimx <- dat[-(1:num),]
  } else{
    dat   <- dat[order(x),]
    x     <- sort(x)
    trimx <- dat[ x > quantile(x,prob=prop, na.rm = TRUE), ]
  }
  return(trimx)
}

#' @rdname eda_trim
#' @export
eda_rtrim <- function(x, prop=.05, num = 0) {
  stopifnot( num <= length(x))
  if ( num > 0){
    x <- sort(x)
    trimx <- x[1:(length(x)-num) ]
  } else{
    trimx <- x[x < quantile(x,prob=(1 - prop), na.rm = TRUE)]
    trimx <- sort(trimx)
  }
  return(trimx)
}

#' @rdname eda_trim
#' @export
eda_rtrim_df <- function(dat, x,prop=.05, num = 0) {
  x <- eval(substitute(x), dat)
  stopifnot( num <= length(x))
  if ( num > 0){
    dat   <- dat[order(x),]  # sort by column
    trimx <- dat[1:(length(x)-num), ]
  } else{
    dat   <- dat[order(x),]
    x     <- sort(x)
    trimx <- dat[ x < quantile(x,prob=(1 - prop), na.rm = TRUE), ]
  }
  return(trimx)
}
