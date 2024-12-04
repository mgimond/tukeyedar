#' @export
#' @importFrom utils modifyList
#' @title Resolve Ties in a Numeric Vector
#' @description Adjusts tied values in a numeric vector by adding or subtracting
#'   a small fraction of the range.
#'
#' @param dat A data frame or a numeric vector containing values that may
#'   have ties.
#' @param x Column containing values that may have ties. Ignored if \code{dat}
#'   is a numeric vector.
#' @param fac Column of categorical values . Ignored if \code{dat} is a numeric
#'   vector.
#' @param f A numeric value specifying the fraction of the range of \code{x} to
#'   use for adjustments. Must be between 0 and 1.
#' @param rand A logical value. If \code{FALSE}, all adjustments are of fixed
#'   size based on \code{f}. If \code{TRUE}, the adjustments are randomized
#'   within the range specified by \code{f}.
#' @param ... not used.
#'
#' @details The function identifies tied values in the input vector \code{x}
#' and perturbs them to break the ties.
#' If \code{rand = TRUE}, the adjustment for each tied value is randomized
#' uniformly with the lower and uper bounds defined by
#' \code{[0, f * diff(range(x))]}. If \code{rand = FALSE}, the adjustment is
#' deterministic and equal to \code{+/- f * diff(range(x))}. Alternating signs
#' (\code{-1} and \code{1}) are used to distribute adjustments symmetrically.
#' The deterministic approach may not eliminate all ties. For example, if four
#' values are tied, the output will split the values into two tied values.
#' Repeating the process on the output as needed will eliminate all remaining
#' ties.
#'
#' @return A numeric vector of the same length as \code{x}, with ties adjusted.
#'
#' @examples
#' set.seed(42)
#' x <- c(1, 2, 2, 2, 3, 4, 4, 5)
#' # Randomized adjustments
#' x1 <- eda_untie(x, f = 0.01, rand = TRUE)
#' x1
#'
#' # Deterministic adjustments
#' x2 <- eda_untie(x, f = 0.01, rand = FALSE)
#' x2
#' x3 <- eda_untie(x2, f = 0.01, rand = FALSE)
#' x3
#'
#' # Adjusting an entire dataframe. Add up to +/- 0.5 inches to
#' # singer height values
#' set.seed(17)
#' singer <- lattice::singer
#' factor <- 0.5 / diff(range(singer$height))
#' eda_jitter(singer, height, voice.part)
#' singer$notie <- eda_untie(singer, height, voice.part, f = factor)
#' eda_jitter(singer, notie, voice.part)

eda_untie <-function(dat, x = NULL, fac = NULL, f=0.01, rand = TRUE, ...){

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  if("data.frame" %in% class(dat)){
    fac <- eval(substitute(fac), dat)
    x   <- eval(substitute(x), dat)

    if (is.null(fac))
      stop("You need to pass a valid factor column to the fac parameter")

    # split(x, fac) <- lapply(split(x, fac),
    #                         FUN = function(x, group) {
    #                                   print(group)
    #                                   eda_untie_f(x, f=f, rand=rand)},
    #                                group = names(split(x, fac)))

    split(x, fac) <- lapply(seq_along(split(x, fac)), function(i) {
      group_name <- names(split(x, fac))[i]
      group_x <- split(x, fac)[[i]]

      # Print the current group being processed
   #   print(paste(group_name,":"))

      # Process the group's data
      eda_untie_f(group_x, name = group_name, f = f, rand = rand)
    })

    return(x)
  } else {
    dat <- eda_untie_f(dat, f=f, rand=rand)
    return(dat)
  }
}

#' Used to break ties in vector element
#' @param x Vector of numeric values that may have ties.
#' @param name Group name passed to function.
#' @param f A numeric value specifying the fraction of the range of \code{x} to
#'   use for adjustments. Must be between 0 and 1.
#' @param rand A logical value. If \code{FALSE}, all adjustments are of fixed
#'   size based on \code{f}. If \code{TRUE}, the adjustments are randomized
#'   within the range specified by \code{f}.
#' @noRd

eda_untie_f <- function(x, name = NULL, f, rand){
  n <- length(x)
  # Find ties
  ties <- duplicated(x) | duplicated(x, fromLast = TRUE)
  nties <- sum(ties)

  if(is.null(name)) print(paste("There were",nties,"input ties."))
  else print(paste(name,"had",nties,"ties."))

  if(rand == FALSE){
    perc <- diff(range(x)) * f
  } else {
    perc <- runif(nties, 0, diff(range(x)) * f)
  }
  signs <- rep(c(-1, 1), length.out = nties)
  x[ties] <- x[ties] + perc * signs
  return(x)
}


