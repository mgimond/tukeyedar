#' @export
#' @importFrom utils modifyList
#'
#' @title
#'  Resolve ties in a numeric vector
#'
#' @description
#'   Adjusts tied values in a numeric vector by adding or subtracting
#'   a small fraction of the range.
#'
#' @param dat A data frame or a numeric vector.
#' @param x Numeric column. Ignored if \code{dat} is a numeric vector.
#' @param fac Column of categorical values. Ignored if \code{dat} is a numeric
#'   vector.
#' @param f A numeric value specifying the fraction of the range of \code{x} to
#'   use for perturbing tied values. Must be between 0 and 1.
#' @param rand A logical value. If \code{FALSE}, all adjustments are of fixed
#'   size based on \code{f}. If \code{TRUE}, the adjustments are randomized
#'   within the range specified by \code{f}.
#' @param ... not used.
#'
#' @details
#' The function identifies tied values in the input vector \code{x}
#' and perturbs them slightly to break the ties.
#' If \code{rand = TRUE}, the adjustment for each tied value is randomized
#' uniformly with the lower and upper bounds defined by
#' \code{[0, f * diff(range(x))]}. If \code{rand = FALSE}, the adjustment is
#' deterministic and equal to \code{+/- f * diff(range(x))}. Alternating signs
#' (\code{-1} and \code{1}) are used to distribute adjustments symmetrically.
#' The deterministic approach may not eliminate all ties. For example, if four
#' values are tied, the output will split the values into two tied values.
#' Repeating the process on the output as needed will eliminate all remaining
#' ties.
#'
#' @return
#' Returns the input numeric data with ties resolved. If `dat` is a
#' vector, a modified vector is returned. If `dat` is a data frame, a modified
#' vector corresponding to the column specified by `x` is returned.
#'
#' @examples
#' set.seed(42)
#' x <- c(1, 2, 2, 2, 3, 4, 4, 5)
#' # Randomized adjustments
#' x1 <- eda_untie(x, f = 0.01, rand = TRUE)
#' x1
#'
#' # Deterministic adjustments. Given that there are three elements sharing the
#' # same value (a value of 2 in this example), the data will need to be
#' # processed twice.
#' x2 <- eda_untie(x, f = 0.01, rand = FALSE)
#' x2
#' x3 <- eda_untie(x2, f = 0.01, rand = FALSE)
#' x3
#'
#' # Random adjustments. Add up to +/- 0.5 inches to singer height values
#' set.seed(17)
#' singer <- lattice::singer
#' factor <- 0.5 / diff(range(singer$height)) # Get fraction that covers 0.5 inches
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
    if(is.factor(fac)) fac <- droplevels(fac)
    x   <- eval(substitute(x), dat)

    if (is.null(fac))
      stop("You need to pass a valid factor column to the fac parameter")

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
    perc <- diff(range(x, na.rm = TRUE)) * f
  } else {
    perc <- runif(nties, 0, diff(range(x, na.rm = TRUE)) * f)
  }
  signs <- rep(c(-1, 1), length.out = nties)
  x[ties] <- x[ties] + perc * signs
  return(x)
}


