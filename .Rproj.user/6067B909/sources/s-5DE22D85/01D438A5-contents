#' @export
#' @title Tukey's letter value summaries
#'
#' @description
#'  \code{eda_lsum} The letter value summary was introduced by John Tukey
#'  and extends the boxplot's 5 number summary by exploring the symmetry
#'  of the batch for depth levels other than the half (median) or the
#'  fourth (quartiles).
#'
#' @param x Vector
#' @param l Number of levels (max = 9)
#' @param all Generate upper, lower and mid summaries if TRUE or
#'             just generate mid summaries if FALSE
#'
#' @details
#'   Outputs a data frame of letter value summaries.
#'
#' @seealso
#'   \url{https://mgimond.github.io/ES218/Week08b.html}
#'
#' @references
#'    Exploratory Data Analysis, John Tukey, 1973.
#' @examples
#' x <- c(22, 8, 11, 3, 26, 1, 14, 18, 20, 25, 24)
#' eda_lsum(x)

eda_lsum <- function (x, l = 5, all = TRUE)
{
  # Limit max letter summaries to 9
  if (l > 9) {
    print("Limit level summary to 9")
    return()
  }
  # letter summary labels
  let <- c("M", "H", "E", "D", "C", "B", "A", "Z", "Y", "X")
  # Remove missing values
  x <- na.omit(x)
  # Sort values
  x <- sort(x)
  # Find depths from each end
  n <- length(x)
  Lrnk <- vector()
  Mrnk <- vector()
  Rrnk <- vector()
  Lrnk[1] <- n
  Mrnk[1] <- n
  Rrnk[1] <- n
  i = 1
  while( (i <= l) & (Lrnk[i] > 1) ){
    i=i + 1
    Lrnk[i] <- floor(Lrnk[i-1] + 1 ) /2
    Mrnk[i] <- floor(Lrnk[i])
    Rrnk[i] <- floor(Lrnk[i] + 0.5)
  }
  # Get final set of letters
  val <- factor(let[1:length(Lrnk[-1])],levels=let[1:length(Lrnk[-1])])
  # Find the summary values
  LO <- (x[Mrnk[-1]] + x[Rrnk[-1]])  / 2
  HI <- (  x[n-Mrnk[-1] + 1] + x[n-Rrnk[-1]+1] ) / 2
  MD <- (LO + HI) / 2
  SP <- HI - LO
  # Generate output
  if(all == TRUE) {
    out <- data.frame(letter=val, depth=Lrnk[-1], lower=LO,
                      mid=MD, upper=HI, spread=SP)
  } else {
    out <- data.frame(letter=val, mid=MD)
  }
  return(out)
}
