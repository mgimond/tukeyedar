#' @export
#' @title  Ladder of powers transformation on a single vector
#'
#' @description
#'  \code{eda_unipow} re-expresses a vector on the ladder of powers and plots
#'  the results using a histogram and a density function. Either the Tukey or Box-Cox
#'  transformation is used in computing the re-expressed values.
#'
#' @param x  vector
#' @param p  vector of powers
#' @param tukey if TRUE (default), apply Tukey's power transformation, if FALSE
#' adopt Box-Cox transformation.
#' @param bins number of bins in the histogram
#' @param cex.main histogram title size (assigned to each histogram plot)
#' @param col histogram fill color
#' @param border histogram border color
#' @param title Overall plot title (set to NULL for no title)
#' @param ... other parameters passed to the graphics::hist function.
#' @details
#'
#'  The output is a lattice of descriptive plots showing the transformed data
#'  across different powers.\cr
#'
#' @references
#'   Exploratory Data Analysis, by John Tukey
#'
#' @examples
#' data(mtcars)
#' eda_unipow(mtcars$mpg, bins=6)


eda_unipow <- function(x, p = c(2, 1, 1/2, 0.33, 0, -0.33, -1/2, -1, -2),
                     tukey = TRUE,
                     bins=5, cex.main=1.3,
                     col="#DDDDDD",border="#AAAAAA",
                     title="Re-expressed data via ladder of powers",
                     ...) {

  # Define the plot layout
  nRows <- max(1,floor(length(p)^(1/2)))
  nCols <- ceiling(length(p)/nRows)

  # Remove missing values
  x <- x[!is.na(x)]

  # Setup plot for each transformation
  M <- matrix(rep(1,nCols),nrow=1)
  for (i in 0: (nRows - 1) ){
    Mi <- matrix( (1:(nCols * 2)) + i * 2 * nCols +1, nrow=2, ncol=nCols, byrow=FALSE)
    M <- rbind(M,Mi)
  }
  .pardef <- par(no.readonly = T)
  OP <- par(mar = c(0,0,0,0))
  lHeight <- c(0.2, rep( c(1,0.3),nRows))
  layout(mat=M, heights= lHeight)

  # Add title to first layout panel
  plot(NA,NA,axes=FALSE,xlim=c(0,1),ylim=c(0,1))
  text(.5,.5,labels=title, cex=1.5)

  # Loop through each power
  for (i in p) {

    # Re-express values
    z <- eda_re(x,i,tukey=tukey)

    # Check that all values are finite
    if ( FALSE %in% is.finite(z)  )
      stop("\nOne or more values did not return a valid
re-expression. For example, a value of 0 will
return an error if a log transformation is chosen.")
    # Compute bin breaks
    rng.z <- diff(range(z))
    breaks <- seq(min(z)- rng.z * 0.0001,max(z)+rng.z * 0.0001,length.out=bins+1)

    # Generate plots
    OP1 <- par(mar = c(1,1,1,1))
    hist(z, breaks=breaks, probability=TRUE, col=col, main=paste("Power = ", i),xlab=NA,
         axes=FALSE,border=border,col.main="#AAAAAA",cex.main=cex.main,...)
    axis(1)
    lines(density(z), lt=1, col="red")
    par(OP1)
    OP2 <- par(mar = c(0,1,0,1))
    boxplot(z, horizontal = TRUE, col="bisque", boxwex =0.8, frame.plot=FALSE, xlab=NA, axes=FALSE, border="#777777")
    par(OP2)
  }
  par(OP)
  par(.pardef)
}
