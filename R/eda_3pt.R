#' @export
#' @import graphics
#' @title 3-point summary plot
#'
#' @description
#'  \code{eda_3pt} splits the data into 3 groups (whose summary locations are
#'  defined by their respective medians), and two half slopes linking the groups.
#'  The function will return a scatter plot showing the half-slopes in red
#'  solid lines. The solid grey slope linking both tail-end groups shows the
#'  desired shape for both half-slopes. The goal is to have the two halve slopes
#'  line up as closely as possible to the solid grey slope via re-expression techniques
#'  when seeking a linear relationship between both variables.
#'
#'  The function will also return the
#'  half-slopes ratio \code{hsrtio} and the direction of re-expression
#'  for both X and Y values on the ladder of powers.
#'
#' @param dat data frame
#' @param x   column name assigned the x axis
#' @param y   column name assigned the y axis
#' @param dir boolean indicating if suggested ladder of power direction should
#'           be displayed
#' @param adj Adjustment parameter for y label
#' @param x.lab X label for output plot
#' @param y.lab Y label for output plot
#' @param pch Plot point size as a fraction (can be larger than 1.0)
#' @param col Plot point color
#' @param ... other parameters passed to the graphics::plot function.
#'
#' @details
#' Outputs a plot showing the three point summary as well as a list of
#' parameters:
#' \itemize{
#'   \item \code{hsrtio}: The ratio between both slopes. A value close to one
#'   suggests that no transformation is needed.
#'   \item \code{xmed}: The x-coordinate values for the three summary points.
#'   \item \code{ymed}: The y-coordinate values for the three summary points.}
#'
#' \emph{References:
#'   \itemize{
#'    \item Applications, Basics and Computing of Exploratory Data Analysis,
#'    by P.F. Velleman and D.C. Hoaglin
#'    \item Understanding robust and exploratory data analysis, by D.C. Hoaglin,
#'    F. Mosteller and J.W. Tukey
#'    \item Exploratory Data Analysis, by John Tukey}}
#'
#' @examples
#'
#' hsratio <- eda_3pt(cars, speed, dist)
#' hsratio <- eda_3pt(cars, speed, dist^(1/3), y.lab=expression("Dist"^{1/3}), adj=-0.1)

eda_3pt <- function(dat, x, y, x.lab = NULL, y.lab = NULL, adj = -.12, dir = TRUE,
                   pch=20,  col="grey40", ...){

  if(is.null(x.lab)){
    x.lab = as.character(substitute(x))
  }
  if(is.null(y.lab)){
    y.lab = as.character(substitute(y))
  }

    if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  # find unique values and add index to dataframe
  x.unique <- unique(x)
  dat2 <- data.frame(x=x, y =y, order = match(x, x.unique[order(x.unique)] ) )

  # We need at least three unique x values
  if (length(x.unique) < 3) stop("You need at least 3 unique x-values.")

  # Find the three thirds
  n <- length( unique(dat2$order) ) # Get the number of unique values
  span <- floor(n/3)
  r    <- n %% 3
  # Compute the max index of each third
  if( r == 0) d <- c(span, span *2, span *3)
  if( r == 1) d <- c(span, span *2 + 1, span *3 + 1)
  if( r == 2) d <- c(span + 1, span * 2 + 1, span *3 + 2)
  # Get X medians
  xmed <- vector(length = 3)
  xmed[1] <- median( dat2$x[dat2$order %in% (1 : d[1]) ] )
  xmed[2] <- median( dat2$x[dat2$order %in% ( (d[1] + 1) : d[2]) ])
  xmed[3] <- median( dat2$x[dat2$order %in% ( (d[2] + 1) : d[3]) ])
  # Get Y medians
  ymed <- vector(length = 3)
  ymed[1] <- median( dat2$y[dat2$order %in% (1 : d[1]) ] )
  ymed[2] <- median( dat2$y[dat2$order %in% ( (d[1] + 1) : d[2]) ])
  ymed[3] <- median( dat2$y[dat2$order %in% ( (d[2] + 1) : d[3]) ])

  # Compute the two slopes
  slope1 <- (ymed[2] - ymed[1]) / (xmed[2] - xmed[1])
  slope2 <- (ymed[3] - ymed[2]) / (xmed[3] - xmed[2])

  # Generate plot
  plot(x,y,las = 1, ylab=NA, xlab= x.lab,pch=20, col=col,
       col.lab="grey65", col.axis="grey65", fg="grey65", ...)
  #  box(col="grey80")
  #  axis(1,col="grey80", col.axis="grey80", labels=TRUE)
  #  axis(2,col="grey80", col.axis="grey80", labels=TRUE, las=1)
  mtext(y.lab, side=3, adj = adj, col="grey65")
  points(cbind(xmed,ymed), pch=16,col=rgb(1,0,0,0.5), cex=1.2)
  # Draw batch boundaries
  abline(v = dat2$x[ dat2$order== d[1] ], lty=3, col="grey")
  abline(v = dat2$x[ dat2$order== d[2] ], lty=3, col="grey")
  lines( cbind( c(min(x), xmed[2]),
                c( -( slope1*(xmed[1] - min(x)) - ymed[1] ), ymed[2])),
         col="red",lty=1)
  lines( cbind( c(xmed[2], max(x)),
                c(ymed[2], slope2*(max(x) - xmed[2]) + ymed[2]) ),
         col="red",lty=1 )
  lines(cbind(xmed[-2], ymed[-2]),lty=1,col="#444444")

  # Compute half-slope ratio
  hsrtio <- slope2/slope1

  # Determine the re-expression directions on the ladder of powers
  if (dir == TRUE) {
    if( sign(slope1) == sign(slope2) )
    {
      if( hsrtio > 1) { rexpx = "up"; rexpy = "down" }
      else if( 1 > hsrtio && hsrtio > 0 ) {rexpx = "down"; rexpy = "up"}
      else if( -1 < hsrtio && hsrtio < 0 ) {rexpx = "down"; rexpy = "down"}
      else if(  hsrtio < -1 ) {rexpx = "up"; rexpy = "up"}
      else if( hsrtio == 0) {rexpx = "N/A"; rexpy = "N/A"}
      else{ rexpx = "No change"; rexpy = "No change" }   # Case where ratio equals 1 or 0
      text.o <- sprintf("X = %s, Y = %s", rexpx, rexpy)
      mtext(text.o, side = 3, col="blue")
    }
    else{
      mtext("Slopes have different signs, no re-expression", side = 3, col="blue")
    }
  }
  out <- list(slope1, slope2, hsrtio, xmed, ymed)
  names(out) <- c("slope1", "slope2","hsrtio","xmed","ymed")
  return(out)
}
