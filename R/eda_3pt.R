#' @export
#' @import graphics
#' @title 3-point summary plot
#'
#' @description \code{eda_3pt} splits the data into 3 groups (whose summary
#' locations are defined by their respective medians), and two half slopes
#' linking the groups. The function will return a scatter plot showing the
#' half-slopes in red solid lines. The solid grey slope linking both tail-end
#' groups shows the desired shape for both half-slopes. The goal is to have the
#' two halve slopes line up as closely as possible to the solid grey slope via
#' re-expression techniques when seeking a linear relationship between both
#' variables.
#'
#' The function will also return the half-slopes ratio \code{hsrtio} and the
#' direction of re-expression for both X and Y values on the ladder of powers.
#'
#' @param dat Data frame
#' @param x   Column name assigned the x axis
#' @param y   Column name assigned the y axis
#' @param px  Power transformation to apply to the x-variable
#' @param py  Power transformation to apply to the y-variable
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param axes Boolean determining if axes are to be drawn.
#' @param dir Boolean indicating if suggested ladder of power direction should
#'   be displayed
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param equal Boolean determining if axes lengths should match (i.e. squate
#'  plot).
#' @param pch Point symbol type
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'  ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only applicable
#'  if \code{rgb()} is not used to define point colors.
#' @param ... Other parameters passed to the graphics::plot function.
#'
#' @return Generates a plot and returns a list with the following named
#'   components:
#'
#' \itemize{
#'   \item \code{hsrtio}: The ratio between both slopes. A value close to one
#'   suggests that no transformation is needed.
#'   \item \code{xmed}: The x-coordinate values for the three summary points.
#'   \item \code{ymed}: The y-coordinate values for the three summary points.}
#'
#' @details Computes the three-point summary originally defined in Tukey's EDA
#' book (see reference).
#'
#' @references
#'   \itemize{
#'    \item Velleman, P. F., and D. C. Hoaglin. 1981. Applications, Basics and Computing of Exploratory Data Analysis. Boston: Duxbury Press.
#'    \item D. C. Hoaglin, F. Mosteller, and J. W. Tukey. 1983. Understanding Robust and Exploratory Data Analysis. Wiley.
#'    \item Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley.}
#'
#' @examples
#'
#' hsratio <- eda_3pt(cars, speed, dist)
#' hsratio <- eda_3pt(cars, speed, dist, py = 1/3, ylab=expression("Dist"^{1/3}))
#'
#' #' # This next example models gas consumption as a function of engine displacement.
#' # It applies a transformation to both variables via the px and py arguments.
#' eda_3pt(mtcars, disp, mpg,  px = -1/3, py = -1,
#'        ylab = "gal/mi", xlab = expression("Displacement"^{-1/3}))
#'

eda_3pt <- function(dat, x, y, px = 1, py = 1, tukey = TRUE, axes = TRUE,
                    pch = 21, equal = TRUE, p.col = "grey50",
                    p.fill = "grey80", size = 0.8, alpha = 0.7,
                    xlab = NULL, ylab = NULL, dir = TRUE, grey = 0.6, ...){

  if(is.null(xlab)){
    xlab = as.character(substitute(x))
  }
  if(is.null(ylab)){
    ylab = as.character(substitute(y))
  }

    if(!missing(dat))
  {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  # Re-express data if required
  x <- eda_re(x, p = px, tukey = tukey)
  y <- eda_re(y, p = py, tukey = tukey)

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
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

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL)
  # y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Compute the margin width (returned in inches before converting to lines)
  # y.wid <- max( strwidth( y.labs[1], units="inches"),
  #               strwidth( y.labs[2], units="inches")) * in2line + 1


  # Set plot parameters
  if(equal == TRUE & axes == TRUE) {
    .pardef <- par(mar = c(2.8,y.wid,2.5,1.5), col = plotcol, pty = "s")
  } else if(axes == TRUE) {
    .pardef <- par(mar = c(2.8,y.wid,2.5,1.5), col = plotcol)
  } else {
    .pardef <- par(mar = c(0,0,0,0), col = plotcol)
  }

  on.exit(par(.pardef))

  # Generate plot
  plot(x, y, las = 1, xlab = NA, ylab = NA, yaxt='n', xaxt='n', pch = pch,
       col = p.col, bg = p.fill, cex = size, col.lab = plotcol,
       col.axis = plotcol, ...)
  if(axes == TRUE){
    # axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    # axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    # mtext(ylab, side=3, adj= -0.2 , col = plotcol, padj = -0.8)
    # title(xlab = xlab, line =1.8, col.lab=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)
    mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2, cex = par("cex"))
    title(xlab = xlab, line =1.8, col.lab=plotcol)
  }


  # Draw batch boundaries
  abline(v = dat2$x[ dat2$order== d[1] ], lty=3, col="grey")
  abline(v = dat2$x[ dat2$order== d[2] ], lty=3, col="grey")

  # Draw 3pt lines
  lines( cbind( c(min(x), xmed[2]),
                c( -( slope1*(xmed[1] - min(x)) - ymed[1] ), ymed[2])),
         col="red",lty=1)
  lines( cbind( c(xmed[2], max(x)),
                c(ymed[2], slope2*(max(x) - xmed[2]) + ymed[2]) ),
         col="red",lty=1 )
  lines(cbind(xmed[-2], ymed[-2]),lty=1,col="#444444")

  # Draw point summaries
  points(cbind(xmed,ymed), pch=21, bg="red", cex=1.2)

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
      mtext(text.o, side = 3, col = plotcol, cex = 0.7)
    }
    else{
      mtext("Slopes have different signs, no re-expression", side = 3,
            col = plotcol, cex = 0.7)
    }
  }
  par(.pardef)
  out <- list(slope1, slope2, hsrtio, xmed, ymed)
  names(out) <- c("slope1", "slope2","hsrtio","xmed","ymed")
  return(out)
}
