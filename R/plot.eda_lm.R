#'@export
#'@title Residuals plot
#'
#'@description
#'  Generate residuals vs fitted or residuals vs dependence plot from
#'  an \code{eda_lm} class object
#'
#'@param x Object of class \code{eda_lm}.
#'@param type Type of residuals plot. Choice between residuals-fit (\code{rf})
#'  and residuals-dependence (\code{rd}) plots.
#'@param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#'@param equal Boolean determining if axes lengths should match (i.e. square
#'  plot).
#'@param pch Point symbol type.
#'@param p.col Color for point symbol.
#'@param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'  ranging from 21-25).
#'@param size Point size (0-1).
#'@param alpha Point transparency (0 = transparent, 1 = opaque). Only applicable
#'  if \code{rgb()} is not used to define point colors.
#'@param loess Boolean indicating if a loess should be fitted to the plot.
#'   function. A robust loess is used by default.
#'@param loess.d  A list of arguments passed to the \code{loess.smooth}
#'   function. A robust loess is used by default.
#'@param loe.col LOESS curve color.
#'@param xlab X label for output plot.
#'@param ylab Y label for output plot.
#'@param ... Note used.
#'
#'@return Does not return a value.
#'
#'@details The function generates a scatter plot of residuals vs dependence or
#'  residuals vs fitted values plot from a model of class \code{eda_lm}. A loess
#'  line is fitted to the data. By default, a robust loess is adopted with the
#'  \code{"symmetric"} family.
#'
#' @examples
#' M1  <- eda_lm(age_height, Months, Height)
#' plot(M1) # Residual-dependence plot
#' plot(M1, type = "rf") # Residual-fit plot


plot.eda_lm <- function(x, type = "rd", xlab = NULL, ylab = NULL, grey = 0.7,
                        pch = 21, equal = TRUE, p.col = "grey50", alpha = 0.7,
                        p.fill = "grey80", size = 0.8, loess = TRUE,
                        loe.col = rgb(.3, .3, 1, 1),
                        loess.d=list(family = "symmetric", span=0.7, degree=1), ...){

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  if (!inherits(x,"eda_lm")) stop("The input object must be of class eda_lm.\n")

  if (!type %in% c("rd", "rf")) stop("Type must be rd or rf.\n")

  # Get x and y values
  y  <- x$residuals
  if (type == "rd"){
    x1 <- x$x
    x_lab <- x$x_lab
  } else {
    x1 <- x$fitted.values
  }

  # Add to default plot list parameters
  loess.l  <- modifyList(list(span = 0.5), loess.d)

  # Get labels
  if(is.null(xlab)){
    if (type == "rd"){
      xlab = x_lab
    } else {
      xlab = "Fitted values"
    }
  }
  if(is.null(ylab)){
    ylab = "Residuals"
  }

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = x1, y = y, type = "n", xlab = "", ylab = "",
       xaxt = "n", yaxt='n', main = NULL)
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Set plot parameters
  if(equal == TRUE ){
    .pardef <- par(mar = c(3,y.wid,3,1), col = plotcol, pty = "s")
  } else {
    .pardef <- par(mar = c(3,y.wid,3,1), col = plotcol)
  }
  on.exit(par(.pardef))

  # Plot residuals
    plot(y ~ x1, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol,
         pch = pch, col = p.col, bg = p.fill, cex = size)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)
    mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2, cex = par("cex"))
    title(xlab = xlab, line =1.8, col.lab=plotcol)

    if(loess == TRUE) lines( do.call( "loess.smooth",c( list(x=x1,y=y), loess.l)),
                       col=loe.col,lw=1.5 , lty=1)
  par(.pardef)
}


