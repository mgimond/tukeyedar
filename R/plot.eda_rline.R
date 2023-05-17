#'@export
#'@title Plot eda_rline model
#'
#'@description \code{plot.eda_rline} A plot method for lists of \code{eda_rline}
#'  class.
#'
#'@param x Object of class \code{eda_rline}
#'@param type Plot type. One of two: "model", "residuals".
#'@param xlab Custom x-axis label. Defaults to column name
#'@param ylab Custom y-axis label. Defaults to column name.
#'@param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#'@param equal Boolean determining if axes lengths should match (i.e. squate
#'  plot).
#'@param pch Point symbol type
#'@param p.col Color for point symbol.
#'@param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'  ranging from 21-25).
#'@param size Point size (0-1)
#'@param alpha Point transparency (0 = transparent, 1 = opaque). Only applicable
#'  if \code{rgb()} is not used to define point colors.
#'@param model Boolean indicating if the resulting model should be added above
#'  plot. Only applies to \code{type = "model"}
#'@param pt3 Boolean indicating if the 3-pt summaries should be added to the
#'  plot. Only applies to \code{type = "model"}
#'@param ... Arguments to be passed to subsequent methods
#'
#'@return Does not return a value.
#'
#'@details The function generates a scatter plot with the fitted model from an
#'  \code{eda_rline} object.
#'
#' @examples
#' r.lm    <- eda_rline(age_height, Months, Height)
#'
#' plot(r.lm)
#' plot(r.lm, type = "residuals")

plot.eda_rline <- function(x, type = "model", xlab = NULL, ylab = NULL, grey = 0.6,
                           pch = 21, equal = TRUE, p.col = "grey50",
                           p.fill = "grey80", size = 0.8, alpha = 0.7,
                           model = TRUE, pt3 = TRUE, ...){
  if (!inherits(x,"eda_rline")) stop("The input object must of class eda_rline")
  if (! type %in% c("residuals", "model" ))
    stop("Paramater \"type=\" must be of \"residuals\", or \"model\" ")

  # Build dataframe
  df <- data.frame(x = x$x, y = x$y, res = x$res)

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Set plot parameters
  if(equal == TRUE & type == "model"){
    .pardef <- par(mar = c(2.8,3.2,1.5,1.5), col = plotcol, pty = "s")
  } else {
    .pardef <- par(mar = c(2.8,3.2,1.5,1.5), col = plotcol)
  }
  on.exit(par(.pardef))


  # Plot the data and fit the model
  if (type == "model"){

    plot(y ~ x, df, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol,
         pch = pch, col = p.col, bg = p.fill, cex = size)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(x$ylab, side=3, adj= -0.05 , col=plotcol, padj = -1)
    title(xlab = x$xlab, line =1.7, col.lab=plotcol)
    if (model == TRUE){
      mtext(sprintf("y = %f + (%f)x",x$a, x$b ), col = plotcol)
    }
    abline(a = x$a, b = x$b, col="red")
    if (pt3 == TRUE){
      points(cbind(x$xmed, x$ymed), pch =21, bg="red", cex = 1.2)
    }
    abline(v= x$x[x$index],lty=3, col = "grey")
  } else {
    plot(res ~ x, df, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol,
         pch = 16, col = rgb(.2,.2,.2,0.5))
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext("Residuals", side=3, adj= -0.05 , col=plotcol, padj = -1)
    title(xlab = x$xlab, line =1.7, col.lab=plotcol)
    abline(v= x$x[x$index],lty=3, col = "grey")
    abline(h= 0,lty=3, col = "grey")
  }

  par(.pardef)
}


