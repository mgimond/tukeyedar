#'@export
#'@title Plot eda_rline model
#'
#'@description \code{plot.eda_rline} A plot method for lists of \code{eda_rline}
#'  class.
#'
#'@param rline Object of class \code{eda_rline}
#'@param type Plot type. One of two: "model", "residuals".
#'@param xlab Custom x-axislabel. Defaults to column name
#'@param ylab Custom y-axis label. Defaults to column name.
#'@param col Color for point symbol. Currently set to \code{rgb(.2,.2,.2,0.5)}.
#'@param model Boolean indicating if the resulting model should be added above
#'  plot. Only applies to \code{type = "model"}
#'@param pt3 Boolean indicating if the 3-pt summaries should be added to the
#'  plot. Only applies to \code{type = "model"}
#'@param ... Arguments to be passed to subsequent methods
#'
#'@return Does not return a value.
#'
#'@details The function generates a scatter plot with the fitted model from an \code{eda_rline} object.
#'
#' @examples
#' r.lm    <- eda_rline(age_height, Months, Height)
#'
#' plot(r.lm)
#' plot(r.lm, type = "residuals")

plot.eda_rline <- function(rline, type = "model", xlab = NULL, ylab = NULL,
                           col = rgb(.2,.2,.2,0.5), model = TRUE, pt3 = TRUE, ...){
  if (!inherits(rline,"eda_rline")) stop("The input object must of class eda_rline")
  if (! type %in% c("residuals", "model" ))
    stop("Paramater \"type=\" must be of \"residuals\", or \"model\" ")

  df <- data.frame(x = rline$x, y = rline$y, res = rline$res)

  .pardef <- par(mar = c(2.8,3.2,1.5,1.5))

  # Plot the data and fit the model
  if (type == "model"){
    plot(y ~ x, df, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab="grey60",
         pch = 16, col = col)
    axis(1,col="grey60", col.axis="grey60", labels=TRUE, padj = -0.5)
    axis(2,col="grey60", col.axis="grey60", labels=TRUE, las=1, hadj = 0.7)
    mtext(rline$ylab, side=3, adj= -0.05 , col="grey60", padj = -1)
    title(xlab = rline$xlab, line =1.7, col.lab="grey60")
    if (model == TRUE){
      mtext(sprintf("y = %f + (%f)x",rline$a, rline$b ), col = "grey60")
    }
    abline(a = rline$a, b = rline$b, col="red")
    if (pt3 == TRUE){
      points(cbind(rline$xmed, rline$ymed), pch =21, bg="red", cex = 1.2)
    }
    abline(v= rline$x[rline$index],lty=3, col = "grey")
  } else {
    plot(res ~ x, df, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab="grey60",
         pch = 16, col = rgb(.2,.2,.2,0.5))
    axis(1,col="grey60", col.axis="grey60", labels=TRUE, padj = -0.5)
    axis(2,col="grey60", col.axis="grey60", labels=TRUE, las=1, hadj = 0.7)
    mtext("Residuals", side=3, adj= -0.05 , col="grey60", padj = -1)
    title(xlab = rline$xlab, line =1.7, col.lab="grey60")
    abline(v= rline$x[rline$index],lty=3, col = "grey")
    abline(h= 0,lty=3, col = "grey")
  }

  par(.pardef)
}


