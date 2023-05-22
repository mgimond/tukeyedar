#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Generates empirical QQ plot and Tukey mean-difference plot
#'
#' @description \code{eda_qq} generates an empirical QQ plot or a Tukey
#'   mean-difference plot
#'
#' @param x   Column assigned to the x axis.
#' @param y   Column assigned to the y axis.
#' @param px  Power transformation to apply to the x-variable.
#' @param py  Power transformation to apply to the y-variable.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
#' @param md Boolean determining if Tukey mean-difference plot should be
#'   generated.
#' @param plot Boolean determining if plot should be generated.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if grey quantile boxes should be plotted
#' @param q.val F-values to use to define the quantile box parameters. Defaults
#'   to mid 68% of values. If more than 2 f-values are defined, the first two
#'   are used to generate the box.
#' @param q.type Quantile type. Defaults to 5 (Cleveland's f-quantile
#'   definition)
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param ... Not used
#'
#' @value Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{x}: X values. May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{px}.
#'   \item \code{b}: Yvalues. May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{py}.
#'   \item \code{px}: Re-expression applied to original X values.
#'   \item \code{py}: Re-expression applied to original Y values.
#'
#'
#' @examples
#'
#' # Compare "Tenor 1" and "Bass 2" singer height batches
#'  singer <- lattice::singer
#'  bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )
#'  tenor1 <- subset(singer, voice.part == "Tenor 1", select = height, drop = TRUE )
#'  eda_qq(bass2, tenor1, xlab="bass 2", ylab="tenor 1")
#'
#'  # There seems to be an additive offset of about 2 inches
#'  eda_qq(tenor1, bass2 - 2,  xlab="bass 2", ylab="tenor 1")
#'
#'  # We can fine-tune by generating the Tukey mean-difference plot
#'  eda_qq(tenor1, bass2 - 2, xlab="bass 2", ylab="tenor 1", md = TRUE)
#'
#'  # An offset of another 0.5 inches seems warranted
#'  eda_qq(tenor1, bass2 - 2.5, xlab="bass 2", ylab="tenor 1", md = TRUE)
#'
#'  # We can also apply the offset to the x variable
#'  eda_qq(tenor1 + 2.5, bass2, xlab="bass 2", ylab="tenor 1", md = TRUE)
#'
#'  # Suppress plot and output values to object
#'  out <- eda_qq(tenor1, bass2, plot = FALSE)

eda_qq <- function(x, y, px = 1, py = 1,  q.type = 5, tukey = FALSE, md = FALSE, plot = TRUE,
                   grey = 0.6, pch = 21, p.col = "grey50", p.fill = "grey80",
                   size = 0.8, alpha = 0.8, q = TRUE, q.val = c(0.25,0.75),
                   xlab = NULL, ylab = NULL, ...) {
  # Parameters check
  if (!is.numeric(x)) stop("X needs to be numeric")
  if (!is.numeric(y)) stop("Y needs to be numeric")

  # Define labels
  if(is.null(xlab)){
    xlab = "X"
  }
  if(is.null(ylab)){
    ylab = "Y"
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

  # Generate qqplot using base function
  qq <- qqplot(x,y, plot.it = FALSE, qtype = q.type)
  x <- qq$x
  y <- qq$y

  # Plot data
  .pardef <- par(pty = "s", col = plotcol)
  on.exit(par(.pardef))

  if(plot == TRUE & md == FALSE){


    # Get quantile parameters
    qx <- quantile(x, q.val, qtype = q.type)
    qy <- quantile(y, q.val, qtype = q.type)
    medx <- median(x)
    medy <- median(y)

    # Generate plot
    xylim <- range(x,y)
    plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
          xlim = xylim, ylim = xylim)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab, side=3, adj= -0.1 , col=plotcol, padj = -1)
    title(xlab = xlab, line =1.8, col.lab=plotcol)
    abline(0, 1, col = plotcol)
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
           col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }
  } else if(plot == TRUE & md == TRUE) {

    # Generate labels
    xlab2 <- paste("Mean of", xlab, "and", ylab)
    ylab2 <- paste(ylab,"-", xlab)

    # Compute m-d variables
    md.y  <- (y - x)
    md.x  <- (y + x) * 0.5

    # Get quantile parameters
    qy <- quantile(md.y, q.val, qtype = q.type)
    medx <- median(md.x)
    medy <- median(md.y)

    # Generate plot
    ylim <- range(md.y, 0)

    plot( x=md.x, y=md.y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
          ylim = ylim)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab2, side=3, adj= -0.1 , col=plotcol, padj = -1)
    title(xlab = xlab2, line =1.8, col.lab=plotcol)
    abline(h = 0,  col = plotcol)
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }
  }

  # Reset plot parameters and  output values
  par(.pardef)
  invisible(list(x = x, y = y, px=px, py=py))
}
