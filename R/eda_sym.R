#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Symmetry QQ plot
#'
#' @description \code{eda_sym} Generates a symmetry QQ plot.
#'
#' @param x  Vector of sample
#' @param p  Power transformation to apply to \code{x}.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (\code{FALSE} adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms used to generate inner shaded region. (See \code{quantile}tile
#'   function).
#' @param plot Boolean determining if plot should be generated.
#' @param show.par Boolean determining if power parameter should be displayed.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param tail.pch Tail-end point symbol type (See \code{tails}).
#' @param tail.p.col Tail-end color for point symbol (See \code{tails}).
#' @param tail.p.fill Tail-end point fill color passed to \code{bg}
#'   (Only used for \code{tail.pch} ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if \code{inner} data region should be shaded.
#' @param inner Fraction of the data considered as "mid values". Defaults to
#'  75\%. Used  to define shaded region boundaries, \code{q}, or to identify
#'  which of the tail-end points are to be symbolized differently, \code{tails}.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch},  \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param xlab X label for output plot. Ignored if \code{x} is a dataframe.
#' @param ylab Y label for output plot. Ignored if \code{x} is a dataframe.
#' @param title Title to add to plot.
#' @param t.size Title size.
#' @param ... Not used
#'
#' @details Generates a symmetry quantile plot that compares the lower half
#'  of the sorted data to its upper half. If the distribution is perfectly
#'  symmetrical, the points will fall on the line.
#'
#' @returns Returns a dataframe of upper and lower halves
#'
#' @references
#'
#' \itemize{
#'   \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)
#'   \item \href{../articles/symqq.html}{Symmetry quantile plot article}}
#'
#' @examples
#'
#'  singer <- lattice::singer
#'  tenor1 <- subset(singer, voice.part == "Tenor 1", select = height, drop = TRUE )
#'
#'  eda_sym(tenor1)


eda_sym <- function(x, p = 1L, tukey = FALSE, q.type = 5, plot = TRUE,
                   show.par = TRUE, grey = 0.6, pch = 21, p.col = "grey50",
                   p.fill = "grey80", size = 0.8, alpha = 0.8,
                   q = TRUE, tails = FALSE, inner = 0.75,
                   tail.pch = 21, tail.p.col = "grey70", tail.p.fill = NULL,
                   xlab = NULL, ylab = NULL, title = NULL,
                   t.size = 1.2, ...) {

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Re-express data if required
  if (p != 1) {
    x <- eda_re(x, p = p, tukey = tukey)
  }
  x.isna <- is.na(x)
  rm.nan <- ifelse( any(x.isna), 1 , 0)

  # Re-expression may produce NaN values. Output warning if TRUE
  if( rm.nan > 0 ) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
    x <- x[!x.isna]
  }

  # Get upper/lower bounds of inner values
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # Split x in half
    med <- median(x)
    len <- length(x)
    x.sort <- sort(x)
    n2 <- ifelse( len%%2 == 0, len/2, (len + 1)/2)
    x <- med - x.sort[1:n2]
    y <- x.sort[ (len + 1) - (1:n2) ] - med
    xlab <- "lower half"
    ylab <- "upper half"

    # Modify grey box parameters
    b.val <- c(0, diff(b.val))

  # Create dataframe for output
  zd <- data.frame(y = y, x = x)
  names(zd) <- c(ylab, xlab)

  # Get XY limits
  xylim <- range(x,y)

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL)
#  y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Get quantile parameters
  qx <- quantile(x, b.val, qtype = q.type)
  qy <- quantile(y, b.val, qtype = q.type)

  # If tail points  are to be plotted differently, identify them
  if(tails == TRUE){
    # If tails are to be plotted separately, identify points outside of the
    # inner region
    if (!is.na(table(x < qx[1])["TRUE"]) & !is.na(table(y < qy[1])["TRUE"])){
      lower.tail <- min(table(x < qx[1])["TRUE"], table(y < qy[1])["TRUE"])
    } else{
      lower.tail <- 0
    }

    if (!is.na(table(x > qx[2])["TRUE"]) & !is.na(table(y > qy[2])["TRUE"])){
      upper.tail <-  max(table(x > qx[2])["TRUE"], table(y > qy[2])["TRUE"])
    } else {
      upper.tail <- 0
    }

    inner.tails <- (lower.tail+1):(length(x) - upper.tail)
    outer.tails <- -inner.tails
  }

  # Set plot parameters
  .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
  on.exit(par(.pardef))

  # QQ plot ----
  if(plot == TRUE){}
    # QQ plot: Empirical ----
      if(tails != TRUE){
        plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
              xlim = xylim, ylim = xylim)
      } else {
        plot( x=x[inner.tails], y=y[inner.tails],  ylab=NA, las=1,
              yaxt='n', xaxt='n', xlab=NA,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
              xlim = xylim, ylim = xylim)

        if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
          points( x=x[outer.tails], y=y[outer.tails],
                col.lab=plotcol, pch = tail.pch, col = tail.p.col,
                bg = tail.p.fill, cex = size)
        }
      }

    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)

    mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2, cex = par("cex"))
    title(xlab = xlab, line =1.8, col.lab=plotcol)

    if(!is.null(title)){
      title(main = title, line =1.2, col.main=plotcol, cex.main=t.size)
    }

    # Add empirical QQ line ----
    abline(0, 1, col = plotcol)


    # Add core boxes ----
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
           col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }

    # Add power/formula parameters to plot
    if (show.par == TRUE) {
      mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
    }

  # Reset plot parameters and  output values
  par(.pardef)
  invisible(zd)
}
