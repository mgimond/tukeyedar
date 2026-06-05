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
#' @param base Base used with the \code{log()} function if \code{px} or \code{py}
#'   is \code{0}.
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
#' @param qcol Fill color of inner quantile box.
#' @param inner Fraction of the input data considered as "mid values". Defaults to
#'  75%. Used  to define shaded region boundaries, \code{q}, or to identify
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
#'  # Default plot
#'  eda_sym(tenor1)
#'
#'  # To remove inner region grey box set q to FALSE
#'  eda_sym(tenor1, q = FALSE)
#'
#'  # You can also choose to have the points outside of the inner region
#'  # symbolized differently
#'  eda_sym(tenor1, q = FALSE, tails = TRUE, tail.pch = 3)


eda_sym <- function(x, p = 1L, tukey = FALSE, base = exp(1),q.type = 5,
                     show.par = TRUE, grey = 0.6, pch = 21,
                    p.col = "grey50", p.fill = "grey80", size = 0.8, alpha = 0.8,
                    inner = 0.75, q = TRUE, qcol = rgb(0, 0, 0, 0.05),
                    tails = FALSE, tail.pch = 21, tail.p.col = "grey70",
                    tail.p.fill = NULL, xlab = NULL, ylab = NULL, title = NULL,
                    t.size = 1.2, plot = TRUE, ...) {

  # Check for valid arguments (include arguments available in plot.defaults)
  dots <- list(...)
  dot_names <- names(dots)
  dot_names <- dot_names[dot_names != ""]
  internal_args <- names(formals(.eda_plot_xy))
  par_args <- names(par())
  allowed_args <- union(internal_args, par_args)

  # Check for invalid names
  invalid <- setdiff(dot_names, allowed_args)
  if (length(invalid) > 0) {
    warning(sprintf("Invalid arguments passed to %s: %s",
                    deparse(substitute(.eda_plot_xy)),
                    paste(invalid, collapse = ", ")))
  }

  # Remove missing elements
  nodata <- which(is.na(x))
  if(length(nodata > 0)){
    x <-  x[-nodata]
    cat(length(nodata), " elements had missing values. These were removed from the data.")
  }

  # Re-express data if required
  if (p != 1) {
    x <- eda_re(x, p = p, tukey = tukey, base = base)
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

  # Get upper bounds of inner values
  b.val = c(0, .5 + inner / 2)

  # Split x in half
  med <- median(x)
  len <- length(x)
  x.sort <- sort(x)
  n2 <- ifelse( len%%2 == 0, len/2, (len + 1)/2)

  # Convert x and y to number of units from the median
  x <- med - x.sort[1:n2]
  y <- x.sort[ (len + 1) - (1:n2) ] - med
  x <- sort(x)
  y <- sort(y)
  xlab <- "lower half"
  ylab <- "upper half"

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

  # Get quantile parameters
  qx <- quantile(x, b.val, qtype = q.type)
  qy <- quantile(y, b.val, qtype = q.type)

  # If tail points  are to be plotted differently, identify them
  if(tails == TRUE){
      lower.tail <- 0
    if (!is.na(table(x > qx[2])["TRUE"]) & !is.na(table(y > qy[2])["TRUE"])){
      upper.tail <-  max(table(x > qx[2])["TRUE"], table(y > qy[2])["TRUE"])
    } else {
      upper.tail <- 0
    }

    inner.tails <- (lower.tail+1):(length(x) - upper.tail)
    outer.tails <- -inner.tails
  }

  # QQ plot ----
  if(plot == TRUE){}
  # QQ plot: Empirical ----
  if(tails != TRUE){
    df <- data.frame(x,y)
    lst0 <- .eda_plot_xy(df, x, y, px = p, py = p, tukey = tukey, base = base,
                         square = TRUE, xlab = xlab, ylab = ylab,
                         xlim = xylim, ylim = xylim,  sd = FALSE, asp = FALSE,
                         grey = grey,  reg = FALSE, loe = FALSE, mean.l = FALSE,
                         inner = inner,q = FALSE, , qcol = qcol, q.type = q.type,
                         p.fill = p.fill, show.par = FALSE, ...)
  } else {
    df <- data.frame(x=x[inner.tails],y = y[inner.tails])
    lst0 <- .eda_plot_xy(df, x, y, px = p, py = p, tukey = tukey, base = base,
                         square = TRUE, xlab = xlab, ylab = ylab,
                         xlim = xylim, ylim = xylim,  sd = FALSE, asp = FALSE,
                         grey = grey,  reg = FALSE, loe = FALSE, mean.l = FALSE,
                         inner = inner, q = FALSE, qcol = qcol, q.type = q.type,
                         p.fill = p.fill, show.par = FALSE, ...)
    if (length(x[outer.tails]) != 0){  # Nothing to plot if tail index is empty
      .post <- par(mar = lst0$parxy)
      on.exit(par(.post))
      points( x=x[outer.tails], y=y[outer.tails],
              col.lab=plotcol, pch = tail.pch, col = tail.p.col,
              bg = tail.p.fill, cex = size)
      par(.post)
    }
  }

  .post <- par(mar = lst0$parxy)
  on.exit(par(.post))

  # Add empirical QQ line ----
  abline(0, 1, col = plotcol)

  # Add core boxes ----
  sq <- par("usr") # get plot corners
  if(q == TRUE){
    rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
         col = qcol, border = NA)
    rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
         col = qcol, border = NA)
  }

  par(.post)

  # Add power/formula parameters to plot
  if (show.par == TRUE) {
    .post <- par(mar = lst0$parxy)
    on.exit(par(.post))
    mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
    par(.post)
  }


}
