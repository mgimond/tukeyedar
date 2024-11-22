#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Theoretical QQ plot
#'
#' @description \code{eda_theo} generates a theoretical QQ plot for many common
#'   distributions including the Normal, uniform and gamma distributions.
#'
#' @param x  Vector of continuous values.
#' @param p  Power transformation to apply to \code{x}.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 4 and 9 selecting one of the nine quantile
#'   algorithms. (See the \code{stats::quantile} function).
#' @param dist Choice of theoretical distribution.
#' @param dist.l List of parameters passed to the distribution quantile function.
#' @param resid Boolean determining if residuals should be plotted. Residuals
#'   are computed using the \code{stat} parameter.
#' @param stat Statistic to use if residuals are to be computed. Currently
#'   \code{mean} (default) or \code{median}.
#' @param plot Boolean determining if plot should be generated.
#' @param show.par Boolean determining if parameters such as power
#'   transformation or formula should be displayed.
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
#' @param med Boolean determining if median lines should be drawn.
#' @param q Boolean determining if \code{inner} data region should be shaded.
#' @param iqr Boolean determining if an IQR line should be fitted to the points.
#' @param grid Boolean determining if a grid should be added.
#' @param inner Fraction of the data considered as "mid values". Defaults to
#'  75\%. Used  to define shaded region boundaries, \code{q}, or to identify
#'  which of the tail-end points are to be symbolized differently, \code{tails}.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch},  \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param xlab X label for output plot.
#' @param ylab Y label for output plot.
#' @param title Title to add to plot.
#' @param t.size Title size.
#' @param ... Not used
#'
#' @details  The function generates a theoretical QQ plot.
#'  Currently, only the Normal QQ plot (\code{dist="norm"}), exponential
#'  QQ plot (\code{dist="exp"}), uniform QQ plot (\code{dist="unif"}),
#'  gamma QQ plot (\code{dist="gamma"}), chi-squared QQ plot
#'  (\code{dist="chisq"}), and the Weibull QQ plot (\code{dist="weibull"}) are
#'  currently supported. By default, the Normal QQ plot maps the unit Normal
#'  quantiles to the x-axis (i.e. centered on a mean of 0 and standard deviation
#'  of 1 unit). \cr \cr
#'  Note that arguments can be passed to the respective quantile functions via
#'  the \code{d.list} argument. Some quantile functions require at least one
#'  argument. For example, the \code{qgamma} function requires that the shape
#'  parameter be specified and the \code{qchisq} function requires that  the
#'  degrees of freedom, \code{df}, be specified. See examples.
#'
#' @returns A dataframe with the input vector elements and matching theoretical
#'   quantiles. Any transformation applied to \code{x} is reflected in the
#'   output.
#'
#'
#' @references
#'
#' \itemize{
#'   \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)
#'   \item \href{../articles/qq.html}{Quantile-Quantile plot article}}
#'
#' @examples
#'
#'  singer <- lattice::singer
#'  bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )
#'
#'  # Generate a normal QQ plot
#'  eda_theo(bass2)
#'
#'  # Generate a chi-squared QQ plot. The distribution requires that the degrees
#'  # of freedom be specified. The inner 70% shaded region is added.
#'  set.seed(270); x <- rchisq(100, df =3)
#'  eda_theo(x, dist = "chisq", dist.l = list(df = 3), q = TRUE)
#'
#'  # Generate a gamma QQ plot. Note that gamma requires at the very least the
#'  # shape parameter. The chi-squared distribion is a special case of the
#'  # gamma distribution where shape = df/2 and rate = 1/2.
#'  eda_theo(x, dist = "gamma", dist.l = list(shape = 3/2, rate = 1/2), q = TRUE)
#'
#'  # Generate a uniform QQ plot
#'  eda_theo(bass2, dist = "unif")
#'
#'  # The uniform QQ plot can double as a quantile plot
#'  eda_theo(bass2, dist = "unif", q = FALSE, med = FALSE,
#'           iqr = FALSE, grid = TRUE, xlab = "f-value")


eda_theo <- function(x, p = 1L, tukey = FALSE, q.type = 5,
                     dist = "norm", dist.l = list(), resid = FALSE, stat = mean,
                     plot = TRUE, show.par = TRUE, grey = 0.6, pch = 21,
                     p.col = "grey50", p.fill = "grey80", size = 1, alpha = 0.8,
                     med = TRUE, q = FALSE, iqr = TRUE, grid = FALSE,
                     tails = FALSE, inner = 0.75, tail.pch = 21,
                     tail.p.col = "grey70", tail.p.fill = NULL, xlab = NULL,
                     ylab = NULL, title = NULL, t.size = 1.2, ...) {

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Currently accepted distributions
  axes_names <- c(norm = "Normal",
                  exp  = "Exponential",
                  unif = "Uniform",
                  gamma = "Gamma",
                  chisq = "Chi-Squared",
                  weibull = "Weibull")

  # Parameters check
  if (! as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Stat must be either the mean or the median")
  if (inner < 0.25)
    stop("The inner parameter must be greater than 0.25.")
  if (!dist %in% names(axes_names))
    stop(paste("Distribution argument, dist, should be one of",
               names(axes_names)))

  # Define axes labels based on distribution type
  if(is.null(xlab)) xlab <- axes_names[dist]
  if(is.null(ylab)) ylab <- deparse(substitute(x))

  # Get values and factors
  xname <- deparse(substitute(x))

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

  # Compute residuals if requested
  if(resid == TRUE){
    x <- x - stat(x)
  }

  # Sort x
  x <- sort(na.omit(x))

  # Setup the type of theoretical distribution to use.
  qtheo <- get(paste0("q", dist), mode = "function")

  # Add matching normal quantiles to each dataframe in the list
  prob <- eda_fval(x, q.type = q.type)

  dfqq <- data.frame(x, do.call(qtheo, c(list(p=prob), dist.l)))
  names(dfqq) <- c(xname, xlab)

  y <- dfqq[, 1]
  x <- dfqq[, 2]

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
  if (plot == TRUE) {

    # Get lines-to-inches ratio
    in2line <- ( par("mar") / par("mai") )[2]

        # Create a dummy plot to extract y-axis labels
    pdf(NULL)
    plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt='n', main = NULL)
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

    medx <- median(x)
    medy <- median(y)

    # Generate plot
    xlim <- range(x)
    ylim <- range(y)

    if(tails != TRUE){
      plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
            col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
    } else {
      plot( x=x[inner.tails], y=y[inner.tails], ylab=NA, las = 1,
            yaxt='n', xaxt='n', xlab=NA, xlim = xlim, ylim = ylim,
            col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
      if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
        points( x=x[outer.tails], y=y[outer.tails], yaxt='n', xaxt='n',
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

    # Add IQR line ----
    if (iqr == TRUE){
      probs = c(0.25, 0.75)
      yy <- as.vector(quantile(y, probs, names = FALSE, type = q.type))
      xx <- do.call(qtheo, c(list(p=probs), dist.l))
      slope <- diff(yy) / diff(xx)
      int <- yy[[1L]] - slope * xx[[1L]]
      abline(int, slope, col = plotcol)
    }

    # Add grid
    if (grid == TRUE) grid()

    # Add medians (omit id sym == TRUE) ----
    if(med == TRUE){
      abline(v = medx, col = "grey80", lty = 2)
      abline(h = medy, col = "grey80", lty = 2)
    }

    # Add core boxes ----
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
           col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }

    # Add power  parameters to plot
    if(show.par == TRUE){
      mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
    }

    # Reset plot parameters and  output values
    par(.pardef)
  }

  invisible(dfqq)
}
