#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Violin plot.
#'
#' @description \code{eda_viol} generates a violin plot.
#'
#' @param dat Vector of values, or a dataframe.
#' @param x  Column of values if \code{dat} is a dataframe, ignored otherwise.
#' @param grp Column of grouping variables if \code{dat} is a dataframe, ignored
#'   otherwise.
#' @param p  Power transformation to apply to all values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (\code{TRUE}) or if a Box-Cox transformation should be adopted (\code{FALSE}).
#' @param show.par Boolean determining if the power transformation used with the
#'   data should be displayed in the plot's upper-right corner.
#' @param sq Boolean determining if the plot should be square.
#' @param inner Fraction of values that should be captured by the inner color
#'   band of the normal and density plots. Defaults to 0.6826 (inner 68\% of
#'   values).
#' @param bw Bandwidth parameter passed to the \code{density()} function.
#' @param kernel Kernel parameter passed to the \code{density()}
#'   function.
#' @param stat Statistical summary to display in plot. Choice of median or mean.
#'   Defaults to both.
#' @param pch Point symbol type.
#' @param size Point side.
#' @param alpha Fill transparency (0 = transparent, 1 = opaque). Only applicable
#'   if \code{rgb()} is not used to define fill colors.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param grey Grey level to apply to plot elements such as axes, labels, etc...
#'   (0 to 1 with 1 = black).
#' @param col.ends Fill color for ends of the Normal distribution.
#' @param col.mid Fill color for middle band of the Normal distribution.
#' @param col.ends.dens Fill color for ends of the density distribution.
#' @param col.mid.dens Fill color for middle band of the density distribution.
#' @param offset A value (in x-axis units) that defines the gap between left and
#'   right side plots. Ignored if \code{dens} is \code{FALSE}.
#' @param tsize Size of plot title.
#' @param xlab X variable label.
#' @param ylab Y variable label.
#' @param ... Note used.
#'
#' @return Does not return a value.
#'
#' @details This function will generate a violin plot from the data.  It
#'   implements the internal \code{density} function when generating the shape of the
#'   plots. As such the \code{bw} and \code{kernel} arguments are passed on to
#'   the \code{density} function.\cr
#'   \cr
#'   The plots have two fill colors: one for the inner band and the other for
#'   the outer band. The inner band shows the area of the curve that encompasses
#'   the desired fraction of values defined by \code{inner}. By default, this
#'   value is 0.6826, or 68.26\% (this is roughly the percentage of values
#'   covered by +/- 1 standard deviations of a Normal distribution). The range
#'   is computed from the actual values and not from a fitted normal
#'   distribution. \cr
#'   \cr
#'   Measures of centrality are added to the plot. By default, both the mean
#'   (dashed line) and the median (solid line) are added to the plot. \cr
#'
#' @seealso \code{\link[stats]{density}}
#'
#' @examples
#'
#' # Explore a skewed distribution
#' set.seed(132)
#' x <- rnbinom(500, 10, .5)
#'
#' # Generate violin plot
#' eda_viol(x)
#'
#' # The inner band's range can be modified. Here, we view the interquartile
#' # range, the +/- 1 standard deviation range and the inner 95% range)
#' OP <- par(mfrow = c(1,3))
#' invisible(sapply(c(0.5, 0.6826, 0.95),
#'       function(prop) eda_viol(x, inner = prop, tsize = 1,
#'                                  ylab = paste(prop*100,"% of values"))))
#' par(OP)
#'
#' # The bandwidth selector can also be specified
#' OP <- par(mfrow=c(2,3))
#'  invisible(sapply(c("SJ-dpi", "nrd0", "nrd", "SJ-ste", "bcv", "ucv" ),
#'        function(band) eda_viol(x, bw = band, tsize=0.9, size=0, offset=0.005,
#'                                   ylab = band)))
#' par(OP)
#'
#' # The bandwidth argument can also be passed a numeric value
#' # (bw = 0.75, 0.5 and 0.3)
#' OP <- par(mfrow=c(1,3))
#' invisible(sapply(c(0.75, 0.5, 0.3 ),
#'        function(band) eda_viol(x, bw = band, tsize=1,size=.5, offset=0.01,
#'                                   ylab = band)))
#' par(OP)
#'
#' # Examples of a few kernel options
#' OP <- par(mfrow=c(1,3))
#' invisible(sapply(c("gaussian", "optcosine", "rectangular" ),
#'       function(k) eda_viol(x, kernel = k, tsize=1, size=.5, offset=0.01,
#'                                ylab = k)))
#' par(OP)
#'
#' # Another example where data are passed as a dataframe
#' set.seed(540)
#' dat <- data.frame(value = rbeta(20, 1, 50),
#'                  grp = sample(letters[1:3], 100, replace = TRUE))
#' eda_viol(dat, value, grp)
#'
#' # Points can be removed and the gap rendered narrower
#' eda_viol(dat, value, grp, size = 0, offset = 0.01)
#'
#' # Gap can be removed all together
#' eda_viol(dat, value, grp, size = 0, offset = 0)
#'
#' # Remove both mean and medians
#' eda_viol(dat, value, grp, size = 0, offset = 0, stat = "none")
#'
#' # Color can be modified. Here we modify the density plot  fill colors
#' eda_viol(dat, value, grp, size = 0, offset = 0.01,
#'             col.ends.dens = "#A1D99B", col.mid.dens = "#E5F5E0")
#'
#' # A power transformation can be applied to the data. Here
#' # we'll apply a log transformation
#' eda_viol(dat, value, grp, p = 0)


eda_viol <- function(dat, x=NULL, grp=NULL, p = 1,  tukey = FALSE,
                        show.par = TRUE, sq = FALSE, inner = 0.6826,
                        bw = "SJ-dpi", kernel = "gaussian", stat = "both",
                        pch = 16, size = 0.8, alpha = 0.3, p.col = "grey50",
                        p.fill = "grey80",grey = 0.6,
                        col.ends = "grey90", col.mid = "#EBC89B",
                        col.ends.dens = "grey90" , col.mid.dens = "#EBC89B",
                        offset = 0.02, tsize=1.5,
                        xlab = NULL, ylab = NULL, ...){

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Prep the data if input is dataframe
  if("data.frame" %in% class(dat)) {
    if(is.null(xlab)){
      xlab = substitute(grp)
    }
    grp <- eval(substitute(grp), dat)
  }

  # Prep the data if input is a vector
  if (is.vector(dat)) {
    grp <- rep(1, length(dat))
    x   <- dat
    if(is.null(ylab)){
      ylab = substitute(dat)
    }
  } else if (is.data.frame(dat) & is.null(grp)) {
    if(is.null(x)) stop("You must must specify an x variable from the dataframe.")
    if(is.null(ylab)){
      ylab = substitute(x)
    }
    grp <- rep(1, length(dat))
    x <- eval(substitute(x), dat)
  } else if (is.data.frame(dat) & !is.null(grp)){
    if(is.null(ylab)){
      ylab = substitute(x)
    }
    grp <- eval(substitute(grp), dat)
    x   <- eval(substitute(x), dat)
  } else {
    stop("You must specify a vector or a data frame.")
  }

  if( !stat %in% c("none", "mean", "median", "both"))
    stop("The stat argument must be either \"none\", \"median\", \"mean\", or \"both\"")

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Re-express data if required
  if(p != 1L){
    x <- eda_re(x, p = p, tukey = tukey)
    x.nan <- is.na(x)
    if( any(x.nan)){
      x <- x[!x.nan]
      grp <- grp[!x.nan]
      warning(paste("\nRe-expression produced NaN values. These observations will",
                    "be removed from output. This will result in fewer points",
                    "in the ouptut."))
    }

  }

  # Remove groups that have just one value
  sngl_val <- ave(1:length(x), grp, FUN = function(x) length(x) == 1)
  x <- x[!sngl_val]
  grp <- grp[!sngl_val]
  if(TRUE %in% sngl_val){
    warning("\nSome groups were removed for having just one value.")
  }

  # Get quantiles from inner range
  lower <- (1 - inner) / 2
  upper <- 1 -lower

  # Get group means
  means <- sapply(split(x,grp), function(x) mean(x, na.rm = TRUE))

  # Create list of density distributions by group
  grp_unique <- unique(grp)
  xi <- split(x, grp)
  out2 <- lapply(split(x, grp), function(x)
  {dnsty <- stats::density(x, n=120, bw = bw, kernel = kernel)
  densx <- dnsty$x
  densy <- dnsty$y
  data.frame(densx,densy)})

  # Get median
  m <- lapply(split(x, grp), function(x) median(x, na.rm = TRUE) )

  # Get upper/lower values from quantiles
  q <- lapply(split(x, grp), function(x) quantile(x,c(lower, upper) ))
  q.norm <- lapply(split(x, grp), function(x)
          qnorm(c(0.5 - inner/2, 0.5 + inner/2) ) *
            sd(x, na.rm = TRUE) + mean(x, na.rm = TRUE))

  # Get axes range
  dx_rng <- range(unlist(lapply(out2, function(x) x$densx)))
  dn_rng <- range(unlist(lapply(out2, function(x) x$densx)))
  dy2_rng <- range(unlist(lapply(out2, function(x) x$densy)))

  out2 <- lapply(out2, function(df){ df$densy <- (df$densy / dy2_rng[2] ) * 0.45
                return(df)})

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = NULL, y = NULL, type = "n", xlab = "", ylab = "", xaxt = "n",
       xlim=c(1 - 0.4, length(grp_unique) + 0.4), ylim = dx_rng,  yaxt='n',
       main = NULL)
  #  y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Set plot parameters
  if (sq == TRUE){
    .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3.3,1))
  } else {
    .pardef <- par( col = plotcol, mar = c(3,y.wid,3.3,1))
  }
  on.exit(par(.pardef))

  # Base plot
  plot(x = 1:length(grp_unique), y = NULL, type = "n", xlab = "", ylab = "", xaxt = "n",
       xlim=c(1 - 0.45, length(grp_unique) + 0.45), ylim = dx_rng,  yaxt='n',
       main = NULL)

  # Add y-label and title
  mtext(ylab,  side=3, line = 1, col=plotcol, adj = 0, padj = 0.5, cex=tsize - 0.3)

  # Add x-axes labels if more than 1 group
  if (length(grp_unique) > 1 ){
    axis(1,col=plotcol, col.axis=plotcol, at = 1:length(grp_unique), padj = -0.8,
         grp_unique)
  }

  # Add x-axis title
  title(xlab = xlab, line =1.8, col.lab=plotcol)

  # Add y-axis values
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
       tck = -0.02)

  # Add polygons and points
  for(i in 1:length(out2)){
    # x2  <- out[[i]]$dn
    # y2  <- out[[i]]$dx
    x3  <- out2[[i]]$densy
    y3  <- out2[[i]]$densx
    xi2  <- xi[[i]]


    # Plot polygons
    polygon(c(-x3 + i - offset, rep(i- offset, length(x3))), c(y3, rev(y3)),
            col = col.ends.dens, border = NA)
    polygon(c(x3 + i + offset, rep(i+ offset, length(x3))), c(y3, rev(y3)),
            col = col.ends.dens, border = NA)

    # Get inner range from normal plot
    q1 <- q.norm[[i]][1] ; q2 <- q.norm[[i]][2]

    # Get inner range of density plot
    q1 <- q[[i]][1] ; q2 <- q[[i]][2]
    xsd2 <- x3[(y3 >= q1) & (y3 <= q2)]
    ysd2 <- y3[(y3 >= q1) & (y3 <= q2) ]

    # Get mean and median line ends for density plot
    xmu <- approx(y3,x3, means[[i]][1])$y
    xm <- approx(y3,x3, m[[i]][1])$y # median

    # Plot inner range
    polygon(c(-xsd2 + i -offset, rep(i -offset, length(xsd2))), c(ysd2, rev(ysd2)),
            col = col.mid.dens, border = NA)
    polygon(c(xsd2 + i + offset, rep(i +offset, length(xsd2))), c(ysd2, rev(ysd2)),
            col = col.mid.dens, border = NA)

    # Plot points
    points(rep(i,length(xi2)), xi2, pch = pch, col = p.col, bg = p.fill, cex = size)

    # Plot central values
    if(stat == "both"){
      lines(x=c(0, xmu)+ i +offset, y=c(means[i], means[i]), lty = 2) # Mean
      lines(x=c(0, -xmu)+ i -offset, y=c(means[i], means[i]), lty = 2) # Mean
      lines(x=c(0, +xm) + i +offset, y=c(m[[i]][1], m[[i]][1])) # Median
      lines(x=c(0, -xm) + i -offset, y=c(m[[i]][1], m[[i]][1])) # Median
    } else if (stat == "mean") {
      lines(x=c(0, xmu)+ i +offset, y=c(means[i], means[i]), lty = 2) # Mean
      lines(x=c(0, -xmu)+ i -offset, y=c(means[i], means[i]), lty = 2) # Mean
    } else if (stat == "median") {
      lines(x=c(0, +xm) + i +offset, y=c(m[[i]][1], m[[i]][1])) # Median
      lines(x=c(0, -xm) + i -offset, y=c(m[[i]][1], m[[i]][1])) # Median
    }

  }

  if(show.par == TRUE){
    mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
  }

  # Reset plot parameters and  output values
  par(.pardef)

}




