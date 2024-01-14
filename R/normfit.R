#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Normal fit vs density plot
#'
#' @description \code{eda_normfit} generates a fitted Normal distribution to the
#'   data with the option to compare it to a density distribution.
#'
#' @param dat Vector of values or a dataframe.
#' @param x  Column of values if \code{dat} is a dataframe, ignored otherwise.
#' @param y  Column of grouping variables if \code{dat} is a dataframe, ignored
#'   otherwise.
#' @param p  Power transformation to apply to both sets of values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param show.par Boolean determining if power transformation should be
#'   displayed in the plot's upper-right corner.
#' @param sq Boolean determining if the plot should be square.
#' @param inner Fraction of values that should be captured by the inner color
#'   band of the normal and density plots. Defaults to 0.6826 (inner 68% of
#'   values).
#' @param dens Boolean determining if the density plot should displayed
#'   alongside the normal fit plot.
#' @para bw Bandwidth parameter passed to the built-in \code{density} function.
#' @param kernel Kernel parameter passed to the built-in \code{density}
#'   function.
#' @param pch Point symbol type.
#' @param size Point side.
#' @param alpha Fill transparency (0 = transparent, 1 = opaque). Only applicable
#'   if \code{rgb()} is not used to define fill colors.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param col.ends Fill color for ends of the Normal distribution.
#' @param col.mid Fill color for middle band of the Normal distribution.
#' @param col.ends.dens Fill color for ends of the density distribution.
#' @param col.mid.dens Fill color for middle band of the density distribution.
#' @param offset A value (in x-axis units) that defines the gap between left and
#'   right side plots. Ignored if \code{dens} is \code{TRUE}.
#' @param t.size Size of plot title.
#' @param xlab X variable label.
#' @param ylab Y variable label.
#' @param ... Note used.
#'
#' @returns Does not return a value.
#'
#' @details This function will generate a (symmetrical) Normal distribution
#'   fitted to the data if \code{dens} is set to \code{FALSE} or a side-by-side
#'   density/Normal fit plot if \code{dens} is set to \code{TRUE}. If the
#'   latter, the density plot will be on the left side and the Normal fit will
#'   be on the right side of the vertical axis. \cr
#'   \cr
#'   The plots have two fill colors: one for the inner band and the other for
#'   the outer band. The inner band shows the area of the curve that encompasses
#'   the desired fraction of values defined by \code{inner}. Be default, this
#'   value is 0.6826, or 68.26 percent (this is roughly the percentage of values
#'   covered by +/- 1 standard deviations of a Normal distribution). For the
#'   Normal fit plot, the range is computed from the theoretical Normal and not
#'   from the actual values. For the density plot, the range is computed from
#'   the actual values. By default, the colors are inverted between the Normal
#'   curve and the density curve. If the density plot is not drawn, then the
#'   Normal plot colors are identical about the vertical axis.
#'   \cr
#'   If a density plot is desired, \code{dens = TRUE}, a gap (defined by
#'   \code{offset}) is created between the left side density plot and the right
#'   side Normal fit plot. Points showing the location of values on the
#'   y-axis are also added to help view their distribution relative to the
#'   density and Normal fit curves. This function makes use of the built-in
#'   \code{density} function. As such, you can pass the \code{bw} and
#'   \code{kernel} parameters as you would for the \code{density} function.
#'   \cr
#'   Measures of centrality are computed differently for the Normal fit and
#'   density plots. The mean is computed for the Normal fit plot and the median
#'   is computed for density plot. These measures of centrality are shown as
#'   black horizontal lines in the plot.
#'   \cr
#'   The areas under the density and Normal fit plots are scaled to their
#'   peak values, respectively. So, the areas should not be compared between
#'   both plots.
#'
#'
#' @seealso \code{\link[stats]{density}}
#'
#' @examples
#'
#' # Explore a skewed distribution
#' set.seed(218)
#' x <- rexp(500)
#' eda_normfit(x)
#'
#' # Limit the plot to just a Normal fit
#' eda_normfit(x, dens = FALSE)
#'
#' # The inner band's range can be modified. Here, we view the inter-quartile
#' # range, +/- 1 standard deviation range and the inner 95% range)
#' OP <- par(mfrow = c(1,3))
#' sapply(c(0.5, 0.6826, 0.95),
#'       function(prop) eda_normfit(x, inner = prop, t.size = 1.1,
#'                                  ylab = paste(prop*100,"% of values")))
#' par(OP)
#'
#' # The bandwidth selector can also be specified
#' OP <- par(mfrow=c(2,3))
#'  sapply(c("SJ-dpi", "nrd0", "nrd", "SJ-ste", "bcv", "ucv" ),
#'        function(band) eda_normfit(x, bw = band, t.size=1, size=0, offset=0.005,
#'                                   ylab = band))
#' par(OP)
#'
#' # The bandwidth argument can also be passed a numeric value
#' OP <- par(mfrow=c(1,3))
#' sapply(c(0.2, 0.1, 0.05 ),
#'        function(band) eda_normfit(x, bw = band, t.size=1,size=.5, offset=0.01,
#'                                   ylab = band))
#' par(OP)
#'
#' # Examples of a few kernel options
#' OP <- par(mfrow=c(1,3))
#' sapply(c("gaussian", "optcosine", "rectangular" ),
#'       function(k) eda_normfit(x, kernel = k, t.size=1.2, size=.5, offset=0.01,
#'                                ylab = k))
#' par(OP)
#'
#' # Another example where data are passed as a dataframe
#' set.seed(540)
#' dat <- data.frame(value = rbeta(20, 1, 50),
#'                  grp = sample(letters[1:3], 100, replace = TRUE))
#' eda_normfit(dat, value, grp)
#'
#' # Points can be removed and the gap rendered narrower
#' eda_normfit(dat, value, grp, size = 0, offset = 0.01)
#'
#' # Color can be modified. Here we modify the density plot  fill colors
#' eda_normfit(dat, value, grp, size = 0, offset = 0.01,
#'             col.ends.dens = "#A1D99B", col.mid.dens = "#E5F5E0")
#'
#' # A power transformation can be applied to the data. Here
#' # we'll apply a log transformation
#' eda_normfit(dat, value, grp, p = 0)




eda_normfit <- function(dat, x=NULL, grp=NULL, p = 1,  tukey = FALSE,
                        show.par = TRUE, sq = TRUE, inner = 0.6826,
                        dens = TRUE, bw = "SJ-dpi", kernel = "gaussian",
                        pch = 16, size = 0.8, alpha = 0.3, p.col = "grey50",
                        p.fill = "grey80",grey = 0.7,
                        col.ends = "grey90", col.mid = "#EBC89B",
                        col.ends.dens = "#EBC89B" , col.mid.dens = "grey90",
                        offset = 0.02, t.size=1.5,
                        xlab = NULL, ylab = NULL, ...){

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

  # If density plot is not desried, set offset to 0
  if(dens == FALSE){
    offset = 0
  }

  # Set title
  if (dens == FALSE){
    title = "Normal fit plot"
  } else {
    title = "Density vs. Normal fit plot"
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

  # Create list of normal distributions by group
  grp_unique <- unique(grp)
  out <- lapply(split(x, grp), function(x)
      {dx <- seq(-sd(x, na.rm = TRUE) * 4,
                  sd(x, na.rm = TRUE) * 4, length.out = 120 ) +
                  mean(x, na.rm = TRUE)
       dn <- dnorm(dx, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
       data.frame(dx,dn)})

  xi <- split(x, grp)

  # Create list of density distributions by group
  out2 <- lapply(split(x, grp), function(x)
  {dnsty <- density(x, n=120, bw = bw, kernel = kernel)
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
  dx_rng <- range(unlist(lapply(out, function(x) x$dx)))
  dx_rng <- range(c(x), dx_rng)
  dn_rng <- range(unlist(lapply(out, function(x) x$dn)))

  dy2_rng <- range(unlist(lapply(out2, function(x) x$densy)))

  # Scale dn values such that the max value is 0.4
  out <- lapply(out, function(df){ df$dn <- (df$dn / dn_rng[2] ) * 0.45
  return(df)})

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
  mtext(title, side=3, line = 2, col=plotcol, adj = 0, cex=t.size)
  mtext(ylab,  side=3, line = 1, col=plotcol, adj = 0, padj = 0.5, cex=t.size - 0.3)

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

  # Add Normal polygons and points
  for(i in 1:length(out)){
    x2  <- out[[i]]$dn
    y2  <- out[[i]]$dx
    x3  <- out2[[i]]$densy
    y3  <- out2[[i]]$densx
    xi2  <- xi[[i]]


    # Plot full range on right side
    polygon(c(x2 + i + offset, rep(i+ offset, length(x2))), c(y2, rev(y2)),
            col = col.ends, border = NA)

    # Plot full range on left side
    if (dens == FALSE){
      polygon(c(-x2 + i - offset, rep(i- offset, length(x2))), c(y2, rev(y2)),
              col = col.ends, border = NA)
    } else {
      polygon(c(-x3 + i - offset, rep(i- offset, length(x3))), c(y3, rev(y3)),
              col = col.ends.dens, border = NA)
    }


    # Get inner range from normal plot

    q1 <- q.norm[[i]][1] ; q2 <- q.norm[[i]][2]
    #x_len <- length(x2)
    # xsd <- x2[(x_len/2-x_len/8) :(x_len/2+x_len/8) ]
    # ysd <- y2[(x_len/2-x_len/8) :(x_len/2+x_len/8) ]
    xsd <- x2[(y2 >= q1) & (y2 <= q2)]
    ysd <- y2[(y2 >= q1) & (y2 <= q2)]

    # Get inner range of density plot
    q1 <- q[[i]][1] ; q2 <- q[[i]][2]
    xsd2 <- x3[(y3 >= q1) & (y3 <= q2)]
    ysd2 <- y3[(y3 >= q1) & (y3 <= q2) ]

    # Get median line end for density plot
    xm <- approx(y3,x3, m[[i]][1])$y

    # Plot inner range
    polygon(c(xsd + i + offset, rep(i +offset, length(xsd))), c(ysd, rev(ysd)),
            col = col.mid, border = NA)

    if(dens == FALSE){
      polygon(c(-xsd + i -offset, rep(i -offset, length(xsd))), c(ysd, rev(ysd)),
              col = col.mid, border = NA)
    } else {
      polygon(c(-xsd2 + i -offset, rep(i -offset, length(xsd2))), c(ysd2, rev(ysd2)),
              col = col.mid.dens, border = NA)
      #    abline(v=i, col = "white", lw = 1)
    }

    # Plot points
    points(rep(i,length(xi2)), xi2, pch = pch, col = p.col, bg = p.fill, cex = size)

    # Plot central values
    if(dens == FALSE){
      lines(x=c(-max(x2), max(x2))+ i, y=c(means[i], means[i])) # mean
    } else {
      lines(x=c(0, max(x2))+ i +offset, y=c(means[i], means[i])) # Mean
      lines(x=c(0, -xm) + i -offset, y=c(m[[i]][1], m[[i]][1])) # Median
    }

  }

  if(show.par == TRUE){
    mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
  }

  # Reset plot parameters and  output values
  par(.pardef)

  if (dens == FALSE){
    message(c("\n!!!!!!!!!!!!!!!!!!!!!!!!\n",
              "Note that this is not a density plot.\nIt's the Normal ",
              "characterization of the data \nusing the data's standard deviation.\n",
              "!!!!!!!!!!!!!!!!!!!!!!!!\n\n"))
  }
}




