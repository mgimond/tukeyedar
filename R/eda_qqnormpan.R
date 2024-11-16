#' @export
#' @import grDevices
#' @import grid
#' @importFrom utils modifyList
#' @title Multi-panel theoretical QQ distribution plots
#'
#' @description \code{eda_qqmulti} Generates a multi-panel Normal QQ plots for
#'   a variable conditioned on a grouping variable
#'
#' @param dat  Data frame.
#' @param x    Continuous variable.
#' @param fac  Categorical variable.
#' @param p  Power transformation to apply to the continuous variable.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms (See \code{quantile} function). Used for the fitting the line
#'   to the points and for computing the boundaries for the shaded region.
#' @param dist Theoretical distribution to use. Defaults to Normal distribution.
#' @param dist.l List of parameters passed to the distribution quantile function.
#' @param ylim Y axes limits.
#' @param resid Boolean determining if residuals should be plotted. Residuals
#'   are computed using the \code{stat} parameter.
#' @param stat Statistic to use if residuals are to be computed. Currently
#'   \code{mean} (default) or \code{median}.
#' @param plot Boolean determining if plot should be generated.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param tail.pch Tail-end point symbol type (See \code{tails}).
#' @param tail.p.col Tail-end color for point symbol (See \code{tails}).
#' @param tail.p.fill Tail-end point fill color passed to \code{bg}
#'   (Only used for \code{tail.pch} ranging from 21-25).
#' @param size Point symbol size (0-1).
#' @param tic.size Size of tic labels (defaults to 0.8).
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param med Boolean determining if median lines should be drawn.
#' @param q Boolean determining if grey box highlighting the \code{inner}
#'   region should be displayed.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch}, \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param inner Fraction of mid-values to highlight in \code{q} or \code{tails}.
#'   Defaults to the inner 75\% of values.
#' @param iqr Boolean determining if an IQR line should be fitted to the points.
#' @param text.size Size for category text above the plot.
#' @param title Title to display. If set to \code{TRUE}, defaults to
#'   \code{"Normal QQ plot"}. If set to \code{FALSE}, omits title from output.
#'   Custom title can also be passed to this argument.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param ... Not used
#'
#' @details The function will generate a multi-panel theoretical QQ plot.
#'  Currently, only the Normal QQ plot (\code{dist="norm"}), exponential
#'  QQ plot (\code{dist="exp"}), and the uniform QQ plot (\code{dist="unif"})
#'  are supported.
#'
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{data}: List with input \code{x} and \code{y} values for each
#'   group. May be interpolated to smallest quantile batch if batch sizes
#'   don't match. Values will reflect power transformation defined in \code{p}}
#'
#' @references
#'
#' \itemize{
#'   \item William S. Cleveland. Visualizing data. (1993)}
#'
#' @examples
#'
#' # Default output
#' singer <- lattice::singer
#' eda_qqtheopan(singer, height, voice.part)
#'
#' # Split into two rows
#' eda_qqtheopan(singer, height, voice.part, nrow = 2)
#'
#' # Compare to a uniform distribution
#' eda_qqtheopan(singer, height, voice.part, nrow = 2, dist = "unif")
#'
#' # A uniform QQ plot is analgous to a Q(f) plot
#' eda_qqtheopan(singer, height, voice.part, nrow = 2, dist = "unif",
#'               iqr = FALSE, xlab = "f-value")
#'
#' # Normal QQ plots of Waterville daily averages. Mean monthly values are
#' # subtracted from the data to recenter all batches around 0.  Color and point
#' # symbols are used to emphasize the inner core of the data (here set to the
#' # inner 80% of values)
#' wat <- tukeyedar::wat05
#' wat$month <- format(wat$date,"%b")
#' eda_qqtheopan(wat,avg, month, resid = TRUE, nrow = 4, inner = 0.8 ,
#'                     tails = T, tail.pch = 3, p.fill = "coral")

eda_qqtheopan <- function(dat, x, fac, p = 1L, tukey = FALSE, q.type = 5,
                          dist = "norm", dist.l = list(), ylim = NULL,
                          resid = FALSE, stat = mean, show.par = FALSE,
                          plot = TRUE, grey = 0.6, pch = 21, nrow = 1,
                          p.col = "grey40", p.fill = "grey60", size = 1,
                          text.size = 0.8, tail.pch = 21, tail.p.col = "grey70",
                          tail.p.fill = NULL, tic.size = 0.7, alpha = 0.8,
                          q = FALSE, tails = FALSE, med = FALSE, inner = 0.75,
                          iqr = TRUE, title = FALSE,
                          xlab = NULL, ylab=NULL, ...) {

  if (!requireNamespace("grid", quietly = TRUE)) {
    stop(
      "Package \"grid\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Currently accepted distributions
  axes_names <- c(norm = "Normal",
                  exp  = "Exponential",
                  unif = "Uniform")

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

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
  if(is.null(ylab)){
    ylab <- deparse(substitute(x))
  }


  # Extract data ----
  # Get values and factors
  xname <- deparse(substitute(x))
  x   <- eval(substitute(x), dat)
  fac <- as.factor(eval(substitute(fac), dat))

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
      bad <- x.isna
      x <- x[!bad]
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

  # Get upper/lower bounds of quantile box
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # Get factor names and counts
  fac_un  <- levels(fac)
  fac_num <- length(fac_un)

  # Loop through each pairs of factors
  lst <- split(x, fac)

  if(resid == TRUE){
    lst <- lapply(lst, FUN = function(x){x - stat(x)})
  }

  # Setup the type of theoretical distribution to use.
  qtheo <- get(paste0("q", dist), mode = "function")

  # Add matching normal quantiles to each dataframe in the list
  lstout <- lapply(names(lst), function(grp) {
    y <- sort(na.omit(lst[[grp]]))  # Remove NAs and sort
    prob <- ppoints(y)
    dfqq <- data.frame(y , theo = do.call(qtheo, c(list(p=prob), dist.l))
    )
    names(dfqq) <- c(xname, "Normal")
    dfqq
  })

  # Set the list names to match the voice parts
  names(lstout) <- fac_un

  # Set plot parameters
  lmin <- min(dev.size("cm")) # Get smallest plot window dimension
  dim.cm <- lmin / fac_num * 1.3 # plot width and height
  num.plots <- (fac_num^2)   # Number of plots

   # Get axes limits for plots
  lim.buffer = 0.05
  if(is.null(ylim)){
      ylim <- range(unlist(lst))
      ylim <- c(ylim[1] - diff(ylim) * lim.buffer  , ylim[2] + diff(ylim) * lim.buffer)
  }
  xlim <- range(unlist(lapply(lstout, function(df) df$Normal)))

  # Define number of rows and columns
  ncol = ceiling(fac_num / nrow)

  # If layout matrix has more slots than plots, expand list with empty vector
  numpanel <- ncol * nrow
  if(numpanel > (fac_num + (ncol -1)))
    stop("You have too many plot panels for the number of QQ plots. Reduce nrow.")
  if(numpanel > fac_num){
    for(i in 1:(numpanel - fac_num ) ) lst <- append(lst, list(none = vector()))
  }

  # Create a dummy plot to extract y-axis width
  in2line <- ( par("mar") / par("mai") )[2]
  pdf(NULL)
  plot(x, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL, ylim = ylim)
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Set up a layout for plot
  top.mar = 1
  if (title != FALSE ) top.mar = 3
  .pardef <- par(no.readonly = TRUE)
  layout(matrix(1:numpanel, nrow = nrow, ncol = ncol, byrow = TRUE))
  par(mar = c(0, 0, 3, 0), oma = c(4, y.wid, top.mar, 2))  # Adjust inner and outer margins
  on.exit(par(.pardef))

  j <- 0  # Used to skip over empty panel

  for (i in 1:numpanel){
       j = j + 1
      if(length(lst[[i]]) == 0){
        plot(1:10, type = "n", axes = FALSE, xlab = NULL, ylab = NULL, main = NULL)
        j = j - 1
      }
      else{
        dfgrp <- lstout[[j]]

        # Extract x and y vectors
        x <- dfgrp[,2]
        y <- dfgrp[,1]

        # Get IQR line parameters
        probs = c(0.25, 0.75)
        yy <- as.vector(quantile(y, probs, names = FALSE, type = q.type))
        xx <- qtheo(probs)
        slope <- diff(yy) / diff(xx)
        int <- yy[[1L]] - slope * xx[[1L]]

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

        # Compute median values
        medx <- median(x)
        medy <- median(y)

        # Generate plot ----
        if(plot == TRUE){
          if(tails != TRUE){
            plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
                  xlim = xlim, ylim = ylim,
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

          # Add x-axis. To prevent overlapping labels remove end labels
          ticks <- pretty(x)
          labels <- ticks
          labels[c(1, length(labels))] <- ""
          axis(1,col=plotcol, col.axis=plotcol,  padj = -0.7,
               labels = TRUE)

          # Add y-axis if marginal plot
          if( i %in% seq(1,fac_num, by=ncol)){
            axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
                 tck = -0.02)
          }

          # Add IQR line
          if(iqr == TRUE){
            abline(int, slope, col = plotcol)
          }

          # Add medians
          if( med == TRUE){
            abline(v = medx, col = "grey80", lty = 2)
            abline(h = medy, col = "grey80", lty = 2)
          }

          # Add core region
          sq <- par("usr") # get plot corners
          if(q == TRUE){
            rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
                 col = rgb(0,0,0,0.05), border = NA)
            rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
                 col = rgb(0,0,0,0.05), border = NA)
          }

          # Add factor name above each plot
          mtext(fac_un[j], side = 3, line = 0.1, cex = text.size, col=plotcol)
      }
    }  # Close plot loop
   }   # Close  loop

  # Add power/formula parameters to plot
  if(show.par == TRUE){
    params <- gsub(";\\s*;?\\s*$", "",  paste0("p=",p))
    mtext(side = 3, text=params, adj=1, cex = 0.65, outer = TRUE, col = plotcol,
          padj = 1)
  }

  # Add x-axis title
  mtext(side = 1, text=xlab, cex = 1, outer = TRUE, col = plotcol,line = 2)

  # Add y-axis title
  mtext(side = 3, text = ylab, adj= 0 ,col=plotcol,  padj = 1.8,
        cex = 1, outer = TRUE)

  # Add title unless not requested
  if(title == TRUE) title <- paste(xlab, "QQ plot")
  if(title != FALSE){
    mtext(side = 3, text = title, adj= 0 ,col=plotcol,  padj = 0,
          cex = 1.4, outer = TRUE)
  }

  # Remind user if power parameter was set to value other than 1
  if ( p !=1 )
  message(paste0("Note that a power transformation of ",p," was applied to the data",
          " before they were processed for the plot."))
  # Reset plot parameters and  output values
  par(.pardef)

  # Output list
  invisible(lstout)
}

df <- lattice::singer
#df$voice.part <- as.character(df$voice.part)
out <- eda_qqtheopan(df, height, voice.part, resid = T, text.size = 0.8,
                     nrow = 3, med = F, tails = T, ylab = "Heigth (inches)", title = F)
out <- eda_qqtheopan(df, height, voice.part, resid = T, text.size = 0.8,
                     nrow = 3, med = F, tails = T, title = TRUE)
out <- eda_qqtheopan(iris, Petal.Width, Species, resid = T, nrow = 2, title = T)

sop1 <- subset(df, subset = voice.part == "Soprano 1", select = height, drop = TRUE)
tukeyedar::eda_qq(sop1, norm = TRUE)

# Normal QQ plots of Waterville daily averages. Color and point symbols are used
# to place emphasis on the inner core of the data (here set to the inner 80% of
# values)
wat <- tukeyedar::wat05
wat$month <- format(wat$date,"%b")
wat$temp <- wat$avg * 100000
out <- eda_qqtheopan(wat,avg, month, resid = F, q = F, nrow = 4, inner = 0.8 ,
                     tails = T, tail.pch = 3, p.fill = "coral")
out <- eda_qqtheopan(wat,temp, month, nrow = 3)
out <- eda_qqtheopan(wat,temp, month, nrow = 3, dist = "unif")
