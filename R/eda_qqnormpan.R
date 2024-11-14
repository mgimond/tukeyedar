#' @export
#' @import grDevices
#' @import grid
#' @importFrom utils modifyList
#' @title QQ plot matrix
#'
#' @description \code{eda_qqnormpan} Generates a panel of Normal QQ plots for
#'   a variable conditioned on a grouping variable
#'
#' @param dat  Data frame.
#' @param x    Continuous variable.
#' @param fac  Categorical variable.
#' @param p  Power transformation to apply to the continuous variable.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
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
#' @param text.size Size for category text above the plot.
#' @param ... Not used
#'
#' @details The function will generate a panel or Normal QQ plots from a
#' dataframe of continuous values and matching categories. T
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
#'   \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)}
#'
#' @examples
#'
#' # Default output
#' singer <- lattice::singer
#' eda_qqnormpan(singer, height, voice.part)


eda_qqnormpan <- function(dat, x, fac, p = 1L, tukey = FALSE, q.type = 5,
                      ylim = NULL, resid = FALSE, stat = mean, show.par = TRUE,
                      plot = TRUE, grey = 0.6, pch = 21, nrow = 1,
                      p.col = "grey40", p.fill = "grey60", size = 1, text.size = 0.8,
                      tail.pch = 21, tail.p.col = "grey70", tail.p.fill = NULL,
                      tic.size = 0.7, alpha = 0.8, q = FALSE, tails = TRUE,
                      med = TRUE, inner = 0.75, ...) {

  if (!requireNamespace("grid", quietly = TRUE)) {
    stop(
      "Package \"grid\" must be installed to use this function.",
      call. = FALSE
    )
  }

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

  # Loop through each pairs of factors
  lst <- split(x, fac)

  if(resid == TRUE){
    lst <- lapply(lst, FUN = function(x){x - stat(x)})
  }
  fac_un <- unique(fac)
  fac_num <- length(fac_un)

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
  xlim <- c(-2.5 , 2.5)

  # Create empty list (to be used for data output)
  lstout <- list()

  # Reset plot window
  grid.newpage()

  # Compute margins needed to accommodate labels
  label <- as.character(max(ylim))
  label_width <- convertWidth(stringWidth(label), "lines", valueOnly = TRUE)
  label_height <- convertWidth(stringHeight(label), "lines", valueOnly = TRUE)
  x_margin <- unit(label_width + 2, "lines")
  y_margin <- unit(label_height + 5, "lines")  # Horizontal margin for x-axis text

  # Define number of rows and columns
  ncol = ceiling(fac_num / nrow)

  # Set up a layout for 4 plots in a 1x4 grid
  layout(matrix(1:fac_num, nrow = nrow, ncol = ncol, byrow = TRUE))
  par(mar = c(0, 0, 3, 0), oma = c(3, 3.5, 4, 2))  # Adjust inner and outer margins

  for (i in 1:fac_num){
      y <- unlist(lst[[i]])

        # Center on mean or median if requested
        if (resid == TRUE){
          y <- y - stat(y)
        }

        # Generate qqplot using base function
        qq <- qqnorm(y, plot.it = FALSE)
        x <- qq$x
        y <- qq$y

        # Get IQR line parameters
        probs = c(0.25, 0.75)
        yy <- as.vector(quantile(y, probs, names = FALSE, type = q.type))
        xx <- qnorm(probs)
        slope <- diff(yy) / diff(xx)
        int <- yy[[1L]] - slope * xx[[1L]]


        # Save values for output
        dfout <- data.frame(x,y)
        names(dfout) <- c("Normal", xname )
        row.names(dfout) <- NULL
        lstout[[i]] <- dfout
        names(lstout)[i] <- fac_un[i]

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

          # Add axes
          axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
          if( i == 1){
            axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
                 tck = -0.02)
          }


            # Add diagonal (1:1) line
            #grid.lines(gp = gpar(cex = 0.8, col = plotcol))
            abline(int, slope, col = plotcol)


          #  grid.text(paste(i,j)) # Used to debug plot placement

          # Add medians
            if( med == TRUE){
              abline(v = medx, col = "grey80", lty = 2)
              abline(h = medy, col = "grey80", lty = 2)
            }

          # Add core boxes ----
            # Add core boxes ----
            sq <- par("usr") # get plot corners
            if(q == TRUE){
              rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
                   col = rgb(0,0,0,0.05), border = NA)
              rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
                   col = rgb(0,0,0,0.05), border = NA)
            }


            # Add factor name above each plot
            mtext(fac_un[i], side = 3, line = 0.5, cex = text.size)

        }  # Close plot loop
    }   # Close  loop

  # Add power/formula parameters to plot
  if(show.par == TRUE){
    params <- gsub(";\\s*;?\\s*$", "",  paste0("p=",p))
    mtext(side = 3, text=params, adj=1, cex = 0.65, outer = TRUE)
  }
  mtext("Normal QQ plot", side = 3, outer = TRUE, line = 2, cex = 1.5)
  names(lstout) <- fac_un
  # Remind user if power parameter was set to value other than 1
  if ( p !=1 )
  message(paste0("Note that a power transformation of ",p," was applied to the data",
          " before they were processed for the plot."))
  invisible(lstout)
}

out <- eda_qqnormpan(df, height, voice.part, resid = T, text.size = 0.8)
out <- eda_qqnormpan(iris, Petal.Width, Species, resid = T)

