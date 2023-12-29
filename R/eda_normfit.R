#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Symmetric Normal distribution plot
#'
#' @description \code{eda_normfit} generates mirrored Normal distribution plots
#'   of the data. Its purpose is to compare batches of values as seen from a
#'   Normal approximation lens. These plots are better suited than a traditional
#'   boxplot when paired with a parametric test such as a t-test or an ANOVA given
#'   that such tests use the Normal approximation to characterize the shape of the
#'   distribution.
#'
#' @param dat Data frame or single vector element
#' @param x   Column of values
#' @param grp Column of grouping variables
#' @param p   Power transformation to apply to input values
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation)
#' @param show.par Boolean determining if power transformation value should
#'   be displayed on plot.
#' @param alpha Point transparency (0 = transparent <-> 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black)
#' @param pch Point symbol type
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param col.ends Fill color for ends of distribution (beyond +/- 1SD)
#' @param col.mid Fill color for middle of distribution (within +/- 1SD)
#' @param xlab X label for output plot
#' @param ylab Y label for output plot
#' @param t.size Title size
#' @param title Title to display above the plot
#' @param ... Not used
#'
#' @details This function will generate Normal distribution plots for each batch.
#'          The plots are mirrored to mimic the look of a violin plot. But it's
#'          important to note that this plot characterizes the distribution following
#'          a Normal (Gaussian) distribution using the batch standard deviation
#'          and its mean--nothing else. \cr
#'          \cr
#'          The plots are colored based on the part that falls between +/- 1
#'          standard deviation (bisque color by default) and the part that falls
#'          outside of it (light grey by default. The plot also adds the mean
#           (horizontal black line) and the original values as semi-transparent points.
#'          This plot serves a few purposes:
#'        \itemize{
#'         \item As a pedagogical tool to show when a Normal distribution may not
#'               be a good characterization of the data's distribution.
#'         \item To be used as a companion plot to a parametric test that
#'               characterizes the distribution as being Normal (as opposed to a
#'               traditional boxplot that uses quantiles to characterize the shape
#'               of a distribution). }
#'
#'
#' @return Does not return a value.
#'
#' @examples
#'
#' # Here's an example of a dataset that can be well approximated by
#' # a normal distribution
#' # First, a boxplot of the data
#' eda_boxls(iris, Sepal.Length, Species) # Check with boxplot
#' # Now, a Normal characterization of the data
#' eda_normfit(iris, Sepal.Length, Species)
#'
#' # Create a skewed dataset (i.e. one not well approximated by a normal
#' # distribution)
#' set.seed(540)
#' dat <- data.frame(value = rbeta(100, 1, 15),
#'                   grp = sample(letters[1:3], 100, replace = TRUE))
#' # The skewness is obvious in the following boxplots
#' eda_boxls(dat, value, grp)
#'
#' # The Normal approximation of the data artificially inflates the lower range
#' # of values. In fact, very few observations fall below -1 standard deviation!
#' eda_normfit(dat, value, grp)
#'
#' # The function allows for a re-expression of the values. Here, we'll
#' # adopt a Box-Cox power transformation of 0.1
#' eda_boxls(dat, value, grp, p = 0.1)  # Looks better
#' eda_normfit(dat, value, grp, p = 0.1)


eda_normfit <- function(dat, x=NULL, grp=NULL, p = 1,  tukey = FALSE,
                        show.par = TRUE, alpha = 0.3, grey = 0.7, pch = 16,
                        p.col = "grey50", p.fill = "grey80", size = 1,
                        col.ends = "grey90", col.mid = "bisque", xlab = NULL,
                        ylab = NULL, t.size=1.5,
                        title = "Normal fit plot", ...){

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

  # Get group means and sd
  means <- sapply(split(x,grp), function(x) mean(x))
  sds   <- sapply(split(x,grp), function(x) sd(x))

  # Create list of normal distributions by group
  grp_unique <- unique(grp)
  out <- lapply(split(x, grp), function(x)
                 {dx <- seq(-sd(x)*4, sd(x)*4, length.out = 120 ) + mean(x)
                  dn <- dnorm(dx, mean = mean(x), sd = sd(x))
                  data.frame(dx,dn)})
  xi <- split(x, grp)

  dx_rng <- range(unlist(lapply(out, function(x) x$dx)))
  dn_rng <- range(unlist(lapply(out, function(x) x$dn)))

  # Scale dn values such that the max value is 0.4
  out <- lapply(out, function(df){ df$dn <- (df$dn / dn_rng[2] ) * 0.4
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

  # Compute the margin width (returned in inches before converting to lines)
  # y.wid <- max( strwidth( y.labs[1], units="inches"),
  #               strwidth( y.labs[2], units="inches")) * in2line + 1

  # Set plot parameters
  .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3.2,1))
  on.exit(par(.pardef))

  # Base plot
  plot(x = 1:length(grp_unique), y = NULL, type = "n", xlab = "", ylab = "", xaxt = "n",
       xlim=c(1 - 0.4, length(grp_unique) + 0.4), ylim = dx_rng,  yaxt='n',
       main = NULL)

  # Add y-label and title
  # mtext(ylab, side=2,  col=plotcol, padj = -0.5, at=par('usr')[4],
  #       las=2)
  mtext(title, side=3, line = 2, col=plotcol, adj = 0, cex=t.size)
  mtext(ylab,  side=3, line = 1, col=plotcol, adj = 0, padj = 0.5, cex=t.size - 0.3)

  # Add axes values
  axis(1,col=plotcol, col.axis=plotcol, at = 1:length(grp_unique), padj = -0.8,
       grp_unique)
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
       tck = -0.02)
  title(xlab = xlab, line =1.8, col.lab=plotcol)

  # Add Normal polygons and points
  for(i in 1:length(out)){
    x2  <- out[[i]]$dn
    y2  <- out[[i]]$dx
    xi2  <- xi[[i]]

    x_len <- length(x2)
    xsd <- x2[(x_len/2-x_len/8) :(x_len/2+x_len/8) ]
    ysd <- y2[(x_len/2-x_len/8) :(x_len/2+x_len/8) ]

    polygon(c(x2 + i, rep(i, length(x2))), c(y2, rev(y2)),
            col = col.ends, border = NA)
    polygon(c(-x2 + i, rep(i, length(x2))), c(y2, rev(y2)),
            col = col.ends, border = NA)
    polygon(c(xsd + i, rep(i, length(xsd))), c(ysd, rev(ysd)),
            col = col.mid, border = NA)
    polygon(c(-xsd + i, rep(i, length(xsd))), c(ysd, rev(ysd)),
            col = col.mid, border = NA)

    points(rep(i,length(xi2)), xi2, pch = pch, col = p.col, bg = p.fill, cex = size)
    lines(x=c(-max(x2), max(x2))+ i, y=c(means[i], means[i]))
  }

  if(show.par == TRUE){
    mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
  }

  # Reset plot parameters and  output values
  par(.pardef)

  message(c("\n!!!!!!!!!!!!!!!!!!!!!!!!\n",
            "Note that this is not a density plot.\nIt's the Normal ",
            "characterization of the data \nusing the data's standard deviation.\n",
            "!!!!!!!!!!!!!!!!!!!!!!!!\n\n"))

}

