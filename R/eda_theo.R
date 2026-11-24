#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Theoretical QQ plot
#'
#' @description \code{eda_theo} generates a theoretical QQ plot for common
#'   distributions (Normal, Exponential, Uniform, Gamma, Chi-Squared, Weibull)
#'   with an optional null reference simulation envelope.
#'
#' @param x  Vector of continuous values.
#' @param p  Power transformation to apply to \code{x}.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param base Base used with the log() function if \code{p = 0}.
#' @param q.type An integer between 4 and 9 selecting one of the nine quantile
#'   algorithms. (See the \code{\link[tukeyedar]{eda_fval}} function).
#' @param dist Choice of theoretical distribution.
#' @param dist.l List of parameters passed to the distribution quantile function.
#' @param resid Boolean determining if residuals should be plotted. Residuals
#'   are computed using the \code{stat} parameter.
#' @param stat Statistic to use if residuals are to be computed. Currently
#'   \code{mean} (default) or \code{median}.
#' @param nsim Integer. Number of random draws to generate for constructing
#'   the null reference envelope. Default is \code{0} (off).
#' @param sim.alpha Numeric. Transparency value (\code{0} to \code{1}) for simulated cloud. Default \code{0.2}.
#' @param sim.col Character. Color for simulated cloud. Default \code{"coral"}.
#' @param sim.pch Integer or character. Plotting symbol for simulated points. Default \code{16}.
#' @param sim.type Character. Format of simulation display: \code{"points"} or \code{"lines"}.
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
#'  75%. Used  to define shaded region boundaries, \code{q}, or to identify
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
#'  supported. By default, the Normal QQ plot maps the unit Normal
#'  quantiles to the x-axis (i.e. centered on a mean of 0 and standard deviation
#'  of 1 unit). \cr \cr
#'  Note that arguments can be passed to the respective quantile functions via
#'  the \code{d.list} argument. Some quantile functions require at least one
#'  argument. For example, the \code{qgamma} function requires that the shape
#'  parameter be specified and the \code{qchisq} function requires that  the
#'  degrees of freedom, \code{df}, be specified. See examples.
#'  \cr \cr
#' To help evaluate whether departures from the theoretical reference line reflect
#' true non-conformance to the target distribution rather than natural sampling
#' variability, setting \code{nsim > 0} generates an empirical null envelope.
#' Under the null hypothesis that the data arise from the specified theoretical
#' distribution, synthetic samples of size \eqn{N} are repeatedly drawn and scaled
#' to match the sample's quantiles. The resulting simulation runs are plotted
#' behind the observed data with alpha transparency (\code{sim.alpha}). If the
#' observed points remain inside the concentration of the simulated cloud, the
#' apparent deviations are consistent with random sampling variation for a sample
#' of size \eqn{N}.
#'
#' @returns A dataframe with the input vector elements and matching theoretical
#'   quantiles. Any transformation applied to \code{x} is reflected in the
#'   output.
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
#'  # Normal QQ plot with 20 null reference simulations
#'  eda_theo(bass2, nsim = 20)
#'
#'  # Normal QQ plot with simulation envelope rendered as spaghetti lines
#'  eda_theo(bass2, nsim = 50, sim.type = "lines")
#'
#'  # Generate a chi-squared QQ plot. The distribution requires that the degrees
#'  # of freedom be specified. The inner 70% shaded region is added.
#'  set.seed(271); x <- rchisq(100, df =3)
#'  eda_theo(x, dist = "chisq", dist.l = list(df = 3), q = TRUE)
#'
#'  # Generate a gamma QQ plot. Note that gamma requires at the very least the
#'  # shape parameter. The chi-squared distribution is a special case of the
#'  # gamma distribution where shape = df/2 and rate = 1/2.
#'  eda_theo(x, dist = "gamma", dist.l = list(shape = 3/2, rate = 1/2), q = TRUE)
#'
#'  # Generate a uniform QQ plot
#'  eda_theo(bass2, dist = "unif")
#'
#'  # The uniform QQ plot can double as a quantile plot
#'  eda_theo(bass2, dist = "unif", q = FALSE, med = FALSE,
#'           iqr = FALSE, grid = TRUE, xlab = "f-value")


eda_theo <- function(x, p = 1L, tukey = FALSE, base = exp(1), q.type = 5,
                      dist = "norm", dist.l = list(), resid = FALSE, stat = mean,
                      nsim = 0,  sim.alpha = 0.2, sim.col = "coral",
                      sim.pch = 16,  sim.type = "points",
                      plot = TRUE, show.par = TRUE, grey = 0.6, pch = 21,
                      p.col = "grey50", p.fill = "grey80", size = 1, alpha = 0.8,
                      med = TRUE, q = FALSE, qcol = rgb(0, 0, 0, 0.05),
                      iqr = TRUE, grid = FALSE,
                      tails = FALSE, inner = 0.75, tail.pch = 21,
                      tail.p.col = "grey70", tail.p.fill = NULL, xlab = NULL,
                      ylab = NULL, title = NULL, t.size = 1.2, ...) {

  # Argument check
  dots <- list(...)
  dot_names <- names(dots)[names(dots) != ""]
  internal_args <- names(formals(.eda_plot_xy))
  par_args <- names(par())
  allowed_args <- union(internal_args, par_args)

  invalid <- setdiff(dot_names, allowed_args)
  if (length(invalid) > 0) {
    warning(sprintf("Invalid arguments passed to %s: %s",
                    deparse(substitute(.eda_plot_xy)),
                    paste(invalid, collapse = ", ")))
  }

  # Save original graphics state for restoration on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # Currently accepted distributions
  axes_names <- c(norm = "Normal",
                  exp  = "Exponential",
                  unif = "Uniform",
                  gamma = "Gamma",
                  chisq = "Chi-Squared",
                  weibull = "Weibull")

  if (! as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Stat must be either the mean or the median")
  if (inner < 0.25)
    stop("The inner parameter must be greater than 0.25.")
  if (!dist %in% names(axes_names))
    stop(paste("Distribution argument, dist, should be one of",
               names(axes_names)))

  # Turn off quantile box shading if tails == TRUE to avoid redundancy
  if (tails == TRUE & q == TRUE) {
    q <- FALSE
  }

  # Define axes labels based on distribution type
  if (is.null(xlab)) xlab <- axes_names[dist]
  if (is.null(ylab)) ylab <- deparse(substitute(x))

  # Get values and factors
  xname <- deparse(substitute(x))

  # Remove missing elements
  nodata <- which(is.na(x))
  if (length(nodata) > 0) {
    x <- x[-nodata]
    cat(length(nodata), " elements had missing values. These were removed from the data.\n")
  }

  # Re-express data if required
  if (p != 1) {
    x <- eda_re(x, p = p, tukey = tukey, base = base)
  }
  x.isna <- is.na(x)
  rm.nan <- ifelse(any(x.isna), 1, 0)

  # Re-expression may produce NaN values. Output warning if TRUE
  if (rm.nan > 0) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the output."))
    x <- x[!x.isna]
  }

  # Get upper/lower bounds of inner values
  b.val <- c(.5 - inner / 2, .5 + inner / 2)

  # Compute residuals if requested
  if (resid == TRUE) {
    x <- x - stat(x)
  }

  # Sort x
  y <- sort(na.omit(x))
  N <- length(y)

  # Setup theoretical distribution functions
  qtheo <- get(paste0("q", dist), mode = "function")
  rtheo <- get(paste0("r", dist), mode = "function")

  # Add matching normal quantiles to each dataframe in the list
  prob <- eda_fval(y, q.type = q.type)
  x_theo <- do.call(qtheo, c(list(p = prob), dist.l))

  # Calculate IQR scaling parameters mapping theoretical quantiles to y scale
  probs_iqr <- c(0.25, 0.75)
  yy_iqr <- as.vector(quantile(y, probs_iqr, names = FALSE, type = q.type))
  xx_iqr <- do.call(qtheo, c(list(p = probs_iqr), dist.l))
  slope_iqr <- diff(yy_iqr) / diff(xx_iqr)
  int_iqr   <- yy_iqr[[1L]] - slope_iqr * xx_iqr[[1L]]

  # Permutation / Simulation Block under H0 ----
  sim_list <- list()
  if (nsim > 0) {
    for (i in seq_len(nsim)) {
      # Draw N random samples from theoretical distribution
      raw_sim <- do.call(rtheo, c(list(n = N), dist.l))

      # Scale draw to match y-axis coordinates if dist doesn't fix scale
      if (dist %in% c("norm", "exp") && length(dist.l) == 0) {
        sim_y <- int_iqr + slope_iqr * sort(raw_sim)
      } else {
        sim_y <- sort(raw_sim)
      }
      sim_list[[i]] <- data.frame(x = x_theo, y = sim_y)
    }
  }
  # End of permutation ---

  dfqq <- data.frame(y, x_theo)
  names(dfqq) <- c(xname, xlab)

  x <- x_theo  # Theoretical quantiles on horizontal axis
  # y is sample data on vertical axis

  # Determine plot axis limits including simulation bounds
  if (nsim > 0) {
    all_sim_y <- unlist(lapply(sim_list, function(d) d$y))
    ylim <- range(c(y, all_sim_y), na.rm = TRUE)
    xlim <- range(x, na.rm = TRUE)
  } else {
    xlim <- range(x, na.rm = TRUE)
    ylim <- range(y, na.rm = TRUE)
  }

  plotcol <- rgb(1 - grey, 1 - grey, 1 - grey)

  if (!is.null(alpha)) {
    if (p.col %in% colors() & p.fill %in% colors()) {
      p.col  <- adjustcolor(p.col, alpha.f = alpha)
      p.fill <- adjustcolor(p.fill, alpha.f = alpha)
    }
  }

  # Render Plot ----
  if (plot == TRUE) {

    # Get lines-to-inches ratio
    in2line <- (par("mar") / par("mai"))[2]

    # Create a dummy plot to extract y-axis labels
    pdf(NULL)
    plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt = "n", main = NULL, xlim = xlim, ylim = ylim)
    y.wid <- max(strwidth(axTicks(2), units = "inches")) * in2line + 1.2
    dev.off()

    # Get quantile parameters
    qx <- quantile(x, b.val, qtype = q.type)
    qy <- quantile(y, b.val, qtype = q.type)

    # If tail points  are to be plotted differently, identify them
    if (tails == TRUE) {
      if (!is.na(table(x < qx[1])["TRUE"]) & !is.na(table(y < qy[1])["TRUE"])) {
        lower.tail <- min(table(x < qx[1])["TRUE"], table(y < qy[1])["TRUE"])
      } else {
        lower.tail <- 0
      }

      if (!is.na(table(x > qx[2])["TRUE"]) & !is.na(table(y > qy[2])["TRUE"])) {
        upper.tail <- max(table(x > qx[2])["TRUE"], table(y > qy[2])["TRUE"])
      } else {
        upper.tail <- 0
      }

      inner.tails <- (lower.tail + 1):(length(x) - upper.tail)
      outer.tails <- -inner.tails
    }

    # Set canvas with square aspect ratio and custom margins
    par(pty = "s", col = plotcol, mar = c(3, y.wid, 3, 1))

    # Base canvas setup
    if (tails != TRUE) {
      plot(x = x, y = y, type = "n", ylab = NA, las = 1, yaxt = "n", xaxt = "n", xlab = NA,
           xlim = xlim, ylim = ylim)
    } else {
      plot(x = x[inner.tails], y = y[inner.tails], type = "n", ylab = NA, las = 1,
           yaxt = "n", xaxt = "n", xlab = NA, xlim = xlim, ylim = ylim)
    }

    box(col = plotcol)
    axis(1, col = plotcol, col.axis = plotcol, labels = TRUE, padj = -0.5)
    axis(2, col = plotcol, col.axis = plotcol, labels = TRUE, las = 1, hadj = 0.9, tck = -0.02)

    # Draw Simulation Cloud (Background)
    if (nsim > 0) {
      sim_color <- adjustcolor(sim.col, alpha.f = sim.alpha)
      for (i in seq_len(nsim)) {
        if (sim.type == "lines") {
          lines(sim_list[[i]]$x, sim_list[[i]]$y, col = sim_color, lwd = 1)
        } else {
          points(sim_list[[i]]$x, sim_list[[i]]$y, pch = sim.pch,
                 col = sim_color, cex = size * 0.8)
        }
      }
    }

    # Re-draw Observed Points (Foreground)
    if (tails != TRUE) {
      points(x = x, y = y, pch = pch, col = p.col, bg = p.fill, cex = size)
    } else {
      points(x = x[inner.tails], y = y[inner.tails], pch = pch, col = p.col,
             bg = p.fill, cex = size)
      if (length(x[outer.tails]) != 0) {
        points(x = x[outer.tails], y = y[outer.tails], pch = tail.pch,
               col = tail.p.col, bg = tail.p.fill, cex = size)
      }
    }

    # Y-label placement
    lbl_width <- strwidth(ylab, units = "inches")
    mar_width <- par("mai")[2]
    loc <- par("usr")
    xscl <- (loc[2] - loc[1]) / par("pin")[1]
    if (lbl_width / 2 > mar_width * 0.6) {
      xloc <- loc[1] + (lbl_width / 2 - mar_width * 0.6) * xscl
    } else {
      xloc <- loc[1]
    }
    text(xloc, loc[4], labels = ylab, col = plotcol, cex = par("cex"), xpd = TRUE, pos = 3, offset = 1)

    title(xlab = xlab, line = 1.8, col.lab = plotcol)
    if (!is.null(title)) {
      title(main = title, line = 1.2, col.main = plotcol, cex.main = t.size)
    }

    # Add IQR Line
    if (iqr == TRUE) {
      abline(int_iqr, slope_iqr, col = plotcol)
    }

    # Add grid
    if (grid == TRUE) grid()

    # Add medians
    if (med == TRUE) {
      medx <- median(x)
      medy <- median(y)
      abline(v = medx, col = "grey80", lty = 2)
      abline(h = medy, col = "grey80", lty = 2)
    }

    # Add Core Quantile Boxes
    sq <- par("usr") # Get plot corners
    if (q == TRUE) {
      rect(xleft = qx[1], xright = qx[2], ybottom = sq[3], ytop = sq[4],
           col = qcol, border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom = qy[1], ytop = qy[2],
           col = qcol, border = NA)
    }

    # Add parameter sub-heading
    if (show.par == TRUE) {
      mtext(side = 3, text = paste0("p=", p), adj = 1, cex = 0.65)
    }
  }

  invisible(dfqq)
}
