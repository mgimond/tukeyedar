#' @title x-y Scatterplot with EDA Enhancements
#'
#' @description
#' Creates an enhanced scatter plot of two variables with extensive
#' customization for exploratory analysis. Options include polynomial regression
#' lines, robust regression, quantile shading, mean/sd lines, loess smoothing,
#' and detailed styling parameters.
#'
#' @param dat Optional data frame containing \code{x} and \code{y}.
#' @param x A numeric vector or column name in \code{dat} for the x-axis.
#' @param y A numeric vector or column name in \code{dat} for the y-axis.
#' @param px Power transformation used in the input data to display if
#'   \code{show.par = TRUE}.
#' @param py  Power transformation used in the input data to display if
#'   \code{show.par = TRUE}.
#' @param base Base used with the log() function if \code{px} or
#'  \code{py} is \code{0}.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param raw_tick Logical. If \code{TRUE}, original (untransformed) equally
#'   spaced tick values are displayed on the re-expressed axes.
#' @param xlab Optional x-axis labels. Defaults to variable names.
#' @param ylab Optional y-axis labels. Defaults to variable names.
#' @param xlim X-axis range.
#' @param ylim Y-axis range.
#' @param show.par Logical; whether to display plot parameter summary on the
#'   plot. Currently only applies to regression model input.
#' @param reg Logical; whether to fit and display a regression line.
#' @param poly Integer; regression model polynomial degree (defaults to 1 for
#'   linear model).
#' @param robust Logical; if \code{TRUE}, uses robust regression (\code{MASS::rlm}).
#' @param rlm.d List; parameters for \code{MASS::rlm}, (e.g., \code{list(psi =
#'   "psi.bisquare")}).
#' @param w Optional numeric vector of weights for regression.
#' @param lm.col Regression line color.
#' @param lm.lw Numeric; Regression line width.
#' @param lm.lty Numeric; Regression line type.
#' @param sd Logical; whether to show ±1 SD lines.
#' @param mean.l Logical; whether to show x and y mean reference lines.
#' @param asp Logical; whether to preserve the aspect ratio (ignored if
#'   \code{square = FALSE}).
#' @param square Logical; whether to create a square plotting window.
#' @param grey Numeric between \code{0-1}; controls grayscale background elements
#' (\code{0 = black}, \code{1 = white}).
#' @param pch Integer; point symbol.
#' @param p.col Point border color.
#' @param p.fill Point fill color.
#' @param size Point size.
#' @param alpha Point transparency level (0 = 100\\% transparent, 1 = 100\\% opaque).
#' @param q Logical; whether to draw inner quantile boxes (quantile shading).
#' @param q.type Integer; type of quantile calculation (see \code{quantile}).
#' @param inner Numeric; defines the inner fraction of values to highlight
#'    with quantile shading.
#' @param qcol Fill color of quantile shading.
#' @param loe Logical; whether to plot loess smooth line.
#' @param loe.lw Numeric; Loess smooth line width.
#' @param loe.col Loess smooth color.
#' @param loe.lty Numeric; Loess smooth line type.
#' @param loess.d List; parameters for \code{loess.smooth}, e.g., \code{list(span = 0.7,
#'   degree = 1)}.
#' @param stats Logical; if \code{TRUE}, displays model statistics (R², β, p-value).
#' @param stat.size Text size for \code{stats} plot display.
#' @param hline Numeric; location(s) of additional horizontal reference lines.
#'   Can be passed via  the \code{c()} function.
#' @param vline Numeric; location(s) of additional vertical reference lines.
#'   Can be passed via the \code{c()} function.
#' @param plot Logical. Generates a plot if \code{TRUE}.
#' @param ... Additional graphical parameters (currently unused but reserved for
#'   future expansion).
#'
#' @return If \code{reg = TRUE}, invisibly returns a list of class
#' \code{"eda_lm"} with the following components:
#' \describe{
#'   \item{residuals}{Residuals from the model.}
#'   \item{a}{Intercept.}
#'   \item{b}{Slope or polynomial coefficients.}
#'   \item{fitted.values}{Predicted values.}
#'   \item{x}{x-values used.}
#'   \item{x_lab}{X-axis label.}
#'   \item{parxy}{\code{par()} parameters defining margins. Useful if user wants to add to the plot.}
#' }
#' If \code{reg = FALSE}, returns only the \code{par()} parameters defining margins.
#'
#' @details This function serves as a flexible plotting engine for creating
#' scatterplots with various enhancements useful in exploratory data analysis.
#'
#' @keywords plot graphics visualization EDA scatterplot
#'
#' @examples
#' # Basic usage
#' \dontrun{
#' .eda_plot_xy(mtcars, x = wt, y = mpg)
#'
#' # With robust regression and 2nd-degree polynomial
#' .eda_plot_xy(mtcars, wt, mpg, robust = TRUE, poly = 2)
#'
#' # With quantiles and loess
#' .eda_plot_xy(mtcars, wt, mpg, q = TRUE, loe = TRUE)
#' }

.eda_plot_xy <- function (dat, x, y, xlab = NULL, ylab = NULL, xlim = NULL,
                          ylim = NULL, px = 1, py = 1, tukey = NULL, base = NULL,
                          raw_tick = FALSE, show.par = TRUE, reg = TRUE,
                          poly = 1, w = NULL, robust = FALSE, rlm.d = list(psi = "psi.bisquare"),
                          lm.col = rgb(1, 0.5, 0.5, 0.8),  lm.lw = 1.7, lm.lty = 1,
                          sd = TRUE, mean.l = TRUE, asp = TRUE, square = TRUE, grey = 0.6,
                          pch = 21, p.col = "grey50", p.fill = "grey80", size = 0.8,
                          alpha = 0.8, q = FALSE, inner = 0.68, qcol = rgb(0, 0, 0, 0.05),
                          q.type = 5,  loe = FALSE, loe.lw = 1.7,  loe.lty = 2,
                          loe.col = rgb(0.3, 0.3, 1, 1),
                          loess.d = list(family = "symmetric", span = 0.7, degree = 1),
                          stats = FALSE,  stat.size = 0.8, hline = NULL, vline = NULL ,
                          plot = TRUE,  ...)
{
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

  # Set default labels if not provided
  if (is.null(xlab)) {
    xlab = as.character(substitute(x))
  }
  if (is.null(ylab)) {
    ylab = as.character(substitute(y))
  }

  # Evaluate x and y from data frame if 'dat' is provided
  if (!missing(dat)) {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }

  # Handle missing values
  nodata <- unique(c(which(is.na(x)), which(is.na(y))))
  if (length(nodata > 0)) {
    x <- x[-nodata]
    y <- y[-nodata]
    cat(length(nodata), " rows had missing values. These were removed from the plot.\n")
  }

  # Plot color settings
  plotcol <- rgb(1 - grey, 1 - grey, 1 - grey)
  if (!is.null(alpha)) {
    if (p.col %in% colors() & p.fill %in% colors()) {
      p.col <- adjustcolor(p.col, alpha.f = alpha)
      p.fill <- adjustcolor(p.fill, alpha.f = alpha)
    }
  }

  # Set initial plot margin
  parxy <- par()$mar

  # Prepare loess/rlm list parameters
  loess.l <- modifyList(list(span = 0.5), loess.d)

  # Set plot window type ("s" = square, "m" = maximum plot region)
  pty <- ifelse(square, "s", "m")
  asp <- ifelse(!square, FALSE, asp) # If not square, no sense in preserving aspect

  # --- Fitting Operation: Fit linear model (lm) or robust linear model (rlm)  ---
  if (robust == FALSE) {
    if (poly > 0) {
      M <- lm(y ~ poly(x, degree = poly, raw = TRUE), weights = w)
    }
    else {
      M <- lm(y ~ 1, weights = w)
    }
  }
  else {
    if (poly > 0) {
      rlm.d <- modifyList(list(formula = y ~ poly(x, degree = poly,
                                                  raw = TRUE)), rlm.d)
      M <- do.call(MASS::rlm, rlm.d) # Uses MASS::rlm
    }
    else { # Handle poly=0 case for rlm [4]
      rlm.d <- modifyList(list(formula = y ~ 1), rlm.d)
      M <- do.call(MASS::rlm, rlm.d)
    }
  }
  # ---------------------------------------------------------------------------------
  if (plot == TRUE){
    # Calculate y-axis limits based on data and fitted values
    if (is.null(ylim)) ylim = range(y, predict(M))

    # Calculate x-axis limits
    if (is.null(xlim)) xlim = range(x)

    # Calculate axis label width using a temporary PDF device
    in2line <- (par("mar")/par("mai"))[2]
    pdf(NULL)
    plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt = "n", main = NULL, ylim = ylim, xlim = xlim, ...)
    y.wid <- max(strwidth(axTicks(2), units = "inches")) * in2line + 1.2
    dev.off()

    # Set plotting parameters and ensure they are reset on exit
    .pardef <- par(pty = pty, col = plotcol, mar = c(3, y.wid, 3, 1))
    on.exit(par(.pardef))

    # Calculate parameters for aspect ratio
    sd.x <- sd(x, na.rm = TRUE)
    sd.y <- sd(y, na.rm = TRUE)
    mean.x <- mean(x, na.rm = T)
    mean.y <- mean(y, na.rm = TRUE)
    asp_val <- ifelse(asp == TRUE, sd.x/sd.y, NA)

    # --- Plotting Operation: Create the plot canvas and add elements ---
    # Initial plot call to set up axes, but no points (type="n")
    plot(x = x, y = y, asp = asp_val, ylab = NA, las = 1, xaxt = "n", yaxt = "n",
         xlab = NA, col.lab = plotcol, pch = pch, col = p.col, bg = p.fill,
         cex = size, ylim = ylim, xlim = xlim, ...)

    box(col = plotcol)

    # Scale tick marks if requested
    if(px != 1 & raw_tick == TRUE){
      x_orig <- eda_backtransform(xlim, p = px, tukey = tukey, base = base)
      ticks_x_lab <- pretty(x_orig)
      ticks_x_at <- eda_re(ticks_x_lab, p = px, tukey = tukey, base = base)
      message("Note: Scaled x-axis displays the untransformed values.\n")
    } else {
      ticks_x_lab <- pretty(xlim)
      ticks_x_at <- ticks_x_lab
    }

    if(py != 1 & raw_tick == TRUE){
      y_orig <- eda_backtransform(ylim, p = py, tukey = tukey, base = base)
      ticks_y_lab <- pretty(y_orig)
      ticks_y_at <- eda_re(ticks_y_lab, p = py, tukey = tukey, base = base)
      message("Note: Scaled y-axis displays the untransformed values.\n")
    } else {
      ticks_y_lab <- pretty(ylim)
      ticks_y_at <- ticks_y_lab
    }

    axis(1, col = plotcol, col.axis = plotcol, labels = ticks_x_lab,
         padj = -0.5, at = ticks_x_at, ... )
    axis(2, col = plotcol, col.axis = plotcol, labels = ticks_y_lab,
         las = 1, hadj = 0.9, tck = -0.02, at = ticks_y_at, ...)
    lbl_width <- strwidth(ylab, units = "inches")
    mar_width <- par("mai")[2]
    loc <- par("usr")
    xscl <- (loc[2] - loc[1])/par("pin")[1]
    if (lbl_width/2 > mar_width * 0.6) {
      xloc <- loc[1] + (lbl_width/2 - mar_width * 0.6) * xscl
    }
    else {
      xloc <- loc[1]
    }

    text(xloc, loc[4], labels = ylab, col = plotcol, cex = par("cex"),
         xpd = TRUE, pos = 3, offset = 1)
    sq <- par("usr")
    if (sd == TRUE) {
      ysd1 <- (mean.y + sd.y)
      ysd2 <- (mean.y - sd.y)
      text(label = "+1sd", x = sq[2] - diff(sq[1:2]) * 0.03,
           y = ysd1 + diff(sq[3:4]) * 0.02, srt = 0, col = "grey70",
           cex = 0.7)
      text(label = "-1sd", x = sq[2] - diff(sq[1:2]) * 0.03,
           y = ysd2 + diff(sq[3:4]) * 0.02, srt = 0, col = "grey70",
           cex = 0.7)
      text(label = "+1sd", y = sq[4] - diff(sq[3:4]) * 0.01,
           x = (mean.x + sd.x), srt = 0, col = "grey70", cex = 0.7)
      text(label = "-1sd", y = sq[4] - diff(sq[3:4]) * 0.01,
           x = (mean.x - sd.x), srt = 0, col = "grey70", cex = 0.7)
    }
    title(xlab = xlab, line = 1.8, col.lab = plotcol)
    if (reg == TRUE) {
      line_x <- seq(min(x), max(x), length.out = 300)
      line_y <- predict(M, newdata = data.frame(x = line_x))
      lines(line_x, line_y, lw = lm.lw, col = lm.col, lty = lm.lty)
    }
    if (mean.l == TRUE) {
      abline(v = mean.x, lty = 1, col = "grey70")
      abline(h = mean.y, lty = 1, col = "grey70")
    }
    if (sd == TRUE) {
      abline(v = mean.x + c(-sd.x, sd.x), lty = 2, col = "grey80")
      abline(h = mean.y + c(-sd.y, sd.y), lty = 2, col = "grey80")
    }
    if (loe == TRUE) {
      check <- try(lines(do.call("loess.smooth", c(list(x = x, y = y),
                                                   loess.l)),
                         col = loe.col, lw = loe.lw, lty = loe.lty), silent = TRUE)
      if (inherits(check, "try-error")) cat("Could not plot loess fit!\n")
    }


    if (stats == TRUE) {
      st <- summary(M)
      mtext(sprintf("R-sq = %0.2f  Beta= %g P(beta) = %0.3f",
                    st$r.sq, st$coef[2, 1], st$coef[2, 4]), side = 3,
            col = "blue", cex = stat.size)
    }

    # Draw quantile boxes if q is TRUE
    if (q == TRUE) {
      b.val = c(0.5 - inner/2, 0.5 + inner/2)
      qx <- quantile(x, b.val, type = q.type)
      #sq <- quantile(x, c(0.25, 0.75), type = 7)
      qy <- quantile(y, b.val, type = q.type)
      rect(xleft = qx[1], xright = qx[2], ybottom = sq[3],
           ytop = sq[4], col = qcol, border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom = qy[1],
           ytop = qy[2], col = qcol, border = NA)
    }

    # Add parameter text to the plot if show.par is TRUE
    if (show.par == TRUE & !is.null(px) & !is.null(py)) {
      params <- paste0("px=", round(px, 2), "\n py=", round(py, 2))
      mtext(side = 3, text = params, adj = 1, cex = 0.65)
    }

    # Add custom vertical and horizontal lines if requested
    if(!is.null(hline)) abline(h = hline, lty = 2, col = plotcol)
    if(!is.null(vline)) abline(v = hline, lty = 2, col = plotcol)
    # -------------------------------------------------------------------------------

    # Save margin parameters for output
    parxy <- par()$mar
    # Restore original plotting parameters on exit
    par(.pardef)
  }


  # Print coefficients if reg is TRUE
  if (reg == TRUE) {
    out_coef <- coef(M)
    if (poly > 0) {
      names(out_coef) <- c("int", paste0(xlab, "^", 1:poly))
    }
    else {
      names(out_coef) <- c("int")
    }
    print(out_coef)

    lst <- list(residuals = residuals(M), a = out_coef[1],
                b = out_coef[-1], fitted.values = predict(M), x = x,
                x_lab = xlab, parxy = parxy)
    class(lst) <- "eda_lm" # Retain class for consistency if needed elsewhere
    invisible(lst)
  }
  else {
    # Return NULL invisibly if reg is FALSE
    lst <- list(parxy = parxy)
    invisible(lst)
  }

}

# Back-transformation function

eda_backtransform <- function(z, p = 0, tukey = FALSE, base = exp(1)) {
  if (p == 0) {
    # Inverse of log(x, base) is base^z
    x <- ifelse(!is.na(z), base^z, NA)
  } else if (tukey == FALSE & p != 1) {
    # Inverse of (x^p - 1)/p is ((p * z) + 1)^(1/p)
    x <- ifelse(!is.na(z), ((p * z) + 1)^(1 / p), NA)
  } else {
    # Inverse of x^p is x^(1/p)
    x <- ifelse(!is.na(z), z^(1 / p), NA)
  }
  return(x)
}
