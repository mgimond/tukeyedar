#' @export
#' @title Plot eda_rline model
#'
#' @description A plot method for lists of \code{eda_rline} class.
#'
#' @param x Object of class \code{eda_rline}.
#' @param plot Plot type. One of two: "model", "residuals".
#' @param xlab Custom x-axis label. Defaults to column name.
#' @param ylab Custom y-axis label. Defaults to column name.
#' @param model Logical; whether to show  the resulting model above
#'  plot. Only applies to \code{type = "model"}.
#' @param pt3 Logical; whether to show the 3-pt summaries.
#'   Only applies to \code{type = "model"}.
#' @param seg Logical; whether line segments showing partitioning of x variables
#'   should be displayed.
#' @param fit Logical; whether to show the resistant fitted line.
#' @param fit.col Resistant line color.
#' @param reg Logical; whether to fit and display an OLS regression line.
#' @param lm.col OLS regression line color (only applicable if \code{reg = TRUE}.
#' @param lm.lty OLS regression line line type (only applicable if \code{reg = TRUE}.
#' @param loe Logical; whether to plot loess smooth line.
#' @param loe.col loess smooth line color (only applicable if \code{loe = TRUE}.
#' @param loe.lty loess smooth line type (only applicable if \code{loe = TRUE}.
#' @param sd  Logical; whether to show ±1 SD lines.
#' @param mean.l Logical; whether to show mean reference lines.
#' @param grey Numeric between 0–1; controls grayscale background elements (0 =
#'   black, 1 = white).
#' @param ... Arguments to be passed to \code{.eda_plot_xy}.
#'
#' @importFrom graphics points abline mtext
#'
#' @return
#' Returns the margins (\code{mar}) used to generate the plot via the
#' \code{par()} function. This parameter can be set with a subsequent call to
#' \code{par()} if additional elements are to be added to the plot.
#'
#' @details
#' The function generates a scatter plot with the fitted model from an
#'  \code{eda_rline} object.
#'
#' @examples
#' r.lm <- eda_rline(age_height, Months, Height)
#'
#' plot(r.lm)
#' plot(r.lm, pt3 = FALSE)
#'
#' # And an OLS regression line for comparison (defaults to blue dashed line)
#' plot(r.lm, reg = TRUE)
#'
#' # Plot residuals vs dependence plot. A Loess smooth is fitted to the data.
#' plot(r.lm, plot = "residuals")

plot.eda_rline <- function(x, plot = "model", xlab = NULL, ylab = NULL,
                           model = TRUE, pt3 = TRUE, seg = TRUE, fit= TRUE,
                           fit.col = rgb(1, 0, 0),
                           grey = 0.6, sd = FALSE, mean.l = FALSE,
                           reg = FALSE, lm.col = rgb(0.7, 0.7, 1, 0.8), lm.lty = 2,
                           loe = NULL, loe.col = rgb(0.7, 0.7, 1, 0.8), loe.lty = 2,
                           ...){

  # S3 method requires that the first plot method input data argument be named x
  # Moving x to "dat" so that x can be used elsewhere in the function
  dat <- x

  # Check for valid arguments
  dots <- list(...)
  dot_names <- names(dots)
  internal_args <- names(formals(.eda_plot_xy))

  # Check for invalid names
  invalid <- setdiff(dot_names, internal_args)
  if (length(invalid) > 0) {
    warning(sprintf("Invalid arguments passed to %s: %s",
                    deparse(substitute(.eda_plot_xy)),
                    paste(invalid, collapse = ", ")))
  }

  # Check parameters
  if (!inherits(dat,"eda_rline")) stop("The input object must be of class eda_rline")
  if (! plot %in% c("residuals", "model" ))
    stop("Paramater \"plot=\" must be of \"residuals\", or \"model\" ")

  # Build dataframe
  x = dat$x
  y = dat$y
  df <- data.frame(x , y , res = dat$residuals)

  # Get labels
  if(is.null(xlab)){
    xlab = dat$xlab
  }
  if(is.null(ylab) & plot == "model"){
    ylab = dat$ylab
  } else if (is.null(ylab) & plot == "residuals"){
    ylab = "Residuals"
  }

  # Check loess fit option
  if( is.null(loe) & (plot == "residuals") ){
    loe <- TRUE
  } else if(is.null(loe) ){
    loe <- FALSE
  }

  # Get power parameters
  px <- dat$px
  py <- dat$py
  tukey <- dat$tukey
  base <- dat$base

  names(df) <- c(xlab, ylab, "residuals")

  if (plot == "model") {
    lst0 <- .eda_plot_xy(df, x, y, px = px, py = py, tukey = tukey, base = base,
                         xlab = xlab, grey = grey,
                         ylab = ylab, reg = reg, lm.col = lm.col, lm.lty = lm.lty,
                         loe = loe, loe.col = loe.col, loe.lty = loe.lty, sd = sd,
                         mean.l = mean.l, ...)
  } else {
    lst0 <- .eda_plot_xy(df, x, y=residuals, px = px, py = py, tukey = tukey, base = base,
                         xlab = xlab, grey = grey, ylab = ylab, reg = reg, loe = loe,
                         sd = sd, mean.l = mean.l, hline = 0, ...)
  }

  # Set plot grey scale.
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  if (plot == "model"){
    .post <- par(mar = lst0$parxy)
    on.exit(par(.post))

    if(fit == TRUE){
      abline(a = dat$a, b = dat$b, col=fit.col)
    }

    if (pt3 == TRUE){
      points(cbind(dat$xmed, dat$ymed), pch =21, bg="red", cex = 1.2)
    }
    if (model == TRUE){
      mtext(sprintf("y = %f + (%f)x",dat$a, dat$b ), col = plotcol, cex = 0.7)
    }
    if (seg == TRUE){
      abline(v= dat$x[dat$index[-length(dat$index)]],lty=3, col = "grey")
    }

    par(.post)
  } else {
    if (seg == TRUE){
      abline(v= dat$x[dat$index[-length(dat$index)]],lty=3, col = "grey")
    }
  }

  invisible(lst0)
}

