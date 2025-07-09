#' @export
#' @title Residuals plot
#'
#' @description
#'  Generate residuals vs fitted or residuals vs dependence plot from
#'  an \code{eda_lm} class object.
#'
#'@param x Object of class \code{eda_lm}.
#'@param plot Type of residuals plot. Choice between residuals-fit (\code{rf})
#'  and residuals-dependence (\code{rd}) plots.
#' @param loe Logical; whether to plot loess smooth line.
#' @param xlab X label for output plot.
#' @param ylab Y label for output plot.
#' @param sd  Logical; whether to show ±1 SD lines.
#' @param ... Passed to \code{.eda_plot_xy} function.
#'
#' @inheritDotParams  .eda_plot_xy
#'
#'
#' @return
#' Returns the margins (\code{mar}) used to generate the plot via the
#' \code{par()} function. This parameter can be set with a subsequent call to
#' \code{par()} if additional elements are to be added to the plot.
#'
#' @details The function generates a scatter plot of residuals vs dependence or
#'  residuals vs fitted values plot from a model of class \code{eda_lm}. A loess
#'  line is fitted to the data. By default, a robust loess is adopted using the
#'  \code{"symmetric"} family.
#'
#' @examples
#' M1  <- eda_lm(mtcars, hp, mpg)
#'
#' # Residual-dependence plot
#' plot(M1)
#'
#' # Residual-fit plot
#' plot(M1, plot = "rf")


plot.eda_lm <- function(x, plot = "rd", xlab = NULL, ylab = NULL, loe = TRUE,
                        sd = FALSE, ...){

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

  if (!inherits(dat,"eda_lm")) stop("The input object must be of class eda_lm.\n")

  if (!plot %in% c("rd", "rf")) stop("plot must be rd or rf.\n")

  # Get x and y values
  y  <- dat$residuals
  if (plot == "rd"){
    x <- dat$x
    x_lab <- dat$x_lab
  } else {
    x <- dat$fitted.values
  }

  # Get labels
  if(is.null(xlab)){
    if (plot == "rd"){
      xlab = x_lab
    } else {
      xlab = "Fitted values"
    }
  }
  if(is.null(ylab)){
    ylab = "Residuals"
  }

  if("show.par" %in% names(dots)){
    show.par <- dots$show.par
    dots[["show.par"]] <- NULL # Remove from list
  } else {
    show.par <- FALSE
  }

  df <- data.frame(x, residuals = dat$residuals)

  lst0 <- do.call(.eda_plot_xy, c(list(df, x, y, px = dat$px, py = dat$py, xlab = xlab,
                       ylab = ylab, loe = loe, reg = FALSE, sd = sd, show.par = show.par,
                       raw_tick = FALSE, hline = 0, mean.l = FALSE), dots))

  invisible(lst0)
}

