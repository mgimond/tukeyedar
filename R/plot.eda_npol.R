#' @title Plot method for \code{eda_npol} objects
#'
#' @description Generates either a variability decomposition plot of factor
#'   effects or a diagnostic plot for an object of class \code{eda_npol}.
#'
#' @param x An object of class \code{eda_npol}.
#' @param plot A character string specifying the type of plot to generate.
#'   \describe{
#'     \item{\code{"effects"} (default)}{Generates a variability decomposition
#'                             plot showing residuals and factor effects.}
#'     \item{\code{"diagnostic"}}{Generates a scatterplot of residuals versus
#'                                comparison values (CV).}
#'   }
#' @param reg Logical. If \code{TRUE}, fits a linear regression line to the
#'   diagnostic plot. Only used when \code{plot = "diagnostic"}. Defaults to
#'   \code{FALSE}.
#' @param ... Additional arguments passed to internal plotting functions.
#'   \describe{
#'     \item{For \code{plot = "effects"}}{Arguments are passed to \code{\link{.eda_plot_vardecomp}}. Common options include:
#'       \itemize{
#'         \item \code{rotate}: Logical. Rotate plot orientation.
#'         \item \code{show.resp}: Logical. Include boxplot of the centered response.
#'         \item \code{outliers}: Logical. Show outliers in boxplots.
#'         \item \code{label}: Logical. Label individual effect levels.
#'         \item \code{order}: Logical. Order effects by spread.
#'         \item \code{cex.txt}: Numeric. Text size for labels.
#'         \item \code{lim}: Numeric vector. Axis limits.
#'         \item \code{overlap}: Character. One of \code{"stack"}, \code{"overplot"}, or \code{"jitter"}.
#'         \item \code{grey}: Numeric or character. Grayscale coloring.
#'         \item \code{type}: Character. Plot type, e.g., \code{"boxpnt"} or \code{"box"}.
#'         \item \code{input}: Character. Either \code{"nway"} or \code{"reg"}.
#'         \item \code{padding}: Numeric. Padding for axis limits.
#'       }}
#'
#'     \item{For \code{plot = "diagnostic"}}{Arguments are passed to \code{\link{.eda_plot_xy}}. Common options include:
#'       \itemize{
#'         \item \code{xlab}, \code{ylab}: Axis labels.
#'         \item \code{xlim}, \code{ylim}: Axis limits.
#'         \item \code{poly}: Integer. Degree of polynomial regression.
#'         \item \code{robust}: Logical. Use robust regression.
#'         \item \code{w}: Numeric vector. Weights for regression.
#'         \item \code{sd}, \code{mean.l}: Logical. Show ±1 SD and mean lines.
#'         \item \code{asp}, \code{square}: Logical. Control aspect ratio and plot shape.
#'         \item \code{grey}: Numeric. Grayscale background.
#'         \item \code{pch}, \code{p.col}, \code{p.fill}, \code{size}, \code{alpha}: Point styling.
#'         \item \code{q}, \code{inner}, \code{q.type}, \code{qcol}: Quantile box options.
#'         \item \code{loe}, \code{loe.col}, \code{loe.lw}, \code{loess.d}: Loess smoothing options.
#'         \item \code{lm.col}, \code{lm.lw}: Regression line styling.
#'         \item \code{stats}, \code{stat.size}: Display model statistics.
#'         \item \code{hline}, \code{vline}: Reference lines.
#'         \item \code{rlm.d}: List. Parameters for \code{MASS::rlm}.
#'       }}
#'   }
#'
#' @details
#'   This method serves as a wrapper to generate two types of plots for
#'   \code{eda_npol} objects:
#'
#'   \strong{1. Variability Decomposition Plot (\code{plot = "effects"})} \cr
#'   Calls \code{\link{.eda_plot_vardecomp}} to visualize residuals and factor
#'   effects. Useful for assessing the relative magnitude and spread of effects
#'   and identifying outliers.
#'
#'   \strong{2. Diagnostic Plot (\code{plot = "diagnostic"})} \cr Calls
#'   \code{\link{.eda_plot_xy}} to plot residuals against comparison values
#'   (CV). Useful for detecting nonadditivity or model misfit. Optional
#'   regression and smoothing lines can be added.
#'
#' @return A plot is generated on the active graphics device. The function
#'   itself returns \code{NULL}.
#'
#' @seealso \code{\link{.eda_plot_vardecomp}}, \code{\link{.eda_plot_xy}}
#'
#' @examples
#'
#' # Generate eda_npol object
#' M0 <- eda_npol(yarn, Cycles, Load, Length, Amplitude)
#'
#' # Plot effects (default)
#' plot(M0)
#'
#' # Add labels
#' plot(M0, label = TRUE)
#'
#' # Rotate plot
#' plot(M0, rotate = TRUE)
#'
#' # Add boxplot of centered response variable
#' plot(M0, show.resp = TRUE)
#'
#' # Generate diagnostic plot (residuals vs CV)
#' plot(M0, plot = "diagnostic")
#'
#' # Fit a robust regression line to diagnostic plot
#' # The function displays the line's slope in the console
#' plot(M0, plot = "diagnostic", reg = TRUE, robust = TRUE, loe = FALSE)
#'
#'
#'
#' @export
plot.eda_npol <- function(x, plot = "effects", reg = FALSE, ...) {

  # Checks
  if (!inherits(x, "eda_npol")) stop("Input object must be of class 'eda_npol'")
  if (!plot %in% c("effects", "diagnostic")) {
    stop("Invalid 'plot' argument. Choose 'effects' or 'diagnostic'.")
  }

  if (plot == "diagnostic") {
    # Check if CV is available
    if (!"cv" %in% names(x$long)) {
      stop("Diagnostic plot requires 'cv' values in the 'long' component of the eda_npol object.")
    }

    cv_values <- x$long$cv
    residuals <- x$long$residuals

    # Remove NA/Inf values which might occur if common_effect was 0
    valid_idx <- !(is.na(cv_values) | is.na(residuals) | is.infinite(cv_values) | is.infinite(residuals))
    cv_values <- cv_values[valid_idx]
    residuals <- residuals[valid_idx]

    if (length(cv_values) == 0) {
      message("No valid data points to plot diagnostic plot after removing NA/Inf CV or residuals.")
      return(invisible(NULL)) # Return invisibly without plotting
    }

    # Check for arguments passed via ...
    dots <- list(...)
    if(!"sd" %in% names(dots)) sd <- FALSE else sd <- dots$sd
    if(!"mean.l" %in% names(dots)) mean.l <- FALSE else mean.l <- dots$mean.l
    if(!"loe" %in% names(dots)) loe <- TRUE else loe <- dots$loe
    if(!"loe.col" %in% names(dots)) loe.col <- rgb(0, 0, 1, 0.7) else loe.col <- dots$loe.col
    if(!"span" %in% names(dots)) span <- 0.5 else span <- dots$span

    # Remove handled args from dots
    dots <- dots[setdiff(names(dots), c("sd", "mean.l", "loe", "loe.col", "span"))]

    if (sum(is.finite(cv_values)) > 1) {
      x2 <- data.frame(Residuals = residuals, cv = cv_values)
      call <- as.call(c(quote(tukeyedar:::.eda_plot_xy),
                        list(x2, x = quote(cv), y =quote(Residuals),
                             xlab = "Comparison Value", reg = reg,
                             mean.l = mean.l, sd = sd, loe = loe, px =1 , py =1,
                             raw_tick = FALSE, show.par = FALSE,
                             loess.d = list(family = "symmetric", span = span),
                             loe.col = loe.col, loe.lw = 1.5,  hline = 0, vline = 0),
                        dots))
     eval(call, envir = parent.frame())

    } else{
      return("CV values are not finite")
    }

  } else if (plot == "effects") {
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = x$effects, ...)
  }

}
