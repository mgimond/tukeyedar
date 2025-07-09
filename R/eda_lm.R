#' @export
#' @import MASS
#' @importFrom utils modifyList
#' @title Regression plot (with optional LOESS fit)
#'
#' @description \code{eda_lm} generates a scatter and EDA enhanced regression
#'   plot.
#'
#' @param dat Dataframe.
#' @param x   Column assigned to the x axis.
#' @param y   Column assigned to the y axis.
#' @param px  Power transformation to apply to the x-variable.
#' @param py  Power transformation to apply to the y-variable.
#' @param base Base used with the log() function if \code{px} or 
#'  \code{py} is \code{0}.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param xlab X label for output plot.
#' @param ylab Y label for output plot.
#' @param ... Passed to \code{.eda_plot_xy} function.
#' 
#' @inheritDotParams  .eda_plot_xy
#'
#' @details The function will plot a regression line and, if requested, a loess
#'   fit. The function adopts the least squares fitting technique by default. It
#'   defaults to a first order polynomial fit. The polynomial order can be
#'   specified via the \code{poly} argument.
#'   \cr\cr
#'   The plot displays the +/- 1 standard deviations as dashed lines. In
#'   theory, if both x and y values follow a perfectly Normal distribution,
#'   roughly 68 percent of the points should fall in between these lines.
#'   \cr \cr
#'   The true 68 percent of values can be displayed as a shaded region by
#'   setting \code{q=TRUE}. It uses the \code{quantile} function to compute
#'   the upper and lower bounds defining the inner 68 percent of values. If the
#'   data follow a Normal distribution, the grey rectangle edges should coincide
#'   with the +/- 1SD dashed lines.
#'   If you wish to show the interquartile ranges (IQR) instead of the inner
#'   68 percent of values, simply set \code{inner = 0.5}.
#'   \cr \cr
#'   The function offers the option to re-express the values via the \code{px} and
#'   \code{py} arguments. But note that if the re-expression produces \code{NaN}
#'    values (such as if a negative value is logged) those points will be
#'    removed from the plot. This will result in fewer observations being
#'    plotted. If observations are removed as a result of a re-expression, a
#'    warning message will be displayed in the console.
#'    The re-expression powers are shown in the upper right side of the plot. To
#'   suppress the display of the re-expressions set \code{show.par = FALSE}.
#'   \cr\cr
#'   If the \code{robust} argument is set to TRUE, \code{MASS}'s
#'   built-in robust fitting model, \code{rlm}, is used to fit the regression
#'   line to the data. \code{rlm} arguments can be passed as a list via the
#'   \code{rlm.d} argument.
#'
#' @return Returns a list of class \code{eda_lm}. Output includes the following
#'    if \code{reg = TRUE}. Returns \code{NULL} otherwise.
#'
#' \itemize{
#'   \item \code{data}: Input data table with residuals
#'   \item \code{residuals}: Regression model residuals
#'   \item \code{a}: Intercept
#'   \item \code{b}: Polynomial coefficient(s)
#'   \item \code{fitted.values}: Fitted values
#'   \item \code{x}: x variable
#'   \item \code{x_lab}: x label}
#'
#' @seealso 
#'  \code{\link[graphics]{plot}}, \code{\link[stats]{loess.smooth}}, 
#'  \code{\link{.eda_plot_xy}}
#'
#'
#' @examples
#'
#' # Add a regular (OLS) regression model and loess smooth to the data
#' eda_lm(mtcars, wt, mpg, loe = TRUE)
#'
#' # Add the inner 68% quantile to compare the true 68% of data to the SD
#' eda_lm(mtcars, wt, mpg, loe = TRUE, q = TRUE)
#'
#' # Show the IQR box
#' eda_lm(mtcars, wt, mpg, loe = TRUE, q = TRUE, sd = FALSE, inner = 0.5)
#'
#' # Fit an OLS to income for Female vs Male
#' inc <- read.csv("https://mgimond.github.io/ES218/Data/Income_education.csv")
#' eda_lm(inc, x=B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             loe = TRUE)
#'
#' # Add the inner 68% quantile to compare the true 68% of data to the SD
#' eda_lm(inc, x = B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             q = TRUE)
#'
#' # Apply a transformation to x and y axes: x -> 1/3 and y -> log
#' eda_lm(inc, x = B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             px = 1/3, py = 0, loe = TRUE)
#'             
#' # You can opt to show the original values on a scaled axis
#' eda_lm(inc, x = B20004013, y = B20004007, xlab = "Female", ylab = "Male",
#'             px = 1/3, py = 0, loe = TRUE, raw_tick = TRUE)
#'
#' # Fit a second order polynomial
#' eda_lm(mtcars, hp, mpg, poly = 2)
#'
#' # Fit a robust regression model
#' eda_lm(mtcars, hp, mpg, robust = TRUE, poly = 2)

# swd <- function (dat, x, y, xlab = NULL, ylab = NULL, px = 1, py = 1, 
#           tukey = FALSE, show.par = TRUE, reg = TRUE, poly = 1, robust = FALSE, 
#           w = NULL, sd = TRUE, mean.l = TRUE, asp = TRUE, grey = 0.6, 
#           pch = 21, p.col = "grey50", p.fill = "grey80", size = 0.8, 
#           alpha = 0.8, q = FALSE, inner = 0.68, q.type = 5,
#           lm.col = rgb(1, 0.5, 0.5, 0.8), loe.col = rgb(0.3, 0.3, 1, 1), 
#           stats = FALSE, stat.size = 0.8, loess.d = list(family = "symmetric",
#           span = 0.7, degree = 1), rlm.d = list(psi = "psi.bisquare"), ...) 

eda_lm <- function (dat, x, y, xlab = NULL, ylab = NULL, px = 1, py = 1, 
                    tukey = FALSE, base = exp(1), ...)

{
  # Check for valid arguments
  dots <- list(...)
  dot_names <- names(dots)
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
  
  # input <- names(list(...))
  # check <- input %in% names(formals(cat))
  # if (any(!check)) 
  #   warning(sprintf("%s is not a valid argument.", paste(input[!check], 
  #                                                        collapse = ", ")))
  if (is.null(xlab)) {
    xlab = as.character(substitute(x))
  }
  if (is.null(ylab)) {
    ylab = as.character(substitute(y))
  }
  if (!missing(dat)) {
    x <- eval(substitute(x), dat)
    y <- eval(substitute(y), dat)
  }
  nodata <- unique(c(which(is.na(x)), which(is.na(y))))
  if (length(nodata > 0)) {
    x <- x[-nodata]
    y <- y[-nodata]
    cat(length(nodata), " rows had missing values. These were removed from the plot.\n")
  }
  x <- eda_re(x, p = px, tukey = tukey, base = base)
  x.nan <- is.na(x)
  y <- eda_re(y, p = py, tukey = tukey, base = base)
  y.nan <- is.na(y)
  if (any(x.nan, y.nan)) {
    warning(paste("\nRe-expression produced NaN values. These observations will", 
                  "be removed from output. This will result in fewer points", 
                  "in the ouptut."))
    bad <- x.nan | y.nan
    x <- x[!bad]
    y <- y[!bad]
  }
 
   dat <- data.frame(x,y)
   names(dat) <- c(xlab, ylab)

   lst0 <- .eda_plot_xy(dat, x, y, px = px, py = py, tukey = tukey, base = base, 
                        xlab = xlab, ylab = ylab, ...)
 
   dat$residuals <- lst0$residuals
   lst0$data <- dat
   lst0$px <- px
   lst0$py <- py
   class(lst0) <- "eda_lm"
   invisible(lst0)
}
