#' Plot method for \code{eda_polish} objects
#'
#' This is an S3 method for plotting objects of class `eda_polish`. It allows
#' visualization of different aspects of the analysis, including residuals,
#' cross-validation values, diagnostic plots, or effects.
#'
#' @param x An object of class \code{eda_polish}, as produced by \code{eda_pol}.
#' @param plot A character string specifying the type of plot to generate. Must
#'   be one of \code{"residuals"}, \code{"cv"}, \code{"diagnostic"}, or
#'   \code{"effects"}.
#' @param add.cv Logical. If \code{TRUE}, adds kCV (k-fold cross-validation) values
#'   to the residuals plot. Requires specifying \code{k}.
#' @param k Numeric. The value of k for kCV. Required when \code{add.cv ==TRUE}.
#' @param colpal A character string specifying the color palette to use for
#'   coloring plots. Must be a name listed in \code{hcl.pals()}. Defaults to
#'   \code{"RdYlBu"}.
#' @param colrev Logical. If \code{TRUE}, reverses the order of colors in the
#'   palette.
#' @param col.eff Logical. Controls coloring for effects plots.
#' @param col.com Logical. Controls coloring for common effects.
#' @param col.quant Logical. If \code{TRUE}, adopt a quantile color classification
#'   break.
#' @param adj.mar Logical. If \code{FALSE}, adjusts graphical margins using
#'   \code{par(mar = c(1.5, 1.5, 1.5, 1.5))}.
#' @param res.size Numeric. Controls the size of residual points or text.
#' @param row.size Numeric. Controls row element size.
#' @param col.size Numeric. Controls column element size.
#' @param round Numeric. Number of digits to round values for display (e.g., in
#'   tables).
#' @param res.txt Logical. If \code{TRUE}, displays residual text.
#' @param label.txt Logical. If \code{TRUE}, displays labels text.
#' @param ... Arguments passed to \code{.eda_plot_xy} if
#'       \code{plot = "diagnostic"}.
#'
#' @inheritDotParams  .eda_plot_vardecomp
#'
#' @return Produces a plot. The returned value may vary depending on the
#'   \code{plot} type.
#'
#' \itemize{
#'   \item \href{../articles/polish.html}{Median polish article}}
#'
#' @method plot eda_polish
#'
#' @importFrom grDevices adjustcolor colors dev.off gray hcl.colors hcl.pals pdf
#'   rgb
#' @importFrom graphics abline axTicks axis box boxplot grid image lines mtext
#'   par rect stripchart strwidth text title
#' @importFrom stats IQR aggregate coef lm median predict quantile reorder
#'   reshape residuals setNames
#' @importFrom utils combn modifyList
#'
#' @examples
#' # Generate median polish object
#' M <- eda_pol(inf_mort, region, edu, perc, plot = FALSE)
#'
#' # Generate residuals plot
#' plot(M)
#'
#' # Generate effects plot
#' plot(M, plot = "effects", label = TRUE)
#'
#' # Generate diagnostic plot
#' plot(M, plot = "diagnostic")
#'
#' # Add a robust regression line
#' plot(M, plot = "diagnostic", reg = TRUE, robust = TRUE)
#'
#' @export

# Modified to accept NA values in residuals
# Added variance partitioning plot

plot.eda_polish <- function (x, plot = "residuals", add.cv = FALSE, k = NULL, col.quant = FALSE,
          colpal = "RdYlBu", colrev = TRUE, col.eff = TRUE, col.com = TRUE,
          adj.mar = TRUE, res.size = 1, row.size = 1, col.size = 1, round = 2,
          res.txt = TRUE, label.txt = TRUE, ...) {

  # Check for valid input values

  if (!inherits(x, "eda_polish"))
    stop("The input object must of class eda_polish")
  if (!plot %in% c("residuals", "cv", "diagnostic", "effects"))
    stop("Paramater \"plot=\" must be of \"residuals\", \"cv\", \"diagnostic\" or \"effects\". ")
  if (add.cv == TRUE & is.null(k))
    stop("You are adding kCV to model, but you don't specify k.")
  if (!colpal %in% hcl.pals())
    stop("Color palette (colpal) should be one listed in hcl.pals().")
  mat <- x$wide
  row <- mat[-1, 1]
  col <- unlist(mat[1, -1])
  cv.mat <- matrix(apply(expand.grid(row, col), 1, prod),
                   ncol = length(col))/x$global


  if (plot == "diagnostic") {
    if (sum(is.finite(x$long[, 7])) > 1) {
      cv <- x$long[, 7]
      residuals <- x$long[, 3]
      x2 <- data.frame(Residuals = residuals, cv = cv)

      # Check for arguments passed via ...
      dots <- list(...)
      if(!"sd" %in% names(dots)) sd <- FALSE else sd <- dots$sd
      if(!"mean.l" %in% names(dots)) mean.l <- FALSE else mean.l <- dots$mean.l
      if(!"loe" %in% names(dots)) loe <- TRUE else loe <- dots$loe
      if(!"loe.col" %in% names(dots)) loe.col <- rgb(0, 0, 1, 0.7) else loe.col <- dots$loe.col
      if(!"span" %in% names(dots)) span <- 0.5 else span <- dots$span
      if(!"reg" %in% names(dots))   reg <- FALSE else reg <- dots$reg

      # Remove handled args from dots
      dots <- dots[setdiff(names(dots), c("sd", "mean.l", "loe",
                                          "loe.col", "span", "reg"))]

      call <- as.call(c(quote(tukeyedar:::.eda_plot_xy),
                        list(x2, x = quote(cv), y =quote(Residuals),
                             xlab = "Comparison Value", reg = reg,
                             mean.l = mean.l, sd = sd, loe = loe,
                             loess.d = list(family = "symmetric", span = span),
                             loe.col = loe.col, loe.lw = 1.5,  hline = 0, vline = 0),
                        dots))
      eval(call, envir = parent.frame())
    }
    else {
      return("CV values are not finite")
    }
  }else if (plot == "effects") {
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = x$effects, ...)
    # call <- as.call( c(quote(package:::.eda_plot_vardecomp),
    #                    list( dat = x$long, response = x$response,
    #                          eff = x$effects), list(...) ))
    # eval(call, envir = parent.frame())
  }else{
    if (plot == "cv") {
      mat[-1, -1] <- cv.mat
    }
    if (plot == "residuals" & add.cv == TRUE) {
      mat[-1, -1] <- mat[-1, -1] - k * cv.mat
    }
    if (adj.mar == FALSE) {
      .pardef <- par(mar = c(1.5, 1.5, 1.5, 1.5))
    }
    else {
      .pardef <- par(mar = c(1.5, max(nchar(rownames(x$wide)))/1.8 *
                               row.size, 1.5, 1.5))
    }
    on.exit(par(.pardef), add = TRUE)
    len <- prod(dim(mat))
    if (col.eff == FALSE & col.com == FALSE) {
      max <- max(abs(range(mat[-1, -1], na.rm = TRUE)))
      quant.range <- mat[-1, -1]
    }
    else if (col.com == FALSE) {
      max <- max(abs(range(unlist(mat)[-1])))
      quant.range <- unlist(mat)[-1]
    }
    else if (col.eff == FALSE) {
      max <- max(abs(range(mat[-1, -1], na.rm = TRUE)))
      quant.range <- mat[-1, -1]
    }
    else {
      max <- max(abs(range(mat, na.rm = TRUE)))
      quant.range <- mat
    }
    if (col.quant == TRUE) {
      colbrk <- quantile(unlist(quant.range), prob = 0:len/len)
    }
    else {
      colbrk <- c(-max, seq(-max, max, length.out = len))
    }
    colMap <- hcl.colors(len, palette = colpal, rev = colrev,
                         alpha = 0.5)
    image(t(mat[nrow(mat):1, ]), axes = FALSE, col = colMap,
          breaks = colbrk)
    grid(nx = ncol(mat), ny = nrow(mat), col = "grey", lty = 1)
    U <- par("usr")
    abline(v = diff(U[1:2])/dim(mat)[2] + U[1], lw = 2)
    abline(h = U[4] - diff(U[3:4])/dim(mat)[1], lw = 2)
    col.ctr <- seq(0, 1, length.out = ncol(mat))
    row.ctr <- seq(1, 0, length.out = nrow(mat))
    if (label.txt == TRUE) {
      mtext(colnames(mat), at = col.ctr, side = 3, cex = col.size)
      mtext(rownames(mat), at = row.ctr, side = 2, las = 2,
            cex = row.size)
    }
    text.crd <- expand.grid(y = row.ctr, x = col.ctr)
    if (res.txt == TRUE) {
      text(x = text.crd[, "x"], y = text.crd[, "y"],
           labels = round(unlist(mat), round), cex = res.size)
    }
    par(.pardef)
  }
}
