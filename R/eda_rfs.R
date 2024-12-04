#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Residual-Fit Spread Plot
#'
#' @description \code{eda_rfs} generates a Cleveland residual-fit spread plot
#'    for univariate or bivariate data.
#'
#' @param dat An eda_lm model, an lm model or a dataframe of univariate data.
#' @param x   Column of values if \code{dat} is a dataframe, ignored otherwise.
#' @param grp Column of categorical variable if \code{dat} is a dataframe, ignored
#'   otherwise.
#' @param p  Power transformation to apply to univariate data. Ignored if linear
#'   model is passed to function.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (\code{TRUE}) or if a Box-Cox transformation should be adopted (\code{FALSE}).
#' @param show.par Boolean determining if the power transformation used with the
#'   data should be displayed in the plot's upper-right corner.
#' @param stat Choice of summary statistic to use when centering the fitted
#'   values around 0. The \code{stat} parameter is also used for fitting
#'   univariate values (i.e. for summarizing groups). \code{stat} can be either
#'   \code{mean} or \code{median}.
#' @param inner Fraction of values that should be captured by the shaded region.
#'   Defaults to inner 90\% of values.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if grey quantile boxes should be plotted.
#' @param ylim Define custom y axis limits.
#' @param bar Boolean determining if spread comparison stacked bars should be
#'   plotted.
#'
#' @return {No values are returned}
#'
#' @details
#' The \code{eda_rfs} function generates a residual-fit spread plot for
#' univariate and bivariate data. Input can be a dataframe with one column
#' storing the continuous variable and another column storing the categorical
#' (grouping) variable or, for a bivariate dataset, a model output from an
#' \code{lm()}, \code{eda_lm()} or \code{eda_rline()} function.\cr
#' \cr
#' The \code{stat} argument only applies to univariate data and allows the user
#' to choose the summary statistic to fit to the data (either mean or median).
#' This statistic is also used to recenter the fitted values in the rfs plot.\cr
#' \cr
#' The \code{q} argument, when set to \code{TRUE}, will add a shaded region to
#' the residual quantile plot highlighting the mid portion of the data defined
#' by the \code{inner} argument (set to 90\% of the mid values, by default). The
#' range defined by the mid portion of the data is highlighted in the left plot
#' for comparison with the the full range defined by the fitted values. \cr
#' \cr
#' The \code{bar} option, when set to \code{TRUE}, adds a narrow stacked barplot
#' that compares the spread covered by the residuals (red bar) with the spread
#' covered by the fitted values (green bar). The residual spread is computed
#' for the portion of the residuals defined by the \code{inner} argument. The
#' values outputted in the console are those used in computing the vertical
#' bars. The red bar is the relative spread of the residuals and the green bar
#' is the relative spread of the fitted values. The stacked bar plot can be
#' helpful in quickly gauging the effect the fitted values have in explaining
#' the variability in the data. The longer the green bar relative to the red
#' bar, the greater the grouping variable's (for univariate data) or linear
#' model's (for bivariate data) effect in minimizing the uncertainty in the
#' estimated value.
#'
#' @references
#'
#' \itemize{
#'   \item William S. Cleveland. Visualizing Data. Hobart Press (1993)
#'   \item \href{../articles/rfs.html}{Residual-fit spread article}}
#'
#' @examples
#'
#' # Generate a basic residual-fit spread plot
#' eda_rfs(mtcars,mpg, cyl)
#'
#' # Add inner 90% region to residuals (grey boxes in plot)
#' # Vertical grey box shows matching y-values
#' eda_rfs(mtcars,mpg, cyl, q = TRUE)
#'
#' # Change guide to encompass mid 75% of residual values
#' eda_rfs(mtcars,mpg, cyl, q = TRUE, inner = 0.75)
#'
#' # Use median instead of the mean to compute group summaries and to
#' # recenter the fitted values around 0.
#' eda_rfs(mtcars,mpg, cyl, stat = median)
#'
#' # Apply power transformation of -1 to mpg. Defaults to box-cox method.
#' eda_rfs(mtcars,mpg, cyl, p = -1)
#'
#' # Display a stacked bar plot showing relative importance in spreads
#' # between fitted values and residuals.
#' eda_rfs(mtcars,mpg, cyl, bar = TRUE)
#'
#' # Generate rfs plot for bivariate model output. Model can be generated from
#' # lm(), eda_lm() or eda_rline()
#' M1 <- lm(hp ~ mpg, mtcars)
#' eda_rfs(M1,q =TRUE)
#'
#' M2 <- eda_lm(mtcars, mpg, hp)
#' eda_rfs(M2,q =TRUE)
#'
#' M3 <- eda_rline(mtcars, mpg, hp)
#' eda_rfs(M3, q =TRUE)


eda_rfs <- function(dat, x=NULL, grp=NULL, p = 1L, tukey = FALSE, show.par = TRUE,
                    stat = mean, grey = 0.7, pch = 21, p.col = "grey50",
                    p.fill = "grey80",inner = 0.9, q = FALSE,
                    size = 0.8, alpha = 0.7, ylim = NULL, bar = FALSE){

  # Check that input is either an eda_lm model or a dataframe
  if (! (inherits(dat,"data.frame") |
         (inherits(dat,"eda_lm") |
          inherits(dat, "lm") |
          inherits(dat, "eda_rline"))))
    stop("The input object must of class eda_lm or a data.frame.")

  # Check other arguments
  if (! as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Stat must be either the mean or the median")

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Get upper/lower f-values
  lower <- (1 - inner) / 2
  upper <- 1 -lower
  b.val  <- c(lower, upper)

  # Univariate scenario
  if(inherits(dat,"data.frame")){
    type <- "univariate"
    x   <- eda_re(eval(substitute(x), dat), p = p, tukey = tukey)
    grp <- eval(substitute(grp), dat)
    if(is.factor(grp)) grp <- droplevels(grp)

    # Remove missing values from the data
    which_na <- which(is.na(x))
    if(length(which_na > 0)){
      x <- x[-which_na]
      grp <- grp[-which_na]
      if(is.factor(grp)) grp <- droplevels(grp)
      warning(cat(length(which_na),"rows were removed due to NAs being present.\n"))
    }

    model <- ave(x, grp, FUN=stat)
    res <- x - model
    x_sort <- sort(x)
    model_sort <- sort(model) - stat(x) # Sort and rescale
    res_sort <- sort(res)
    fval <- (1:length(x) - 0.5) / length(x)
    if(is.null(ylim)){
      ylim <- range(res_sort, model_sort)
    }


    # Get quantiles for box boundaries
    qy <- quantile(res, b.val, qtype = 5)
    # Get matching quantiles in the fitted data
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # lm model scenario
  if(inherits(dat,"lm")){
    res_sort <- sort(dat$residuals)
    model_sort <- sort(dat$fitted.values - stat(dat$fitted.values))
    fval <- (1:length(res_sort) - 0.5) / length(res_sort)
    if(is.null(ylim)){
      ylim <- range(res_sort, model_sort)
    }
    show.par <- FALSE
    # Get quantiles for box boundaries
    qy <- quantile(res_sort, b.val, qtype = 5)
    # Get matching quantiles in the fitted data
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # eda_lm model scenario
  if(inherits(dat,"eda_lm") | inherits(dat,"eda_rline")){
    res_sort <- sort(dat$residuals)
    model_sort <- sort(dat$fitted.values - stat(dat$fitted.values))
    fval <- (1:length(res_sort) - 0.5) / length(res_sort)
    if(is.null(ylim)){
      ylim <- range(res_sort, model_sort)
    }
    show.par <- FALSE
    # Get quantiles for box boundaries
    qy <- quantile(res_sort, b.val, qtype = 5)
    # Get matching quantiles in the fitted data
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # Compare spreads using user defined quantiles
  res.spd <- diff(qy)  # Residual spread
  fit.spd <- diff(range(model_sort))  # Fitted model spread
  res.fit <- fit.spd / res.spd  # ratio
  cat(sprintf("The mid %0.1f%% of residuals covers about %0.2f units.\n",
               diff(b.val)*100, res.spd))
  cat(sprintf("The fitted values cover a range of %0.2f units, ",
              fit.spd))
  cat(sprintf("or about %0.1f%% of the mid %0.1f%% of residuals.",
              res.fit * 100, diff(b.val)*100))

  # Generate the plot

  # Get lines-to-inches ratio
  in2liney <- ( par("mar") / par("mai") )[2]

    # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = model_sort, y = res_sort, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL)
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2liney + 1.2
  dev.off()

  # X-axis labels
  xtics <- c(0, .25, 0.5, 0.75, 1)

  # Side-by-side plots

  # Set plot parameters
  .pardef <- par(no.readonly = TRUE)
  on.exit(par(.pardef))
  if (bar == TRUE){
    layout(matrix(c(1,2,3),1,3,byrow=TRUE), widths = c(0.49,0.02,0.49))
  } else {
    layout(matrix(c(1,2),1,2,byrow=TRUE), widths = c(0.5,0.5))
  }
  par(mai = c(0.6,0,0.2,0), cex = 1, oma = c(2, y.wid, 1,0.5), col = plotcol)

    # Fit plot
    plot(fval, model_sort, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, col.lab=plotcol,
         pch = pch, col = p.col, bg = p.fill, cex = size, ylim = ylim,
         main = paste("Fit minus",as.character(substitute(stat))), col.main = "grey40")
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5, at = xtics)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)
    title(xlab = "f-value", line =1.4, col.lab=plotcol)
    grid(nx=NA, ny = NULL)
    abline(v = xtics,lty = "dotted", col = "lightgray")

    # Add box to fit plot
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }

    # Add stacked bar plot if desired
    if (bar == TRUE){
      res.top <- res.spd / (fit.spd + res.spd)
      plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "",
           ylab = "", xaxt="n", yaxt="n", bty="o",xaxs="i",yaxs="i")
      rect(xleft = 0.03, ybottom = 0, xright = 1, ytop = res.top, col = "#FAC4C3",
           border = FALSE)
      rect(xleft = 0.03, ybottom = res.top , xright = 1, ytop = 1, col = "#CEE6B5",
           border = FALSE)
      box(col=plotcol)
    }

    # Residual plot
    plot(fval, res_sort, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
         col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
         ylim = ylim, main = "Residuals", col.main = "grey40")
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5,
         at = xtics[-1])
    title(xlab = "f-value", line =1.4, col.lab=plotcol)
    grid(nx=NA, ny = NULL)
    abline(v = xtics,lty = "dotted", col = "lightgray")

    # Add box to fit plot
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = b.val[1], xright = b.val[2], ybottom=sq[3],ytop=sq[4],
          col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }

    # Parameters to plot
    if(show.par == TRUE){
      params <- gsub(";\\s*;?\\s*$", "",  paste0("p=", p))
      mtext(side = 3, text=params, adj=1, cex = 0.65)
    }

  par(.pardef)
}

