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
#' @param grp Column of grouping variables if \code{dat} is a dataframe, ignored
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
#' @param inner Fraction of values that should be captured by the inner color
#'   band of the normal and density plots. Defaults to 0.6826 (inner 68\% of
#'   values).
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
#'
#' @references
#'
#' \itemize{
#'   \item William S. Cleveland. Visualizing Data. Hobart Press (1993)}
#'
#' @examples
#'
#' # Generate a basic residual-fit spread plot
#' eda_rfs(mtcars,mpg, cyl)
#'
#' # Add inner 68.4% region to residuals (grey boxes in plot)
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
                    p.fill = "grey80",inner = 0.6826, q = FALSE,
                    size = 0.8, alpha = 0.7, ylim = NULL){

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
  cat(sprintf("The mid %0.1f%% of residuals covers about %0.2f units.\n",
               diff(b.val)*100, diff(qy)))
  cat(sprintf("The fitted values cover a range of about %0.2f units,\n",
              diff(range(model_sort))))
  cat(sprintf("or about %0.1f%% of the mid %0.1f%% of residuals.",
              diff(range(model_sort)) / diff(qy) * 100, diff(b.val)*100))

  # Generate the plot

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = model_sort, y = res_sort, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL)
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # X-axis labels
  xtics <- c(0, .25, 0.5, 0.75, 1)

  # Side-by-side plots

  # Set plot parameters
  .pardef <- par(mfrow = c(1,2),mai = c(0.6,0,0.2,0),
                 oma = c(2, y.wid, 1,0.5), col = plotcol)
   on.exit(par(.pardef))

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
      # rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
      #       col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
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

