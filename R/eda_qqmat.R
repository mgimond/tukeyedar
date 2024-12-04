#' @export
#' @import grDevices
#' @import grid
#' @importFrom utils modifyList
#' @title QQ plot matrix
#'
#' @description \code{eda_qqmat} Generates a matrix of empirical QQ plots
#'
#' @param dat  Data frame.
#' @param x    Continuous variable.
#' @param fac  Categorical variable.
#' @param p  Power transformation to apply to the continuous variable.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
#' @param upper  Boolean determining if both upper and lower triangular matrix
#' should be plotted. If set to \code{FALSE}, only the lower triangular matrix
#' is plotted.
#' @param xylim X and Y axes limits.
#' @param resid Boolean determining if residuals should be plotted. Residuals
#'   are computed using the \code{stat} parameter.
#' @param stat Statistic to use if residuals are to be computed. Currently
#'   \code{mean} (default) or \code{median}.
#' @param plot Boolean determining if plot should be generated.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param tail.pch Tail-end point symbol type (See \code{tails}).
#' @param tail.p.col Tail-end color for point symbol (See \code{tails}).
#' @param tail.p.fill Tail-end point fill color passed to \code{bg}
#'   (Only used for \code{tail.pch} ranging from 21-25).
#' @param size Point symbol size (0-1).
#' @param tic.size Size of tic labels (defaults to 0.8).
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param med Boolean determining if median lines should be drawn.
#' @param q Boolean determining if grey box highlighting the \code{inner}
#'   region should be displayed.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch}, \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param inner Fraction of mid-values to highlight in \code{q} or \code{tails}.
#'   Defaults to the inner 75\% of values.
#' @param text.size Size for category text in diagonal box.
#' @param ... Not used
#'
#' @details The function will generate an empirical QQ plot matrix. Most of the
#'   arguments available in `eda_qq` are echoed in this function. The one
#'   notable difference is the default settings. By default, `eda_qqmat` will
#'   generate a plain vanilla set of plots. \cr
#'   \cr
#'   The QQ plot matrix is most effective in comparing residuals after the data
#'   are fitted by the mean or median. To plot the residuals, set
#'   \code{resid=TRUE}. By default, the \code{mean} is used. You can change the
#'   statistic to the median by setting \code{stat=median}. \cr
#'   \cr
#'   The function also allows for batch transformation of values via the
#'   \code{p} argument. The transformation is applied to the data prior to
#'   computing the residuals.
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{data}: List with input \code{x} and \code{y} values for each
#'   group. May be interpolated to smallest quantile batch if batch sizes
#'   don't match. Values will reflect power transformation defined in \code{p}.
#'   \item \code{p}: Transformation applied to original values.}
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
#' # Default output
#' singer <- lattice::singer
#' eda_qqmat(singer, height, voice.part)
#'
#' # Symbolize points outside of the "inner" region using an open point symbol
#' eda_qqmat(singer, height, voice.part, tails = TRUE)
#'
#' # Set the inner region to cover 80% and change the outer point symbol to "+"
#' eda_qqmat(singer, height, voice.part, inner = 0.8, tails = TRUE, tail.pch = 3)
#'
#' # Add the upper triangle to the matrix
#' eda_qqmat(singer, height, voice.part, upper = TRUE)
#'
#' # Plot residuals after fitting mean to each batch
#' eda_qqmat(singer, height, voice.part, resid = TRUE)
#'
#' # Log transform the data, then plot the residuals after fitting the mean model
#' eda_qqmat(iris, Petal.Length, Species, resid = TRUE, p = 0)
#'
#' # Fit the median model instead of the mean
#' eda_qqmat(iris, Petal.Length, Species, resid = TRUE, p = 0, stat = median)
#'
#' # Shade the "inner" regions (defaults to the mid 70% of values)
#' eda_qqmat(iris, Petal.Length, Species, resid = TRUE, q = TRUE, p = 0)
#'
#' # Change inner region point symbols to dark orange and change inner region
#' # range to cover 90% of mid values
#' eda_qqmat(iris, Petal.Length, Species, resid = TRUE, p = 0, inner = 0.9,
#'           tail.pch = 3, p.fill = "orange2")

eda_qqmat <- function(dat, x, fac, p = 1L, tukey = FALSE, q.type = 5,
                      upper = FALSE, xylim = NULL, resid = FALSE, stat = mean,
                      plot = TRUE, grey = 0.6, pch = 21, p.col = "grey40",
                      p.fill = "grey60", size = 1, text.size = 1,
                      tail.pch = 21, tail.p.col = "grey70", tail.p.fill = NULL,
                      tic.size = 0.7, alpha = 0.8, q = FALSE, tails = FALSE,
                      med = FALSE, inner = 0.75, ...) {

  if (!requireNamespace("grid", quietly = TRUE)) {
    stop(
      "Package \"grid\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Parameters check
  if (! as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Stat must be either the mean or the median")
  if (inner < 0.25)
    stop("The inner parameter must be greater than 0.25.")

  # Extract data ----
  # Get values and factors. Drop levels not present in the data
  x   <- eval(substitute(x), dat)
  fac <- as.factor(eval(substitute(fac), dat))
  fac <- droplevels(fac)

  # Re-express data if required
  if (p != 1) {
    x <- eda_re(x, p = p, tukey = tukey)
  }
  x.isna <- is.na(x)
  rm.nan <- ifelse( any(x.isna), 1 , 0)

  # Re-expression may produce NaN values. Output warning if TRUE
  if( rm.nan > 0 ) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
      bad <- x.isna
      x <- x[!bad]
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

  # Get upper/lower bounds of quantile box
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # Loop through each pairs of factors
  lst <- split(x, fac)

  if(resid == TRUE){
    lst <- lapply(lst, FUN = function(x){x - stat(x)})
  }
  fac_un <- unique(fac)
  fac_num <- length(fac_un)

  # Set plot parameters
  lmin <- min(dev.size("cm")) # Get smallest plot window dimension
  dim.cm <- lmin / fac_num * 1.3 # plot width and height
  num.plots <- (fac_num^2)   # Number of plots

   # Get axes limits for plots
  lim.buffer = 0.05
  if(is.null(xylim)){
      xylim <- range(unlist(lst))
      xylim <- c(xylim[1] - diff(xylim) * lim.buffer  , xylim[2] + diff(xylim) * lim.buffer)
  }

  # Create empty list (to be used for data output)
  lstout <- list()

  # Reset plot window
  grid.newpage()

  # Compute margins needed to accommodate labels
  #label <- as.character(signif(max(xylim),2))
  label <- as.character(format(signif(max(xylim),2), scientific = FALSE))
  label_width <- convertWidth(stringWidth(label), "lines", valueOnly = TRUE)
  label_height <- convertWidth(stringHeight(label), "lines", valueOnly = TRUE)
  x_margin <- unit(label_width + 2, "lines")
  y_margin <- unit(label_height + 5, "lines")  # Horizontal margin for x-axis text

  # Define main viewport
  main <- viewport(width = unit(1, "npc") - x_margin,
                   height = unit(1, "npc") - y_margin,
                   layout=grid.layout(fac_num, fac_num, respect = TRUE))

  pushViewport(main)

  jj <- 0
  for(j in levels(fac_un)){
    jj <- jj + 1
    ii <- 0
    for(i in levels(fac_un)){
      ii <- ii + 1
      x <- unlist(lst[i])
      y <- unlist(lst[j])

      # Check if only lower triangular matrix.
      if(upper == TRUE | ii <= jj){
        # Center on mean or median if requested
        # if (resid == TRUE){
        #   x <- x - stat(x)
        #   y <- y - stat(y)
        # }

        # Generate qqplot using base function
        qq <- qqplot(x,y, plot.it = FALSE, qtype = q.type)
        x <- qq$x
        y <- qq$y

        # Save values for output
        dfout <- data.frame(x,y)
        row.names(dfout) <- NULL
        lstout[[paste(i,j)]] <- dfout

        # Set point color parameters.
        if(!is.null(alpha)){
          if(p.col %in% colors() & p.fill %in% colors() ){
            p.col  <- adjustcolor( p.col,  alpha.f = alpha)
            p.fill <- adjustcolor( p.fill, alpha.f = alpha)
          }
        }

        # Get quantile parameters
        qx <- quantile(x, b.val, qtype = q.type)
        qy <- quantile(y, b.val, qtype = q.type)

        if(tails == TRUE){
          # If tails are to be plotted separately, identify points outside of the
          # inner region
          if (!is.na(table(x < qx[1])["TRUE"]) & !is.na(table(y < qy[1])["TRUE"])){
            lower.tail <- min(table(x < qx[1])["TRUE"], table(y < qy[1])["TRUE"])
          } else{
            lower.tail <- 0
          }

          if (!is.na(table(x > qx[2])["TRUE"]) & !is.na(table(y > qy[2])["TRUE"])){
            upper.tail <-  max(table(x > qx[2])["TRUE"], table(y > qy[2])["TRUE"])
          } else {
            upper.tail <- 0
          }

          inner.tails <- (lower.tail+1):(length(x) - upper.tail)
          outer.tails <- -inner.tails
        }

        # Compute median values
        medx <- median(x)
        medy <- median(y)

        # Generate plot ----
        if(plot == TRUE){
          vp <- viewport(layout.pos.col = ii, layout.pos.row = jj, xscale = xylim, yscale = xylim)
          pushViewport(vp)
          grid.rect(gp = gpar(col = plotcol))

          # Define point size based in plot window size
          vp_width <- convertX(unit(1, "npc"), "inches", valueOnly = TRUE)
          vp_height <- convertY(unit(1, "npc"), "inches", valueOnly = TRUE)
          point_size <- min(vp_width, vp_height) * 10 * size

          # Add points
          if( ii != jj){
            if(tails != TRUE){
              grid.points(x=unit(x,"native"), y=unit(y,"native"), size = unit(point_size, "points"),
                          gp = gpar(fill = p.fill, col = p.col), pch = pch)
            } else {
              grid.points(x=unit(x[inner.tails],"native"),
                          y=unit(y[inner.tails],"native"),
                          size = unit(point_size, "points"),
                          gp = gpar(fill = p.fill, col = p.col), pch = pch)
              if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
                grid.points(x=unit(x[outer.tails],"native"),
                            y=unit(y[outer.tails],"native"),
                            size = unit(point_size, "points"),
                            gp = gpar(fill = tail.p.fill, col = tail.p.col), pch = tail.pch)
              }

            }

            # Add diagonal (1:1) line
            grid.lines(gp = gpar(cex = 0.8, col = plotcol))

          } else {
            # Print labels on diagonal
            # Compute factor label size based on label text width
            vp_side <-convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
            max_width <- vp_side * 0.5  # We use 70% of the available width to fit the text
            font_size <- max_width / max(nchar(as.character(fac_un)))  # Estimate font size by dividing by text length

            if(font_size > 2) font_size <- 2
            grid.text(i, gp = gpar(col = "grey40", cex = font_size * text.size))
          }

          # Add y-axis
          if( (ii == 1) & (jj %% 2 !=0)) {
            grid.yaxis(gp = gpar(cex = tic.size, col = plotcol))
          } else if( (ii == fac_num) & (jj %% 2 ==0)) {
            grid.yaxis(gp = gpar(cex = tic.size, col = plotcol), main = FALSE)
          }

          # Add x-axis
          if(jj == 1 & (ii %% 2 == 0)) {
            grid.xaxis(main = FALSE, gp = gpar(cex = tic.size, col = plotcol))
          } else if (jj == fac_num & (ii %% 2 != 0)){
            grid.xaxis( gp = gpar(cex = tic.size, col = plotcol))
          }
          #  grid.text(paste(i,j)) # Used to debug plot placement

          # Add medians
          if(med == TRUE& ii != jj){
            grid.segments(x0 = medx, x1 = medx, y0 = xylim[1], y1 = xylim[2],
                          default.units = "native", gp = gpar(col = "grey80", lty = 2))
            grid.segments(y0 = medy, y1 = medy, x0 = xylim[1], x1 = xylim[2],
                          default.units = "native", gp = gpar(col = "grey80", lty = 2))
          }

          # Add core boxes ----
          if(q == TRUE & ii != jj){
            grid.polygon(x = c(qx[1], qx[2], qx[2], qx[1]),
                         y = c(xylim[1], xylim[1], xylim[2], xylim[2]),
                         gp = gpar(fill = rgb(0,0,0,0.05), col = NA),
                         default.units = "native")
            grid.polygon(y = c(qy[1], qy[2], qy[2], qy[1]),
                         x = c(xylim[1], xylim[1], xylim[2], xylim[2]),
                         gp = gpar(fill = rgb(0,0,0,0.05), col = NA),
                         default.units = "native")
          }
          upViewport()
        }  # Close plot loop
      }
    }   # Close ii loop
  }     # Close jj loop

  # Remind user if power parameter was set to value other than 1
  if ( p !=1 )
  message(paste0("Note that a power transformation of ",p," was applied to the data",
          " before they were processed for the plot."))
  popViewport(0)
  invisible(list(qq_values = lstout, p = p))
}


