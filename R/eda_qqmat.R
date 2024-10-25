#' @export
#' @import grDevices
#' @import grid
#' @importFrom utils modifyList
#' @title Matrix QQ plot
#'
#' @description \code{eda_qqmat} Generates a matrix of empirical QQ plots
#'
#' @param dat  Data frame
#' @param x    Column name assigned to the values
#' @param fac  Column name assigned to the factor the values are to be
#' conditioned on
#' @param p  Power transformation to apply to both sets of values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
#' @param diag  Boolean determining if upper diagonal should be plotted
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
#' @param size Point symbol size (0-1)
#' @param tic.size Size of tic symbols (defaults to 0.8)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param show.med Boolean determining if median lines should be drawn.
#' @param q Boolean determining if grey quantile boxes should be plotted.
#' @param inner Fraction of mid-values to display in the quantile box. Defaults
#'   to the IQR.
#' @param ... Not used
#'
#' @details When the function is used to generate an empirical QQ plots, the plot
#'   will displays the IQR via grey boxes for both x and y values. The box
#'   widths can be changed via the  \code{inner} argument.  The middle dashed
#'   line represents each batch's median value. \cr
#'   \cr
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{data}: List with input \code{x} and \code{y} values for each
#'   group.
#'   May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{p}.
#'   \item \code{p}: Re-expression applied to original values.}
#'
#' @references
#'
#' \itemize{
#'   \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)}
#'
#' @examples
#'
#' singer <- lattice::singer
#' eda_qqmat(singer, height, voice.part)
#' eda_qqmat(singer, height, voice.part, diag = FALSE)
#' eda_qqmat(singer, height, voice.part, diag = FALSE, resid = TRUE)
#' eda_qqmat(mtcars, mpg, cyl,resid = TRUE, q = FALSE)


eda_qqmat <- function(dat, x, fac, p = 1L, tukey = FALSE, q.type = 5, diag = TRUE,
                      xylim = NULL, resid = FALSE, stat = mean,
                      plot = TRUE, grey = 0.6, pch = 21,
                      p.col = "grey50", p.fill = "grey80", size = 0.5,
                      tic.size = 0.7, alpha = 0.8, q = TRUE,
                      show.med = TRUE, inner = 0.75, ...) {

  library(grid)

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Parameters check
  if (! as.character(substitute(stat)) %in% c("mean", "median"))
    stop("Stat must be either the mean or the median")

  # Extract data ----
  # Get values and factors
  x   <- eval(substitute(x), dat)
  fac <- as.factor(eval(substitute(fac), dat))

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

  # Get upper/lower bounds of quantil box
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # Loop through each pairs of factors
  lst <- split(x, fac)
  if(resid == TRUE){
    lst <- lapply(lst, FUN = function(x){x - mean(x)})
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
  label <- as.character(max(xylim))
  label_width <- convertWidth(stringWidth(label), "lines", valueOnly = TRUE)
  x_margin <- unit(label_width + 0.5, "lines")
  y_margin <- unit(4, "lines")  # Horizontal margin for x-axis text

  # Define main viewport
  main <- viewport(width = unit(1, "npc") - x_margin,
                   height = unit(1, "npc") - y_margin,
                   layout=grid.layout(fac_num, fac_num, respect = TRUE))

  pushViewport(main)

  for(jj in 1:fac_num){
    for(ii in 1:fac_num){
      i <- fac_un[ii]
      j <- fac_un[jj]
      x <- unlist(lst[i])
      y <- unlist(lst[j])

      # Check if only lower diagonal should be plotted
      if(diag == TRUE | ii <= jj){
        # Center on mean or median if requested
        if (resid == TRUE){
          x <- x - stat(x)
          y <- y - stat(y)
        }

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

        # Compute median values
        medx <- median(x)
        medy <- median(y)

        # Generate plot ----
        if(plot == TRUE){
          vp <- viewport(layout.pos.col = ii, layout.pos.row = jj, xscale = xylim, yscale = xylim)
          pushViewport(vp)
          grid.rect(gp = gpar(col = plotcol))

          if( ii != jj){
            grid.points(x=unit(x,"native"), y=unit(y,"native"),
                        gp = gpar(fill = p.fill, col = p.col,cex = size), pch = pch)
            grid.lines(gp = gpar(cex = 0.8, col = plotcol))
          } else {
            # Print labels on diagonal
            # Compute factor label size based on label text width
            vp_side <-convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
            max_width <- vp_side * 0.5  # We use 70% of the available width to fit the text
            font_size <- max_width / max(nchar(as.character(fac_un)))  # Estimate font size by dividing by text length

            if(font_size > 2) font_size <- 2
            grid.text(i, gp = gpar(col = "grey", cex = font_size))
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
          if(show.med == TRUE& ii != jj){
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


