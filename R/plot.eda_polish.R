#' @export
#' @title Plot eda_polish tables or diagnostic plots
#'
#' @description \code{plot.eda_pol} A plot method for lists of eda_polish class.
#'
#' @param x A list of class eda_polish.
#' @param type Plot type. One of three: "residuals", "cv" or "diagnostic".
#' @param add.cv Whether to add kCV to the model when plotting "residuals".
#' @param k Custom k to use if kCV is to be added to model. A value of NULL
#'   makes us of slope.
#' @param col.quant Boolean indicating if a quantile classification scheme
#'   should be used.
#' @param colpal Color palette to adopt (should be one listed in hcl.pals()).
#' @param colrev Should color palette be reversed? (default TRUE).
#' @param col.eff Boolean indicating if effects and common value should
#'   contribute to color gradient.
#' @param col.com Boolean indicating if common value should contribute to color
#'   gradient.
#' @param adj.mar Boolean indicating if margin width needs to accommodate labels.
#' @param res.size Size of residual values in plot [0-1].
#' @param row.size Size of row effect values in plot [0-1].
#' @param col.size Size of column effect values in plot [0-1].
#' @param res.txt Boolean indicating if values should be added to plot.
#' @param label.txt Boolean indicating if margin and column labels should be
#'   plotted.
#' @param grey Grey level to apply to plot elements in the diagnostic plot
#'             (0 to 1 with 1 = black).
#' @param pch Point symbol type for the diagnostic plot.
#' @param p.col Color for point symbol in the diagnostic plot.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1) in the diagnostic plot.
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param ... Arguments to be passed to subsequent methods.
#'
#' @return Returns a single element vector if \code{"type"} is \code{"diagnostic"} and no value
#'   otherwise.
#'
#' @details The function plots a polish table of residuals or CV values. I will
#' also generate a diagnostic plot if \code{type} is set to \code{diagnostic}
#'
#' @examples
#' # Create dataset
#' df <- data.frame(region =  rep( c("NE", "NC", "S", "W"), each = 5),
#' edu = rep( c("ed8", "ed9to11", "ed12", "ed13to15", "ed16"), 4),
#' perc = c(25.3, 25.3, 18.2, 18.3, 16.3, 32.1, 29, 18.8,
#'         24.3, 19, 38.8, 31, 19.3, 15.7, 16.8, 25.4, 21.1, 20.3, 24, 17.5))
#'
#' # Generate median polish output
#' out <- eda_pol(df, row = region, col = edu, val = perc, plot = FALSE)
#'
#' # Plot table
#' plot(out, type = "residuals")
#'
#'
#' # Plot table using CV values
#' plot(out, type = "cv")
#'
#' # Generate diagnostic plot
#' plot(out, type = "diagnostic")

plot.eda_polish <- function(x, type = "residuals", add.cv = FALSE, k = NULL,
                            col.quant = FALSE, colpal = "RdYlBu", colrev = TRUE,
                            col.eff = TRUE, col.com = TRUE, adj.mar = TRUE,
                            res.size = 1, row.size = 1, col.size = 1,
                            res.txt = TRUE, label.txt = TRUE, grey = 0.6,
                            pch=21, p.col="grey30",  p.fill = "grey60",
                            size = 0.9, alpha = 0.8,  ...){

  if (!inherits(x,"eda_polish")) stop("The input object must of class eda_polish")
  if (! type %in% c("residuals", "cv", "diagnostic" ))
    stop("Paramater \"type=\" must be of \"residuals\", \"cv\" or \"diagnostic\" ")
  if (add.cv == TRUE & is.null(k)) stop("You are adding kCV to model, but you don't specify k.")
  if (!colpal %in% hcl.pals()) stop("Color palette (colpal) should be one listed in hcl.pals().")

  # Extract wide table
  mat <- x$wide

  # Compute CV matrix
  row <- mat[-1,1]
  col <- unlist(mat[1,-1])
  cv.mat <- matrix(apply(expand.grid(row, col),1, prod), ncol = length(col)) / x$global

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  if(type == "diagnostic"){
    if(sum(is.finite(x$cv[,4])) > 1){
      cv <- x$cv[,4]
      residuals <- x$cv[,1]

      # Get lines-to-inches ratio
      in2line <- ( par("mar") / par("mai") )[2]

      # Create a dummy plot to extract y-axis labels
      pdf(NULL)
      plot(x = cv, y = residuals, type = "n", xlab = "", ylab = "", xaxt = "n",
            yaxt='n', main = NULL)
      # y.labs <- range(axTicks(2))
      y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
      dev.off()

      # Compute the margin width (returned in inches before converting to lines)
      # y.wid <- max( strwidth( y.labs[1], units="inches"),
      #               strwidth( y.labs[2], units="inches")) * in2line + 1

      # Set plot parameters
      .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
      on.exit(par(.pardef))

    #  plot(cv,residuals, pch=16, col = rgb(0,0,0,0.5))
      plot(cv,residuals, col.lab=plotcol, pch = pch, col = p.col,
           bg = p.fill, cex = size, ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA)
      axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
      axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
           tck = -0.02)
      mtext("Residuals", side=3, adj= -0.06 ,col=plotcol,  padj = -1.2)
      title(xlab = "CV", line =1.8, col.lab=plotcol)

      abline(h = median(residuals), lty=2, col = plotcol)
      abline(v = median(cv), lty=2, col = plotcol)
      fit.r <- MASS::rlm(residuals~cv, psi = MASS::psi.bisquare)
      abline(fit.r, col = rgb(1,0,0,0.35))
      tryCatch(
        {
        fit.l <- loess(residuals~cv, family = "symmetric")
        lines(sort(cv), predict( fit.l , sort(cv)), col = rgb(0,0,1,0.5), lty = 2)
        },
        error=function(cond) {
          message("Loess fit could not be generated.")
          }
      )
      # Reset plot parameters and  output values
      par(.pardef)

      return(list(slope = fit.r$coefficients[2]))

    } else {
      return("CV values are not finite")
    }

  }

  if(type == "cv") {
    # row <- mat[-1,1]
    # col <- unlist(mat[1,-1])
    # mat[-1,-1] <- matrix(apply(expand.grid(row, col),1, prod), ncol = length(col)) / x$global
    mat[-1,-1] <- cv.mat
  }

  # If CV is to be added to the model before computing the residuals
  if(type == "residuals" & add.cv == TRUE){
    mat[-1,-1] <- mat[-1,-1] - k * cv.mat
  }

  # Expand margin to accommodate row names if requested
  if (adj.mar == FALSE){
    .pardef <- par(mar = c(1.5,1.5,1.5,1.5))
  } else {
    .pardef <- par(mar = c(1.5, max(nchar(rownames(x$wide) ))/1.8 * row.size ,1.5,1.5) )
  }
  on.exit(par(.pardef), add = TRUE)

  # Extract total number of values
  len <- prod(dim(mat))

  # Get min/max range to be mapped to color gradient
  if (col.eff == FALSE & col.com == FALSE){
    max <- max(abs(range(mat[-1,-1]))) # exclude effects
    quant.range <- mat[-1,-1]
  } else if(col.com == FALSE){
    max <- max(abs(range(unlist(mat)[-1]))) # exclude common value
    quant.range <- unlist(mat)[-1]
  } else if(col.eff == FALSE){
    max <- max(abs(range(mat[-1,-1]))) # exclude effects
    quant.range <- mat[-1,-1]
  } else {
    max <- max(abs(range(mat))) # Get max upper or lower bound value
    quant.range <- mat
  }

  if(col.quant == TRUE){
    colbrk <- quantile(unlist(quant.range), prob = 0:len/len)
  } else {
    colbrk <- c( -max, seq(-max, max, length.out = len))
  }

  colMap <- hcl.colors(len, palette = colpal, rev = colrev, alpha = 0.5)
  image(t(mat[nrow(mat):1, ]), axes = FALSE, col = colMap, breaks = colbrk)
  grid(nx = ncol(mat), ny = nrow(mat), col = 'grey', lty = 1)

  U <- par("usr")
  abline( v = diff(U[1:2]) / dim(mat)[2]  + U[1] , lw = 2)
  abline( h =  U[4] - diff(U[3:4]) / dim(mat)[1]    , lw = 2)
  col.ctr <- seq(0,1,length.out = ncol(mat))
  row.ctr <- seq(1,0,length.out = nrow(mat))
  if(label.txt == TRUE){
    mtext(colnames(mat), at = col.ctr, side = 3, cex = col.size)
    mtext(rownames(mat), at = row.ctr, side = 2, las = 2, cex = row.size)
  }
  text.crd <- expand.grid(y = row.ctr, x = col.ctr)
  if(res.txt == TRUE) {
    text(x=text.crd[,"x"], y=text.crd[,"y"], labels = round(unlist(mat),2), cex=res.size)
  }
  par(.pardef)
}

