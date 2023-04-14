#' @export
#' @title Plot eda_polish tables or diagnostic plots
#'
#' @description
#'  \code{eda_pol} A plot method for lists of eda_polish class.
#'
#' @param x A list of class eda_polish
#' @param type Plot type. One of three: "residuals", "cv" or "diagnostic".
#' @param k If cv values are to be plotted, define the k parameter
#' @param col.quant Boolean indicating if a quantile classification scheme should be used
#' @param colpal Color palette to adopt
#' @param col.eff Boolean indicating if effects and common value should contribute to color gradient
#' @param col.com Boolean indicating if common value should contribute to color gradient
#' @param adj.mar Boolean indicating if margin width needs to accommodate labels
#' @param res.size Size of residual values in plot [0-1]
#' @param row.size Size of row effect values in plot [0-1]
#' @param col.size Size of column effect values in plot [0-1]
#' @param res.txt Boolean indicating if values should be added to plot
#' @param label.txt Boolean indicating if margin and column labels should be plotted
#' @param ... Arguments to be passed to subsequent methods
#'
#' @details
#'  The function plots a polish table of residuals or CV values. I will also generate
#'  a diagnostic plot if \code{type} is set to \code{diagnostic}
#' @examples
#' # Create dataset
#' df <- data.frame(region =  rep( c("NE", "NC", "S", "W"), each = 5),
#' edu = rep( c("ed8", "ed9to11", "ed12", "ed13to15", "ed16"), 4),
#' perc = c(25.3, 25.3, 18.2, 18.3, 16.3, 32.1, 29, 18.8,
#'         24.3, 19, 38.8, 31, 19.3, 15.7, 16.8, 25.4, 21.1, 20.3, 24, 17.5))
#'
#' # Generate median polish output
#' out <- eda_pol(df, row = "region", col = "edu", val = "perc", plot = FALSE)
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

plot.eda_polish <- function(x, type = "residuals", k = 1, col.quant = FALSE,
                            colpal = "RdYlBu", col.eff = TRUE, col.com = TRUE,
                            adj.mar = FALSE, res.size = 1, row.size = 1, col.size = 1,
                            res.txt = TRUE, label.txt = TRUE, ...){
  if (!inherits(x,"eda_polish")) stop("The input object must of class eda_polish")
  if (! type %in% c("residuals", "cv", "diagnostic" ))
    stop("Paramater \"type=\" must be of \"residuals\", \"cv\" or \"diagnostic\" ")

  mat <- x$wide

  if(type == "diagnostic"){
    cv <- x$cv[,4]
    residuals <- x$cv[,1]
    fit <- MASS::rlm(residuals~cv)
    plot(cv,residuals, pch=16, col = rgb(0,0,0,0.5))
    abline(h = median(residuals), lty=2, col = "grey")
    abline(v = median(cv), lty=2, col = "grey")
    abline(fit, col = rgb(0,0,1,0.3))
    return(list(slope = fit$coefficients[2]))
  }

  if(type == "cv") {
    row <- x$row$effect
    col <- x$col$effect
    mat[-1,-1] <- mat[-1,-1] + k * matrix(apply(expand.grid(row, col),1, sum), ncol = length(col)) / x$global
  }

  if (adj.mar == FALSE){
    OP <- par(mar = c(1.5,1.5,1.5,1.5))
  } else {
    OP <- par(mar = c(1.5, max(nchar(rownames(x$wide) ))/2.5 * row.size ,1.5,1.5) )
  }

  # Extract total number of values
  len <- prod(dim(mat))

  # Get min/max range to be mapped to color gradient
  if(col.com == FALSE){
    max <- max(abs(range(mat[-1]))) # exclude common value
    quant.range <- mat[-1]
  } else if(col.eff == TRUE){
    max <- max(abs(range(mat))) # Get max upper or lower bound value
    quant.range <- mat
  } else {
    max <- max(abs(range(mat[-1,-1]))) # exclude effects
    quant.range <- mat[-1,-1]
  }

  if(col.quant == TRUE){
    colbrk <- quantile(unlist(quant.range), prob = 0:len/len)
  } else {
    colbrk <- c( -max, seq(-max, max, length.out = len))
  }

  colMap <- hcl.colors(len, palette = colpal, rev = TRUE, alpha = 0.5)
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
  par(OP)
}
