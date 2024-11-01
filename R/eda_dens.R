#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Overlapping density distributions for two variables
#'
#' @description \code{eda_dens} generates overlapping density distributions for
#'   two variables.
#'
#' @param x  Vector for first variable, or a dataframe.
#' @param y  Vector for second variable, or column defining the continuous
#'   variable if \code{x} is a dataframe.
#' @param fac Column defining the categorical variable if \code{x} is a dataframe.
#' @param p  Power transformation to apply to both sets of values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param fx Formula to apply to x variable. This is computed after any
#'   transformation is applied to the x variable.
#' @param fy Formula to apply to y variable. This is computed after any
#'   transformation is applied to the y variable.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param col Fill color for second density distribution.
#' @param size Point size (0-1).
#' @param alpha Fill transparency (0 = transparent, 1 = opaque). Only applicable
#'   if \code{rgb()} is not used to define fill colors.
#' @param legend Boolean determining if a legend should be added to the plot.
#' @param xlab X variable label. Ignored if \code{x} is a dataframe. This is
#' not the x-axis label!
#' @param ylab Y variable label. Ignored if \code{x} is a dataframe. This is
#' not the y-axis label!
#' @param xaxis Label for x-axis. Defaults to \code{"Value"}.
#' @param show.par Boolean determining if parameters such as power
#'   transformation or formula should be displayed.
#' @param switch Boolean determining if the axes should be swapped. Only applies
#'   to dataframe input. Ignored if vectors are passed to the function.
#' @param ... Arguments passed to the \code{stats::density()} function.
#'
#' @details This function will generate overlapping density plots with the first
#'   variable assigned a grey color and the second variable assigned the default
#'   red color.
#'
#' @returns Does not return a value.
#'
#' @examples
#'
#'  # Passing data as two separate vector objects
#'  set.seed(207)
#'  x <- rbeta(1000,2,8)
#'  y <- x * 1.5 + 0.1
#'  eda_dens(x, y)
#'
#'  # Passing data as a dataframe
#'  dat <- data.frame(val = c(x, y),
#'                    grp = c(rep("x", length(x)), rep("y", length(y))))
#'  eda_dens(dat, val, grp)



eda_dens <- function(x, y, fac = NULL, p = 1L, tukey = FALSE, fx = NULL,
                     fy = NULL, grey = 0.6, col = "red", size = 0.8,
                     show.par= TRUE, alpha = 0.4, xlab = NULL, ylab = NULL,
                     xaxis = NULL, switch = FALSE, legend = TRUE, ...) {

  # Extract data
  if("data.frame" %in% class(x)){
    val <- eval(substitute(y), x)
    if(is.null(xaxis)){
      xaxis <- as.character(substitute(fac))
    }
    #fac <- eval(substitute(fac), x)
    fac <- as.factor(eval(substitute(fac), x))
    fac <- droplevels(fac)

    # Get level order (determines axes order)
    if(is.null(levels(fac))) {
      g <- unique(fac)
    } else {
      g <- levels(fac)
    }

    # Switch axes if requested
    if (switch == TRUE){
      g <- rev(g)
    }

    if( length(g) != 2){
      stop(paste("Grouping column fac has", length(g),
                 "unique values. It needs to have two exactly."))
    }
    x <- val[fac == g[1]]
    y <- val[fac == g[2]]
    xlab <- g[1]
    ylab <- g[2]
  } else {
    if(is.null(xlab)){
      xlab = substitute(x)
    }
    if(is.null(ylab)){
      ylab = substitute(y)
    }
    if(is.null(xaxis)){
      xaxis = "Value"
    }

  }

  # Re-express data if required
    x <- eda_re(x, p = p, tukey = tukey)
    y <- eda_re(y, p = p, tukey = tukey)
    x.nan <- is.na(x)
    y.nan <- is.na(y)
    if(any(x.nan, y.nan)){
      x <- x[!x.nan]
      y <- y[!y.nan]
      warning(paste("\nRe-expression produced NaN values. These observations will",
                    "be removed from output."))
    }

  # Apply formula if present
  if(!is.null(fx) & !is.null(fy))
      warning(paste("You should apply a formula to just one variable.\n",
                    "You are applying the fomrula", fx,"to the x-axis",
                    "and the formula",fy ,"to the y-axis."))
  if(!is.null(fx)){
    fx <- tolower(fx)
    if(!grepl("x", fx)) stop("Formula fx does not contain \"x\" variable.")
    x <- eval(parse(text=fx))
  }
  if(!is.null(fy)){
    fy <- tolower(fy)
    if(!grepl("y", fy)) stop("Formula fx does not contain \"y\" variable.")
    y <- eval(parse(text=fy))
  }

  # Set plot elements color
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Set point color parameters
  colx  <- adjustcolor( "grey",  alpha.f = alpha)
  if(!is.null(alpha)){
    if(col %in% colors() ){
      coly  <- adjustcolor( col, alpha.f = alpha)
    }
  }

  # Calculate density  distributions
  dx <- density(x, ...)
  dy <- density(y, ...)

  # Get ranges
  xlim <- range(dx$x, dy$x)
  ylim <- range(dx$y, dy$y)

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = dx, y = NULL, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL,  xlim = xlim, ylim = ylim)
  # y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Compute the margin width (returned in inches before converting to lines)
  # y.wid <- max( strwidth( y.labs[1], units="inches"),
  #               strwidth( y.labs[2], units="inches")) * in2line + 1

  # Plot data
  .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
  on.exit(par(.pardef))

  # Generate plot
  plot( dx,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA, main = "",
        col.lab=plotcol, col = "grey", xlim = xlim, ylim = ylim)
  polygon(dx, col = colx)
  polygon(dy, col = coly)
  box(col=plotcol)
  axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5, tck = -0.02)
  axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.8,
       tck = -0.02)
  mtext("Density", side=3, adj= -0.1 , col=plotcol, padj = -1)

  # Add x-axis label
  title(xlab = xaxis, line = 1.8, col.lab=plotcol)

  if(show.par == TRUE){
     mtext(side = 3, text=paste0("p=",p,";",fx," ",fy,sep=""), adj=1, cex = 0.65)
  }

   if (legend == TRUE){
    sq <- par("usr") # get plot corners
    lx <- diff(sq[1:2]) * 0.5 + sq[1]
    ly <- diff(sq[3:4]) * 0.02 + sq[4]
    legend(lx, ly, legend = c(xlab, ylab), fill = c(colx, coly),
           box.col = plotcol, xpd = TRUE, xjust = 0.5, yjust = 0,
           horiz=TRUE, cex=0.8, x.intersp = 0.3, y.intersp = 0.3,
           bty = "n")
  }

    # Reset plot parameters and  output values
  par(.pardef)

}
