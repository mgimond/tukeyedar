#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Generates empirical QQ plot and Tukey mean-difference plot
#'
#' @description \code{eda_qq} generates an empirical QQ plot or a Tukey
#'   mean-difference plot
#'
#' @param x  Vector for first variable or a dataframe.
#' @param y  Vector for second variable or column defining the continuous
#'   variable if \code{x} is a dataframe.
#' @param fac Column defining the grouping variable if \code{x} is a dataframe.
#' @param p  Power transformation to apply to both sets of values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
#' @param md Boolean determining if Tukey mean-difference plot should be
#'   generated.
#' @param fx Formula to apply to x variable. This is computed after any
#'   transformation is applied to the x variable.
#' @param fy Formula to apply to y variable. This is computed after any
#'   transformation is applied to the y variable.
#' @param plot Boolean determining if plot should be generated.
#' @param show.par Boolean determining if parameters such as power
#'   transformation or formula should be displayed.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if grey quantile boxes should be plotted.
#' @param b.val Quantiles to define the quantile box parameters. Defaults to the
#'   IQR. Two values are needed.
#' @param l.val Quantiles to define the quantile line parameters. Defaults to
#'   the mid 75\% of values. Two values are needed.
#' @param xlab X label for output plot. Ignored if \code{x} is a dataframe.
#' @param ylab Y label for output plot. Ignored if \code{x} is a dataframe.
#' @param ... Not used
#'
#' @details The QQ plot will displays the IQR via grey boxes for both x and y
#'   values. The box widths can be changed via the  \code{b.val} argument. The
#'   plot will also display the mid 75\% of values via light colored dashed
#'   lines. The line positions can be changed via the \code{l.val} argument. The
#'   middle dashed line represents each batch's median value. Console output
#'   prints the suggested multiplicative and additive offsets.
#'
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{x}: X values. May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{p}.
#'   \item \code{b}: Yvalues. May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{p}.
#'   \item \code{p}: Re-expression applied to original values.
#'   \item \code{fx}: Formula applied to x variable.
#'   \item \code{fy}: Formula applied to y variable.}
#'
#'
#' @examples
#'
#' # Passing data as a dataframe
#'  singer <- lattice::singer
#'  dat <- singer[singer$voice.part  %in% c("Bass 2", "Tenor 1"), ]
#'  eda_qq(dat, height, voice.part)
#'
#' # Passing data as two separate vector objects
#'  bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )
#'  tenor1 <- subset(singer, voice.part == "Tenor 1", select = height, drop = TRUE )
#'
#'  eda_qq(bass2, tenor1)
#'
#'  # There seems to be an additive offset of about 2 inches
#'  eda_qq(bass2, tenor1, fx = "x - 2")
#'
#'  # We can fine-tune by generating the Tukey mean-difference plot
#'  eda_qq(bass2, tenor1, fx = "x - 2", md = TRUE)
#'
#'  # An offset of another 0.5 inches seems warranted
#'  # We can sat that overall, bass2 singers are 2.5 inches taller than  tenor1.
#'  # The offset is additive.
#'  eda_qq(bass2, tenor1, fx = "x - 2.5", md = TRUE)
#'
#'  # Example 2: Sepal width
#'  setosa <- subset(iris, Species == "setosa", select = Petal.Width, drop = TRUE)
#'  virginica <- subset(iris, Species == "virginica", select = Petal.Width, drop = TRUE)
#'
#'  eda_qq(setosa, virginica)
#'
#'  # The points are not completely parallel to the  1:1 line suggesting a
#'  # multiplicative offset. The slope may be difficult to eyeball. The function
#'  # outputs a suggested slope and intercept. We can start with that
#'  eda_qq(setosa, virginica, fx = "x *  1.7143")
#'
#'  # Now let's add the suggested additive offset.
#'  eda_qq(setosa, virginica, fx = "x *  1.7143  + 1.6286")
#'
#'  # We can confirm this value via the mean-difference plot
#'  # Overall, we have both a multiplicative and additive offset between the
#'  # species' petal widths.
#'  eda_qq(setosa, virginica, fx = "x *  1.7143 + 1.6286", md = TRUE)
#'


eda_qq <- function(x, y, fac = NULL, p = 1L, tukey = FALSE, md = FALSE,
                   q.type = 5, fx = NULL, fy = NULL, plot = TRUE, show.par = TRUE,
                   grey = 0.6, pch = 21, p.col = "grey50", p.fill = "grey80",
                   size = 0.8, alpha = 0.8, q = TRUE, b.val = c(0.25,0.75),
                   l.val = c(0.125, 0.875), xlab = NULL, ylab = NULL, ...) {

  # Parameters check
  if (length(b.val)!= 2) stop("The b.val argument must have two values.")
  if (length(l.val)!= 2) stop("The b.val argument must have two values.")

  # Extract data
  if("data.frame" %in% class(x)){
    val <- eval(substitute(y), x)
    fac <- eval(substitute(fac), x)
    g <- unique(fac)
    if( length(g) != 2){
      stop(paste("Column", fac, "has", length(g),
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
  }


  # Define labels
  if(is.null(xlab)){
    xlab = substitute(x)
  }
  if(is.null(ylab)){
    ylab = substitute(y)
  }

  # Re-express data if required
    x <- eda_re(x, p = p, tukey = tukey)
    y <- eda_re(y, p = p, tukey = tukey)

  # Get suggested multiplicative and additive offsets (off of original data)
  zl <- qqplot(x, y, plot.it = FALSE, qtype = q.type)
 # zd <- data.frame(y = zl$y - zl$x, x = (zl$y + zl$x) / 2)
  zd <- data.frame(y = zl$y - zl$x, x = zl$x)
  zd$x <- zd$x - median(zd$x); zd$y <- zd$y - median(zd$y)
  z <- eda_rline(zd,x,y)
  x.multi <-  1 + z$b
  x.add <- median(zl$y - sort(zl$x * x.multi))

  # Apply formula if present
  if(!is.null(fx) & !is.null(fy))
      warning(paste("You should apply a formula to just one axis.\n",
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

  # Set point color parameters.
  if(!is.null(alpha)){
    if(p.col %in% colors() & p.fill %in% colors() ){
      p.col  <- adjustcolor( p.col,  alpha.f = alpha)
      p.fill <- adjustcolor( p.fill, alpha.f = alpha)
    }
  }

  # Generate qqplot using base function
  qq <- qqplot(x,y, plot.it = FALSE, qtype = q.type)
  x <- qq$x
  y <- qq$y

  # Plot data
  .pardef <- par(pty = "s", col = plotcol, mar = c(3,3,3,1))
  on.exit(par(.pardef))

  if(plot == TRUE & md == FALSE){


    # Get quantile parameters
    qx <- quantile(x, b.val, qtype = q.type)
    qy <- quantile(y, b.val, qtype = q.type)
    lx <- quantile(x, l.val, qtype = q.type)
    ly <- quantile(y, l.val, qtype = q.type)
    medx <- median(x)
    medy <- median(y)

    # Generate plot
    xylim <- range(x,y)
    plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
          xlim = xylim, ylim = xylim)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab, side=3, adj= -0.1 , col=plotcol, padj = -1)
    title(xlab = xlab, line =1.8, col.lab=plotcol)
    abline(0, 1, col = plotcol)
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
           col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
      abline(v = lx, lty = 3, col = "grey90")
      abline(h = ly, lty = 3, col = "grey90")
    }
    mtext(side = 3, text=paste0("p=",p,";",fx," ",fy,sep=""), adj=1, cex = 0.65)
  } else if(plot == TRUE & md == TRUE) {

    # Generate labels
    xlab2 <- paste("Mean of", xlab, "and", ylab)
    ylab2 <- paste(ylab,"-", xlab)

    # Compute m-d variables
    md.y  <- (y - x)
    md.x  <- (y + x) * 0.5

    # Get quantile parameters
    qy <- quantile(md.y, b.val, qtype = q.type)
    ly <- quantile(md.y, l.val, qtype = q.type)
    lx <- quantile(md.x, l.val, qtype = q.type)
    medx <- median(md.x)
    medy <- median(md.y)

    # Generate plot
    ylim <- range(md.y, 0)

    plot( x=md.x, y=md.y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
          ylim = ylim)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab2, side=3, adj= -0.1 , col=plotcol, padj = -1)
    title(xlab = xlab2, line =1.8, col.lab=plotcol)
    abline(h = 0,  col = plotcol)
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
    abline(h = ly, col = "grey90", lty = 3)
    abline(v = lx, col = "grey90", lty = 3)
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }
    if(show.par == TRUE){
      mtext(side = 3, text=paste("p=", p, fx ,fy,sep="  "), adj=1, cex = 0.65)
    }
  }

  # Reset plot parameters and  output values
  par(.pardef)
  print(paste0("Suggested offsets:", "y = ", "x * ", round(x.multi,4),
              " + (", round(x.add,4),")"))
  invisible(list(x = x, y = y, p = p, fx = fx, fy = fy))

}
