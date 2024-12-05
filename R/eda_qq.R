#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Quantile-Quantile and Tukey mean-difference plots
#'
#' @description \code{eda_qq} Generates an empirical QQ plot and a Tukey
#'   mean-difference plot
#'
#' @param x  Vector for first variable, or a dataframe.
#' @param y  Vector for second variable, or column defining the continuous
#'   variable if \code{x} is a dataframe.
#' @param fac Column defining the categorical variable if \code{x} is a
#'   dataframe. The categorical column must be limited to two levels (groups).
#' dataframe. Ignored if \code{x} and \code{y} are vectors.
#' @param norm Defunct. Use \code{eda_theo} instead.
#' @param sym Defunct. Use \code{eda_sym} instead.
#' @param p  Power transformation to apply to continuous variable(s).
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile} function).
#' @param md Boolean determining if a Tukey mean-difference plot should be
#'   generated.
#' @param fx Formula to apply to x variable before pairing up with y. This is
#'   computed after any transformation is applied to the x variable.
#' @param fy Formula to apply to y variable before pairing up with x. This is
#'   computed after any transformation is applied to the y variable.
#' @param plot Boolean determining if plot should be generated.
#' @param show.par Boolean determining if parameters such as power
#'   transformation and formula should be displayed.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param tail.pch Tail-end point symbol type (See \code{tails}).
#' @param tail.p.col Tail-end color for point symbol (See \code{tails}).
#' @param tail.p.fill Tail-end point fill color passed to \code{bg}
#'   (Only used for \code{tail.pch} ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point color.
#' @param med Boolean determining if median lines should be drawn.
#' @param q Boolean determining if \code{inner} data region should be shaded.
#' @param inner Fraction of the data considered as "mid values". Defaults to
#'  75\%. Used  to define shaded region boundaries, \code{q}, or to identify
#'  which of the tail-end points are to be symbolized differently,
#'  \code{tails=TRUE}.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch},  \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param switch Boolean determining if the axes should be swapped in an
#'  empirical QQ plot. Only applies to dataframe input. Ignored if vectors are
#'  passed to the function.
#' @param xlab X label for output plot. Ignored if \code{x} is a dataframe.
#' @param ylab Y label for output plot. Ignored if \code{x} is a dataframe.
#' @param title Title to add to plot.
#' @param t.size Title size.
#' @param ... Not used
#'
#' @details By default, the QQ plot will highlight the inner 75\% of the data
#'   for both x and y axes to mitigate the visual influence of extreme values.
#'   The \code{inner} argument controls the extent of this region. For example
#'   \code{inner = 0.5} will highlight the IQR region. \cr
#'   \cr
#'   If the shaded regions are too distracting, you can opt to have the
#'   tail-end points symbolized differently by setting \code{tails = TRUE} and
#'   \code{q = FALSE}. The tail-end point symbols can be customized via the
#'   \code{tail.pch}, \code{tail.p.col} and \code{tail.p.fill} arguments.
#'   \cr
#'   The middle dashed line represents each batch's median value. It can be
#'   turned off by setting \code{med = FALSE}\cr
#'   \cr
#'   Console output prints the suggested multiplicative and additive offsets. It
#'   adopts a resistant line fitting technique to derive the coefficients. The
#'   suggested offsets output applies to the raw or re-expressed data but it
#'   ignores any \code{fx} or \code{fy} transformations applied to the data.
#'   Note that the suggested offsets may not always be the most parsimonious fit.
#'   Eyeballing the offsets may sometimes result in a more satisfactory
#'   characterization of the differences between batches. See the QQ plot
#'   article for an introduction on its use and interpretation.
#'   \cr \cr
#'   To generate a Tukey mean-difference plot, set \code{med = TRUE}.
#'   \cr \cr
#'   For more information on this function and on interpreting a QQ plot see
#'   the \href{../articles/qq.html}{QQ plot article}.
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{data}: Dataframe with input \code{x} and \code{y} values.
#'   Data will be interpolated to smallest quantile batch if batch sizes differ.
#'   Values will reflect power transformation defined in \code{p}.
#'   \item \code{p}: Re-expression applied to original values.
#'   \item \code{fx}: Formula applied to x variable.
#'   \item \code{fy}: Formula applied to y variable.}
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
#' # Passing data as a dataframe
#'  singer <- lattice::singer
#'  dat <- singer[singer$voice.part  %in% c("Bass 2", "Tenor 1"), ]
#'  eda_qq(dat, height, voice.part)
#'
#' # If the shaded region is too distracting, you can apply a different symbol
#' # to the tail-end points and different color to the points falling in the
#' # inner region.
#' eda_qq(dat, height, voice.part, q = FALSE, tails = TRUE, tail.pch = 3,
#'        p.fill = "coral", size = 1.2, med = FALSE)
#'
#' # For a more traditional look to the QQ plot
#' eda_qq(dat, height, voice.part, med = FALSE, q = FALSE)
#'
#' # Passing data as two separate vector objects
#'  bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )
#'  tenor1 <- subset(singer, voice.part == "Tenor 1", select = height, drop = TRUE )
#'
#'  eda_qq(bass2, tenor1)
#'
#'  # The function suggests an offset of the form y = x * 1.04 - 5.2
#'  eda_qq(bass2, tenor1, fx = "x * 1.04 - 5.2")
#'
#'  # The suggested offset helps align the points along the x=y line, but we
#'  # we might come up with a better characterization of this offset.
#'  # There seems to be an additive offset of about 2 inches. By subtracting 2
#'  # from the x variable, we should have points line up with the x=y line
#'  eda_qq(bass2, tenor1, fx = "x - 2")
#'
#'  # We can fine-tune by generating the Tukey mean-difference plot
#'  eda_qq(bass2, tenor1, fx = "x - 2", md = TRUE)
#'
#'  # An offset of another 0.5 inches seems warranted
#'  # We can say that overall, bass2 singers are 2.5 inches taller than  tenor1.
#'  # The offset is additive.
#'  eda_qq(bass2, tenor1, fx = "x - 2.5", md = TRUE)
#'
#'  # Note that the "suggested offset" in the console could have also been
#'  # applied to the data (though this formula is a bit more difficult to
#'  # interpret than our simple additive model)
#'  eda_qq(bass2, tenor1, fx = "x * 1.04 -5.2", md = TRUE)
#'
#'  # Example 2: Sepal width
#'  setosa <- subset(iris, Species == "setosa", select = Petal.Width, drop = TRUE)
#'  virginica <- subset(iris, Species == "virginica", select = Petal.Width, drop = TRUE)
#'
#'  eda_qq(setosa, virginica)
#'
#'  # The points are not completely parallel to the  x=y line suggesting a
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



eda_qq <- function(x, y = NULL, fac = NULL, norm = FALSE, sym = FALSE, p = 1L,
                   tukey = FALSE, md = FALSE,
                   q.type = 5, fx = NULL, fy = NULL, plot = TRUE,
                   show.par = TRUE, grey = 0.6, pch = 21, p.col = "grey50",
                   p.fill = "grey80", size = 1, alpha = 0.8,
                   med = TRUE, q = TRUE, tails = FALSE, inner = 0.75,
                   tail.pch = 21, tail.p.col = "grey70", tail.p.fill = NULL,
                   switch = FALSE, xlab = NULL, ylab = NULL, title = NULL,
                   t.size = 1.2, ...) {

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Parameters check
  if(norm == TRUE)
    stop("This function no longer generates a Normal QQ plot. Use eda_theo() instead.")
  if(sym == TRUE)
    stop("This function no longer generates a symmetry QQ plot. Use eda_sym() instead.")
  if(!"data.frame" %in% class(x) & switch == TRUE)
     warning(paste("The argument switch was set to TRUE yet the input dataset ",
                   "is not a dataframe. Switch only applies to data stored ",
                   "in a dataframe."))

  # Extract data ----
  if("data.frame" %in% class(x)){
    val <- eval(substitute(y), x)
    #fac <- eval(substitute(fac), x)
    fac <- as.factor(eval(substitute(fac), x))
    fac <- droplevels(fac)

    if (is.null(fac))
      stop("You need to pass a valid factor column to the fac parameter")

    # Get level order (determines axes order)
    if(is.null(levels(fac))) {
      g <- unique(fac)
    } else {
      g <- levels(fac)
    }

    # Check if there are more than 2 unique groups
    if(length(g) > 2)
       stop(cat("You must limit the category to just 2 unique groups.",
                "You currenlty have",length(g),"groups:",
                paste(g, collapse = ","),"\n"))

    # Switch axes if requested
    if (switch == TRUE){
      g <- rev(g)
    }

    if( length(g) != 2){
      stop(paste("Column", fac, "has", length(g),
                 "unique values. It needs to have two exactly."))
    }

    # Extract X and Y values
    x <- val[fac == g[1]]
    y <- val[fac == g[2]]
    xlab <- g[1]
    ylab <- g[2]
  } else {

    if(is.null(xlab)){
      xlab = deparse(substitute(x))
    }

    if(is.null(ylab)){
      ylab <- deparse(substitute(y))
    }
  }

  # Remove NA values
  nodata_x <- which(is.na(x))
  nodata_y <- which(is.na(y))
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  if(length(nodata_x) > 0){
    cat(length(nodata_x), " elements in ",xlab ,
        "had missing values. These were removed from the plot.\n")
  }
  if(length(nodata_y) > 0){
    cat(length(nodata_y), " elements in ",ylab ,
        "had missing values. These were removed from the plot.\n")
  }

  # Re-express data if required
  x <- eda_re(x, p = p, tukey = tukey)
  x.isna <- is.na(x)
  rm.nan <- ifelse( any(x.isna), 1 , 0)
  y <- eda_re(y, p = p, tukey = tukey)
  y.isna <- is.na(y)
  rm.nan <- ifelse( any(y.isna), 1 , 0) + rm.nan

  # Re-expression may produce NaN values. Output warning if TRUE
  if( rm.nan > 0 ) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))

  bad <- x.isna | y.isna
  x <- x[!bad]
  y <- y[!bad]

  }

  # Get upper/lower bounds of inner values
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # Compute x and y values ----
  zl <- qqplot(x, y, plot.it = FALSE, qtype = q.type) # Interpolate (if needed)

  zd <- data.frame(y = zl$y - zl$x, x = zl$x)
  zd$x <- zd$x - median(zd$x); zd$y <- zd$y - median(zd$y)

  # Get suggested multiplier and offset (only for empirical data)
  z <- eda_rline(zd,x,y)
  x.multi <-  1 + z$b
  x.add <- median(zl$y - sort(zl$x * x.multi))

  # Apply formula if present
  if(!is.null(fx) & !is.null(fy))
    warning(paste("You should apply a formula to just one axis.\n",
                  "You are applying the formula", fx,"to the x-axis",
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

  # Generate plots ----

  # Get lines-to-inches ratio
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = x, y = y, type = "n", xlab = "", ylab = "", xaxt = "n",
       yaxt='n', main = NULL)
#  y.labs <- range(axTicks(2))
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Get quantile parameters
  qx <- quantile(x, b.val, qtype = q.type)
  qy <- quantile(y, b.val, qtype = q.type)

  # If tail points  are to be plotted differently, identify them
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

  # Set plot parameters
  .pardef <- par(pty = "s", col = plotcol, mar = c(3,y.wid,3,1))
  on.exit(par(.pardef))

  # QQ plot ----
  if(plot == TRUE & md == FALSE ){

    medx <- median(x)
    medy <- median(y)

    # Generate plot
    xylim <- range(x,y)

    # QQ plot: Empirical ----
    if(tails != TRUE){
        plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
              xlim = xylim, ylim = xylim)
    } else {
        plot( x=x[inner.tails], y=y[inner.tails],  ylab=NA, las=1,
              yaxt='n', xaxt='n', xlab=NA,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
              xlim = xylim, ylim = xylim)

        if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
          points( x=x[outer.tails], y=y[outer.tails],
                col.lab=plotcol, pch = tail.pch, col = tail.p.col,
                bg = tail.p.fill, cex = size)
        }
    }

    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.9,
         tck = -0.02)

    mtext(ylab, side=3, adj= -0.06 ,col=plotcol,  padj = -1.2, cex = par("cex"))
    title(xlab = xlab, line =1.8, col.lab=plotcol)

    if(!is.null(title)){
      title(main = title, line =1.2, col.main=plotcol, cex.main=t.size)
    }

    # Add empirical QQ line ----
    abline(0, 1, col = plotcol)

    # Add medians (omit id sym == TRUE) ----
    if(med == TRUE){
      abline(v = medx, col = "grey80", lty = 2)
      abline(h = medy, col = "grey80", lty = 2)
    }

    # Add core boxes ----
    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = qx[1], xright = qx[2], ybottom=sq[3],ytop=sq[4],
           col = rgb(0,0,0,0.05), border = NA)
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }

    # Add power/formula parameters to plot
    if(show.par == TRUE){
      params <- gsub(";\\s*;?\\s*$", "",  paste0("p=", p,"; ",fx,"; ",fy))
      params <- gsub("\\; \\;", ";", params)
      mtext(side = 3, text=params, adj=1, cex = 0.65)
    }


    #  M-D plot ----
  } else if(plot == TRUE & md == TRUE) {

    # Generate labels
    xlab2 <- paste("Mean of", xlab, "and", ylab)
    ylab2 <- paste(ylab,"-", xlab)

    # Compute m-d variables
    md.y  <- (y - x)
    md.x  <- (y + x) * 0.5

    # Get quantile parameters
    qy <- quantile(md.y, b.val, qtype = q.type)
    medx <- median(md.x)
    medy <- median(md.y)

    # Generate plot
    ylim <- range(md.y, 0)
    xlim <- range(md.x)

    if(tails != TRUE){
      plot( x=md.x, y=md.y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
            col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
            ylim = ylim, xlim = xlim)
    } else {
      plot( x=md.x[inner.tails], y=md.y[inner.tails],  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
            col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
            xlim = xlim, ylim = ylim, col.lab=plotcol)
      if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
        points( x=md.x[outer.tails], y=md.y[outer.tails], yaxt='n', xaxt='n',
                col.lab=plotcol, pch = tail.pch, col = tail.p.col,
                bg = tail.p.fill, cex = size)
      }
    }

    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab2, side=3, adj= -0.06 ,col=plotcol,  padj = -1.1, cex = par("cex"))
    title(xlab = xlab2, line =1.8, col.lab=plotcol)
    if(!is.null(title)){
      title(main = title, line =1.2, col.main=plotcol, cex.main=t.size)
    }

    abline(h = 0,  col = plotcol)

    # Add median lines
    if(med == TRUE){
      abline(v = medx, col = "grey80", lty = 2)
      abline(h = medy, col = "grey80", lty = 2)
    }

    sq <- par("usr") # get plot corners
    if(q == TRUE){
      rect(xleft = sq[1], xright = sq[2], ybottom=qy[1],ytop=qy[2],
           col = rgb(0,0,0,0.05), border = NA)
    }
    if(show.par == TRUE){
      params <- gsub(";\\s*;?\\s*$", "",  paste0("p=", p,"; ",fx,"; ",fy))
      params <- gsub("\\; \\;", ";", params)
      mtext(side = 3, text=params, adj=1, cex = 0.65)
    }

  }

  # Reset plot parameters and  output values
  par(.pardef)
  if(norm == FALSE & sym == FALSE){
    print(paste0("Suggested offsets:", "y = ", "x * ", round(x.multi,4),
                 " + (", round(x.add,4),")"))
  }
  data <- data.frame(x, y)
  names(data) <- c(xlab, ylab)
  invisible(list(data = data, p = p, fx = fx, fy = fy))
}
