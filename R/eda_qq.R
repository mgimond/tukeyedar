#' @export
#' @import grDevices
#' @import lattice
#' @importFrom utils modifyList
#' @title Quantile-Quantile plot, mean-difference Tukey plot, and symmetry QQ
#' plot
#'
#' @description \code{eda_qq} Generates an empirical or Normal QQ plot as well
#' as a Tukey mean-difference plot. Can also be used to generate a symmetry QQ
#' plot.
#'
#' @param x  Vector for first variable or a dataframe.
#' @param y  Vector for second variable or column defining the continuous
#'   variable if \code{x} is a dataframe.
#' @param fac Column defining the grouping variable if \code{x} is a dataframe.
#' @param norm Boolean determining if a Normal QQ plot is to be generated.
#' @param sym Boolean determining if a symmetry QQ plot is to be generated.
#' @param p  Power transformation to apply to both sets of values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param q.type An integer between 1 and 9 selecting one of the nine quantile
#'   algorithms. (See \code{quantile}tile function).
#' @param md Boolean determining if Tukey mean-difference plot should be
#'   generated.
#' @param qd Boolean determining if a quantile-difference plot should be
#'   generated (overrides the \code{md} option).
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
#' @param tail.pch Tail-end point symbol type (See \code{tails}).
#' @param tail.p.col Tail-end color for point symbol (See \code{tails}).
#' @param tail.p.fill Tail-end point fill color passed to \code{bg}
#'   (Only used for \code{pch} ranging from 21-25).
#' @param size Point size (0-1)
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param q Boolean determining if grey quantile boxes should be plotted.
#' @param inner Fraction of mid-values to display in the quantile box. Defaults
#'   to the IQR.
#' @param tails Boolean determining if points outside of the \code{inner} region
#'   should be symbolized differently. Tail-end points are symbolized via the
#'  \code{tail.pch},  \code{tail.p.col} and \code{tail.p.fill} arguments.
#' @param switch Boolean determining if the axes should be swapped. Only applies
#'   to dataframe input. Ignored if vectors are passed to the function.
#' @param xlab X label for output plot. Ignored if \code{x} is a dataframe.
#' @param ylab Y label for output plot. Ignored if \code{x} is a dataframe.
#' @param title Title to add to plot.
#' @param t.size Title size.
#' @param ... Not used
#'
#' @details When the function is used to generate an empirical QQ plot, the plot
#'   will displays the inner 75% of the data via grey boxes for both x and y
#'   values. The inner values can be changed via the  \code{inner} argument.
#'   The middle dashed line represents each batch's median value.
#'   Console output prints the suggested multiplicative and additive offsets.
#'   See the QQ plot vignette for an introduction on its use and interpretation.\cr
#'   \cr
#'   The function can generate a Normal QQ plot when the
#'   \code{norm} argument is set to  \code{TRUE}.
#'    Note that the "suggested offsets" output is disabled, nor
#'   can you generate an M-D version of the Normal QQ plot. Also note
#'   that the formula argument is ignored in this mode.\cr
#'   \cr
#'   The function can be used to generate a symmetry QQ plot when the
#'   \code{sym} argument is set to  \code{TRUE}. This plot helps assess the
#'   symmetry of a variable by splitting it into two halves, upper and lower,
#'   using the batch's median as the cutoff point. The values for each half are
#'   the distances of each observation to the median value in \code{x}'s units.
#'   The distance starts at 0 in the bottom-left corner of the plot. The grey
#'   box width, \code{inner},  will be
#'   measured from the origin given that the batch's center of mass is at 0.
#'   Power transformations can be applied to \code{x} but any formula passed
#'   via \code{fx} or \code{fy} is ignored. This plot is inspired from the
#'   symmetry plot described by Chambers et al. in section 2.8 of their book
#'   (see reference).
#'
#' @returns Returns a list with the following components:
#'
#' \itemize{
#'   \item \code{data}: Dataframe with input \code{x} and \code{y} values.
#'   May be interpolated to smallest quantile batch.
#'   Values will reflect power transformation defined in \code{p}.
#'   \item \code{p}: Re-expression applied to original values.
#'   \item \code{fx}: Formula applied to x variable.
#'   \item \code{fy}: Formula applied to y variable.}
#'
#' @references
#'
#' \itemize{
#'   \item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
#'   Graphical Methods for Data Analysis (1983)}
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
#'  # An alternative to the mean-difference plot is the quantile-difference plot
#'  # where the quantile (instead of the mean) is mapped to the x-axis
#'  eda_qq(bass2, tenor1, fx = "x - 2", qd = TRUE)
#'
#'  # An offset of another 0.5 inches seems warranted
#'  # We can say that overall, bass2 singers are 2.5 inches taller than  tenor1.
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
#'  # Function can generate a Normal QQ plot
#'  eda_qq(bass2, norm = TRUE)
#'
#'  # Function can also generate a symmetry QQ plot
#'  eda_qq(tenor1, sym = TRUE)


eda_qq <- function(x, y = NULL, fac = NULL, norm = FALSE, sym = FALSE, p = 1L,
                   tukey = FALSE, md = FALSE, qd = FALSE,
                   q.type = 5, fx = NULL, fy = NULL, plot = TRUE,
                   show.par = TRUE, grey = 0.6, pch = 21, p.col = "grey50",
                   p.fill = "grey80", size = 0.8, alpha = 0.8, q = TRUE,
                   inner = 0.75, tails = TRUE,
                   tail.pch = 21, tail.p.col = "grey70", tail.p.fill = NULL,
                   switch = FALSE, xlab = NULL, ylab = NULL, title = NULL,
                   t.size = 1.2, ...) {

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Parameters check
  if ("data.frame" %in% class(x) & norm == TRUE)
    stop("x needs to be a vector if norm=TRUE")
  if(norm == TRUE & md == TRUE)
    stop("A rotated version of Normal QQ plot is not yet implemented.")
  if(norm == TRUE & qd == TRUE)
    stop("A rotated version of Normal QQ plot is not yet implemented.")
  if(norm == TRUE & sym == TRUE)
    stop(paste("This function cannot combine a normal QQ plot with a symmetry QQ plot.",
               " Either set norm to FALSE or sym to FALSE."))
   if(sym == TRUE & ("data.frame" %in% class(x)) )
    stop("x must be avector and not a dataframe to generate a symmetry QQ plot")
   if(sym == TRUE)
    cat(" This is a symmetry QQ plot.\n",
        "Values are distances from each observation to the median value\n",
        "Function arguments (fx and fy) are ignored")
   if(md == TRUE & qd == TRUE)
     warning(paste("Both md and qd are set to TRUE. ",
                   "qd will override md."))
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
  } else if (sym != TRUE){

    if(is.null(xlab)){
      if( norm == FALSE) {
        xlab = deparse(substitute(x))
      } else {
        xlab = "Normal quantile"
      }
    }

    if(is.null(ylab) & norm == FALSE){
      ylab <- deparse(substitute(y))
    } else if (is.null(ylab) & norm == TRUE){
      ylab <- substitute(x)
    }
  }

  # Re-express data if required
  if (p != 1) {
    x <- eda_re(x, p = p, tukey = tukey)
  }
  x.isna <- is.na(x)
  rm.nan <- ifelse( any(x.isna), 1 , 0)
  if(norm == FALSE & sym == FALSE) {
    y <- eda_re(y, p = p, tukey = tukey)
    y.isna <- is.na(y)
    rm.nan <- ifelse( any(y.isna), 1 , 0) + rm.nan
  }

  # Re-expression may produce NaN values. Output warning if TRUE
  if( rm.nan > 0 ) {
    warning(paste("\nRe-expression produced NaN values. These observations will",
                  "be removed from output. This will result in fewer points",
                  "in the ouptut."))
    if( norm == FALSE){
      bad <- x.isna | y.isna
      x <- x[!bad]
      y <- y[!bad]
    } else {
      x <- x[!x.isna]
    }
  }

  # Get upper/lower bounds of inner values
  b.val = c(.5 - inner / 2 , .5 + inner / 2)

  # If a symmetry QQ plot is requested, split x in half
  if(sym == TRUE) {
    med <- median(x)
    len <- length(x)
    x.sort <- sort(x)
    n2 <- ifelse( len%%2 == 0, len/2, (len + 1)/2)
    x <- med - x.sort[1:n2]
    y <- x.sort[ (len + 1) - (1:n2) ] - med
    xlab <- "lower half"
    ylab <- "upper half"

    # Modify grey box parameters
    b.val <- c(0, diff(b.val))

    # Ignore any functions applied to the data
    fx <- NULL
    fy <- NULL
  }

  # Compute x and y values ----
  if(norm == FALSE){
    zl <- qqplot(x, y, plot.it = FALSE, qtype = q.type) # Interpolate (if needed)
  } else {
    zl <- qqnorm(x, plot.it = FALSE)  # Get Normal quantiles
  }

  zd <- data.frame(y = zl$y - zl$x, x = zl$x)
  zd$x <- zd$x - median(zd$x); zd$y <- zd$y - median(zd$y)

  # Get suggested multiplier and offset (only for empirical data)
  if(norm == FALSE & sym == FALSE ){
    z <- eda_rline(zd,x,y)
    x.multi <-  1 + z$b
    x.add <- median(zl$y - sort(zl$x * x.multi))
  }


  # Apply formula if present
  if(!is.null(fx) & !is.null(fy) & norm == FALSE)
    warning(paste("You should apply a formula to just one axis.\n",
                  "You are applying the formula", fx,"to the x-axis",
                  "and the formula",fy ,"to the y-axis."))
  if(!is.null(fx) & norm == FALSE){
    fx <- tolower(fx)
    if(!grepl("x", fx)) stop("Formula fx does not contain \"x\" variable.")
    x <- eval(parse(text=fx))
  }
  if(!is.null(fy) & norm == FALSE){
    fy <- tolower(fy)
    if(!grepl("y", fy)) stop("Formula fx does not contain \"y\" variable.")
    y <- eval(parse(text=fy))
  }
  if( (!is.null(fx) | !is.null(fy)) & norm == TRUE)
    warning("Formula is ignored when generating a Normal QQ plot")

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
  if(norm != TRUE){
    qq <- qqplot(x,y, plot.it = FALSE, qtype = q.type)
  } else {
    qq <- qqnorm(sort(x), plot.it = FALSE)
  }

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
  if(plot == TRUE & md == FALSE & qd == FALSE){

    medx <- median(x)
    medy <- median(y)

    # Generate plot
    if(norm != TRUE){
      xylim <- range(x,y)
    } else {
      xlim <- range(x)
      ylim <- range(y)
    }

    # QQ plot: Empirical ----
    if(norm == FALSE){
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

      # QQ plot: Normal ----
    } else {
      if(tails != TRUE){
        plot( x=x, y=y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
      } else {
        plot( x=x[inner.tails], y=y[inner.tails], ylab=NA, las = 1,
              yaxt='n', xaxt='n', xlab=NA, xlim = xlim, ylim = ylim,
              col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size)
        if (length(x[outer.tails]) !=0){  # Nothing to plot if tail index is empty
          points( x=x[outer.tails], y=y[outer.tails], yaxt='n', xaxt='n',
                  col.lab=plotcol, pch = tail.pch, col = tail.p.col,
                  bg = tail.p.fill, cex = size)
        }
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
    if(norm != TRUE){
      abline(0, 1, col = plotcol)

      # Add Normal QQ line ----
    } else {
      probs = c(0.25, 0.75)
      yy <- as.vector(quantile(y, probs, names = FALSE, type = q.type))
      xx <- qnorm(probs)
      slope <- diff(yy) / diff(xx)
      int <- yy[[1L]] - slope * xx[[1L]]
      abline(int, slope, col = plotcol)
      abline(v = c(-1,1), col = "grey90", lty = 3)
    }

    # Add medians (omit id sym == TRUE) ----
    if(sym != TRUE){
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
    if(norm != TRUE & show.par == TRUE){
      params <- gsub(";\\s*;?\\s*$", "",  paste0("p=", p,"; ",fx,"; ",fy))
      params <- gsub("\\; \\;", ";", params)
      mtext(side = 3, text=params, adj=1, cex = 0.65)
    } else if (show.par == TRUE) {
      mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
    }


    #  M-D plot ----
  } else if(plot == TRUE & md == TRUE & norm == FALSE & qd == FALSE) {

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
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
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

    #  Q-D plot  ----
  } else if(plot == TRUE & qd == TRUE){

    # Generate labels
    xlab2 <- paste("Quantile")
    ylab2 <- paste(ylab,"-", xlab)

    # Compute q-d variables
    md.y  <- (y - x)
    md.x  <- (1:length(y) - 0.5) /length(y)

    # Get quantile parameters
    qy <- quantile(md.y, b.val, qtype = q.type)
    lx <- c(0.25, 0.5, 0.75) # Vertical lines showing IQR an median

    medx <- median(md.x)
    medy <- median(md.y)

    # If f-vall type is not 5, print warning
    if(q.type != 5) warning("Currently, only q.type = 5 is implemented.")

    # Generate plot
    ylim <- range(md.y, 0)
    xlim <- range(md.x)

    if(tails != TRUE){
      plot( x=md.x, y=md.y,  ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
            col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
            ylim = ylim)
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
    axis(1,col=plotcol, col.axis=plotcol, labels=lx, at=lx, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext(ylab2, side=3, adj= -0.06 ,col=plotcol,  padj = -1.1, cex = par("cex"))
    title(xlab = xlab2, line =1.8, col.lab=plotcol)
    if(!is.null(title)){
      title(main = title, line =1.2, col.main=plotcol, cex.main=t.size)
    }

    abline(h = 0,  col = plotcol)
    abline(v = medx, col = "grey80", lty = 2)
    abline(h = medy, col = "grey80", lty = 2)
    abline(v = lx, col = "grey90", lty = 3)
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
