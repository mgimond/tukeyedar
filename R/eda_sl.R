#' @export
#' @title Spread-location and spread-level plots
#'
#' @description The \code{eda_sl} function generates William Cleveland's
#' spread-location plot for univariate and bivariate data. The function will also
#' generate Tukeys' spread-level plot.
#'
#' @param dat Dataframe of univariate data or a linear model.
#' @param x Continuous variable column (ignored if \code{dat} is a linear model).
#' @param fac Categorical variable column (ignored if \code{dat} is a linear
#'   model).
#' @param type s-l plot type. \code{"location"} = spread-location,
#'   \code{"level"} = spread-level. If the input is a model, a spread-level plot
#'   is generated.
#' @param p  Power transformation to apply to variable. Ignored if input is a
#'   linear model.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param sprd Choice of spreads used in the spread-versus-level plot. Either
#'   interquartile, \code{sprd = "IQR"} or
#'   fourth-spread, \code{sprd = "frth"} (default).
#' @param jitter Jittering parameter for the spread-location plot. A fraction of
#'   the range of location values.
#' @param robust Boolean indicating if robust regression should be used on the
#'  spread-level plot.
#' @param loess.d Arguments passed to the internal loess function. Applies only
#'  to the linear model spread-level plot.
#' @param label Boolean determining if group labels are to be added to the
#'  spread-location plot.
#' @param plot Boolean determining if plot should be generated.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color passed to \code{bg} (Only used for \code{pch}
#'   ranging from 21-25).
#' @param size Point size (0-1).
#' @param alpha Point transparency (0 = transparent, 1 = opaque). Only
#'   applicable if \code{rgb()} is not used to define point colors.
#' @param label.col Color assigned to group labels (only applicable if
#'   \code{type = location}).
#' @param labelxbuff Buffer to add to the edges of the plot to make room for
#'   the labels in a spread-location plot. Value is a fraction of the plot width.
#' @param labelybuff Buffer to add to the top of the plot to make room for
#'   the labels in a spread-location plot. Value is a fraction of the plot width.
#' @param show.par Boolean determining if the power transformation applied to
#'   the data should be displayed.
#'
#' @return Returns a dataframe of level and spread values.
#'
#' @details
#'  The function generates a few variations of the spread-location/spread-level
#'   plots depending on the data input type and parameter passed to the
#'   \code{type} argument. The residual spreads are mapped to the y-axis and the
#'   levels are mapped to the x-axis. Their values are computed as follows:
#'  \itemize{
#'
#'    \item \code{type = "location"} (univariate data):\cr\cr
#'           William Cleveland's spread-location plot applied to univariate
#'           data.\cr
#'           \eqn{\ spread = \sqrt{|residuals|}} \cr
#'           \eqn{\ location = medians}
#'
#'    \item \code{type = "level"} (univariate data):\cr\cr
#'     Tukey's spread-level plot (aka spread-versus-level plot).
#'     This plot is commonly used to help find a power transformation that will
#'     help stabilize the spread in the data. This option will output the slope
#'     of the fitted line in the console. By default, the fourth spread is
#'     used to define the spread. Alternatively, the IQR can be used by setting
#'     \code{spread = "IQR"}. The output will be nearly identical except for
#'     small datasets where the two methods may diverge slightly in output.\cr
#'           \eqn{\ spread = log(fourth\ spread(residuals))} \cr
#'           \eqn{\ location = log(medians)}
#'
#'    \item \code{type = "location"} if input is a model of class \code{lm},
#'          \code{eda_lm} or \code{eda_rline}:\cr\cr
#'           William Cleveland's spread-location plot applied to residuals of
#'           a linear model.\cr
#'           \eqn{\ spread = \sqrt{|residuals|}} \cr
#'           \eqn{\ location = fitted\ values}
#'
#'  }
#'
#'
#' @references
#'  \itemize{
#'   \item Understanding Robust and Exploratory Data Analysis, Hoaglin,
#'    David C., Frederick Mosteller, and John W. Tukey, 1983.
#'    \item William S. Cleveland. Visualizing Data. Hobart Press (1993)
#' }
#'
#' @examples
#' cars <- MASS::Cars93
#' # Cleveland's spread-location plot applied to univariate data
#' eda_sl(cars, MPG.city, Type)
#'
#' # The function can also generate Tukey's spread-level plot to identify a
#' # power transformation that can stabilize spread across fitted values
#' # following power = 1 - slope
#' eda_sl(cars, MPG.city, Type, type = "level")
#'
#' # A slope of around 3 is computed from the s-l plot, therefore, a suggested
#' # power is 1 - 3 = -2. We can apply a power transformation within the
#' # function via the p argument. By default, a Box-Cox transformation method
#' # is adopted.
#' eda_sl(cars, MPG.city, Type, p = -2)
#'
#' # Spread-location plot can also be generated from residuals of a linear model
#' M1 <- lm(mpg ~ hp, mtcars)
#' eda_sl(M1)
#'

eda_sl <- function(dat, x=NULL, fac=NULL, type = "location", p = 1, tukey = FALSE,
                   sprd = "frth", jitter = 0.01, robust = TRUE,
                   loess.d = list(family = "symmetric", degree=1, span = 1),
                   label = TRUE, label.col = "lightsalmon", plot = TRUE,
                   grey = 0.6, pch = 21, p.col = "grey50", p.fill = "grey80",
                   size = 1,  alpha = 0.8, labelxbuff = 0.05,
                   labelybuff = 0.05, show.par = TRUE) {

  # Check that input is either an eda_lm model or a dataframe
  if (! (inherits(dat,"data.frame") |
         (inherits(dat,"eda_lm") |
          inherits(dat, "lm") |
          inherits(dat, "eda_rline"))))
    stop("The input object must of class eda_lm or a data.frame.")

  # Parameters check
  if (!sprd %in% c("frth", "IQR")) stop("Argument \"sprd\" must be one of \"frth\" or \"IQR\".",
                                        call. = FALSE)

  # Initialize some variables
  ylim = NULL
  xlim = NULL

  # Extract data
  if(inherits(dat,"data.frame")){     # Univariate input
    x   <- eval(substitute(x), dat)
    fac <- eval(substitute(fac), dat)

    # Check that each group has at least two values (only applies to univariate
    # data). Remove groups with less than 2 records.
    group_n <- table(fac)
    x   <-   x[fac %in% names(group_n[group_n > 1])]
    fac <- fac[fac %in% names(group_n[group_n > 1])]
    if( any(group_n < 2) )
      warning(paste("One or more groups was removed from the dataset",
                  "for having less than two observations:",
                  names(group_n[group_n < 2]), "\n"))
  } else {   # Model input
    type <- "model"
    y <- dat$residuals
    x <- dat$fitted.values
  }




  # Re-express data if required (only applies to univariate data)
  if(inherits(dat,"data.frame")){
    x <- eda_re(x, p = p, tukey = tukey)
    x.nan <- is.na(x)
    if( any(x.nan)){
      x <- x[!x.nan]
      fac <- fac[!x.nan]
      warning(paste("\nRe-expression produced NaN values. These observations will",
                    "be removed from output. This will result in fewer points",
                    "in the ouptut."))
    }
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

  # Custom function
  frth_sprd <- function(x) {
    lsum <- eda_lsum(x, l=2)
    return(lsum[2,6]) # Returns spread
  }

  # Spread-location plot options
  if(type == "location"){  # univariate spread-location
    meds <- tapply(x, fac, median)
    level <- meds[as.character(fac)]
    spread <- sqrt(abs(x - level))
    level <- jitter(level, amount = jitter * diff(range(level)))
    spread_med <- tapply(spread, fac, median)
    df4 <- data.frame(Level = level, Spread = spread, grp = fac)
    ylim <- c(0, max(spread) + max(spread) * labelybuff )
    rangex <- range(level)
    xlim <- c(rangex[1] - diff(rangex) * labelxbuff,
              rangex[2] + diff(rangex) * labelxbuff)
  } else if (type == "level"){   # univariate spread-level
    # Split data into groups
    x_fac <- split(x, fac)
    level <- log(unlist(lapply(x_fac, median)))
    if( sprd == "frth"){
      spread <- log(unlist(lapply(x_fac, frth_sprd)))
    } else if(sprd == "IQR") {
      spread <- log(unlist(lapply(x_fac, IQR)))
    }
    df4 <- data.frame(Level = level, Spread = spread)
  } else { # linear model spread-level
     level <- x
     spread <- sqrt(abs(y))
     df4 <- data.frame(Level = level, Spread = spread)
  }

  # Generated plot (if requested)
    if(plot == TRUE){

    # Get lines-to-inches ratio
    in2line <- ( par("mar") / par("mai") )[2]

    # Create a dummy plot to extract y-axis labels
    pdf(NULL)
    plot(x = level, y = spread, type = "n", xlab = "", ylab = "", xaxt = "n",
         yaxt='n', main = NULL, xlim=xlim, ylim=ylim)
    y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
    dev.off()

    # Compute the margin width (returned in inches before converting to lines)
    # y.wid <- max( strwidth( y.labs[1], units="inches"),
    #               strwidth( y.labs[2], units="inches")) * in2line + 1

    .pardef <- par(col = plotcol, mar = c(3,y.wid,3,1))
    on.exit(par(.pardef))

    plot( x=level, y=spread , ylab=NA, las=1, yaxt='n', xaxt='n', xlab=NA,
          col.lab=plotcol, pch = pch, col = p.col, bg = p.fill, cex = size,
          ylim = ylim, xlim = xlim)
    box(col=plotcol)
    axis(1,col=plotcol, col.axis=plotcol, labels=TRUE, padj = -0.5)
    axis(2,col=plotcol, col.axis=plotcol, labels=TRUE, las=1, hadj = 0.7)
    mtext("Spread", side=3, adj= -0.05 , col=plotcol, padj = -1)

    if (type == "location"){
      title(xlab = "Location", line = 1.8, col.lab=plotcol)
      lines(sort(meds),spread_med[order(meds)], col = rgb(1, 0.5, 0.5, 0.9), lw = 2)
      points(meds, spread_med, col = rgb(1, 0.5, 0.5, 0.8), pch = 15)
      if (label == TRUE){
        with(df4, label_placement(Level, Spread, grp, label.col))
      }
      if(show.par == TRUE){
        mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
      }
    } else if (type == "level") {
      if(robust == TRUE){
        Mlevel <- MASS::rlm(Spread ~ Level, df4)
      } else {
        Mlevel <- lm(Spread ~ Level, df4)
      }
      abline(Mlevel, col = rgb(1, 0.5, 0.5, 0.9), lw = 2)
      cat("Slope = ", Mlevel$coefficients[2])
      title(xlab = "Location", line = 1.8, col.lab=plotcol)
      if(show.par == TRUE){
        mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
      }
    } else {
      loess.l  <- modifyList(list(), loess.d)
      lines( do.call( "loess.smooth",c( list(x=df4$Level,y=df4$Spread), loess.l)),
             col=rgb(1, 0.5, 0.5, 0.9), lw=2 )
      title(xlab = "Level", line = 1.8, col.lab=plotcol)
    }
    par(.pardef)
  }
  invisible(df4)
}


label_placement <- function(x,y,grp, label.col){
  df <- data.frame(x,y,grp)

  # Get range of values for each group
  ranges <- tapply(y, grp, FUN = function(x)diff(range(x)))

  # Calculate label positions
  label_positions <- aggregate(cbind(x, y) ~  grp, df,
                               FUN = function(x) c(mean = mean(x), max = max(x)))
  # Loop through each group
  for (i in 1:nrow(label_positions)) {
    group <- label_positions$grp[i]
    mean_level <- label_positions$x[i, "mean"]
    max_spread <- label_positions$y[i, "max"]

    # Place the text above the highest point in the cluster
    text(x = mean_level, y = max_spread + 0.07 * ranges[i], labels = group,
         col = label.col, cex = 0.8)
  }
}

