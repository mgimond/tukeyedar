#' @export
#' @title Spread-location and spread-level plots
#'
#' @description The \code{eda_sl} function generates William Cleveland's
#' \strong{spread-location plot} for univariate and bivariate data. The function will also
#' generate Tukeys' \strong{spread-level plot}.
#'
#' @param dat Dataframe of univariate data or a linear model.
#' @param x Continuous variable column (ignored if \code{dat} is a linear model).
#' @param fac Categorical variable column (ignored if \code{dat} is a linear
#'   model).
#' @param type s-l plot type. \code{"location"} = spread-location,
#'   \code{"level"} = spread-level (only for univariate data).
#'   \code{"dependence"} = spread-dependence (only for bivariate model input).
#' @param p  Power transformation to apply to variable. Ignored if input is a
#'   linear model.
#' @param tukey Logical; Determines if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param base Base used with the \code{log()} function if \code{px} or \code{py}
#'   is \code{0}.
#' @param sprd Choice of spreads used in the spread-versus-level plot (i.e.
#'  when \code{type = "level"}). Either
#'   interquartile, \code{sprd = "IQR"} or
#'   fourth-spread, \code{sprd = "frth"} (default).
#' @param jitter Jittering parameter for the spread-location plot. A fraction of
#'   the range of location values.
#' @param robust Logical; Indicates if robust regression should be used on the
#'  spread-level plot.
#' @param loess.d Arguments passed to the internal loess function. Applies only
#'  to the bivariate model s-l plots and the spread-level plot.
#' @param nsim Integer. Number of random residual permutations to generate for
#'   constructing an empirical reference envelope (default is \code{0}, which turns
#'   off simulation). Supported for univariate data inputs (\code{type = "location"}
#'   and \code{type = "level"}).
#' @param sim.type Format of the simulation display—either \code{"lines"} (default)
#'   or \code{"points"}. Note that when \code{type = "level"}, \code{sim.type} is
#'   automatically set to \code{"lines"}.
#' @param sim.col Color of the simulated reference envelope (default is \code{"grey"}).
#' @param sim.alpha Transparency setting (\code{0} to \code{1}, default is \code{0.5})
#'   allowing overlapping simulation runs to form a dense reference band.
#' @param sim.pch Plotting symbol used when \code{sim.type = "points"} (default is \code{16}).
#' @param label Logical; Determines if group labels are to be added to the
#'  spread-location plot.
#' @param plot Logical; Determines if plot should be generated.
#' @param xlab X label for output plot.
#' @param ylab Y label for output plot.
#' @param label.col Color assigned to group labels (only applicable if
#'   \code{type = location}).
#' @param labelxbuff Buffer to add to the edges of the plot to make room for
#'   the labels in a spread-location plot. Value is a fraction of the plot width.
#' @param labelybuff Buffer to add to the top of the plot to make room for
#'   the labels in a spread-location plot. Value is a fraction of the plot width.
#' @param show.par Boolean determining if the power transformation applied to
#'   the data should be displayed.
#' @param ... Arguments to be passed to \code{.eda_plot_xy}.
#'
#' @inheritDotParams  .eda_plot_xy
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
#'     Tukey's spread-level plot (aka spread-versus-level plot, Hoaglin et al.,
#'     p 260). If the pattern is close to linear, the plot can help find a power
#'     transformation that will help stabilize the spread in the data by
#'     subtracting one from the fitted slope. This option outputs the slope of
#'     the fitted line in the console. A loess is added to assess linearity.
#'     By default, the fourth spread is used to define the spread. Alternatively,
#'     the IQR can be used by setting \code{spread = "IQR"}. The output will be
#'     nearly identical except for small datasets where the two methods may
#'     diverge slightly in output.\cr
#'           \eqn{\ spread = log(fourth\ spread(residuals))} \cr
#'           \eqn{\ location = log(medians)}
#'
#'    \item \code{type = "location"} if input is a model of class \code{lm},
#'          \code{eda_lm} or \code{eda_rline}:\cr\cr
#'           William Cleveland's spread-location plot (aka scale-location plot)
#'           applied to residuals of a linear model.\cr
#'           \eqn{\ spread = \sqrt{|residuals|}} \cr
#'           \eqn{\ location = fitted\ values}
#'
#'    \item \code{type = "dependence"} if input is a model of class \code{lm},
#'          \code{eda_lm} or \code{eda_rline}:\cr\cr
#'           William Cleveland's spread-location plot applied to residuals of
#'           a linear model.\cr
#'           \eqn{\ spread = \sqrt{|residuals|}} \cr
#'           \eqn{\ dependence = x\ variable}
#'  }
#'
#' To assess whether observed differences in group spreads (or an apparent trend in
#' a spread-level plot) reflect genuine scale dependence (heteroscedasticity) rather than
#' natural sampling noise, \code{eda_sl()} includes an optional permutation simulation
#' routine triggered by setting \code{nsim > 0} (supported for univariate data).
#'
#' Rather than shuffling factor labels (which would artificially collapse group medians
#' toward the grand median) the simulation holds observed group locations fixed on the
#' x-axis and randomly permutes the centered residuals across all observations under
#' the benchmark of equal variance (homoscedasticity).
#'
#' When \code{type = "location"}, group median spreads (\eqn{\sqrt{|\text{residuals}|}})
#' are recomputed from the shuffled residuals across each iteration. This renders a visual
#' reference envelope (spaghetti lines when \code{sim.type = "lines"}) across the full location
#' spectrum. If the observed red summary line sits comfortably within the simulation cloud,
#' group spread differences are consistent with random variation.
#'
#' When \code{type = "level"}, the log(spread) versus log(location) regression line is refitted
#' across each permutation iteration. This builds a visual slope envelope demonstrating how
#' much slope variation occurs naturally under homoscedasticity. This provides a visual benchmark
#' before selecting a power transformation (\eqn{p = 1 - b}).
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
#' # You can specify the exact form of the spread on the y-axis
#' # via the ylab argument
#' eda_sl(cars, MPG.city, Type, ylab = expression(sqrt(abs(residuals))) )
#'
#' # The function can also generate Tukey's spread-level plot to identify a
#' # power transformation that can stabilize spread across fitted values
#' # following power = 1 - slope
#' eda_sl(cars, MPG.city, Type, type = "level")
#'
#' # Spread-level plot with a 100-run permutation line envelope
#' eda_sl(cars, MPG.city, Type, type = "level", nsim=100)
#'
#' # A slope of around 3 is computed from the s-l plot, therefore, a suggested
#' # power is 1 - 3 = -2. We can apply a power transformation within the
#' # function via the p argument. By default, a Box-Cox transformation method
#' # is adopted.
#' # The resulting s-l plot shows a slope that is more representative of naturally
#' # occurring variability under homoscedasticity.
#' eda_sl(cars, MPG.city, Type, type = "level", p=-2, nsim=100)
#'
#' # Spread-location plot can also be generated from residuals of a linear model
#' M1 <- lm(mpg ~ hp, mtcars)
#' eda_sl(M1)
#'
#' # Spread can be compared to X instead of fitted value
#' eda_sl(M1, type = "dependence")

eda_sl <- function(dat, x=NULL, fac=NULL, type = "location", p = 1, tukey = FALSE,
                   base = exp(1), sprd = "frth", jitter = 0.01, robust = TRUE,
                   loess.d = list(family = "symmetric", degree=1, span = 1),
                   nsim = 0, sim.alpha = 0.5, sim.col = "grey",
                   sim.pch = 16, sim.type = "lines",
                   label = TRUE, label.col = "lightsalmon",
                   xlab = NULL, ylab = NULL, labelxbuff = 0.05,
                   labelybuff = 0.05, show.par = FALSE, plot = TRUE,  ...) {

  # Check that input is either an eda_lm model or a dataframe
  if (! (inherits(dat,"data.frame") |
         (inherits(dat,"eda_lm") |
          inherits(dat, "lm") |
          inherits(dat, "eda_rline"))))
    stop("The input object must of class eda_lm or a data.frame.")

  # Parameters check
  if (!sprd %in% c("frth", "IQR"))
    stop("Argument \"sprd\" must be one of \"frth\" or \"IQR\".",
         call. = FALSE)

  # Check that type is properly specified
  if(!type %in% c("location", "level", "dependence"))
    stop("type argument is invalid.")

  # Check: Force sim.type = "lines" when type = "level"
  if (nsim > 0 && type == "level" && sim.type == "points") {
    warning("sim.type = 'points' is not supported when type = 'level'. Resetting sim.type to 'lines'.",
            call. = FALSE)
    sim.type <- "lines"
  }

  # Initialize some variables
  ylim = NULL
  xlim = NULL
  reg = FALSE
  loe = FALSE

  # Extract data
  if(inherits(dat,"data.frame")){     # Univariate input
    dtype <- "univariate"
    x   <- eval(substitute(x), dat)
    fac <- eval(substitute(fac), dat)
    if(is.factor(fac)) fac <- droplevels(fac)

    # Remove missing values from the data
    which_na <- which(is.na(x))
    if(length(which_na > 0)){
      x <- x[-which_na]
      fac <- fac[-which_na]
      if(is.factor(fac)) fac <- droplevels(fac)
      warning(cat(length(which_na),"rows were removed due to NAs being present.\n"))
    }

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
    y <- dat$residuals
    if (type == "dependence"){
      if(inherits(dat, "lm")){
        x <- dat$model[,2]
      } else {
        x <- dat$x
      }
    } else {
      x <- dat$fitted.values
    }
    dtype <- "model"
  }

  # The simulation option is currently only available for univariate data
  if (nsim > 0 && dtype == "model") {
    warning("Permutation simulation for eda_sl() is currently supported for univariate data frames only.")
  }

  # Get labels
  if(is.null(xlab)){
    if (dtype == "model" & type == "location"){
      xlab = "Fitted values"
    } else if (dtype == "model" & type == "dependence"){
      xlab = "X"
    } else {
      xlab = "Location"
    }
  }
  if(is.null(ylab)){
    if (dtype == "model"){
      ylab <- expression(sqrt(abs(residuals)))
    } else {
      ylab <- "Spread"
    }
  }

  # Re-express data if required (only applies to univariate data)
  if(inherits(dat,"data.frame")){
    x <- eda_re(x, p = p, tukey = tukey, base = base)
    x.nan <- is.na(x)
    if( any(x.nan)){
      x <- x[!x.nan]
      fac <- fac[!x.nan]
      warning(paste("\nRe-expression produced NaN values. These observations will",
                    "be removed from output. This will result in fewer points",
                    "in the ouptut."))
    }
  }

  # # Set point color parameters.
  # if(!is.null(alpha)){
  #   if(p.col %in% colors() & p.fill %in% colors() ){
  #     p.col  <- adjustcolor( p.col,  alpha.f = alpha)
  #     p.fill <- adjustcolor( p.fill, alpha.f = alpha)
  #   }
  # }

  # Custom function
  frth_sprd <- function(x) {
    lsum <- eda_lsum(x, l=2)
    return(lsum[2,6]) # Returns spread
  }

  # Spread-location plot options
  if(type == "location" & dtype == "univariate"){  # univariate spread-location
    reg = FALSE
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
  } else if (type == "level" & dtype == "univariate"){   # univariate spread-level
    reg = TRUE
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
    loe = TRUE
    level <- x
    spread <- sqrt(abs(y))
    df4 <- data.frame(Level = level, Spread = spread)
  }

  # --- Permutation Simulation Block (Univariate only) ----
  sim_lines <- list()

  if (nsim > 0 && dtype == "univariate") {
    for (i in seq_len(nsim)) {

      if (type == "location") {
        # 1. Compute observed group medians and centered residuals
        meds_obs <- tapply(x, fac, median)
        res_obs  <- x - meds_obs[as.character(fac)]

        # 2. Shuffle residuals across all observations (holding locations fixed)
        res_sim <- sample(res_obs)
        spread_sim <- sqrt(abs(res_sim))
        spread_med_sim <- tapply(spread_sim, fac, median)

        # 3. Store simulation line spanning the full observed x-range
        sim_lines[[i]] <- data.frame(
          x = sort(meds_obs),
          y = spread_med_sim[order(meds_obs)]
        )

      } else if (type == "level") {
        # For log(spread) vs log(median), permuting residuals across groups
        meds_obs <- tapply(x, fac, median)
        res_obs  <- x - meds_obs[as.character(fac)]

        res_sim <- sample(res_obs)
        x_sim <- meds_obs[as.character(fac)] + res_sim
        x_fac_sim <- split(x_sim, fac)

        level_sim  <- log(unlist(lapply(x_fac_sim, median)))
        spread_sim <- if (sprd == "frth") {
          log(unlist(lapply(x_fac_sim, frth_sprd)))
        } else {
          log(unlist(lapply(x_fac_sim, IQR)))
        }

        df_sim <- data.frame(Level = level_sim, Spread = spread_sim)

        # Filter out non-finite values (-Inf from log(0) or NaN/NA)
        df_sim <- df_sim[is.finite(df_sim$Level) & is.finite(df_sim$Spread), ]

        # Fit model only if at least 2 valid group levels remain
        if (nrow(df_sim) >= 2) {
          M_sim  <- if (robust) {
            suppressWarnings(MASS::rlm(Spread ~ Level, df_sim))
          } else {
            lm(Spread ~ Level, data = df_sim)
          }
        }

        rx <- range(level_sim)
        sim_lines[[i]] <- data.frame(
          x = rx,
          y = predict(M_sim, newdata = data.frame(Level = rx))
        )
      }
    }
  }
  # End of simulation block

  df <- data.frame(level, spread)

  if(plot == TRUE){

    lst0 <- .eda_plot_xy(df, level, spread, xlab = xlab, ylab = ylab, reg = reg,
                         loe = loe, loess.d = loess.d, sd = FALSE, mean.l = FALSE,
                         py = p, px = 1, tukey = tukey, base = base, asp = FALSE,
                         xlim = xlim, ylim = ylim, show.par = show.par, robust = robust,  ...)

    # Render Simulation Cloud (Background Layer) ---
    if (nsim > 0 && dtype == "univariate") {
      .post <- par(mar = lst0$parxy)
      sim_color <- adjustcolor(sim.col, alpha.f = sim.alpha)

      for (i in seq_len(nsim)) {
        if (type == "location") {
          if (sim.type == "lines") {
            lines(sim_lines[[i]]$x, sim_lines[[i]]$y, col = sim_color, lwd = 1)
          } else {
            points(sim_lines[[i]]$x, sim_lines[[i]]$y, pch = sim.pch, col = sim_color, cex = 0.8)
          }
        } else if (type == "level") {
          lines(sim_lines[[i]]$x, sim_lines[[i]]$y, col = sim_color, lwd = 1, lty = 1)
        }
      }
    }

    # Draw Observed Summary Lines (Foreground Layer) ---
    if (type == "location" & dtype == "univariate"){

      .post <- par(mar = lst0$parxy)
      on.exit(par(.post))

    #  title(xlab = xlab, line = 1.8, col.lab=plotcol)
      lines(sort(meds),spread_med[order(meds)], col = rgb(1, 0.5, 0.5, 0.9), lw = 2)
      points(meds, spread_med, col = rgb(1, 0.5, 0.5, 0.8), pch = 15)
      if (label == TRUE){
        with(df4, label_placement(Level, Spread, grp, label.col))
      }
      # if(show.par == TRUE){
      #   mtext(side = 3, text=paste0("p=",p), adj=1, cex = 0.65)
      # }
      par(.post)
    }
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
