#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @title Residual-Fit Spread Plot
#'
#' @description
#' \code{eda_rfs} generates a Cleveland residual-fit spread plot
#' with an optional null reference permutation envelope.
#'
#' @param dat Univariate data frame or model object (\code{lm}, \code{eda_lm},
#'   or \code{eda_rline}).
#' @param x Continuous variable (if \code{dat} is a data frame).
#' @param grp Grouping variable (if \code{dat} is a data frame).
#' @param p Power transformation to apply to \code{x}. Applies to univariate
#'   scenario only.
#' @param tukey Logical determining if a Tukey transformation should
#'   be adopted. Applies to univariate scenario only.
#' @param base Base used with \code{log()} if \code{p = 0}. Applies to
#'   univariate scenario only.
#' @param nsim Integer. Number of random permutations to generate for
#'   constructing null reference envelopes. Default is \code{0} (off).
#' @param sim.alpha Numeric. Transparency value (\code{0} to \code{1})
#'   for simulated cloud. Default \code{0.2}.
#' @param sim.col Character. Color for simulated cloud.
#'   Default \code{"coral"}.
#' @param sim.pch Integer or character. Plotting symbol for simulated
#'   points. Default \code{16}.
#' @param sim.type Character. Format of simulation display:
#'   \code{"points"} or \code{"lines"}.
#' @param show.par Logical determining if transformation parameters
#'   are displayed.
#' @param stat Summary statistic function (\code{mean} or
#'   \code{median}).
#' @param grey Grey level for plot elements (0 to 1).
#' @param pch Point symbol type.
#' @param p.col Color for point symbol.
#' @param p.fill Point fill color.
#' @param inner Fraction of input data considered as "mid values"
#'   (default 0.9).
#' @param q Logical determining if inner quantile region should
#'   be shaded.
#' @param qtype Integer specifying quantile algorithm type (1-9).
#' @param size Point size.
#' @param alpha Point transparency.
#' @param ylim Y-axis limits.
#' @param bar Logical determining if a proportion-of-spread stacked
#'   bar is added.
#' @param ... Additional arguments.
#'
#' @details
#' The \code{eda_rfs} function generates a residual-fit spread plot for
#' univariate and bivariate data. Input can be a dataframe with one column
#' storing the continuous variable and another column storing the categorical
#' (grouping) variable or, for a bivariate dataset, a model output from an
#' \code{lm()}, \code{eda_lm()} or \code{eda_rline()} function.\cr
#' \cr
#' The \code{stat} argument only applies to univariate data and allows the user
#' to choose the summary statistic to fit to the data (either mean or median).
#' This statistic is also used to recenter the fitted values in the rfs plot.\cr
#' \cr
#' The \code{q} argument, when set to \code{TRUE}, will add a shaded region to
#' the residual quantile plot highlighting the mid portion of the data defined
#' by the \code{inner} argument (set to 90 percent of the mid values, by default). The
#' range defined by the mid portion of the data is highlighted in the left plot
#' for comparison with the the full range defined by the fitted values. \cr
#' \cr
#' The \code{bar} option, when set to \code{TRUE}, adds a narrow stacked barplot
#' that compares the spread covered by the residuals (red bar) with the spread
#' covered by the fitted values (green bar). The residual spread is computed
#' for the portion of the residuals defined by the \code{inner} argument. The
#' values outputted in the console are those used in computing the vertical
#' bars. The red bar is the relative spread of the residuals and the green bar
#' is the relative spread of the fitted values. The stacked bar plot can be
#' helpful in quickly gauging the effect the fitted values have in explaining
#' the variability in the data. The longer the green bar relative to the red
#' bar, the greater the grouping variable's (for univariate data) or linear
#' model's (for bivariate data) effect in minimizing the uncertainty in the
#' estimated value.
#' \cr \cr
#' Setting \code{nsim > 0} generates an empirical null envelope for
#' both the fitted values and residual panels. For univariate grouping
#' models, group labels are repeatedly permuted across observations
#' using \code{\link[base]{sample}}. For linear models, response
#' values are permuted relative to predictors. Comparing observed
#' curves to the null clouds allows direct visual assessment of
#' whether the model explains variation beyond random chance.
#' @export
#'
#' @references
#'
#' \itemize{
#'   \item William S. Cleveland. Visualizing Data. Hobart Press (1993)
#'   \item \href{../articles/rfs.html}{Residual-fit spread article}}
#' @examples
#'
#' # Generate a basic residual-fit spread plot
#' eda_rfs(mtcars,mpg, cyl)
#'
#' # Add inner 90% region to the residuals plot (shaded area in plot)
#' # Vertical grey box shows matching y-values
#' eda_rfs(mtcars,mpg, cyl, q = TRUE)
#'
#' # Change guide to encompass mid 75% of residual values
#' eda_rfs(mtcars,mpg, cyl, q = TRUE, inner = 0.75)
#'
#' # Use median instead of the mean to compute group summaries and to
#' # recenter the fitted values around 0.
#' eda_rfs(mtcars,mpg, cyl, stat = median)
#'
#' # Apply power transformation of -1 to mpg. Defaults to box-cox method.
#' eda_rfs(mtcars,mpg, cyl, p = -1)
#'
#' # Display a stacked bar plot showing relative importance in spreads
#' # between fitted values and residuals.
#' eda_rfs(mtcars,mpg, cyl, bar = TRUE)
#'
#' # Generate rfs plot for bivariate model output. Model can be generated from
#' # lm(), eda_lm() or eda_rline()
#' M1 <- lm(hp ~ mpg, mtcars)
#' eda_rfs(M1,q =TRUE)
#'
#' M2 <- eda_lm(mtcars, mpg, hp)
#' eda_rfs(M2,q =TRUE)
#'
#' M3 <- eda_rline(mtcars, mpg, hp)
#' eda_rfs(M3, q =TRUE)
#'
#' # Add simulaton envelopes to plots
#' eda_rfs(M1,q =TRUE,nsim=60, sim.type="lines")
#' eda_rfs(mtcars,mpg, cyl, nsim=60)
#'
eda_rfs <- function(dat, x = NULL, grp = NULL, p = 1L, tukey = FALSE, base = exp(1),
                     nsim = 0, sim.alpha = 0.2, sim.col = "coral",
                     sim.pch = 16, sim.type = "points",
                     show.par = TRUE, stat = mean, grey = 0.7, pch = 21,
                     p.col = "grey50", p.fill = "grey80", inner = 0.9, q = FALSE,
                     qtype = 5, size = 0.8, alpha = 0.7, ylim = NULL,bar = FALSE,
                     ...) {
  # Check for invalid arguments passed via ...
  dots <- list(...)

  dot_names <- names(dots)
  dot_names <- dot_names[dot_names != ""]

  # Valid graphical parameters for base plot functions
  allowed_args <- names(par())
  invalid <- setdiff(dot_names, allowed_args)

  if (length(invalid) > 0) {
    warning(sprintf(
      "Invalid argument(s) passed to eda_rfs(): %s",
      paste(invalid, collapse = ", ")
    ))
  }

  # Save graphics state for automatic restoration on exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (!(
    inherits(dat, "data.frame") |
    inherits(dat, "eda_lm") |
    inherits(dat, "lm") |
    inherits(dat, "eda_rline")
  )) {
    stop("The input object must be of class eda_lm, lm, eda_rline, or data.frame.")
  }

  if (!as.character(substitute(stat)) %in% c("mean", "median")) {
    stop("Stat must be either the mean or the median")
  }

  plotcol <- rgb(1 - grey, 1 - grey, 1 - grey)

  lower <- (1 - inner) / 2
  upper <- 1 - lower
  b.val <- c(lower, upper)

  sim_fit_list <- list()
  sim_res_list <- list()

  # Univariate Grouping Scenario
  if (inherits(dat, "data.frame")) {
    type <- "univariate"

    x_val <- eda_re(
      eval(substitute(x), dat),
      p = p,
      tukey = tukey,
      base = base
    )

    grp_val <- eval(substitute(grp), dat)

    if (is.factor(grp_val)) {
      grp_val <- droplevels(grp_val)
    }

    which_na <- which(is.na(x_val))

    if (length(which_na) > 0) {
      x_val <- x_val[-which_na]
      grp_val <- grp_val[-which_na]

      if (is.factor(grp_val)) {
        grp_val <- droplevels(grp_val)
      }

      warning(cat(
        length(which_na),
        "rows were removed due to NAs being present.\n"
      ))
    }

    model <- ave(x_val, grp_val, FUN = stat)
    res <- x_val - model

    model_sort <- sort(model) - stat(x_val)
    res_sort <- sort(res)

    fval <- (seq_along(x_val) - 0.5) / length(x_val)

    # Permutation simulation under H0
    if (nsim > 0) {
      N <- length(x_val)

      for (i in seq_len(nsim)) {
        grp_sim <- sample(grp_val)

        model_sim <- ave(x_val, grp_sim, FUN = stat)

        res_sim <- x_val - model_sim

        sim_fit_list[[i]] <- sort(model_sim) - stat(x_val)
        sim_res_list[[i]] <- sort(res_sim)
      }
    }

    qy <- quantile(res, b.val, type = qtype)
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # Standard lm model scenario
  if (inherits(dat, "lm")) {
    res_sort   <- sort(dat$residuals)
    model_sort <- sort(dat$fitted.values - stat(dat$fitted.values))
    fval <- (seq_along(res_sort) - 0.5) / length(res_sort)
    show.par <- FALSE

    if (nsim > 0) {
      mf <- model.frame(dat)
      resp_name <- names(mf)[1]
      y_orig <- mf[[resp_name]]
      for (i in seq_len(nsim)) {
        mf_sim <- mf
        mf_sim[[resp_name]] <- sample(y_orig)
        mod_sim <- update(dat, data = mf_sim) # Rerun lm() model. Update preserves all arguments used in intial lm()
        sim_fit_list[[i]] <- sort(mod_sim$fitted.values - stat(mod_sim$fitted.values))
        sim_res_list[[i]] <- sort(mod_sim$residuals)
      }
    }
    qy <- quantile(res_sort, b.val, type = qtype)
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # eda_lm model scenario
  if (inherits(dat, "eda_lm")) {
    res_sort <- sort(dat$residuals)
    model_sort <- sort(dat$fitted.values - stat(dat$fitted.values))
    fval <- (seq_along(res_sort) - 0.5) / length(res_sort)
    show.par <- FALSE
    if (nsim > 0) {
      x_val <- dat$data[[1]]
      y_val <- dat$data[[2]]
      poly_deg <- length(dat$b) # Extract polynomial degree from coefficient vector
      for (i in seq_len(nsim)) {
        y_sim <- sample(y_val)
        capture.output(mod_sim <- eda_lm(
          data.frame(x_val, y_sim),
          x_val,
          y_sim,
          poly = poly_deg,
          plot = FALSE
        ))
        sim_fit_list[[i]] <- sort(mod_sim$fitted.values - stat(mod_sim$fitted.values))
        sim_res_list[[i]] <- sort(mod_sim$residuals)
      }
    }

    qy <- quantile(res_sort, b.val, type = qtype)
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # eda_rline model scenario ----

  if (inherits(dat, "eda_rline")) {
    res_sort <- sort(dat$residuals)
    model_sort <- sort(dat$fitted.values - stat(dat$fitted.values))
    fval <- (seq_along(res_sort) - 0.5) / length(res_sort)
    show.par <- FALSE
    if (nsim > 0) {
      x_val <- dat$x
      y_val <- dat$y
      for (i in seq_len(nsim)) {
        df_sim <- data.frame(x = x_val, y = sample(y_val))
        mod_sim <- eda_rline(df_sim, x, y, px = 1, py = 1,tukey = dat$tukey,
                             maxiter = dat$iter)
        sim_fit_list[[i]] <- sort(mod_sim$fitted.values - stat(mod_sim$fitted.values))
        sim_res_list[[i]] <- sort(mod_sim$residuals)
      }
    }

    qy <- quantile(res_sort, b.val, type = qtype)
    e <- ecdf(model_sort)
    qx <- c(e(qy))
  }

  # Expand axis limits to include simulation bounds
  if (is.null(ylim)) {
    if (nsim > 0) {
      all_sim_fit <- unlist(sim_fit_list)
      all_sim_res <- unlist(sim_res_list)

      ylim <- range(c(res_sort, model_sort, all_sim_fit, all_sim_res),
                    na.rm = TRUE)

    } else {
      ylim <- range(res_sort, model_sort, na.rm = TRUE)
    }
  }

  # Summary statistics printout
  res.spd <- diff(qy)
  fit.spd <- diff(range(model_sort))
  # res.fit <- fit.spd / res.spd
  tot.spd <- fit.spd + res.spd
  pct.fit <- (fit.spd / tot.spd) * 100
  pct.res <- (res.spd / tot.spd) * 100

  cat(
    sprintf(
      "The mid %0.1f%% of residuals covers about %0.2f units.\n",
      diff(b.val) * 100, res.spd) )
  cat(sprintf("The fitted values cover a range of %0.2f units.\n", fit.spd))
  cat(sprintf(
      "Relative spread breakdown: %0.1f%% explained (fitted) vs %0.1f%% unexplained (residuals).\n",
      pct.fit,
      pct.res ) )

  # Render Plot ----

  in2liney <- (par("mar") / par("mai"))[2]

  pdf(NULL)

  plot( x = model_sort, y = res_sort, type = "n", xlab = "", ylab = "",
        xaxt = "n", yaxt = "n")

  y.wid <- max(strwidth(axTicks(2), units = "inches")) * in2liney + 1.2

  dev.off()

  xtics <- c(0, 0.25, 0.5, 0.75, 1)

  if (bar == TRUE) {
    layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE), widths = c(0.49, 0.02, 0.49))

  } else {
    layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(0.5, 0.5))
  }

  par(mai = c(0.46, 0, 0.2, 0), cex = 1, oma = c(1, y.wid, 1, 0.5), col = plotcol
  )
  # --- Panel 1: Fit minus stat ---

  plot(
    fval,
    model_sort,
    type = "n",
    ylab = NA,
    las = 1,
    yaxt = "n",
    xaxt = "n",
    xlab = NA,
    ylim = ylim,
    main = paste("Fit minus", as.character(substitute(stat))),
    col.main = "grey40"
  )

  axis(
    1,
    col = plotcol,
    col.axis = plotcol,
    labels = TRUE,
    padj = -0.5,
    at = xtics
  )

  axis(
    2,
    col = plotcol,
    col.axis = plotcol,
    labels = TRUE,
    las = 1,
    hadj = 0.9,
    tck = -0.02
  )

  title(xlab = "f-value", line = 1.4,  col.lab = plotcol)

  grid(nx = NA, ny = NULL)

  abline(v = xtics, lty = "dotted", col = "lightgray")

  # Simulation Cloud (Fit Panel)

  if (nsim > 0) {
    sim_color <- adjustcolor(sim.col, alpha.f = sim.alpha)

    for (i in seq_len(nsim)) {
      if (sim.type == "lines") {
        lines(fval, sim_fit_list[[i]], col = sim_color, lwd = 1)

      } else {
        points(fval, sim_fit_list[[i]], pch = sim.pch, col = sim_color,
          cex = size * 0.8 )
        }
    }
  }

  # Observed Points (Fit Panel)

  points(fval, model_sort, pch = pch, col = p.col, bg = p.fill, cex = size)

  if (q == TRUE) {
    sq <- par("usr")

    rect(xleft = sq[1], xright = sq[2],  ybottom = qy[1], ytop = qy[2],
         col = rgb(0, 0, 0, 0.05), border = NA)
  }

  # --- Optional Stacked Bar ---

  if (bar == TRUE) {
    res.top <- res.spd / (fit.spd + res.spd)

    plot(0, type = "n", xlim = c(0, 1),  ylim = c(0, 1), xlab = "", ylab = "",
         xaxt = "n", yaxt = "n", bty = "o", xaxs = "i", yaxs = "i")

    rect(xleft = 0.03, ybottom = 0, xright = 1, ytop = res.top, col = "#FAC4C3",
         border = FALSE )

    rect(xleft = 0.03, ybottom = res.top, xright = 1, ytop = 1, col = "#CEE6B5",
         border = FALSE)

    box(col = plotcol)
  }

  # --- Panel 2: Residuals ---

  plot(fval, res_sort, type = "n", ylab = NA, las = 1, yaxt = "n", xaxt = "n",
       xlab = NA, ylim = ylim, main = "Residuals", col.main = "grey40")

  axis(1, col = plotcol, col.axis = plotcol, labels = TRUE, padj = -0.5,
       at = xtics[-1])

  title(xlab = "f-value", line = 1.4, col.lab = plotcol)

  grid(nx = NA, ny = NULL)

  abline(v = xtics, lty = "dotted", col = "lightgray")

  # Simulation Cloud (Residual Panel)

  if (nsim > 0) {
    sim_color <- adjustcolor(sim.col, alpha.f = sim.alpha)

    for (i in seq_len(nsim)) {
      if (sim.type == "lines") {
        lines(fval, sim_res_list[[i]], col = sim_color, lwd = 1)

      } else {
        points(fval, sim_res_list[[i]], pch = sim.pch, col = sim_color,
               cex = size * 0.8)
      }
    }
  }

  # Observed Points (Residual Panel)

  points(fval, res_sort, pch = pch, col = p.col, bg = p.fill, cex = size)

  if (q == TRUE) {
    sq <- par("usr")
    rect(xleft = b.val[1], xright = b.val[2], ybottom = sq[3], ytop = sq[4],
         col = rgb(0, 0, 0, 0.05), border = NA)

    rect(xleft = sq[1], xright = sq[2], ybottom = qy[1], ytop = qy[2],
         col = rgb(0, 0, 0, 0.05), border = NA)
  }

  if (show.par == TRUE) {
    params <- paste0("p=", p)
    mtext(side = 3, text = params, adj = 1, cex = 0.65)
  }

  invisible(data.frame(fval, Fit_minus_residuals = model_sort, Residuals = res_sort))
}
