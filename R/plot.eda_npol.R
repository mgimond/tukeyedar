#' @title Plot method for eda_npol objects
#'
#' @description Generates either a variability decomposition plot of factor
#'   effects or a diagnostic plot for an object of class \code{eda_npol}.
#'
#' @param x An object of class \code{eda_npol}.
#' @param plot A character string specifying the type of plot to generate.
#'   \describe{
#'     \item{\code{"effects"} (default)}{Generates a variability decomposition
#'                             plot showing residuals and factor effects.}
#'     \item{\code{"diagnostic"}}{Generates a diagnostic plot. Its behavior is
#'                                controlled by the \code{margin} argument.}
#'   }
#' @param reg Logical. If \code{TRUE}, fits a linear regression line to the
#'   diagnostic plot. Only used when \code{plot = "diagnostic"}. This is disabled
#'   when \code{margin = "all"}. Defaults to \code{FALSE}.
#' @param margin Character string. Only used when \code{plot = "diagnostic"}. Specifies which
#'   diagnostic plot to generate. Can be the name of a two-way interaction
#'   (e.g., \code{"Load:Length"}), \code{"residuals"} (the default), or \code{"all"}
#'   to overlay all two-way interaction diagnostics.
#' @param legend Logical. If \code{TRUE}, legend is added to plot when \code{margin = "all"}.
#' @param legend.pos The position of the legend when \code{margin = "all"}. Can be
#' \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"},
#' \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"} and \code{"center"}.
#' Defaults to \code{"topright"}.
#' @param legend.inset The amount of inset for the legend from the plot border
#'   when \code{margin = "all"}. Defaults to \code{0.03}.
#' @param ... Additional arguments passed to internal plotting functions.
#'
#' @details
#'   This method generates two types of plots for \code{eda_npol} objects:
#'
#'   \strong{1. Variability Decomposition Plot (\code{plot = "effects"})} \cr
#'   Visualizes residuals and the spread of all fitted effects.
#'   Calls \code{\link{.eda_plot_vardecomp}}.
#'
#'   \strong{2. Diagnostic Plot (\code{plot = "diagnostic"})} \cr
#'   When used with an object from \code{eda_npol}, this plot's behavior is
#'   controlled by the \code{margin} argument.
#'   Calls \code{\link{.eda_plot_xy}}.
#'   \itemize{
#'     \item \code{margin = "residuals"}: Plots final residuals against their
#'       n-way comparison values to diagnose higher-order non-additivity.
#'     \item \code{margin = "FactorA:FactorB"}: Plots the fitted two-way
#'       interaction effects against their specific comparison values.
#'     \item \code{margin = "all"}: Overlays the diagnostic plots for all
#'       two-way interactions onto a single graph, with each interaction
#'       represented by a different color and symbol.
#'   }
#'   For a main-effect only model, this generates a single
#'   diagnostic plot of residuals versus a composite comparison value.
#'
#' @return A plot is generated on the active graphics device. The function
#'   itself returns \code{NULL}.
#'
#' @seealso \code{\link{eda_npol}}, \code{\link{.eda_plot_vardecomp}}, \code{\link{.eda_plot_xy}}
#'
#' @examples
#'
#' # Main effect median polish (i.e. no interaction)
#' M1 <- eda_npol(yarn, Cycles, Load, Length, Amplitude)
#' plot(M1) # Plot effect values and residuals
#' plot(M1, plot = "diagnostic") # Plot residuals vs comparison value
#'
#' # Full effect median polish (i.e. include two-way interactions)
#' M2 <- eda_npol(yarn, Cycles, Load, Length, Amplitude, max_order = 2)
#' plot(M2, plot = "diagnostic") # Plot residuals vs higher-order CV
#'
#' # Overlay all two-way interaction diagnostics
#' plot(M2, plot = "diagnostic", margin = "all")
#'
#' # Generate the diagnostic plot for a specific two-way interaction
#' plot(M2, plot = "diagnostic", margin = "Load:Length", reg = TRUE)
#'
#' # Generate side-by-side diagnostic plots for all two-way interactions
#' numplots <- length(M2$cv) - 1
#' nameplots <- names(M2$cv)[-(numplots+1)]
#' nc <- ceiling(sqrt(numplots))      # number of columns
#' nr <- ceiling(numplots / nc)       # number of row
#' OP <- par(mfrow=c(nr,nc))
#' invisible(sapply(nameplots, \(x) plot(M2, plot="diagnostic", margin = x, reg=TRUE)))
#' par(OP)
#'
#' @export
#'
plot.eda_npol <- function(x, plot = "effects", reg = TRUE, margin = "residuals",
                           legend = TRUE, legend.pos = "bottomright",
                           legend.inset = 0.03, ...) {

  # Checks
  if (!inherits(x, "eda_npol")) stop("Input object must be of class 'eda_npol'")
  if (!plot %in% c("effects", "diagnostic")) {
    stop("Invalid 'plot' argument. Choose 'effects' or 'diagnostic'.")
  }

  if (plot == "diagnostic") {

    # Check if the detailed 'cv' list from eda_npol exists.
    if (!is.null(x$cv) && length(x$cv) > 0) {
      # --- New logic for objects with detailed CVs ---

      available_margins <- c(names(x$cv), "all")
      if (!margin %in% available_margins) {
        stop("Invalid 'margin' provided. Available margins are: ", paste(sQuote(available_margins), collapse = ", "))
      }

      if (margin == "all") {
        # Create point symbol description vector if legend is to be printed to console
        pch_description <- c(
          "open circle", "open triangle up", "plus", "cross", "open diamond",
          "open triangle down", "square cross", "star", "diamond cross",
          "circle plus", "triangles up/down", "square plus", "circle cross", "square + triangle",
          "filled square", "filled circle", "filled triangle up", "filled diamond",
          "solid circle", "small circle",
          "filled circle (bordered)", "filled square (bordered)",
          "filled diamond (bordered)", "filled triangle up (bordered)",
          "filled triangle down (bordered)")

        # Prevent regression from being added
        if (reg) {
          message("For 'margin = \"all\"', regression lines are disabled to avoid confusion.")
        }

        interaction_margins <- names(x$cv)[names(x$cv) != "residuals"]
        if (length(interaction_margins) == 0) {
          message("No two-way interaction CVs found to plot.")
          return(invisible(NULL))
        }

        all_cvs <- c()
        all_effects <- c()
        all_groups <- c()

        for (int_margin in interaction_margins) {
          cv_vals <- x$cv[[int_margin]]
          margin_vars <- strsplit(int_margin, ":")[[1]]
          keys <- interaction(x$long[, margin_vars, drop = FALSE], drop = TRUE, sep = ":")
          effect_vals <- x$effects[[int_margin]][as.character(keys)]

          valid_idx <- !(is.na(cv_vals) | is.na(effect_vals))
          all_cvs <- c(all_cvs, cv_vals[valid_idx])
          all_effects <- c(all_effects, effect_vals[valid_idx])
          all_groups <- c(all_groups, rep(int_margin, sum(valid_idx)))
        }

        if (length(all_cvs) == 0) {
          message("No valid data points to plot for 'margin = \"all\"'.")
          return(invisible(NULL))
        }

        dots <- list(...)
        unique_groups <- unique(all_groups)
        num_groups <- length(unique_groups)

        # Pre-apply alpha transparency to avoid bug in .eda_plot_xy
        user_alpha <- if ("alpha" %in% names(dots)) dots$alpha else 0.8
        palette_cols <- grDevices::hcl.colors(num_groups, palette = "Dark 3")
        palette_cols_alpha <- adjustcolor(palette_cols, alpha.f = user_alpha)

        palette_pch <- (1:num_groups) %% 25

        group_factor <- factor(all_groups, levels = unique_groups)
        colors_for_plot <- palette_cols_alpha[group_factor]
        pch_for_plot <- palette_pch[group_factor]

        dots <- dots[setdiff(names(dots), c("p.col", "p.fill", "pch", "alpha"))]

        xlim_all <- range(all_cvs, na.rm = TRUE, finite = TRUE)
        ylim_all <- range(all_effects, na.rm = TRUE, finite = TRUE)

        x2 <- data.frame(y = all_effects, cv = all_cvs)
        arg_list <- c(list(x2, x = quote(cv), y = quote(y),
                           xlab = "Comparison Value", ylab = "Effect",
                           mean.l = FALSE, hline = 0, vline = 0,
                           xlim = xlim_all, ylim = ylim_all, sd = FALSE,
                           p.col = colors_for_plot, p.fill = colors_for_plot,
                           pch = pch_for_plot, reg = FALSE, loe = FALSE, alpha = NULL),
                      dots)
        do.call(.eda_plot_xy, arg_list)

        if (legend){
          legend(legend.pos, legend = unique_groups, col = palette_cols, border = colors_for_plot,
                 pch = palette_pch, bg = "white", cex = 0.6, inset = legend.inset,
                 xpd = NA)
        } else {
          print(data.frame(Groups=unique_groups,Symbol=pch_description[palette_pch],
                           Color = palette_cols) )
        }


      } else {
        # --- Logic for single margins ---
        cv_values <- x$cv[[margin]]
        xlab_str <- paste("CV for", margin)

        if (margin == "residuals") {
          y_values <- x$long$residuals
          ylab_str <- "Residuals"
        } else {
          if (!margin %in% names(x$effects)) {
            stop("Internal error: CV margin '", margin, "' not found in effects list.")
          }
          margin_vars <- strsplit(margin, ":")[[1]]
          keys <- interaction(x$long[, margin_vars, drop = FALSE], drop = TRUE, sep = ":")
          y_values <- x$effects[[margin]][as.character(keys)]
          ylab_str <- paste("Effect for", margin)
        }

        valid_idx <- !(is.na(cv_values) | is.na(y_values) | is.infinite(cv_values) | is.infinite(y_values))
        cv_values <- cv_values[valid_idx]
        y_values <- y_values[valid_idx]

        if (length(cv_values) == 0) {
          message("No valid data points to plot for margin '", margin, "'.")
          return(invisible(NULL))
        }

        dots <- list(...)
        if(!"sd" %in% names(dots)) sd <- FALSE else sd <- dots$sd
        if(!"mean.l" %in% names(dots)) mean.l <- FALSE else mean.l <- dots$mean.l
        if(!"loe" %in% names(dots)) loe <- FALSE else loe <- dots$loe
        if(!"loe.col" %in% names(dots)) loe.col <- rgb(0, 0, 1, 0.7) else loe.col <- dots$loe.col
        if(!"span" %in% names(dots)) span <- 0.5 else span <- dots$span
        dots <- dots[setdiff(names(dots), c("sd", "mean.l", "loe", "loe.col", "span"))]

        x2 <- data.frame(y = y_values, cv = cv_values)
        arg_list <- c(list(x2, x = quote(cv), y = quote(y),
                           xlab = xlab_str, ylab = ylab_str, reg = reg,
                           mean.l = mean.l, sd = sd, loe = loe, px =1 , py =1,
                           raw_tick = FALSE, show.par = FALSE,
                           loess.d = list(family = "symmetric", span = span),
                           loe.col = loe.col, loe.lw = 1.5,  hline = 0, vline = 0),
                      dots)
        do.call(.eda_plot_xy, arg_list)
      }
    } else {
      # --- Fallback to original logic for backward compatibility ---

      if (!"cv" %in% names(x$long)) {
        stop("Diagnostic plot requires 'cv' values. Please run eda_npol() for detailed diagnostics.")
      }
      cv_values <- x$long$cv
      residuals <- x$long$residuals

      valid_idx <- !(is.na(cv_values) | is.na(residuals) | is.infinite(cv_values) | is.infinite(residuals))
      cv_values <- cv_values[valid_idx]
      residuals <- residuals[valid_idx]

      if (length(cv_values) == 0) {
        message("No valid data points to plot diagnostic plot after removing NA/Inf CV or residuals.")
        return(invisible(NULL))
      }

      dots <- list(...)
      if(!"sd" %in% names(dots)) sd <- FALSE else sd <- dots$sd
      if(!"mean.l" %in% names(dots)) mean.l <- FALSE else mean.l <- dots$mean.l
      if(!"loe" %in% names(dots)) loe <- TRUE else loe <- dots$loe
      if(!"loe.col" %in% names(dots)) loe.col <- rgb(0, 0, 1, 0.7) else loe.col <- dots$loe.col
      if(!"span" %in% names(dots)) span <- 0.5 else span <- dots$span
      dots <- dots[setdiff(names(dots), c("sd", "mean.l", "loe", "loe.col", "span"))]

      if (sum(is.finite(cv_values)) > 1) {
        x2 <- data.frame(Residuals = residuals, cv = cv_values)
        arg_list <- c(list(x2, x = quote(cv), y = quote(Residuals),
                           xlab = "Comparison Value", reg = reg,
                           mean.l = mean.l, sd = sd, loe = loe, px =1 , py =1,
                           raw_tick = FALSE, show.par = FALSE,
                           loess.d = list(family = "symmetric", span = span),
                           loe.col = loe.col, loe.lw = 1.5,  hline = 0, vline = 0),
                      dots)
        do.call(.eda_plot_xy, arg_list)
      } else {
        return("CV values are not finite")
      }
    }
  } else if (plot == "effects") {
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = x$effects, ...)
  }
}
