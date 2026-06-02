#' @export
#' @title Plot method for \code{eda_mean_sweep} objects
#'
#' @description
#' Generates decomposition plots or diagnostic plots from an object of class
#' \code{"eda_mean_sweep"}.
#'
#' @param x An object of class \code{eda_mean_sweep}
#' @param plot A character string specifying the type of plot.
#'   \describe{
#'     \item{\code{"effects"}}{(default) Plots the raw centered effects.}
#'     \item{\code{"ms"}}{Scales effects by \code{sqrt(N / df)} to reflect their
#'       relative contribution to variance.}
#'     \item{\code{"diagnostic"}}{Generates a diagnostic plot of residuals versus
#'       comparison values to check for non-additivity (interactions). This is
#'       typically used on a model with main effects only.}
#'   }
#' @param reg Logical. If \code{TRUE} (the default), fits a linear regression
#'   line to the diagnostic plot. This is disabled when \code{margin = "all"}.
#' @param margin Character string. Only used when \code{plot = "diagnostic"}.
#'   Specifies which interaction to diagnose. Can be the name of a two-way
#'   interaction (e.g., \code{"FactorA:FactorB"}) or \code{"all"} (the default)
#'   to overlay diagnostics for all two-way interactions.
#' @param legend Logical. If \code{TRUE}, a legend is added when \code{margin = "all"}.
#' @param legend.pos The position of the legend, e.g., \code{"bottomright"}.
#' @param legend.inset The amount of inset for the legend from the plot border.
#' @param ... Additional arguments passed to the internal plotting function. See
#'   \code{\link{.eda_plot_vardecomp}} for the "effects" plot or
#'   \code{\link{.eda_plot_xy}} for the "diagnostic" plot (e.g., `loe`, `sd`).
#'
#' @return A plot visualizing residuals and factor effects.
#'
#' @details
#' This plot method can generate two types of plots:
#'
#' \strong{1. Variability Decomposition Plot (\code{plot = "effects"} or \code{"ms"})} \cr
#' This plot, handled by \code{\link{.eda_plot_vardecomp}}, visualizes the
#' additive overlays: the residuals and the centered main and interaction effects.
#'
#' \strong{2. Diagnostic Plot (\code{plot = "diagnostic"})} \cr
#' This plot is a key tool from Exploratory Data Analysis for assessing if an
#' additive model is sufficient. It plots the residuals from the model against a
#' set of "comparison values". For a two-way model, the comparison value is: \cr
#' \code{(row effect) * (column effect) / (grand mean)} \cr
#' A sloping trend in this plot suggests a hidden interaction.
#'
#' @references
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @seealso \code{\link{eda_mean_sweep}}, \code{\link{.eda_plot_vardecomp}}, \code{\link{.eda_plot_xy}}
#' @examples
#'
#' # A default plot
#' M0 <- eda_mean_sweep(feav5_12, Weight, Level, Type, max_order = 2)
#' plot(M0)
#'
#' # Adding labels
#' plot(M0, label = TRUE)
#'
#' # Options are available for dot plots when tes are present. By default, points
#' # are stacked. Other options include "jitter",
#' plot(M0, overlap = "jitter")
#'
#' # ... or "overplot" (you can modify the point transparency via the "alpha" argument)
#' plot(M0, overlap = "overplot")
#'
#' # Plot can be rotated
#' plot(M0, rotate = TRUE)
#'
#' # Original response variable can be added as a boxplot
#' plot(M0, show.resp = TRUE)
#'
#' # If "mean squares" are to be compared, the effects need to be adjusted
#' # by setting plot = "ms" (see page 174 of the referenced source)
#' plot(M0, plot = "ms")
#'
#' # Generating diagnostic plots
#' M1 <- eda_mean_sweep(yarn, Cycles, Load, Length, Amplitude)
#'
#' # Create an overlay of all two-way diagnostic plots
#' # Note: For mean-sweep, this plots Residuals vs. CV for each interaction
#' plot(M1, plot = "diagnostic", margin = "all")
#'
#' # Create a diagnostic plot for a specific interaction
#' plot(M1, plot = "diagnostic", margin = "Load:Length", reg = TRUE)

plot.eda_mean_sweep <- function(x, plot = "effects", reg = TRUE, margin = NULL,
                                 legend = TRUE, legend.pos = "bottomright",
                                 legend.inset = 0.03, ...) {

  # Checks
  if (!inherits(x, "eda_mean_sweep"))
    stop("Input object must be of class eda_mean_sweep.
")
  if (!plot %in% c("effects", "ms", "diagnostic")) {
    stop("Invalid 'plot' argument. Choose 'effects', 'ms', or 'diagnostic'.")
  }

  if (plot == "effects") {
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = x$effects, ...)
  } else if (plot == "ms") {
    anova_table <- eda_anova_table(x)
    adjusted_effects <- list()
    n <- nrow(x$long)
    for (effect_name in names(x$effects)) {
      original_effect_values <- x$effects[[effect_name]]
      f_stat_row <- anova_table[anova_table$Effect == effect_name, ]
      df  <- f_stat_row$df
      adjusted_effects[[effect_name]] <- original_effect_values * (n / df)^0.5
    }
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = adjusted_effects, ...)
  } else if (plot == "diagnostic") {
    eff_names <- names(x$effects)
    if (length(eff_names) < 2) {
      stop("Diagnostic plot requires a model with at least two factors.")
    }

    # Handle the 'margin' argument
    if (is.null(margin)) {
      if (length(eff_names) == 2) {
        margin <- paste(eff_names, collapse=":")
      } else {
        # Default to 'all' if margin is not specified for >2 factors
        margin <- "all"
        message("Defaulting to 'margin = \"all\"'. Specify a margin like 'Factor1:Factor2' for a single plot.")
      }
    }

    gm <- x$global
    if (abs(gm) < .Machine$double.eps) {
      stop("Grand mean is too close to zero to create comparison values.")
    }
    residuals <- x$long$residuals

    if (margin == "all") {
      if (reg) {
        message("For 'margin = \"all\"', regression lines are disabled to avoid confusion.")
        reg <- FALSE
      }

      interaction_margins <- utils::combn(eff_names, 2, FUN = function(x) paste(x, collapse=":"))

      all_cvs <- c()
      all_residuals <- c()
      all_groups <- c()

      for (int_margin in interaction_margins) {
        margin_factors <- strsplit(int_margin, ":")[[1]]
        row_eff_name <- margin_factors[1]
        col_eff_name <- margin_factors[2]

        row_effects <- x$effects[[row_eff_name]]
        col_effects <- x$effects[[col_eff_name]]

        obs_row_effects <- row_effects[as.character(x$long[[row_eff_name]])]
        obs_col_effects <- col_effects[as.character(x$long[[col_eff_name]])]

        cv_vals <- (obs_row_effects * obs_col_effects) / gm

        valid_idx <- !(is.na(cv_vals) | is.na(residuals))
        all_cvs <- c(all_cvs, cv_vals[valid_idx])
        all_residuals <- c(all_residuals, residuals[valid_idx])
        all_groups <- c(all_groups, rep(int_margin, sum(valid_idx)))
      }

      if (length(all_cvs) == 0) {
        message("No valid data points to plot for 'margin = \"all\"'.")
        return(invisible(NULL))
      }

      dots <- list(...)
      unique_groups <- unique(all_groups)
      num_groups <- length(unique_groups)

      user_alpha <- if ("alpha" %in% names(dots)) dots$alpha else 0.8
      palette_cols <- grDevices::hcl.colors(num_groups, palette = "Dark 3")
      palette_cols_alpha <- adjustcolor(palette_cols, alpha.f = user_alpha)
      palette_pch <- (1:num_groups)

      group_factor <- factor(all_groups, levels = unique_groups)
      colors_for_plot <- palette_cols_alpha[group_factor]
      pch_for_plot <- palette_pch[group_factor]

      dots <- dots[setdiff(names(dots), c("p.col", "p.fill", "pch", "alpha"))]

      plot_data <- data.frame(y = all_residuals, cv = all_cvs)
      arg_list <- c(list(plot_data, x = quote(cv), y = quote(y),
                         xlab = "Comparison Value", ylab = "Residuals",
                         mean.l = FALSE, hline = 0, vline = 0, sd = FALSE,
                         p.col = colors_for_plot, p.fill = colors_for_plot,
                         pch = pch_for_plot, reg = FALSE, loe = FALSE, alpha = NULL),
                    dots)
      do.call(.eda_plot_xy, arg_list)

      if (legend) {
        legend(legend.pos, legend = unique_groups, col = palette_cols,
               pch = palette_pch, bg = "white", cex = 0.8, inset = legend.inset)
      }

    } else { # Logic for a single margin
      margin_factors <- strsplit(margin, ":")[[1]]
      if (length(margin_factors) != 2 || !all(margin_factors %in% eff_names)) {
        stop("Invalid 'margin'. Use format 'FactorA:FactorB' with valid factor names.")
      }

      row_eff_name <- margin_factors[1]
      col_eff_name <- margin_factors[2]
      row_effects <- x$effects[[row_eff_name]]
      col_effects <- x$effects[[col_eff_name]]

      obs_row_effects <- row_effects[as.character(x$long[[row_eff_name]])]
      obs_col_effects <- col_effects[as.character(x$long[[col_eff_name]])]

      cv <- (obs_row_effects * obs_col_effects) / gm

      plot_data <- data.frame(y = residuals, cv = cv)

      valid_idx <- !(is.na(plot_data$y) | is.na(plot_data$cv) | is.infinite(plot_data$y) | is.infinite(plot_data$cv))
      plot_data <- plot_data[valid_idx, ]

      if (nrow(plot_data) == 0) {
        message("No valid data points to generate a diagnostic plot.")
        return(invisible(NULL))
      }

      dots <- list(...)
      # Handle common graphical parameters, providing defaults if not specified
      if(!"sd" %in% names(dots)) sd <- FALSE else sd <- dots$sd
      if(!"mean.l" %in% names(dots)) mean.l <- FALSE else mean.l <- dots$mean.l
      if(!"loe" %in% names(dots)) loe <- FALSE else loe <- dots$loe
      dots <- dots[setdiff(names(dots), c("sd", "mean.l", "loe"))]

      arg_list <- c(list(plot_data, x = quote(cv), y = quote(y),
                         xlab = paste("Comparison Value for", margin), ylab = "Residuals",
                         reg = reg, sd = sd, mean.l = mean.l, loe = loe,
                         hline = 0, vline = 0),
                    dots)

      do.call(.eda_plot_xy, arg_list)
    }
  }
}
