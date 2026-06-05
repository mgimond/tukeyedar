#' @title N-way Median Polish with Interaction Support
#'
#' @description Apply median polish to a multi-way table to extract common, main,
#' and interactive effects.
#'
#' @param dat A data frame in long form containing the response and factor
#'   variables.
#' @param response The response variable (must be numeric). Note that nesting is
#' not currently supported.
#' @param ... Unquoted factor variable names.
#' @param max_order The maximum number of factors to combine for interaction
#'   effects. Defaults to 1 (main effects only).
#' @param maxiter Maximum number of iterations for the median polish algorithm.
#' @param tolerance Convergence threshold for median polish adjustments.
#' @param stat Location statistic to use. Defaults to \code{median}. While
#'   flexible, resistant statistics like \code{median} are strongly recommended
#'   to align with Exploratory Data Analysis (EDA) principles. Using \code{mean}
#'   transforms the decomposition into a standard ANOVA-like fit.
#' @param p Re-expression power parameter.
#' @param tukey Boolean determining if Tukey's power transformation should be
#'   used.
#' @param base Base used with the \code{log()} function if \code{p = 0}.
#'
#' @details This function implements the "sweeping approach" to data
#' decomposition. The response is modeled as a sum of components: a common
#' value, main effects for each factor, and interactive overlays for factor
#' combinations up to \code{max_order}.
#'
#' In unreplicated tables (one observation per cell), setting \code{max_order}
#' to the total number of factors will result in zero residuals as all
#' variation is swept into the highest-order interaction.
#'
#' If replicates are present (i.e. more than one response value per unique
#' combination of factor levels), the values are combined into a single value
#' using the function defined by the \code{stat} argument).
#'
#' The Comparison Value (cv) generated in the \code{long} component of the output
#' is computed differently depending on whether the model is run in main-effect
#' mode (i.e. \code{max_order = 1}) or in full-effect mode
#' (i.e. \code{max_order > 1}).
#'
#' In \bold{main-effect} mode, the \bold{cv} column represents the
#' \bold{composite comparison value}. This value is used to diagnose
#' nonadditivity that is embedded within the residuals when interactions have
#' not been explicitly separated.
#'
#' \deqn{
#' CV_{composite} = \frac{\sum_{1 \le i < j \le n} \hat{a}_i \hat{a}_j}{m}
#' }
#'
#' where: \eqn{n} is the number of factors, \eqn{m} is the estimated common value,
#' \eqn{\hat{a}_i} and \eqn{\hat{a}_j} are the estimated main effects for the
#' specific levels of factors  \eqn{i} and  \eqn{j}.
#'
#' In \bold{full-effect} mode, the two-factor interactions have already been
#' swept out into their own overlays. Therefore, the \bold{cv} column in \code{long}
#' represents the product of all main effects divided by the common term  \eqn{m}
#' raised to the power of \eqn{(n−1)}. For a model with \eqn{n} factors, this
#' gives us:
#'
#' \deqn{
#' CV_{residual} = \frac{\prod_{i=1}^{n} \hat{a}_i}{m^{n-1}}
#' }
#'
#'  For a standard 3-factor layout (factors \eqn{a}, \eqn{b}, and \eqn{c}),
#'  the equation simplifies to the triple-product formula,
#'
#' \deqn{
#' CV_{ABC} = \frac{\hat{a}_i \hat{b}_j \hat{c}_k}{m^2}
#' }
#'
#' where \eqn{\hat{a}_i}, \eqn{\hat{b}_j}, \eqn{\hat{c}_k}  represent
#' the main effects for each factor.
#'
#' @return A list of class \code{eda_npol} containing:
#' \item{global}{The estimated common effect.}
#' \item{response}{Response column name.}
#' \item{effects}{A nested list of effects, where names correspond to margins
#'       (e.g., "A", "A:B").}
#' \item{long}{A dataframe including residuals, a composite comparison value (cv) for backward compatibility, and fits.}
#' \item{cv}{A list containing detailed comparison values. Names correspond to
#' the interaction margin (e.g., "A:B") or "residuals" for the n-way residual CV.
#' \bold{This component is empty} if a main-effect model is run (i.e. \code{max_order = 1})}
#'
#' \item{converged}{Boolean indicating if convergence was reached.}
#' \item{iter}{The number of iterations performed.}
#' \item{fitted_values}{The sum of common and all extracted margin effects.}
#' \item{power}{The power transformation applied.}
#'
#' @examples
#' # Main effect median polish (i.e. no interaction)
#' M1 <- eda_npol(yarn, Cycles, Load, Length, Amplitude)
#' plot(M1) # Plot effect values and residuals
#' plot(M1,  plot = "diagnostic") # Plot residuals vs comparison value
#'
#' # Full effect median polish (i.e. include two-way interactions)
#' M2 <- eda_npol(yarn, Cycles, Load, Length, Amplitude, max_order = 2)
#' plot(M2, plot = "diagnostic") # Plot residuals vs higher-order CV
#'
#' # Overlay all two-way interaction diagnostics
#' plot(M2, plot = "diagnostic", margin = "all")
#'
#' # Generate the diagnostic plot for a specific two-way interaction
#' plot(M2, plot = "diagnostic", margin = "Load:Length")
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
#' @seealso
#' \link{eda_pol} for an implementation of the median polish
#'  on a two-way (two factor) table and,
#' \link{eda_mean_sweep} for a sweeping implementation using the mean
#' instead of the median.
#'
#' @references
#' Cook, N. R. (1985). Three-Way Analyses. In D. C. Hoaglin, F. Mosteller,
#'   & J. W. Tukey (Eds.), Exploring Data Tables, Trends, and Shapes (pp. 125-188).
#'   New York: Wiley.
#'
#' Emerson, J. D., & Wong, G. Y. (1983). Resistant Nonadditive Fits for Two-Way
#'   Tables. In D. C. Hoaglin, F. Mosteller, & J. W. Tukey (Eds.), Understanding
#'   Robust and Exploratory Data Analysis (pp. 67-124). New York: Wiley.
#'
#' @export
eda_npol <- function(dat, response, ..., max_order = 1, maxiter = 20,
                      tolerance = 1e-6, stat = median, p = 1,
                      tukey = FALSE, base = exp(1)) {


  ### Capture Arguments ----
  response_expr <- substitute(response)
  response_chr <- deparse(response_expr)
  factor_exprs <- substitute(list(...))[-1]
  factors_chr <- vapply(factor_exprs, deparse, character(1))

  ### Validate input ----
  if (!is.data.frame(dat)) stop("Input 'dat' must be a data frame.")
  n_factors <- length(factors_chr)
  if (max_order > n_factors) {
    stop("max_order cannot exceed the total number of factors.")
  }

  ### Check for duplicate combinations (Replication Check) ----
  is_unreplicated <- TRUE
  if (n_factors > 0) {
    combination_key <- interaction(dat[factors_chr], drop = TRUE)
    if (any(duplicated(combination_key))) {
      is_unreplicated <- FALSE
    }
  }

  ### Check for missing combinations ----
  if (n_factors > 0) {
    all_levels <- lapply(dat[factors_chr], function(x) unique(as.character(x)))
    all_combos <- expand.grid(all_levels, stringsAsFactors = FALSE)
    total_combinations <- nrow(all_combos)

    # Use a more robust way to create keys that handles different column orders
    observed_keys <- apply(dat[factors_chr], 1, paste, collapse = "||")
    possible_keys <- apply(all_combos[factors_chr], 1, paste, collapse = "||")

    missing_keys_bool <- !(possible_keys %in% observed_keys)

    if (any(missing_keys_bool)) {
      num_missing <- sum(missing_keys_bool)
      warning(paste("Input data is incomplete. Missing values for",
                    num_missing, "out of", total_combinations,
                    "possible combinations of factors. Analysis proceeds on available data."))
    }
  }

  ### Warning for confounded interactions ----
  if (is_unreplicated && max_order == n_factors && n_factors > 1) {
    warning("max_order equals the number of factors in an unreplicated table. ",
            "Residuals will be swept to zero, confounding interactions with error.")
  }

  ### Initialization ----
  y <- dat[[response_chr]]
  if (p != 1L) {
    y <- eda_re(y, p = p, tukey = tukey, base = base)
    dat[[response_chr]] <- y
  }

  residuals <- y
  common_effect <- 0
  factor_effects <- list()

  # Interaction Generation: Pre-calculate margin combinations (orders 1 to max_order)
  all_margins <- list()
  for (m in 1:max_order) {
    combos <- combn(factors_chr, m, simplify = FALSE)
    for (combo in combos) {
      margin_name <- paste(combo, collapse = ":")
      all_margins[[margin_name]] <- combo

      # Initialize effect vectors for unique interaction levels
      keys_init <- interaction(dat[combo], drop = TRUE, sep = ":")
      levs <- levels(keys_init)
      factor_effects[[margin_name]] <- setNames(rep(0, length(levs)), levs)
    }
  }

  ### Iterative Polish ----
  converged <- FALSE
  if (maxiter > 0 && nrow(dat) > 0 && n_factors > 0) {
    for (iter in 1:maxiter) {
      residuals_prev <- residuals

      # Iterate over all generated margin combinations (The "Sweeping" Loop)
      for (margin_name in names(all_margins)) {
        margin_vars <- all_margins[[margin_name]]
        keys <- interaction(dat[margin_vars], drop = TRUE, sep = ":")

        # Extract location statistics (e.g., medians) from current residuals
        delta_effects <- tapply(residuals, keys, stat, na.rm = TRUE)
        delta_effects[is.na(delta_effects)] <- 0

        # Update residuals and cumulative margin effects
        residuals <- residuals - delta_effects[as.character(keys)]
        factor_effects[[margin_name]] <- factor_effects[[margin_name]] +
          delta_effects[names(factor_effects[[margin_name]])]

        # Center the interactive effect: sweep out the common term
        centering_step <- stat(factor_effects[[margin_name]], na.rm = TRUE)
        factor_effects[[margin_name]] <- factor_effects[[margin_name]] - centering_step
        common_effect <- common_effect + centering_step
      }

      if (max(abs(residuals - residuals_prev), na.rm = TRUE) < tolerance) {
        converged <- TRUE
        break
      }
    }
  }

  ### Fitted Values ----
  fitted_values <- rep(common_effect, nrow(dat))
  for (margin_name in names(all_margins)) {
    keys <- interaction(dat[all_margins[[margin_name]]], drop = TRUE, sep = ":")
    fitted_values <- fitted_values + factor_effects[[margin_name]][as.character(keys)]
  }

  ### Individual Comparison Value (CV) Calculations ----
  cv_list <- list()
  if (max_order >= 2) {
    if (abs(common_effect) > .Machine$double.eps) {
      # Get main effects for each row as a list of vectors
      main_effects_per_row <- lapply(factors_chr, function(fac) {
        effect_values <- factor_effects[[fac]]
        factor_levels_in_data <- as.character(dat[[fac]])
        effect_values[factor_levels_in_data]
      })
      names(main_effects_per_row) <- factors_chr

      # 1. Calculate CVs for all 2-way interactions
      if (n_factors >= 2) {
        two_way_combos <- combn(factors_chr, 2, simplify = FALSE)
        for (combo in two_way_combos) {
          factor1 <- combo[1]
          factor2 <- combo[2]
          margin_name <- paste(factor1, factor2, sep = ":")

          eff1_vec <- main_effects_per_row[[factor1]]
          eff2_vec <- main_effects_per_row[[factor2]]

          cv_vec <- (eff1_vec * eff2_vec) / common_effect

          # Construct explicit interaction names for the vector
          levels1 <- as.character(dat[[factor1]])
          levels2 <- as.character(dat[[factor2]])
          interaction_names <- paste(levels1, levels2, sep = ":")
          names(cv_vec) <- interaction_names

          cv_list[[margin_name]] <- cv_vec
        }
      }

      # 2. Calculate CV for the n-way residual
      if (n_factors >= 2) { # Formula is general for n>=2
        product_of_main_effects <- Reduce("*", main_effects_per_row)
        denominator <- common_effect^(n_factors - 1)

        if (abs(denominator) > .Machine$double.eps) {
          cv_vec_nway <- product_of_main_effects / denominator

          # Construct explicit n-way interaction names for the vector
          all_levels_per_row <- lapply(factors_chr, function(fac) as.character(dat[[fac]]))
          nway_interaction_names <- Reduce(function(x, y) paste(x, y, sep=":"), all_levels_per_row)
          names(cv_vec_nway) <- nway_interaction_names

          cv_list[["residuals"]] <- cv_vec_nway
        }
      }
    }
  }

  ### Comparison Value (cv) ----
  # Two outcomes based on max_order value (i.e. main-effect
  # vs. full-effect)
  cv <- rep(NA_real_, nrow(dat))
  if (abs(common_effect) > .Machine$double.eps && n_factors > 1) {
    main_eff_names <- factors_chr
    # Main-effect model run
    if(max_order == 1){
      cv <- apply(dat[main_eff_names], 1, function(row_vals) {
        effs <- vapply(seq_along(main_eff_names), function(i) {
          factor_effects[[main_eff_names[i]]][as.character(row_vals[i])]
        }, numeric(1))
        sum(combn(effs, 2, prod)) / common_effect
      })
      # Full-effect model run
    } else{
      cv <- cv_list$residuals
    }
  }

  long_data <- cbind(dat, residuals = residuals, cv = cv, fit = fitted_values)

  result <- list(
    global = common_effect,
    response = response_chr,
    effects = factor_effects,
    long = long_data,
    cv = cv_list,
    converged = converged,
    iter = iter,
    fitted_values = fitted_values,
    power = p
  )
  class(result) <- "eda_npol"
  return(result)
}
