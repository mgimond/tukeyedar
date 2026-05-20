#' @title N-way Median Polish with Interaction Support
#'
#' @description Apply median polish to a multiway table to extract common, main,
#' and interactive effects.
#'
#' @param dat A data frame in long form containing the response and factor
#'   variables.
#' @param response The response variable (must be numeric).
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
#' @details This function implements the "overlay approach" to data
#' decomposition. The response is modeled as a sum of components: a common
#' value, main effects for each factor, and interactive overlays for factor
#' combinations up to \code{max_order}. Each overlay replicates the dimensions
#' of the entire layout but isolates a specific structural component (e.g., the
#' "FactorA:FactorB" interaction).
#'
#' In unreplicated tables (one observation per cell), setting \code{max_order}
#' to the total number of factors will result in zero residuals, as all
#' variation is swept into the highest-order interaction.
#'
#' If replicates are present (i.e. more than one response value per unique
#' combination of factor levels), the values are combined into a single value
#' using the \code{stat} function).
#'
#' @return A list of class \code{eda_npol} containing: \item{global}{The
#' estimated common effect.} \item{response}{Response column name.}
#' \item{effects}{A nested list of effects, where names correspond to margins
#'       (e.g., "A", "A:B").}
#' \item{long}{A dataframe including residuals, comparison values (cv), and
#' fits.} \item{converged}{Boolean indicating if convergence was reached.}
#' \item{iter}{The number of iterations performed.} \item{fitted_values}{The sum
#' of common and all extracted margin effects.} \item{power}{The power
#' transformation applied.}
#'
#' @examples
#' # Example 1:
#' M0 <- eda_npol(yarn, Cycles, Load, Length, Amplitude)
#'
#' # Extract global and factor effects from model
#' M0$global
#' M0$effects
#'
#' # Visualize the data decomposition
#' plot(M0)
#'
#' # Generate a diagnostic plot (used to assess interaction effects)
#' plot(M0, plot = "diagnostic", robust = T)
#'
#' # Re-express response variable by applying the log transformation
#' # Apply a base 10 log transformation
#' M1 <- eda_npol(yarn, Cycles, Load, Length, Amplitude, p = 0, base = 10)
#' plot(M1)
#' plot(M1, plot = "diagnostic", robust = T)
#'
#' # Example 2:
#' # Example of a 3-way table with missing values
#' # Note that the function returns a Warning with the number of
#' # missing combinations (e.g. 14 out of 120)
#' M0 <- eda_npol(logan, delay, am_pm, carrier, month, maxiter = 30)
#' plot(M0)
#' plot(M0, plot = "diagnostic")
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

  ### Comparison Value (cv) ----
  # Calculates pairwise products of main effects.
  cv <- rep(NA_real_, nrow(dat))
  if (abs(common_effect) > .Machine$double.eps && n_factors > 1) {
    main_eff_names <- factors_chr
    cv <- apply(dat[main_eff_names], 1, function(row_vals) {
      effs <- vapply(seq_along(main_eff_names), function(i) {
        factor_effects[[main_eff_names[i]]][as.character(row_vals[i])]
      }, numeric(1))
      sum(combn(effs, 2, prod)) / common_effect
    })
  }

  long_data <- cbind(dat, residuals = residuals, cv = cv, fit = fitted_values)

  result <- list(
    global = common_effect,
    response = response_chr,
    effects = factor_effects,
    long = long_data,
    converged = converged,
    iter = iter,
    fitted_values = fitted_values,
    power = p
  )
  class(result) <- "eda_npol"
  return(result)
}


