#' @title 
#' N-way median polish
#' 
#' @description
#' Apply median polish to a multi-way table
#'
#'
#' @param dat A data frame in long form containing the response and factor variables.
#' @param response The response variable (must be numeric).
#' @param ... Unquoted factor variable names.
#' @param maxiter Maximum number of iterations for the median polish algorithm.
#' @param tolerance Convergence threshold for median polish adjustments. 
#' @param stat Location statistic to use. Defaults to \code{median}.
#' @param p Re-expression power parameter.
#' @param tukey Boolean determining if Tukey's power transformation should be used.
#' If \code{FALSE} (the default), the Box-Cox transformation is adopted.
#' @param base Base used with the \code{log()} function if \code{p = 0}.

#'
#' @details
#' This function applies the median polish algorithm to a dataset containing a
#' response variable and multiple factor variables. It iteratively adjusts
#' factor effects to reveal common patterns in the data. It estimates the 
#' \strong{common effect}, \strong{factor effects}, and \strong{residuals}. 
#' The function does not accept tables with replicates (i.e. where each combination
#' of factor level may have more than one response variable). If the data contains 
#' replicates, the values should be summarized for each combination of factor 
#' levels prior to running \code{eda_npol}. \cr\cr
#'
#' Note on missing combinations: Median polish can proceed with incomplete
#' data tables by operating on available values. This function
#' includes a check to warn the user if some combinations of factor levels
#' are not present in the input data. The polishing algorithm 
#' handles these missing combinations by only using the data that are provided. 
#' \cr\cr
#'
#' \code{eda_npol} iterates through each factor within a single polish step, removing 
#' medians from residuals for each level of that factor, and centering the 
#' factor's effects by removing their median (the median-of-medians extracted 
#' from the residuals for that factor), which is then added to the common effect.
#' This differs from \code{eda_pol}'s implementation where the centering of 
#' row and column effects, and adding to the global effect, happens after the 
#' medians for the current factor have been removed from the residuals. This 
#' explains the slight differences in output between the two. Neither approach
#' is better than the other, but if the dataset is a two-way table, it is 
#' recommended to use the \code{eda_pol} function given its richer set of 
#' features. \cr \cr
#' 
#' If the algorithm does not converge within \code{maxiter}, a warning is issued.
#'
#' @return
#' A list of class \code{eda_npol} containing:
#' 
#' \item{global}{The estimated common (global) effect.}
#' \item{response}{Response column name from dataframe input.}
#' \item{effects}{A named list of main effects.}
#' \item{long}{A dataframe of estimated factor values, residuals, comparison
#' values (cv) and fitted values.}
#' \item{converged}{Boolean indicating if convergence was reached.}
#' \item{iter}{The number of iterations performed.}
#' \item{fitted_values}{The fitted values (sum of common effect and factor effects) 
#'       for the observed data points.}
#' \item{power}{The power transformation applied (if any).}
#'
#' @references
#'   \itemize{
#'      \item{Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985). 
#'            Exploring data tables, trends, and shapes.}
#'      \item{Tukey, John W. (1977). Exploratory Data Analysis. Addison-Wesley.}
#'      \item{Emerson, John D., and David C. Hoaglin. (1983). Understanding 
#'            Robust and Exploratory Data Analysis. John Wiley & Sons.}
#'     }  
#' 
#' @seealso
#'   \code{\link{eda_pol}}, \code{\link{plot.eda_npol}} 
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
#' plot(M0, plot = "diagnostic") 
#' 
#' # Re-express response variable by applying the log transformation 
#' # Apply a base 10 log transformation
#' M1 <- eda_npol(yarn, Cycles, Load, Length, Amplitude, p = 0, base = 10)
#' plot(M1)
#' plot(M1, plot = "diagnostic")
#'
#' # Example 2: 
#' # Example of a 3-way table with missing values
#' # Note that the function returns a Warning with the number of 
#' # missing combinations (e.g. 14 out of 120)
#' M0 <- eda_npol(logan, delay, am_pm, carrier, month, maxiter = 30)
#' plot(M0)
#' plot(M0, plot = "diagnostic")
#' @export
#' 
eda_npol <- function(dat, response, ..., maxiter = 20, tolerance = 1e-6,
                     stat = median, p = 1, tukey = FALSE, base = exp(1)) {
  
  # Capture Arguments ----
  response_expr <- substitute(response)
  response_chr <- deparse(response_expr)
  factor_exprs <- substitute(list(...))[-1]
  factors_chr <- vapply(factor_exprs, deparse, character(1))
  
  # Validate input ----
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a data frame in long form.")
  }
  if (!(response_chr %in% names(dat))) {
    stop(paste("Response column '", response_chr, "' not found in dat.", sep = ""))
  }
  if (!all(factors_chr %in% names(dat))) {
    missing_factors <- factors_chr[!(factors_chr %in% names(dat))]
    stop(paste("Factor(s) '", paste(missing_factors, collapse = ", "), "' not found in dat.", sep = ""))
  }
  
  # Check for duplicate combinations in input data ----
  if (length(factors_chr) > 0) {
    combination_key <- interaction(dat[factors_chr], drop = TRUE)
    duplicate_flag <- duplicated(combination_key)
    if ( sum(duplicate_flag) > 0 ){ 
      stop("At least one combination of effects occurs more than once in the input table. Each combination should appear at most once for a standard median polish input.")
    }
  }
  
  # --- CHECK FOR MISSING COMBINATIONS ---
  # Generate all possible combinations from unique levels
  if (length(factors_chr) > 0) {
    all_levels <- lapply(dat[factors_chr], unique)
    all_combos <- expand.grid(all_levels, stringsAsFactors = FALSE)
    total_combinations <- nrow(all_combos)
    all_combo_key <- interaction(all_combos, drop = TRUE)
    
    # Generate keys for observed data
    observed_combo_key <- interaction(dat[factors_chr], drop = TRUE)
    
    # Find missing keys and corresponding combinations
    missing_keys <- setdiff(all_combo_key, observed_combo_key)
    
    missing_combos_df <- all_combos[all_combo_key %in% missing_keys, ]
    
    if (length(missing_keys) > 0) {
      warning(paste("Input data is incomplete. Missing values for",
                    length(missing_keys), "out of", total_combinations,
                    "possible combinations of factors. Analysis proceeds on available data."))
    }
  } else {
    # Case with no factors, just polishing a single batch 
    total_combinations <- nrow(dat) 
    missing_combos_df <- data.frame() 
  }
  # --- END CHECK FOR MISSING COMBINATIONS ---
  # Check if response variable is numeric
  if (!is.numeric(dat[[response_chr]])) {
    stop(paste("Response variable '", response_chr, "' must be numeric.", sep = ""))
  }
  

  # Initialization ----
  # Sort data to group identical factor combinations together for consistent iteration
  # Sorting helps ensure that the 'idx' indices capture the correct rows for each level
  if (length(factors_chr) > 0) {
    dat <- dat[do.call(order, dat[factors_chr]), ]
  }
  
  y <- dat[[response_chr]]
  
  # Apply re-expression if needed
  if (p != 1L){
    y <- eda_re(y, p = p, tukey = tukey, base = base)
    dat[[response_chr]] <- y
  }
  
  # Initialize residuals with the re-expressed response 
  residuals <- y # Initialize residuals with re-expressed response 
  common_effect <- 0
  factor_effects <- list()
  
  # Initialize factor effects to zero for all unique levels found in the data 
  for (f_col in factors_chr) {
    levels <- unique(dat[[f_col]])
    factor_effects[[f_col]] <- setNames(rep(0, length(levels)), as.character(levels))
  }
  
  # Iterative Polish ----
  converged <- FALSE
  if (maxiter > 0 && nrow(dat) > 0 && length(factors_chr) > 0) { # Ensure there's data and factors to polish
    for (iter in 1:maxiter) {
      residuals_prev <- residuals # Store residuals from previous iteration 
      
      # Iterate through factors (columns) 
      for (f_col in factors_chr) {
        levels <- unique(dat[[f_col]])
        medians_this_factor <- numeric(length(levels))
        names(medians_this_factor) <- as.character(levels)
        
        # Calculate median for each level within the current factor 
        for (i in seq_along(levels)) {
          level <- levels[i]
          # Get indices in the *original data frame* corresponding to this level 
          idx <- which(dat[[f_col]] == level)
          
          # Calculate the median (or other stat) of the residuals for these specific rows
          # na.rm = TRUE handles NAs if they were present in the original response data 
          level_median <- stat(residuals[idx], na.rm = TRUE)
          
          # Subtract the median from the residuals for all rows belonging to this level
          residuals[idx] <- residuals[idx] - level_median
          
          # Store the median that was subtracted for this level
          medians_this_factor[i] <- level_median
        }
        
        # Adjust factor effects and common effect
        # The sum of medians removed for each factor should be zeroed out and added to common
        median_of_medians <- stat(medians_this_factor, na.rm = TRUE)
        
        # Update the effect for each level of this factor
        # Subtract the median of medians from each level's median to center effects around zero
        factor_effects[[f_col]] <- factor_effects[[f_col]] + (medians_this_factor - median_of_medians)
        
        # Add the overall median of medians to the common effect
        common_effect <- common_effect + median_of_medians
      }
      
      # Check for convergence based on sum of absolute differences in residuals
      if (sum(abs(residuals - residuals_prev), na.rm = TRUE) < tolerance) {
        converged <- TRUE
        break
      }
    }
  } else {
    # Handle cases with no factors or no data for polishing loop to run
    iter <- 0
    if (nrow(dat) == 0) warning("Input data frame is empty.")
    if (length(factors_chr) == 0) warning("No factors specified for median polish.")
  }
  
  
  if (!converged && maxiter > 0) {
    warning(paste("Polishing routine did not converge within", maxiter, "iterations."))
  }
  
  # Calculate fitted values for the *observed* data points
  fitted_values <- common_effect # Start with the common effect
  # Add the appropriate factor effects for each observed data point
  for (f_col in factors_chr) {
    # Use match to get the effect corresponding to each data row's factor level
    fitted_values <- fitted_values + factor_effects[[f_col]][match(dat[[f_col]], names(factor_effects[[f_col]]))]
  }
  
  # Compute the comparison value (cv) for each row in the data ----
  # This calculates a composite cv based on the sum of pairwise products
  # of the factor effects for the factor level combination in each specific row,
  # divided by the common effect.
  
  cv <- numeric(nrow(dat)) # Initialize a vector to store cv for each row
  
  if (abs(common_effect) < .Machine$double.eps){
    warning("Common effect is zero. Comparison Value (cv) cannot be computed and will be set to NA for all rows.")
    cv <- NA_real_ 
  } else {
    # Iterate through each row of the original data to get its specific factor levels
    for (row_idx in 1:nrow(dat)) {
      effects_this_row <- numeric(length(factors_chr))
      # Get the specific effect value for each factor for this row
      for (i in seq_along(factors_chr)) {
        f_col <- factors_chr[i]
        level <- as.character(dat[[f_col]][row_idx])
        # Look up the final effect value for this factor and level
        effects_this_row[i] <- factor_effects[[f_col]][level]
      }
      
      # Calculate sum of pairwise products for the effects in this row
      sum_of_products <- 0
      n_factors <- length(effects_this_row)
      # Need at least two factors to have pairwise products
      if (n_factors >= 2) {
        combinations_indices <- combn(1:n_factors, 2)
        for (i in 1:ncol(combinations_indices)) {
          idx1 <- combinations_indices[1, i]
          idx2 <- combinations_indices[2, i]
          sum_of_products <- sum_of_products + (effects_this_row[idx1] * effects_this_row[idx2])
        }
      }

      # Calculate the cv for this row
      cv[row_idx] <- sum_of_products / common_effect
    }
  }
  
  # Add cv to the long data frame
  # 'dat' is the sorted version of the original input data used in the polish.
  long_data <- cbind(dat, residuals = residuals, cv = cv, fit = fitted_values)
  
  # Prepare output structure
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
