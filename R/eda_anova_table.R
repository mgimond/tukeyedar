#' Generate an ANOVA Table from \code{eda_mean_sweep} output
#'
#' This function generates an analysis-of-variance (ANOVA) table from the output
#' of the \code{eda_mean_sweep()} function. It calculates sums of squares (SS),
#' degrees of freedom (df), mean squares (MS), F-statistics, and p-values for
#' each decomposed effect.
#'
#' @param sweep_result A list object returned by \code{eda_mean_sweep()}, containing
#'   the global mean, effects, residuals, and long-form data.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{Effect}{Name of the factor or interaction term}
#'   \item{SS}{Sum of squares for the effect}
#'   \item{df}{Degrees of freedom for the effect}
#'   \item{MS}{Mean square for the effect}
#'   \item{F}{F-statistic comparing the effect MS to the residual MS}
#'   \item{p}{p-value from the F-test}
#' }
#'
#' @details This function assumes that each effect in the \code{effects} list of the
#' mean sweep output corresponds to a centered vector of fitted values. The residuals
#' are used to compute the residual mean square. 
#'
#' @examples
#' # Example of a 3-way table with nested factors
#' M0 <- eda_mean_sweep(feav1_5, votes, State, Year, Grouping, 
#'                      nesting = c("Grouping", "State")) 
#' eda_anova_table(M0)
#' 
#' # Example of 3-way table with two way interactions
#' M0 <- eda_mean_sweep(feav6_8, Hard, Dentist, Method, Alloy, Temp, 
#'                      max_order = 2)
#' eda_anova_table(M0)                     

#' @export

eda_anova_table <- function(sweep_result) {
  effects <- sweep_result$effects
  residuals <- sweep_result$residuals
  grand_mean <- sweep_result$global
  total_n <- length(residuals)
  original_data <- sweep_result$long 
  
  # Common term
  common_ss <- total_n * grand_mean^2
  common_df <- 1
  
  rows <- list()
  total_effect_df <- 0 
  total_effect_ss <- 0 
  
  for (name in names(effects)) {
    effect_values <- effects[[name]] # The named vector of centered effects for this term
    k_levels_in_effect <- length(effect_values) # Number of unique levels/combinations for this effect
    
    df_current_term <- 0 
    
    # Check if it's an interaction effect by looking for ":" in the name
    if (grepl(":", name)) {
      constituent_factors <- strsplit(name, ":") 
      constituent_factors <- unlist(constituent_factors)
      # Calculate interaction DF as the product of (levels - 1) for each constituent factor
      df_interaction_product <- 1
      for (f_name in constituent_factors) {
        # Get the number of levels for the original factor from the data frame
        num_levels_factor <- nlevels(original_data[[f_name]])
        df_interaction_product <- df_interaction_product * (num_levels_factor - 1)
      }
      df_current_term <- df_interaction_product
    } else {
      # It's a main effect (single factor)
      df_current_term <- k_levels_in_effect - 1
    }
    
    # Calculate Sum of Squares (SS) and Mean Squares (MS) for the current term
    # Replications here means the number of observations contributing to each effect value
    replications_per_effect_level <- total_n / k_levels_in_effect 
    ss <- replications_per_effect_level * sum(effect_values^2)
    
    # Handle cases where DF might be 0 to prevent division by zero for MS
    if (df_current_term > 0) {
      ms <- ss / df_current_term
    } else {
      ms <- NA # Mean square is undefined if DF is 0
    }
    
    total_effect_df <- total_effect_df + df_current_term # Accumulate DFs
    total_effect_ss <- total_effect_ss + ss
    
    rows[[name]] <- data.frame(
      Effect = name,
      SS = ss,
      df = df_current_term,
      MS = ms,
      F = NA, # F and p values will be filled later
      p = NA
    )
  }
  
  # Calculate Residual Degrees of Freedom
  # This is total observations minus DF for common mean and all effects
  residual_df <- total_n - common_df - total_effect_df 
  
  # Ensure residual_df is not negative (can happen with complex or problematic models)
  if (residual_df < 0) {
    warning("Calculated residual degrees of freedom is negative. This might indicate an issue with the model or data.")
    residual_df <- 0 # Set to 0 to prevent errors in pf(), though F/p values will be meaningless
  }
  
  residual_ss <- sum(residuals^2)
  
  # Calculate Residual Mean Square
  if (residual_df > 0) {
    residual_ms <- residual_ss / residual_df
  } else {
    residual_ms <- 0 # No residual MS if no residual DF, or cannot compute F/p
    warning("Residual degrees of freedom is zero. F-statistics and p-values for effects cannot be computed.")
  }
  
  # Add F-statistics and p-values for all effects
  for (i in seq_along(rows)) {
    if (residual_ms > 0 && !is.na(rows[[i]]$MS) && rows[[i]]$df > 0) {
      rows[[i]]$F <- rows[[i]]$MS / residual_ms
      rows[[i]]$p <- pf(rows[[i]]$F, rows[[i]]$df, residual_df, lower.tail = FALSE)
    } else {
      rows[[i]]$F <- NA
      rows[[i]]$p <- NA
    }
  }
  
  # Combine all rows into the final ANOVA table
  anova_table <- do.call(rbind, rows)
  
  # Add common and residual rows at the appropriate positions
  anova_table <- rbind(
    data.frame(Effect = "Common", SS = common_ss, df = common_df, MS = common_ss, F = NA, p = NA),
    anova_table,
    data.frame(Effect = "Residual", SS = residual_ss, df = residual_df, MS = residual_ms, F = NA, p = NA)
  )
  row.names(anova_table) <- NULL
  return(anova_table)
}