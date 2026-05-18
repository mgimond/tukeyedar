#' @export
#' @title
#'  Shuffle one or more columns in a data frame.
#'
#' @description
#'  Permutes the values within one or more specified columns of a data frame,
#'  leaving all other columns unchanged.
#'
#' @param df A data frame.
#' @param ... One or more unquoted column names to be shuffled.
#' @param replace A logical value indicating whether to sample with replacement.
#'   Defaults to `FALSE` (permutation).
#'
#' @return
#' A data frame with the same dimensions as the input `df`, but with the
#' values in the specified columns permuted.
#'
#' @details
#' This function shuffles each specified column independently. It is a convenient
#' wrapper around `sample()` for use in permutation tests or for generating
#' randomized datasets. If a shuffled column is a factor, the original factor
#' levels and their order are preserved in the output.
#'
#' @examples
#' # Load the yarn dataset
#' data(yarn)
#'
#' # Shuffle a single column ('Load')
#' shuffled_df1 <- eda_shuffle(yarn, Load)
#'
#' # Shuffle two columns independently ('Load' and 'Amplitude')
#' shuffled_df2 <- eda_shuffle(yarn, Load, Amplitude)
#'
#' # Shuffle a column with replacement (bootstrap sampling)
#' shuffled_df3 <- eda_shuffle(yarn, Cycles, replace = TRUE)
#'
eda_shuffle<- function(df, ..., replace = FALSE) {

  # Capture the ... arguments as a list of expressions
  cols_to_shuffle_expr <- substitute(list(...))[-1]

  # Convert expressions to a character vector of column names
  col_names <- sapply(cols_to_shuffle_expr, deparse)

  # Check if any columns were provided
  if (length(col_names) == 0) {
    warning("No columns specified to shuffle. Returning original data frame.")
    return(df)
  }

  # Iterate over the column names and shuffle each one
  for (col_name in col_names) {
    # Validate that the column exists
    if (!col_name %in% names(df)) {
      stop(paste0("Column '", col_name, "' not found in the data frame."))
    }

    # Check if the column is a factor and preserve levels if it is
    if (is.factor(df[[col_name]])) {
      original_levels <- levels(df[[col_name]])
      shuffled_values <- sample(df[[col_name]], replace = replace)
      df[[col_name]] <- factor(shuffled_values, levels = original_levels)
    } else {
      # Shuffle the column
      df[[col_name]] <- sample(df[[col_name]], replace = replace)
    }
  }

  # Return the modified data frame
  return(df)
}
