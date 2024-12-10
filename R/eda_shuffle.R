#' @export
#' @title Shuffle Values Across Groups
#' @description
#' This function shuffles the values in a specified column of a data frame across
#' all groups.
#'
#' @param df A data frame.
#' @param x The name of the column containing the values to be shuffled.
#' @param grp The name of the column containing the grouping factor.
#' @param replace_x A logical value indicating whether to sample values with
#'        replacement  (default: FALSE).
#' @param replace_grp A logical value indicating whether to preserve the factor
#'        levels of the grouping column (default: TRUE).
#' @param ... Not used
#'
#' @return
#' A data frame with the shuffled values and the original group assignments.
#' The column names of the output data frame will match the input column names.
#'
#' @details
#' Shuffling/permuting values across groups has, for effect, sampling from a same
#' distribution within each group level. This simulates a process where differences
#' between group levels are solely a function of random noise. \cr \cr
#' By default, the group distribution does not change. If you wish to allow the
#' group distribution to change (i.e. having a different number of group levels),
#' set \code{replace_grp = TRUE}. But, note that for small datasets, this may
#' result in empty group levels.
#'
#'
#' @examples
#' # Original data
#' eda_boxls(mtcars, hp, cyl, reorder = FALSE)
#'
#' # Permute values across cyl groups
#' df1 <- eda_shuffle(mtcars, hp, cyl)
#' eda_boxls(df1, hp, cyl)
#'
#' # Is the variability explained by cyl due to chance alone or is it
#' # systematic?
#' OP <- par(mfrow = c(2,2))
#'  eda_boxls(mtcars, hp, cyl, reorder = FALSE)
#'  title("Original", col.main = "darkred")
#'  eda_boxls(eda_shuffle(mtcars, hp, cyl), hp, cyl, reorder = FALSE)
#'  eda_boxls(eda_shuffle(mtcars, hp, cyl), hp, cyl, reorder = FALSE)
#'  eda_boxls(eda_shuffle(mtcars, hp, cyl), hp, cyl, reorder = FALSE)
#' par(OP)

eda_shuffle  <- function(df, x, grp, replace_x = FALSE, replace_grp = FALSE, ...){

  # Check for invalid arguments
  input <- names(list(...))
  check <- input %in% names(formals(cat))
  if (any(!check)) warning(sprintf("%s is not a valid argument.",
                                   paste(input[!check], collapse = ", ")))

  # Get column names as character strings
  x_name <- deparse(substitute(x))
  grp_name <- deparse(substitute(grp))

  # Extract relevant columns
  x <- df[[x_name]]
  groups <- df[[grp_name]]

  # Shuffle x values across all groups with or without replacement
  shuffled_X <- sample(x, size = length(x), replace = replace_x)
  shuffled_grp <- sample(groups, size = length(x), replace = replace_grp)

  # Create a new data frame with shuffled x values
  shuffled_df <- data.frame(x = shuffled_X, group = shuffled_grp)

  # Rename columns to match input
  names(shuffled_df) <- c(x_name, grp_name)

  # Preserve factor levels if requested
  if (is.factor(df[[grp_name]])) {
    shuffled_df[[grp_name]] <- factor(shuffled_df[[grp_name]],
                                      levels = levels(df[[grp_name]]))
  }
  return(shuffled_df)
}
