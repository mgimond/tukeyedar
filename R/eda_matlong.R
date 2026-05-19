#' @export
#' @title Convert N-way data tables Between Long and Array Formats
#'
#' @description Reshapes an N-way data table from a long format data frame to a
#'   multi-dimensional array, or an array back to a long format data frame.
#'
#' @param data A data frame in long format (for `direction = "to_array"`)
#'   or a multi-dimensional array/matrix (for `direction = "to_long"`).
#' @param response A bare (unquoted) variable name representing the response/value
#'   column.
#' @param ... One or more bare (unquoted) variable names representing the
#'   factor/dimension columns.
#' @param direction A character string specifying the conversion direction. Must
#'   be one of `"to_array"` or `"to_long"`.
#' @param fun The function to apply when aggregating data in the `to_array`
#'   conversion, in case of duplicate entries. Defaults to `mean`.
#'
#' @return If `direction = "to_array"`, returns a multi-dimensional array.
#'   If `direction = "to_long"`, returns a data frame in long format.
#'
#' @details
#' This function generalizes the concept of reshaping for multi-way tables.
#'
#' For `to_array`, it uses `tapply` to create an array where the dimensions
#' are defined by the factor columns specified in `...` and the cell values are
#' determined by the `response` column.
#'
#' For `to_long`, it uses `as.data.frame(as.table(...))` to reshape the array
#' into a long data frame and renames the columns in the order in which the
#' column names are passed to the function. It's important that the order
#' of the column names passed to the function match the order the factor
#' elements are stored in the array. You can find that order with the command
#' \code{names(dimnames(array_name))}.
#'
#' @examples
#' # Long to 2D array (matrix)
#' dl2 <- eda_matlong(inf_mort, perc, region, edu)
#' dl2
#'
#' # 2D array to long
#' # The column names passed to the function must be in the same order
#' # that the factor elements are listed in the array
#' names(dimnames(dl2))
#' eda_matlong(dl2, perc, region, edu, direction = "to_long")
#'
#' # Long to 3D array
#' dl3 <- eda_matlong(yarn, Cycles, Length, Amplitude, Load)
#' dl3
#'
#' # 3D array to long
#' names(dimnames(dl3)) # Get the order of factor elements
#' eda_matlong(dl3, Cycles, Length, Amplitude, Load, direction = "to_long")
#'
eda_matlong <- function(data, response, ...,
                         direction = c("to_array", "to_long"),
                         fun = mean) {
  # Match the direction argument
  direction <- match.arg(direction)

  # Capture the unquoted variable names
  response_name <- deparse(substitute(response))
  factor_exprs  <- substitute(list(...))[-1]
  factor_names  <- sapply(factor_exprs, deparse)

  if (direction == "to_array") {
    # --- Convert from Long Data Frame to N-dimensional Array ---

    # Check if input is a data frame
    if (!is.data.frame(data)) {
      stop("For 'to_array', 'data' must be a data frame.")
    }

    # Check if all specified columns exist in the data frame
    all_cols <- c(response_name, factor_names)
    if (!all(all_cols %in% names(data))) {
      missing_cols <- all_cols[!all_cols %in% names(data)]
      stop(paste("The following columns were not found in the data frame:",
                 paste(missing_cols, collapse = ", ")))
    }

    # Check for replicates
    repl <- any(duplicated(data[factor_names]))
    if(repl == TRUE)
      warning("Some factor combinations were encountered more than once.")

    # Use tapply to create the multi-dimensional array
    # The list of factors is created from the data frame columns
    index_list <- data[factor_names]
    array_out <- tapply(data[[response_name]], index_list, fun)

    return(array_out)

  } else { # direction == "to_long"
    # --- Convert from N-dimensional Array to Long Data Frame ---

    # Check if input is an array or matrix
    if (!is.array(data)) {
      stop("For 'to_long', 'data' must be an array or a matrix.")
    }

    # Melt the array into a long data frame
    df_long <- as.data.frame(as.table(data), stringsAsFactors = FALSE)
    names(df_long)[ncol(df_long)] <- "Freq" # Temporarily name the value column

    # The number of factor columns created by as.table
    num_factors_in_df <- ncol(df_long) - 1

    # Check if the number of provided factor names matches the array dimensions
    if (length(factor_names) != num_factors_in_df) {
      stop(paste("The number of factor names provided (", length(factor_names),
                 ") does not match the number of dimensions of the array (", num_factors_in_df, ")."))
    }

    # Rename the columns
    names(df_long) <- c(factor_names, response_name)

    # Convert factor columns back to their original type if possible
    # This part is a bit tricky, but we can try to infer types. For now, they are characters.
    # A more robust solution might involve attributes, but this is good for now.

    return(df_long)
  }
}
