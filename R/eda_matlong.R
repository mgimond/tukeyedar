#' @title Convert 2-way data tables Between Long and Matrix Formats
#'
#' @description Reshapes a 2-way data table from long format to a matrix, 
#' or a matrix back to a data frame in long format.
#'
#' @param data A data frame in long format (for \code{direction = "long_to_matrix"})
#'   or a matrix (for \code{direction = "matrix_to_long"}).
#' @param row A \strong{bare (unquoted)} variable name from \code{data} (or a desired
#'   column name for the output) representing the row identifiers in the matrix.
#' @param col A \strong{bare (unquoted)} variable name from \code{data} (or a desired
#'   column name for the output) representing the column identifiers in the
#'   matrix.
#' @param response A \strong{bare (unquoted)} variable name from \code{data} (or a
#'   desired column name for the output) representing the values in the matrix.
#' @param direction A character string specifying the direction of the
#'   conversion. Must be one of \code{"long_to_matrix"} or \code{"matrix_to_long"}.
#'   Defaults to \code{"long_to_matrix"}.
#'
#' @return If \code{direction = "long_to_matrix"}, returns a matrix. If \code{direction =
#'   "matrix_to_long"}, returns a data frame in long format.
#'
#' @examples
#' eda_matlong(edtts2.12, col = year, row = institution, response = perc)
#' @export

eda_matlong <- function(data, row, col, response,
                        direction = c("long_to_matrix", "matrix_to_long")) {
  # Define direction of conversion
  direction <- match.arg(direction)
  
  # Capture un-evaluated expressions
  row_field_quo   <- substitute(row)
  col_field_quo   <- substitute(col)
  value_field_quo <- substitute(response)
  
  # Convert to character column names
  row <- as.character(row_field_quo)
  col <- as.character(col_field_quo)
  response <- as.character(value_field_quo)
  
  if (direction == "long_to_matrix") {
    if (!all(c(row, col, response) %in% names(data))) {
      stop("One or more field names are not present in the data.")
    }
    
    mat <- tapply(data[[response]],
                  list(data[[row]], data[[col]]),
                  mean)  # Or other summary function
    return(mat)
    
  } else if (direction == "matrix_to_long") {
    if (!is.matrix(data)) {
      stop("For matrix_to_long, 'data' must be a matrix.")
    }
    
    df_long <- as.data.frame(as.table(data), stringsAsFactors = FALSE)
    names(df_long) <- c(row, col, response)
    return(df_long)
  }
}


