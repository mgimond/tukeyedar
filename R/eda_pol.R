#' @export
#' @title Median polish of two-way tables
#'
#' @description \code{eda_pol} Polishes two-way tables using median, means, or
#'   any customizable functions.
#'
#' @param x A three column data frame.
#' @param row Name of column assigned to the row effect.
#' @param col Name of column assigned to the column effect.
#' @param val Name of column assigned to the response variable.
#' @param stat Polishing statistic (default is median).
#' @param plot Boolean determining if an output plot should be generated.
#' @param eps  Convergence tolerance parameter.
#' @param maxiter Maximum number of iterations.
#' @param sort Boolean determining if the effects row/columns should be sorted.
#' @param p Re-expression power parameter.
#' @param tukey Boolean determining if Tukey's power transformation should used.
#'   If FALSE, the Box-Cox transformation is adopted.
#' @param base Base used with the log() function if \code{p} is \code{0}.
#' @param offset Offset to add to values if at leat one value is 0 and the power
#'   is negative.
#' @param col.quant Boolean determining if a quantile classification scheme
#'   should be used.
#' @param colpal Color palette to adopt.
#' @param adj.mar Boolean determining if margin width needs to accomodate labels.
#' @param res.size Size of residual values in plot \code{[0-1]}.
#' @param row.size Size of row effect values in plot \code{[0-1]}.
#' @param col.size Size of column effect values in plot \code{[0-1]}.
#' @param res.txt Boolean determining if values should be added to plot.
#' @param label.txt Boolean determining if margin and column labels should be
#'   plotted.
#' @param ... Not used
#'
#' @returns A list of class \code{eda_polish} with the following named
#'   components:
#'
#' \itemize{
#' \item \code{input} The median polish residuals with three columns: Column levels,
#'   row levels and residual values.
#' \item \code{wide} The median polish residuals table in wide form.
#' \item \code{row}  Row effects table
#' \item \code{col}  Column effects table
#' \item \code{global}  Overall value (common value)
#' \item \code{iter}  Number of iterations before polish stabilizes.
#' \item \code{long}  Table of residuals, row effects, column effects and CV values in long form.
#' \item \code{power}  Transformation power applied to values prior to polishing.
#' \item \code{IQ_row}  Ratio between interquartile row effect values and 80th quantile of residuals.
#' \item \code{IQ_col}  Ratio between interquartile column effect values and 80th quantile of residuals.}
#'
#' @details The function performs a polish on a two way table. By default, it
#'   applies a median polish, but other statistical summaries such
#'   as the mean can be passed to the function via the \code{stat = } argument.
#'   The function returns a list of row/column effects along with global and
#'   residual values. It will also generate a colored table if \code{plot =
#'   TRUE}. \cr
#'   For an N-way table with more than two factors, see \code{\link{eda_npol}}.
#'
#' @references
#'
#' \itemize{
#'   \item Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985). 
#' Exploring data tables, trends, and shapes.
#'   \item Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley
#'   \item \href{../articles/polish.html}{Median polish article}}
#'
#' @examples
#' M <- eda_pol(inf_mort, row = region, col = edu, val = perc)


eda_pol <- function (x, row= NULL, col = NULL, val = NULL, stat = median, plot = TRUE,
                     eps = 0.01, maxiter = 5, sort = FALSE, p = 1, tukey = FALSE,
                     offset = 0.00001, col.quant = FALSE, colpal = "RdYlBu",
                     adj.mar = TRUE, res.size = 1, row.size = 1, col.size = 1,
                     res.txt = TRUE, label.txt = TRUE, base = exp(1), ...){
  
  
  if(!missing(x))
  {
    row <- deparse(substitute(row))
    col <- deparse(substitute(col))
    val <- deparse(substitute(val))
  }
  
  if(is.data.frame(x)){
    # Check that table is properly formatted ----
    if(dim(x)[2] < 3) stop("The data frame must be in long form with at least 3 columns.")
    if(is.null(row)) stop("You need to define the row name column.")
    if(is.null(col)) stop("You need to define the column name column.")
    if(is.null(val)) stop("You need to define the response values column.")
  } else {
    stop("Table must be a three column dataframe.")
  }
  
  
  x2 <- data.frame(x[[col]], x[[row]], x[[val]])
  colnames(x2) <- c(col, row, val)
  
  if( (any(x2[[val]] < 0)) & (p < 1 )) {
    stop(paste("You are applying a power of ",p," to negative values."))
  }
  
  if( (any(x2[[val]] == 0)) & (p < 0 )) {
    x2[[val]] <-  x2[[val]] + offset
    print("Warning: zeros were found in the data. Given a negative power, an offset was added")
    print("This may lead to large residuals")
  }
  
  
  # Check re-expression is desired
  if(p !=1){
    x2[[val]] <- eda_re(x2[[val]], p = p, tukey = tukey, base = base)
  }
  
  # Keep copy of orignal values
  x0 <- x2[3]
  
  # Get values column ----
  # val <- colnames(x)[-which(colnames(x) %in% c(row, col))]
  # val <- substitute(val)
  
  # row.eff <-  aggregate(val ~ eval(row), data = x2, FUN = \(x) 0)
  row.eff <-  aggregate(x2[[val]] ~ x2[[row]], FUN = \(x) 0)
  col.eff <-  aggregate(x2[[val]] ~ x2[[col]], FUN = \(x) 0)
  colnames(row.eff) <- c(row, val)
  colnames(col.eff) <- c(col, val)
  
  # Get initial global effect and residuals ----
  # If maxiter == 0, output original data table
  if(maxiter == 0) {
    global <- 0
    row.eff[[val]] <- 0
    col.eff[[val]] <- 0
    x2 <- as.data.frame(x2)
    i <- 0
  } else {
    global <- stat(x2[[val]], na.rm = TRUE)
    x2[[val]] <- x2[[val]] - global
    res.sum1 <- 0
    
    # Proceed with the polish routine ----
    for (i in seq_len(maxiter)){
      
       # Step 1: Calculate row medians from current residuals
       row_medians_calc <- aggregate(x2[[val]] ~ x2[[row]], FUN = stat)
       # Extract values and names (levels) correctly from the aggregate output
       row_levels_order <- row_medians_calc[[1]] 
       row_medians_val <- row_medians_calc[[2]]  
       
       # Step 2: Update row effects (add row medians)
       match_idx_row_eff <- match(row_levels_order, row.eff[[row]])
       row.eff[[val]][match_idx_row_eff] <- row.eff[[val]][match_idx_row_eff] + row_medians_val
       
       # Step 3: Update global effect (add median of current column effects)
       global <- global + stat(col.eff[[val]], na.rm = TRUE)
       
       # Step 4: Center column effects (subtract median from col.eff)
       col_eff_median <- stat(col.eff[[val]], na.rm = TRUE)
       col.eff[[val]] <- col.eff[[val]] - col_eff_median
       
       # Step 5: Subtract row medians from residuals
       # Create a named vector for easy lookup by row level
       row_medians_named <- setNames(row_medians_val, as.character(row_levels_order)) 
       # Subtract the appropriate row median for each row in x2
       # Use as.character for robust lookup, especially if row names are purely numeric
       x2[[val]] <- x2[[val]] - row_medians_named[as.character(x2[[row]])]
       
       # Step 6: Calculate column medians from new residuals
       col_medians_calc <- aggregate(x2[[val]] ~ x2[[col]], FUN = stat)
       # Extract values and names (levels) correctly from the aggregate output
       col_levels_order <- col_medians_calc[[1]] # CORRECTED: Access first column (levels)
       col_medians_val <- col_medians_calc[[2]]  # CORRECTED: Access second column (calculated values)
       
       # Step 7: Update column effects (add column medians)
       # Use match to correctly add to the potentially reordered col.eff
       match_idx_col_eff <- match(col_levels_order, col.eff[[col]])
       col.eff[[val]][match_idx_col_eff] <- col.eff[[val]][match_idx_col_eff] + col_medians_val
       
       # Step 8: Update global effect (add median of current row effects)
       global <- global + stat(row.eff[[val]], na.rm = TRUE)
       
       # Step 9: Center row effects (subtract median from row.eff)
       row_eff_median <- stat(row.eff[[val]], na.rm = TRUE)
       row.eff[[val]] <- row.eff[[val]] - row_eff_median
       
       # Step 10: Subtract column medians from residuals
       # Create a named vector for easy lookup by col level
       col_medians_named <- setNames(col_medians_val, as.character(col_levels_order)) # Use as.character here too for safety
       # Subtract the appropriate col median for each col in x2
       # Use as.character for robust lookup
       x2[[val]] <- x2[[val]] <- x2[[val]] - col_medians_named[as.character(x2[[col]])]
       
       # Convergence check follows as in your original code:
       res.sum2 <- sum(abs(x2[[val]]), na.rm = TRUE)
       if(res.sum2 == 0 | abs(res.sum2 - res.sum1) < eps){
         break
       } else{
         res.sum1 <- res.sum2
       }
      
      # Legacy loop ----
      #row.pol <- aggregate(x2[[val]] ~ x2[[row]], FUN = stat)
      # colnames(row.pol) <- c(row, val)
      # row.eff[[val]] <- row.eff[[val]] + row.pol[[val]]
      # global  <- global + stat(col.eff[[val]])
      # col.eff[[val]] <- col.eff[[val]] - stat(col.eff[[val]])
      # x2 <- merge(x2, row.pol, by = row, suffixes = c("",".fun"))
      # x2[[val]] <- x2[[val]] - x2[[paste0(val,".fun")]]
      # x2[[paste0(val,".fun")]] <- NULL
      # 
      # col.pol <- aggregate(x2[[val]] ~ x2[[col]], FUN = stat)
      # colnames(col.pol) <- c(col, val)
      # col.eff[[val]] <- col.eff[[val]] + col.pol[[val]]
      # global  <- global + stat(row.eff[[val]])
      # row.eff[[val]] <- row.eff[[val]] - stat(row.eff[[val]])
      # x2 <- merge(x2, col.pol, by = col, suffixes = c("",".fun"))
      # x2[[val]] <- x2[[val]]  - x2[[paste0(val,".fun")]]
      # x2[[paste0(val,".fun")]] <- NULL
      # 
      # res.sum2 <- sum(abs(x2[[val]]))
      # if(res.sum2 == 0 | abs(res.sum2 - res.sum1) < eps){
      #   break
      # } else{
      #   res.sum1 <- res.sum2
      # }
      # End of legacy loop ---- 
    }
  }
  # End of polish routine ----
  
  # Append original values to the residuals
  names(x2)[3] <- "residuals"
  x2 <- cbind(x2,x0)
 
  # Create wide version of table ----
  x2w <- reshape(x2, direction = "wide",
                 idvar = row, timevar = col, sep = "##")
  colnames(x2w) <- gsub( ".*##", "", colnames(x2w))
  rownames(x2w) <- as.character(x2w[[row]])
  x2w <- x2w[ , -1]
  x2w <- x2w[ as.character(row.eff[[row]]), ] # Sort row before binding
  x2w <- cbind(eff = row.eff[[val]], x2w)
  x2w <- x2w[ , c("eff", as.character(col.eff[[col]]))] # Sort column before binding
  x2w <- rbind( eff = c(global, col.eff[[val]]), x2w)
  
  # Respect column levels (if present)
  if(!is.null(levels(x[[col]])) & sort == FALSE) {
    x2w <-  x2w[ , c(colnames(x2w[[col]]), c("eff", levels(x[[col]]))) ]
    x2[[col]] <- factor(x2[[col]], levels = levels(x[[col]]) )
  }else if (sort == TRUE){
    col.lvl <- levels(reorder(col.eff[,1],col.eff[,2]))
    x2[[col]] <- factor(x2[[col]], levels = col.lvl )
    x2w <-  x2w[ , c(colnames(x2w[[col]]), c("eff", col.lvl)) ]
  }
  
  # Respect row levels (if present)
  if(!is.null(levels(x[[row]])) & sort == FALSE) {
    x2w <-  x2w[c("eff", levels(x[[row]])),  ]
    x2[[row]] <- factor(x2[[row]], levels = levels(x[[row]]) )
  }else if (sort == TRUE){
    row.lvl <- levels(reorder(row.eff[,1],row.eff[,2]))
    x2[[row]] <- factor(x2[[row]], levels = row.lvl )
    x2w <-  x2w[c("eff", row.lvl),  ]
  }
  
  # Create output list ----
  names(row.eff)[2] <- "effect"
  names(col.eff)[2] <- "effect"

  # Compute comparison value
  cv <- merge(x2, row.eff)

  # Build table
  names(cv)[5] <- paste0(row,".eff")
  cv <- merge(cv, col.eff)
  names(cv)[6] <- paste0(col,".eff")
  cv$cv <-  cv[,5] * cv[,6] / global
  names(cv)[3] <- "residuals"
  cv$fit <- cv[5] + cv[6] + global
  names(cv$fit) <- "fit"

  # Compute IQRoQ
  IQRrow <- IQR(row.eff$effect)
  IQRcol <- IQR(col.eff$effect)
  resQ   <- as.vector(quantile(abs(x2[[val]]), 0.8, na.rm = TRUE))
  IQ_row <- IQRrow / resQ
  IQ_col <- IQRcol / resQ
  
  # Create list of effects
  effects <- list(
    row = setNames(row.eff[[2]], row.eff[[1]]),
    col = setNames(col.eff[[2]], col.eff[[1]])
  )
  names(effects) <- c(row, col)
  
  # Generate list
  out <- list(wide = x2w, response = val, effects = effects, row = row.eff, col = col.eff,
              global = global, iter = i, long = cv,
              power = p, IQ_row = IQ_row, IQ_col=IQ_col)
  class(out) <- "eda_polish"
  
  
  # Generate plot ----
  if(plot == TRUE){
    plot.eda_polish(x=out, type = "residuals", k = 1,
                    col.quant = col.quant, colpal = colpal, adj.mar = adj.mar,
                    res.size = res.size, row.size = row.size, col.size = col.size,
                    res.txt = res.txt, label.txt = label.txt)
  }
  
  return(out)
}

