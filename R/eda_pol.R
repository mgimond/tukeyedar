#' @export
#' @title Polish two-way tables
#'
#' @description
#'  \code{eda_pol} Polishes two-way tables using median, means, or any customizable functions.
#'
#' @param x A three column data frame
#' @param row Name of column assigned to the row effect
#' @param col Name of column assigned to the column effect
#' @param val Name of column assigned to the response variable
#' @param stat Polishing statistic (default is median)
#' @param plot Boolean determining if an output plot should be generated
#' @param eps  Convergence tolerance parameter
#' @param maxiter Maximum number of iterations
#' @param sort Boolean determining if the effects row/columns should be sorted
#' @param p Re-expression power parameter
#' @param tukey Boolean determining if Tukey's power transformation should used. If FALSE,
#'              the Box-Cox transformation is adopted.
#' @param col.quant Boolean determining if a quantile classification scheme should be used
#' @param colpal Color palette to adopt
#' @param adj.mar Boolean determining if margin width needs to accomodate labels
#' @param res.size Size of residual values in plot [0-1]
#' @param row.size Size of row effect values in plot [0-1]
#' @param col.size Size of column effect values in plot [0-1]
#' @param res.txt Boolean determining if values should be added to plot
#' @param label.txt Boolean determining if margin and column labels should be plotted
#'
#' @details
#'  The function performs a polish on a two way table. By default, it applies a
#'  median polish, but other statistic such as the mean can be passed to the function
#'  via the \code{stat = } parameter.
#'  The function returns a list of row/column effects along with global and residual values.
#'  It will also generate a colored table if \code{plot = TRUE}.
#'  Returns a list of class \code{eda_polish}

#'
#' @examples
#' df <- data.frame(region =  rep( c("NE", "NC", "S", "W"), each = 5),
#' edu = rep( c("ed8", "ed9to11", "ed12", "ed13to15", "ed16"), 4),
#' perc = c(25.3, 25.3, 18.2, 18.3, 16.3, 32.1, 29, 18.8,
#'         24.3, 19, 38.8, 31, 19.3, 15.7, 16.8, 25.4, 21.1, 20.3, 24, 17.5))
#'
#' eda_pol(df, row = "region", col = "edu", val = "perc", plot = FALSE)


eda_pol <- function (x, row= NULL, col = NULL, val = NULL, stat = median, plot = TRUE,
                        eps = 0.01, maxiter = 5, sort = FALSE, p = 1, tukey = FALSE,
                        col.quant = FALSE, colpal = "RdYlBu", adj.mar = FALSE,
                        res.size = 1, row.size = 1, col.size = 1,
                        res.txt = TRUE, label.txt = TRUE){
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

  # Check re-expression is desired
  if(p !=1){
    x2[[val]] <- eda_re(x2[[val]], p = p, tukey = tukey)
  }

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
    global <- stat(x2[[val]])
    x2[[val]] <- x2[[val]] - global
    res.sum1 <- 0

    # Proceed with the polish routine ----
    for (i in seq_len(maxiter)){

      row.pol <- aggregate(x2[[val]] ~ x2[[row]], FUN = stat)
      colnames(row.pol) <- c(row, val)
      row.eff[[val]] <- row.eff[[val]] + row.pol[[val]]
      global  <- global + stat(col.eff[[val]])
      col.eff[[val]] <- col.eff[[val]] - stat(col.eff[[val]])
      x2 <- merge(x2, row.pol, by = row, suffixes = c("",".fun"))
      x2[[val]] <- x2[[val]] - x2[[paste0(val,".fun")]]
      x2[[paste0(val,".fun")]] <- NULL

      col.pol <- aggregate(x2[[val]] ~ x2[[col]], FUN = stat)
      colnames(col.pol) <- c(col, val)
      col.eff[[val]] <- col.eff[[val]] + col.pol[[val]]
      global  <- global + stat(row.eff[[val]])
      row.eff[[val]] <- row.eff[[val]] - stat(row.eff[[val]])
      x2 <- merge(x2, col.pol, by = col, suffixes = c("",".fun"))
      x2[[val]] <- x2[[val]]  - x2[[paste0(val,".fun")]]
      x2[[paste0(val,".fun")]] <- NULL

      res.sum2 <- sum(abs(x2[[val]]))

      if(res.sum2 == 0 | abs(res.sum2 - res.sum1) < eps){
        break
      } else{
        res.sum1 <- res.sum2
      }
    }
  }

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
  names(cv)[4] <- paste0(row,".eff")
  cv <- merge(cv, col.eff)
  names(cv)[5] <- paste0(col,".eff")
  cv = cv[ ,-(1:2)]
  cv$cv <-  cv[,2] * cv[,3] / global

  # Compute IQRoQ
  IQRrow <- IQR(row.eff$effect)
  IQRcol <- IQR(col.eff$effect)
  resQ <- as.vector(quantile(abs(x2[[val]]), 0.8))
  IQ_row <- IQRrow / resQ
  IQ_col <- IQRcol / resQ

  # Generate list
  out <- list(long = x2, wide = x2w, row = row.eff, col = col.eff,
              global = global, iter = i, cv = cv,
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

