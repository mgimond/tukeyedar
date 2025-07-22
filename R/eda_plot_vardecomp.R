#' Variability decomposition plots
#'
#' This is an internal helper function used to generate plots
#' that visualize the effects and residuals from the analysis of n-way tables or
#' a regression model.
#' The function is not intended for direct use by the end-user.
#'
#' @param dat A data frame in long form, containing the data to be plotted.
#' @param response A character string specifying the name of the response
#'   variable column in the \code{dat} data frame.
#' @param type A character string specifying the type of plot to generate.
#'   Must be either \code{"boxpnt"} or \code{"box"}.
#' @param input A character string. \code{"reg"} = bivariate model input.
#'   \code{"nway"} = univariate model or N-way table input.
#' @param eff A list of effect values. Required when \code{input = "nway"}.
#' @param rotate Logical. If \code{TRUE}, rotates the plot orientation.
#' @param padding Numeric. Controls padding for plot limits.
#' @param show.resp Logical. If \code{TRUE}, includes a boxplot for the response
#'   variable.
#' @param outliers Logical. If \code{TRUE}, outliers are displayed in boxplots.
#' @param label Logical. Controls whether labels are displayed.
#' @param order Logical. Controls ordering (likely of factors or effects).
#' @param cex.txt Numeric. Controls text size.
#' @param lim Numeric. Explicit limits for the plot axes.
#' @param overlap Character. Specifies how to handle overlapping points,
#'   must be one of \code{"stack"}, \code{"overplot"}, or \code{"jitter"}.
#' @param pch Point symbol type. Only applicable if \code{type = "boxpnt"}.
#' @param p.col Point border color. Only applicable if \code{type = "boxpnt"}.
#' @param p.fill Point fill color. Only applicable if \code{type = "boxpnt"}.
#' @param size Point size. Only applicable if \code{type = "boxpnt"}.
#' @param alpha Transparency level for points (0 = transparent, 1 = opaque).
#' @param grey Numeric. Controls grayscale coloring for plot elements
#'   and axes.
#' @param title Plot title. If title is to be omitted, set to \code{NULL}.
#' @param ... Additional arguments passed to underlying plotting functions.
#'
#' @keywords internal
#'
#' @importFrom grDevices boxplot.stats
#'
#' @return Primarily called for producing a plot.

# Variability decomposition plots
.eda_plot_vardecomp <- function(dat, response, type="boxpnt",
                               input = "nway", eff = NULL, rotate = FALSE,
                               padding = 0.1, show.resp = FALSE, outliers = TRUE,
                               label = FALSE, order = TRUE, cex.txt = 1, lim = NULL,
                               overlap = c("stack", "overplot", "jitter"),
                               pch = 16, p.col = "grey50", p.fill = "grey80", size = 1,
                               alpha = 0.5, grey = 0.6,
                               title = "Variability decomposition", ...){
  # Check for valid arguments
  dots <- list(...)
  dot_names <- names(dots)
  internal_args <- names(formals(.eda_plot_vardecomp))

  # Check for invalid names
  invalid <- setdiff(dot_names, internal_args)
  if (length(invalid) > 0) {
    warning(sprintf("Invalid arguments passed to %s: %s",
                    deparse(substitute(.eda_plot_vardecomp)),
                    paste(invalid, collapse = ", ")))
  }

  # Capture Arguments ----
  response_chr <- response

  # Validate input ----
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a data frame in long form.")
  }
  if (!(response_chr %in% names(dat))) {
    stop(paste("Response column '", response_chr, "' not found in dat.", sep = ""))
  }
  if(input == "nway" & is.null(eff)){
    stop("N-way diagnostic plot needs list of effect values via the eff argument.")
  }
  if(!type %in% c("boxpnt", "box")){
    stop("Input 'type' must be 'box' or 'boxpnt'.")
  }

  # Plot color settings
  plotcol <- if (is.numeric(grey)) gray(grey) else "black"
  plotcol <- rgb(1 - grey, 1 - grey, 1 - grey)
  if (!is.null(alpha)) {
    if (p.col %in% colors() & p.fill %in% colors()) {
      p.col <- adjustcolor(p.col, alpha.f = alpha)
      p.fill <- adjustcolor(p.fill, alpha.f = alpha)
    }
  }

  # Get response variable
  y <- dat[[response_chr]]
  y <- y[is.finite(y)]
  y <- y - median(y, na.rm = TRUE)
  res <- dat[["residuals"]]
  res <- res[is.finite(res)]

  # Get effects if N-way or univariate
  if(input == "nway"){
    effects <- unlist(eff)
    # Sort effects
    if(order){
      ranges <- sapply(eff, function(x) diff(range(x)))
      eff <- eff[order(ranges)]
    }
  }

  # Get fitted values if bivariate model
  if(input == "reg"){
    effects <- unlist(eff)
  }

  # Initialize values
  in2line <- (par("mar")/par("mai"))[2]

  # Get axis limits (for non-rotated case)
  if(is.null(lim)){
    bxp_res <- with(boxplot.stats(res), c(stats, if(outliers) out))
    bxp_y <- with(boxplot.stats(y), c(stats, if(outliers) out))
    y_range <- range(c(bxp_res, effects, show.resp * bxp_y), na.rm = TRUE, finite = TRUE)
  } else {
    y_range <- lim
  }

  x_lim_padded <- c(0.5, 1.5 + length(eff) + show.resp)

  # Add padding
  y_padding <- diff(y_range) * padding
  y_lim_padded <- c(y_range[1] - y_padding, y_range[2] + y_padding)

  # Adjust top margin based on presence or absence of title
  top.mar <- ifelse(is.null(title), 1, 3.2)

  if (rotate) {
    # Rotate plot by flipping coordinates

    # Get margin width
    max_label_width <- max(strwidth(c(as.character(names(eff)), "Residuals",
                                      ifelse(show.resp, response_chr, NA)),
                                    units = "inches", cex = cex.txt))
    y.wid <- max_label_width/par("csi") + 2

    # Start plot
    .pardef <- par(pty = "m", col.axis = plotcol, col.lab = plotcol, col = plotcol,
                   col.main = plotcol, col.sub = plotcol, mar = c(3, y.wid, top.mar, 1),
                   cex.axis = cex.txt, cex.lab = cex.txt)
    on.exit(par(.pardef))

    plot("", type = "n",
         xlim = y_lim_padded,
         ylim = x_lim_padded,
         yaxt = "n",
         ylab = "",
         xlab = "",
         main = title)
    if(show.resp){
      boxplot(y,
              main = title,
              ylim = y_lim_padded,
              xlim = x_lim_padded,
              at = 1 ,
              yaxt = "n", ylab="",
              outline = outliers,
              col = "bisque",
              boxwex = 0.7,
              add = TRUE,
              horizontal = TRUE)
      axis(side = 1, col = plotcol)
      axis(2, at = 1 , labels = response_chr, tick = FALSE, line = 0, las =1)
      abline(h = 1.5, col = "darkgray", lty = 2)
    }

    boxplot(res,
            main = title,
            ylim = y_lim_padded,
            xlim = x_lim_padded,
            at = 1 + show.resp,
            yaxt = "n", ylab="n" ,
            outline = outliers,
            col = "lightgray",
            boxwex = 0.7,
            add = TRUE,
            horizontal = TRUE)
    axis(side = 1, col = plotcol)
    abline(v = 0, col = "darkgray", lty = 2)


    # Adjust factor effect positions for rotated plot
    factor_y_positions <- 1 + seq_along(eff)
    for (i in seq_along(eff)) {
      factor_name <- names(eff)[i]
      effects <- eff[[i]]
      if ( "FALSE" %in% names(table(is.finite(effects))) )
        warning(cat("One or more effect values in ", factor_name,
                    " are not finite!\n"), call. = FALSE)
      y_pos <- factor_y_positions[i] # Flip positioning
      if(type == "boxpnt"){
        stripchart(effects, add = TRUE, at = y_pos + show.resp,
                   method = overlap, pch = pch, col = p.col, bg = p.fill,
                   cex = size)
        if(label){
          suppressWarnings(
          text(effects, y_pos + show.resp, labels = names(effects), pos = 3,
               cex = 0.6 * cex.txt, srt = 45)
          )
          }
      } else {
        suppressWarnings(
        boxplot(effects, add = TRUE, at = y_pos + show.resp, horizontal = TRUE,
                col = "grey95", xaxt = "n", yaxt = "n", outline = outliers,
                boxwex = 0.7)
        )
      }
      axis(2, at = y_pos + show.resp, labels = factor_name, tick = FALSE, line = 0,las =1)
    }
    axis(2, at = 1 + show.resp, labels = "Residuals", tick = FALSE, line = 0, las = 1)

  } else {
    # Vertical plot
    y.wid <- max(strwidth(axTicks(2), units = "inches", cex = cex.txt)) * in2line + 1.5

    .pardef <- par(pty = "m", col.axis = plotcol, col.lab = plotcol, col = plotcol,
                   col.main = plotcol, col.sub = plotcol, mar = c(3, y.wid, top.mar, 1),
                   cex.axis = cex.txt, cex.lab = cex.txt)
    on.exit(par(.pardef))

    plot(1, type = "n",
         xlim = x_lim_padded,
         ylim = y_lim_padded,
         xaxt = "n", yaxt = "n",
         xlab = "",
         ylab = "",
         main = title)

    if(show.resp){
      boxplot(y,
              main = title,
              ylab = "",
              ylim = y_lim_padded,
              xlim = x_lim_padded,
              at = 1 ,
              xaxt = "n", yaxt = "n",
              outline = outliers,
              col = "bisque",
              boxwex = 0.7,
              add = TRUE)
      axis(1, at = 1 , labels = response_chr, tick = FALSE, line = 0,
           col.ticks = plotcol)
      abline(v = 1.5, col = "darkgray", lty = 2)
    }

    boxplot(res,
            main = title,
            ylab = "",
            ylim = y_lim_padded,
            xlim = x_lim_padded,
            at = 1 + show.resp,
            xaxt = "n", yaxt = "n",
            outline = outliers,
            col = "lightgray",
            boxwex = 0.7,
            add = TRUE,
            las = 2)

    axis(side = 2, las = 2, col = plotcol)

    abline(h = 0, col = "darkgray", lty = 2)

    # Plot factor effects
    factor_x_positions <- 1 + seq_along(eff)
    for (i in seq_along(eff)) {
      factor_name <- names(eff)[i]
      effects <- eff[[i]]
      if ( "FALSE" %in% names(table(is.finite(effects))) )
                     warning(cat("One or more effect values in ", factor_name,
                                 " are not finite!\n"), call. = FALSE)
      x_pos <- factor_x_positions[i]
      if(type == "boxpnt"){
        stripchart(effects, add = TRUE, vertical = TRUE, at = x_pos + show.resp,
                   method = overlap, pch = pch, col = p.col, bg = p.fill,
                   cex = size)
        if(label){
          suppressWarnings(text(x_pos + show.resp, effects, labels = names(effects), pos = 2,
               cex = 0.6 * cex.txt ))
        }
      } else {
        suppressWarnings(
        boxplot(effects, add = TRUE, at = x_pos + show.resp, col = "grey95", yaxt = "n",
                outline = outliers)
        )
      }


      axis(1, at = x_pos + show.resp, labels = factor_name, tick = FALSE, line = 0)

    }
    axis(1, at = 1 + show.resp, labels = "Residuals", tick = FALSE, line = 0)
  }
  par(.pardef)
}



