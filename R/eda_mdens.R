#' @export
#' @import grDevices
#' @importFrom utils modifyList
#' @importFrom stats density bw.nrd0
#' @title Overlapping density distributions for multiple variables
#'
#' @description \code{eda_mdens} generates overlapping density distributions for
#'   multiple batches.
#'
#' @param ... Numeric vectors representing individual batches, or a dataframe
#'            and its continuous and categorical variables. Can also contain
#'            arguments passed to `stats::density()` like `bw`, `n`, `from`, `to`.
#'
#' @param p  Power transformation to apply to all values.
#' @param tukey Boolean determining if a Tukey transformation should be adopted
#'   (FALSE adopts a Box-Cox transformation).
#' @param base Base used with the log() function if \code{p = 0}.
#' @param grey Grey level to apply to plot elements (0 to 1 with 1 = black).
#' @param alpha Fill transparency (0 = transparent, 1 = opaque).
#' @param legend Boolean determining if a legend should be added to the plot.
#' @param title Plot title. Defaults to \code{"NULL"}.
#' @param show.par Boolean determining if parameters such as power
#'   transformation should be displayed.
#' @param kernel The kernel to be used. Can be one of the kernels supported by
#'   \code{stats::density} ("gaussian", "epanechnikov", "rectangular",
#'   "triangular", "biweight", "cosine", "optcosine"), or "sliding" for a
#'   custom sliding-window rectangular kernel estimator.
#' @param cols A vector of colors for the density fills or one of the R built-in
#'  color palettes (see \code{hcl.pals()}). If NULL, colors default to "Dark 3".
#' @param outline Boolean determining if outline should be shown without
#'   transparency.
#'
#' @details This function extends `eda_dens` to allow for the comparison of more
#'   than two batches. Input can be a list of numeric vectors, individual
#'   numeric vectors passed directly, or a dataframe in long format.
#'
#' @returns Does not return a value.
#'
#' @examples
#'
#' # Example using individual vectors as input. A
#' # bandwidth of 0.5 is used for the density plots
#' # Plots are ordered following input order
#' set.seed(123)
#' x <- rnorm(100, 0, 1)
#' y <- rnorm(100, 2, 1.5)
#' z <- rnorm(100, 4, 2)
#' w <- rnorm(100, 8, 3)
#' eda_mdens(x, y, z, w, bw = 0.7)
#'
#'
#' # Example using a dataframe as input
#' # Plots are ordered alphabetically
#' df <- data.frame( values = c(x, y, z, w),
#'                   group = rep(c("x","y","z","w"), each = 100))
#' eda_mdens(df, values, group, bw=0.7)
#'
#' # To specify an order when using a dataframe,
#' # use factors
#' df$group <- factor(df$group, levels = c(c("x","y","z","w")) )
#' eda_mdens(df, values, group, bw=0.7)
#'
#' # Colors can be passed as a vector of color names or as a predefined
#' # hcl.pals() palette name. The fill transparency can be controlled with the
#' # alpha argument.
#' eda_mdens(df, values, group, cols = "OrRd", alpha = 0.6)
#'
#' # Most density() arguments can be passed via ...
#' eda_mdens(df, values, group, bw = 0.5, n = 20, from=0, to=14,kernel="cosine")
#'
#' # A built-in custom "sliding" window kernel is also available.
#' # Note that the bandwidth for "sliding" is in the variable's units and not
#' # the fraction of observations as implemented by density()
#' eda_mdens(df, values, group, bw = 5, n = 20, from=0, to=14, kernel="sliding")
#'

eda_mdens <- function(..., p = 1L, tukey = FALSE, base = exp(1),
                     grey = 0.6, alpha = 0.4, legend = TRUE, title = NULL, show.par= TRUE,
                     kernel = "gaussian", cols = NULL, outline = FALSE) {

  # --- Argument Handling ---
  mc   <- match.call(expand.dots = FALSE)

  list_of_batches <- NULL
  group_names     <- NULL
  density_args    <- list()

  dot_exprs <- mc$...

  # Case 1: dataframe as first argument

  if (length(dot_exprs) >= 1 && is.data.frame(eval.parent(dot_exprs[[1]]))) {

    mc <- match.call(expand.dots = FALSE)
    dot_exprs <- mc$...

    if (length(dot_exprs) < 3)
      stop("When passing a data frame, supply value and group columns.")

    df <- eval.parent(dot_exprs[[1]])

    value_name <- deparse(dot_exprs[[2]])
    group_name <- deparse(dot_exprs[[3]])

    values <- df[[value_name]]
    groups <- factor(df[[group_name]])

    if (!is.numeric(values))
      stop("Value column must be numeric.")

    list_of_batches <- split(values, groups)
    group_names     <- names(list_of_batches)


    if (length(dot_exprs) > 3) {
      density_args <- lapply(dot_exprs[-(1:3)], eval.parent)
      names(density_args) <- names(dot_exprs)[-(1:3)]
    }

  } else {

    dots <- list(...)
    dot_names <- names(dots)

    # Case 2: vector / list input
    batch_exprs <- substitute(list(...))[-1]

    batch_names <- vapply(
      batch_exprs,
      function(e) {
        if (is.name(e) || is.call(e)) deparse(e) else NA_character_
      },
      character(1)
    )

    dot_names <- names(dots)

    for (i in seq_along(dots)) {

      val  <- dots[[i]]
      name <- dot_names[i]

      # Named arguments always go to density / plot parameters
      if (!is.null(name) && name != "") {
        density_args[[name]] <- val
        next
      }

      # Unnamed numeric vectors are treated as batches
      if (is.numeric(val) && length(val) > 0) {
        nm <- batch_names[i]
        if (is.na(nm) || nm == "")
          nm <- paste0("Batch", length(list_of_batches) + 1)
        list_of_batches[[nm]] <- val
      } else {
        density_args[[length(density_args) + 1]] <- val
      }
    }

    group_names <- names(list_of_batches)
  }

  if (length(list_of_batches) == 0)
    stop("No numeric batch vectors found.")

  # --- Data Processing and Plotting --- (This part remains largely the same)

  # Ensure all batches are non-empty after initial filtering
  valid_indices <- sapply(list_of_batches, length) > 0
  list_of_batches <- list_of_batches[valid_indices]
  group_names <- group_names[valid_indices]
  if (length(list_of_batches) == 0) {
      stop("All provided batches are empty after filtering.")
  }

  # --- Data Transformation (similar to eda_dens) ---
  # Apply transformation to each batch
  list_of_batches <- lapply(list_of_batches, function(batch) {
    batch <- eda_re(batch, p = p, tukey = tukey, base = base)
    batch[!is.na(batch)] # Remove NA values after re-expression
  })


  # --- Prepare for Density Calculation ---
  if (p != 1 && !is.null(density_args$bw)) {
    warning("When using a power transformation (p != 1) and manually specifying 'bw', ensure that 'bw' is appropriate for the TRANSFORMED data scale.")
  }

  # Calculate common bw and n_points if not provided
  all_transformed_values <- unlist(list_of_batches)
  bw_common <- if (!is.null(density_args$bw)) density_args$bw else bw.nrd0(all_transformed_values)
  n_points_common <- if (!is.null(density_args$n)) density_args$n else 512

  # Determine grid boundaries for all batches, applying re-expression if necessary
  final_from <- if (!is.null(density_args$from)) {
    eda_re(density_args$from, p = p, tukey = tukey, base = base)
  } else {
    min(all_transformed_values) - 3 * bw_common
  }

  final_to <- if (!is.null(density_args$to)) {
    eda_re(density_args$to, p = p, tukey = tukey, base = base)
  } else {
    max(all_transformed_values) + 3 * bw_common
  }

  grid_points <- seq(final_from, final_to, length.out = n_points_common)

  # --- Density Calculation for all batches ---
  list_of_densities <- lapply(list_of_batches, function(batch) {
    if (kernel == "sliding") {
      strict_kde <- function(x_grid, y_data, bw) {
        n <- length(y_data)
        sapply(x_grid, function(x0) {
          sum(abs(y_data - x0) <= (bw/2) ) / (n * bw)
        })
      }
      # bw for each batch might differ if not specified globally
      bw_batch <- if (!is.null(density_args$bw)) density_args$bw else bw.nrd0(batch)
      list(x = grid_points, y = strict_kde(grid_points, batch, bw_batch))
    } else {
      # Remove 'from', 'to', 'bw', 'n' from density_args to ensure final_from/to, bw_common, n_points_common are used
      clean_density_args <- density_args[!names(density_args) %in% c("from", "to", "bw", "n")]
      do.call(stats::density,
              c(list(x = batch, kernel = kernel, from = final_from, to = final_to, n = n_points_common, bw = bw_common),
                clean_density_args))
    }
  })

  # --- Plotting Setup ---
  plotcol <- rgb(1-grey, 1-grey, 1-grey)

  # Generate colors
  if (is.null(cols)) {
    cols <- hcl.colors(length(list_of_densities), palette = "Dark 2")
  } else if (length(cols) == 1 &&  cols %in% hcl.pals()){
    cols <- hcl.colors(length(list_of_densities), palette = cols)
  } else if (length(cols) < length(list_of_densities)) {
    warning("Number of provided colors is less than the number of batches. Colors will be recycled.")
    cols <- rep(cols, length.out = length(list_of_densities))
  }

  # Apply alpha to fill colors
  fill_cols <- adjustcolor(cols, alpha.f = alpha)

  # Blend outline with fill if outline is not to be shown
  if(outline == FALSE){
    out_cols <- fill_cols
  } else {
    out_cols <- cols
  }

  # Determine global plotting ranges
  all_density_y <- unlist(lapply(list_of_densities, `[[`, "y"))

  xlim <- range(grid_points) # Explicitly use final grid_points for xlim
  ylim <- range(0, all_density_y)

  # Get lines-to-inches ratio (for plotting labels)
  in2line <- ( par("mar") / par("mai") )[2]

  # Create a dummy plot to extract y-axis labels
  pdf(NULL)
  plot(x = 0, y = 0, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt='n', main = NULL,  xlim = xlim, ylim = ylim)
  y.wid <- max( strwidth( axTicks(2), units="inches")) * in2line + 1.2
  dev.off()

  # Set plotting parameters
  plot_width_in <- par("pin")[1]
  label_offset_in <- par("mgp")[2] / in2line # Determine the distance from the axis to the labels

  ## ---- Legend layout planning ----
  if (legend == TRUE) {
    legend_cex <- 0.75
    label_widths <- strwidth(group_names, units = "inches", cex = legend_cex)
    item_total_widths <- label_widths + 0.4

    if (sum(item_total_widths) <= plot_width_in) {
      ncol_legend <- length(group_names)
    } else {
      # Otherwise, calculate how many columns of the *widest* item fit
      max_item_width <- max(item_total_widths)
      ncol_legend <- max(1, floor(plot_width_in / max_item_width))
    }
    legend_rows <- ceiling(length(group_names) / ncol_legend)

  } else {
    ncol_legend <- 1
    legend_rows <- 0
  }

  # Make room for legend
  legend_lines <- ceiling(length(group_names) / max(1, ncol_legend))
  #top_mar <- 3 + legend_lines * 0.9   # about 1 line per legend row
  bottom_mar <- 3 + legend_rows * 1.2  # room for legend rows

  # Define plot margins
  .pardef <- par( col = plotcol, mar = c(bottom_mar,y.wid,3,1))
  on.exit(par(.pardef))

  # --- Generate Plot ---
  plot(x = xlim[1], y = ylim[1], type = "n", xlab = NA, ylab = NA, xaxt = "n", yaxt = "n",
       xlim = xlim, ylim = ylim, main = "") # Setup empty plot area

  # Draw polygons for each density
  for (i in seq_along(list_of_densities)) {
    dens <- list_of_densities[[i]]
    # Augment coordinates for polygon drawing
    dens_poly <- list(x = c(dens$x[1], dens$x, dens$x[length(dens$x)]), y = c(0, dens$y, 0))
    polygon(dens_poly, col = fill_cols[i], border = out_cols[i]) # Border also set to fill color
  }

  box(col = plotcol)
  axis(1, col = plotcol, col.axis = plotcol, labels = TRUE, padj = -0.8, tck = -0.02)
  axis(2, col = plotcol, col.axis = plotcol, labels = TRUE, las = 1, hadj = 0.8, tck = -0.02)
  max_label_width_in <- max(strwidth(axTicks(2), units = "inches")) # Get widest tick label
  adj_val <- -(max_label_width_in + label_offset_in) / plot_width_in # Calculate the required 'adj' value
  mtext("Density", side = 3, adj = adj_val, col = plotcol, padj = -1) # y-axis label for density

  # Add title if desired
  title(main = title, line = 1.8, col.main = plotcol)

  if (show.par == TRUE && p != 1) { # Only show p if transformation is applied
    mtext(side = 3, text = paste0("p=", p), adj = 1, cex = 0.65)
  }

  # Add legend if requested
  if (legend == TRUE) {
    # Dynamically calculate the inset to avoid the legend encroaching on the
    # x-axis labels, especially on short, wide plot windows.
    plot_height_in <- par("pin")[2]
    # We need to move the legend down by the plot height, plus enough space for
    # the x-axis labels. We estimate this space as 1.5 times the height of a
    # character.
    xaxis_label_space_in <- strheight("X", units = "inches") * 1.5
    # The inset is a fraction of the plot height. To move it down by
    # plot_height_in + xaxis_label_space_in, the inset value is:
    # (plot_height_in + xaxis_label_space_in) / plot_height_in
    dynamic_y_inset <- 1 + (xaxis_label_space_in / plot_height_in)

    legend(x = "top",
           # inset moves the legend downward from the top of the plot region.
           # A value > 1 places it below the plot. We calculate it dynamically
           # to clear the x-axis labels regardless of aspect ratio.
           inset = c(0, dynamic_y_inset),
           legend = group_names,
           fill = fill_cols,
           ncol = ncol_legend, # Uses the columns calculated in the planning step
           xpd = TRUE,         # Crucial: Allows the legend to draw in the margin area [4, 6]
           bty = "n",          # Removes the legend box border for a cleaner look
           cex = 0.75)         # Shrinks text slightly to ensure it fits plot width
  }

  # Reset plot parameters
  par(.pardef)
}
