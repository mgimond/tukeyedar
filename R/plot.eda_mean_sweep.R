#' @export
#' @title Plot method for \code{eda_mean_sweep} objects
#'
#' @description
#' Generates decomposition plots of factor effects and residuals from
#' an object of class \code{"eda_mean_sweep"}. These plots aid in visualizing
#' the additive decomposition of the response variable.
#'
#' @param x An object of class \code{eda_mean_sweep}
#' @param plot A character string specifying the type of plot.
#'   \describe{
#'     \item{\code{"effects"}}{(default) Plots the raw centered effects.}
#'     \item{\code{"ms"}}{Scales effects by \code{sqrt(N / df)} to reflect their
#'       relative contribution to variance.}
#'   }
#' @param ... Additional arguments passed to the internal plotting function
#'   \code{\link{.eda_plot_vardecomp}}. Common options include:
#'   \itemize{
#'     \item \code{type}: Character. String specifying the type of plot to generate. 
#'                        Must be either \code{"boxpnt"} (default) or \code{"box"}
#'                        if the effect values are to be displayed as boxplots.
#'     \item \code{rotate}: Logical. If \code{TRUE}, rotates the plot orientation.
#'     \item \code{show.resp}: Logical. If \code{TRUE}, includes a boxplot of the centered response.
#'     \item \code{outliers}: Logical. If \code{TRUE}, displays outliers in boxplots.
#'     \item \code{label}: Logical. If \code{TRUE}, adds labels to effect levels.
#'     \item \code{order}: Logical. If \code{TRUE}, orders effects by spread.
#'     \item \code{lim}: Numeric. Vector of length 2 specifying axis limits.
#'     \item \code{overlap}: Character. One of \code{"stack"}, \code{"overplot"}, or \code{"jitter"}.
#'     \item \code{pch}: Numeric. Controls the plot symbol type.
#'     \item \code{p.col}: Character. Controls the color of the plot symbol's outline.
#'     \item \code{p.fill}: Character. Controls the fill color of the plot symbol.
#'     \item \code{size}: Numeric. Controls the size of the plot symbols.
#'     \item \code{alpha}: Numeric (0–1). Controls the transparency of the plot symbols.
#'     \item \code{grey}: Numeric (0–1) or character. Controls grayscale coloring.
#'     \item \code{padding}, \code{cex.txt}, \code{type}, \code{input}: See \code{.eda_plot_vardecomp}.
#'   }
#'
#' @return A plot visualizing residuals and factor effects.
#'
#' @details
#' This plot method leverages the value-splitting and sweeping procedure
#' performed by \code{\link{eda_mean_sweep}()} to provide a graphical
#' representation of the decomposed data. It visualizes the additive overlays:
#' the residuals and the centered main and interaction effects. This
#' allows for an exploratory assessment of the relative magnitudes of
#' different effects and the variability remaining in the residuals.
#' Such displays are emphasized in EDA to gain insight into data structure.
#'
#' \cr
#' The actual plotting is handled by the internal utility function 
#' \code{\link{.eda_plot_vardecomp}}, which
#' plots the residuals as a boxplot and overlays factor effects as individual
#' dot plots (by default, \code{type = "boxpnt"} is used internally 
#' within \code{.eda_plot_vardecomp}).
#'
#' The \code{order} argument (default TRUE) helps in quickly seeing which effects
#' contribute most to the data's range by sorting their range visually.
#' 
#' @references 
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991). 
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#' 
#' @seealso \code{\link{eda_mean_sweep}}, \code{\link{.eda_plot_vardecomp}}, \code{\link{eda_anova_table}}
#' 
#' @examples
#' 
#' # A default plot
#' M0 <- eda_mean_sweep(feav5_12, Weight, Level, Type, max_order = 2)
#' plot(M0)
#' 
#' # Adding labels
#' plot(M0, label = TRUE)
#' 
#' # Options are available for dot plots when tes are present. By default, points
#' # are stacked. Other options include "jitter",
#' plot(M0, overlap = "jitter")
#' 
#' # ... or "overplot" (you can modify the point transparency via the "alpha" argument)
#' plot(M0, overlap = "overplot")
#' 
#' # Plot can be rotated
#' plot(M0, rotate = TRUE)
#' 
#' # Original response variable can be added as a boxplot 
#' plot(M0, show.resp = TRUE)
#' 
#' # If "mean squares" are to be compared, the effects need to be adjusted
#' # by setting plot = "ms" (see page 174 of the referenced source)
#' plot(M0, plot = "ms")

plot.eda_mean_sweep <- function(x, plot = "effects", ...) {
  
  # Checks
  if (!inherits(x, "eda_mean_sweep"))
    stop("Input object must be of class eda_mean_sweep.\n")
  
  if(plot == "effects"){
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = x$effects, ...)
  } else {
    anova_table <- eda_anova_table(x)
    adjusted_effects <- list()
    n <- nrow(x$long)
    for (effect_name in names(x$effects)) {
      # Retrieve the original effect values (centered deviations for each level).
      original_effect_values <- x$effects[[effect_name]]
      
      # Find the row in the anova_table corresponding to the current effect.
      f_stat_row <- anova_table[anova_table$Effect == effect_name, ]
      
      # Get the df for this effect.
      df  <- f_stat_row$df 
      
      # Compute adjusted effects
      adjusted_effects[[effect_name]] <- original_effect_values * (n / df)^0.5

    }
    # Plot
    .eda_plot_vardecomp(dat = x$long, response = x$response, eff = adjusted_effects, ...)
  }
  
}
