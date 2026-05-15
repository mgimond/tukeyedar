# x-y Scatterplot with EDA Enhancements

Creates an enhanced scatter plot of two variables with extensive
customization for exploratory analysis. Options include polynomial
regression lines, robust regression, quantile shading, mean/sd lines,
loess smoothing, and detailed styling parameters.

## Usage

``` r
.eda_plot_xy(
  dat,
  x,
  y,
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  px = 1,
  py = 1,
  tukey = NULL,
  base = NULL,
  raw_tick = FALSE,
  show.par = TRUE,
  reg = TRUE,
  poly = 1,
  w = NULL,
  robust = FALSE,
  rlm.d = list(psi = "psi.bisquare"),
  lm.col = rgb(1, 0.5, 0.5, 0.8),
  lm.lw = 1.7,
  lm.lty = 1,
  sd = TRUE,
  mean.l = TRUE,
  asp = TRUE,
  square = TRUE,
  grey = 0.6,
  pch = 21,
  p.col = "grey50",
  p.fill = "grey80",
  size = 0.8,
  alpha = 0.8,
  q = FALSE,
  inner = 0.68,
  qcol = rgb(0, 0, 0, 0.05),
  q.type = 5,
  loe = FALSE,
  loe.lw = 1.7,
  loe.lty = 2,
  loe.col = rgb(0.3, 0.3, 1, 1),
  loess.d = list(family = "symmetric", span = 0.7, degree = 1),
  stats = FALSE,
  stat.size = 0.8,
  hline = NULL,
  vline = NULL,
  plot = TRUE,
  ...
)
```

## Arguments

- dat:

  Optional data frame containing `x` and `y`.

- x:

  A numeric vector or column name in `dat` for the x-axis.

- y:

  A numeric vector or column name in `dat` for the y-axis.

- xlab:

  Optional x-axis labels. Defaults to variable names.

- ylab:

  Optional y-axis labels. Defaults to variable names.

- xlim:

  X-axis range.

- ylim:

  Y-axis range.

- px:

  Power transformation used in the input data to display if
  `show.par = TRUE`.

- py:

  Power transformation used in the input data to display if
  `show.par = TRUE`.

- tukey:

  Boolean determining if a Tukey transformation should be adopted (FALSE
  adopts a Box-Cox transformation).

- base:

  Base used with the log() function if `px` or `py` is `0`.

- raw_tick:

  Logical. If `TRUE`, original (untransformed) equally spaced tick
  values are displayed on the re-expressed axes.

- show.par:

  Logical; whether to display plot parameter summary on the plot.
  Currently only applies to regression model input.

- reg:

  Logical; whether to fit and display a regression line.

- poly:

  Integer; regression model polynomial degree (defaults to 1 for linear
  model).

- w:

  Optional numeric vector of weights for regression.

- robust:

  Logical; if `TRUE`, uses robust regression
  ([`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html)).

- rlm.d:

  List; parameters for
  [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html), (e.g.,
  `list(psi = "psi.bisquare")`).

- lm.col:

  Regression line color.

- lm.lw:

  Numeric; Regression line width.

- lm.lty:

  Numeric; Regression line type.

- sd:

  Logical; whether to show ±1 SD lines.

- mean.l:

  Logical; whether to show x and y mean reference lines.

- asp:

  Logical; whether to preserve the aspect ratio (ignored if
  `square = FALSE`).

- square:

  Logical; whether to create a square plotting window.

- grey:

  Numeric between `0-1`; controls grayscale background elements
  (`0 = black`, `1 = white`).

- pch:

  Integer; point symbol.

- p.col:

  Point border color.

- p.fill:

  Point fill color.

- size:

  Point size.

- alpha:

  Point transparency level (0 = 100\\ transparent, 1 = 100\\ opaque).

- q:

  Logical; whether to draw inner quantile boxes (quantile shading).

- inner:

  Numeric; defines the inner fraction of values to highlight with
  quantile shading.

- qcol:

  Fill color of quantile shading.

- q.type:

  Integer; type of quantile calculation (see `quantile`).

- loe:

  Logical; whether to plot loess smooth line.

- loe.lw:

  Numeric; Loess smooth line width.

- loe.lty:

  Numeric; Loess smooth line type.

- loe.col:

  Loess smooth color.

- loess.d:

  List; parameters for `loess.smooth`, e.g.,
  `list(span = 0.7, degree = 1)`.

- stats:

  Logical; if `TRUE`, displays model statistics (R², β, p-value).

- stat.size:

  Text size for `stats` plot display.

- hline:

  Numeric; location(s) of additional horizontal reference lines. Can be
  passed via the [`c()`](https://rdrr.io/r/base/c.html) function.

- vline:

  Numeric; location(s) of additional vertical reference lines. Can be
  passed via the [`c()`](https://rdrr.io/r/base/c.html) function.

- plot:

  Logical. Generates a plot if `TRUE`.

- ...:

  Additional graphical parameters (currently unused but reserved for
  future expansion).

## Value

If `reg = TRUE`, invisibly returns a list of class `"eda_lm"` with the
following components:

- residuals:

  Residuals from the model.

- a:

  Intercept.

- b:

  Slope or polynomial coefficients.

- fitted.values:

  Predicted values.

- x:

  x-values used.

- x_lab:

  X-axis label.

- parxy:

  [`par()`](https://rdrr.io/r/graphics/par.html) parameters defining
  margins. Useful if user wants to add to the plot.

If `reg = FALSE`, returns only the
[`par()`](https://rdrr.io/r/graphics/par.html) parameters defining
margins.

## Details

This function serves as a flexible plotting engine for creating
scatterplots with various enhancements useful in exploratory data
analysis.

## Examples

``` r
# Basic usage
if (FALSE) { # \dontrun{
.eda_plot_xy(mtcars, x = wt, y = mpg)

# With robust regression and 2nd-degree polynomial
.eda_plot_xy(mtcars, wt, mpg, robust = TRUE, poly = 2)

# With quantiles and loess
.eda_plot_xy(mtcars, wt, mpg, q = TRUE, loe = TRUE)
} # }
```
