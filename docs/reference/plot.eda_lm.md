# Residuals plot

Generate residuals vs fitted or residuals vs dependence plot from an
`eda_lm` class object.

## Usage

``` r
# S3 method for class 'eda_lm'
plot(x, plot = "rd", xlab = NULL, ylab = NULL, loe = TRUE, sd = FALSE, ...)
```

## Arguments

- x:

  Object of class `eda_lm`.

- plot:

  Type of residuals plot. Choice between residuals-fit (`rf`) and
  residuals-dependence (`rd`) plots.

- xlab:

  X label for output plot.

- ylab:

  Y label for output plot.

- loe:

  Logical; whether to plot loess smooth line.

- sd:

  Logical; whether to show ±1 SD lines.

- ...:

  Arguments passed on to
  [`.eda_plot_xy`](https://mgimond.github.io/tukeyedar/reference/dot-eda_plot_xy.md)

  `dat`

  :   Optional data frame containing `x` and `y`.

  `y`

  :   A numeric vector or column name in `dat` for the y-axis.

  `px`

  :   Power transformation used in the input data to display if
      `show.par = TRUE`.

  `py`

  :   Power transformation used in the input data to display if
      `show.par = TRUE`.

  `base`

  :   Base used with the log() function if `px` or `py` is `0`.

  `tukey`

  :   Boolean determining if a Tukey transformation should be adopted
      (FALSE adopts a Box-Cox transformation).

  `raw_tick`

  :   Logical. If `TRUE`, original (untransformed) equally spaced tick
      values are displayed on the re-expressed axes.

  `xlim`

  :   X-axis range.

  `ylim`

  :   Y-axis range.

  `show.par`

  :   Logical; whether to display plot parameter summary on the plot.
      Currently only applies to regression model input.

  `reg`

  :   Logical; whether to fit and display a regression line.

  `poly`

  :   Integer; regression model polynomial degree (defaults to 1 for
      linear model).

  `robust`

  :   Logical; if `TRUE`, uses robust regression
      ([`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html)).

  `rlm.d`

  :   List; parameters for
      [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html), (e.g.,
      `list(psi = "psi.bisquare")`).

  `w`

  :   Optional numeric vector of weights for regression.

  `lm.col`

  :   Regression line color.

  `lm.lw`

  :   Numeric; Regression line width.

  `lm.lty`

  :   Numeric; Regression line type.

  `mean.l`

  :   Logical; whether to show x and y mean reference lines.

  `asp`

  :   Logical; whether to preserve the aspect ratio (ignored if
      `square = FALSE`).

  `square`

  :   Logical; whether to create a square plotting window.

  `grey`

  :   Numeric between `0-1`; controls grayscale background elements
      (`0 = black`, `1 = white`).

  `pch`

  :   Integer; point symbol.

  `p.col`

  :   Point border color.

  `p.fill`

  :   Point fill color.

  `size`

  :   Point size.

  `alpha`

  :   Point transparency level (0 = 100\\ transparent, 1 = 100\\
      opaque).

  `q`

  :   Logical; whether to draw inner quantile boxes (quantile shading).

  `q.type`

  :   Integer; type of quantile calculation (see `quantile`).

  `inner`

  :   Numeric; defines the inner fraction of values to highlight with
      quantile shading.

  `qcol`

  :   Fill color of quantile shading.

  `loe.lw`

  :   Numeric; Loess smooth line width.

  `loe.col`

  :   Loess smooth color.

  `loe.lty`

  :   Numeric; Loess smooth line type.

  `loess.d`

  :   List; parameters for `loess.smooth`, e.g.,
      `list(span = 0.7, degree = 1)`.

  `stats`

  :   Logical; if `TRUE`, displays model statistics (R², β, p-value).

  `stat.size`

  :   Text size for `stats` plot display.

  `hline`

  :   Numeric; location(s) of additional horizontal reference lines. Can
      be passed via the [`c()`](https://rdrr.io/r/base/c.html) function.

  `vline`

  :   Numeric; location(s) of additional vertical reference lines. Can
      be passed via the [`c()`](https://rdrr.io/r/base/c.html) function.

## Value

Returns the margins (`mar`) used to generate the plot via the
[`par()`](https://rdrr.io/r/graphics/par.html) function. This parameter
can be set with a subsequent call to
[`par()`](https://rdrr.io/r/graphics/par.html) if additional elements
are to be added to the plot.

## Details

The function generates a scatter plot of residuals vs dependence or
residuals vs fitted values plot from a model of class `eda_lm`. A loess
line is fitted to the data. By default, a robust loess is adopted using
the `"symmetric"` family.

## Examples

``` r
M1  <- eda_lm(mtcars, hp, mpg)

#>         int        hp^1 
#> 30.09886054 -0.06822828 

# Residual-dependence plot
plot(M1)


# Residual-fit plot
plot(M1, plot = "rf")
```
