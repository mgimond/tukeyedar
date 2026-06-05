# Variability decomposition plot

Generate plots that visualize the fit/effect and residuals from `eda_*`
objects.

## Usage

``` r
eda_vd(
  dat,
  y = NULL,
  x = NULL,
  stat = median,
  p = 1L,
  tukey = FALSE,
  base = exp(1),
  ...
)
```

## Arguments

- dat:

  A model of type `eda_pol`, `eda_npol`, `eda_lm`, or `lm`, or a
  dataframe with a response variable, `y`, and a categorical variable,
  `x`.

- y:

  response (continuous) variable if `dat` is a dataframe, `NULL`
  otherwise.

- x:

  categorical variable if `dat` is a dataframe, `NULL` otherwise.

- stat:

  statistical function used to fit `y` by `x` if `dat` is a dataframe,
  ignored otherwise.

- p:

  Power transformation to apply to univariate data. Ignored if `dat` is
  not a dataframe.

- tukey:

  Boolean determining if a Tukey transformation should be adopted (TRUE)
  or if a Box-Cox transformation should be adopted (FALSE). Ignored if
  `dat` is not a dataframe.

- base:

  Base used with the [`log()`](https://rdrr.io/r/base/Log.html) function
  if `p = 0`.

- ...:

  Arguments passed on to
  [`.eda_plot_vardecomp`](https://mgimond.github.io/tukeyedar/reference/dot-eda_plot_vardecomp.md)

  `response`

  :   A character string specifying the name of the response variable
      column in the `dat` data frame.

  `type`

  :   A character string specifying the type of plot to generate. Must
      be either `"boxpnt"` or `"box"`.

  `input`

  :   A character string. `"reg"` = bivariate model input. `"nway"` =
      univariate model or N-way table input.

  `eff`

  :   A list of effect values. Required when `input = "nway"`.

  `rotate`

  :   Logical. If `TRUE`, rotates the plot orientation.

  `padding`

  :   Numeric. Controls padding for plot limits.

  `show.resp`

  :   Logical. If `TRUE`, includes a boxplot for the response variable.

  `outliers`

  :   Logical. If `TRUE`, outliers are displayed in boxplots.

  `label`

  :   Logical. Controls whether labels are displayed.

  `order`

  :   Logical. Controls ordering (likely of factors or effects).

  `cex.txt`

  :   Numeric. Controls text size.

  `lim`

  :   Numeric. Explicit limits for the plot axes.

  `overlap`

  :   Character. Specifies how to handle overlapping points, must be one
      of `"stack"`, `"overplot"`, or `"jitter"`.

  `pch`

  :   Point symbol type. Only applicable if `type = "boxpnt"`.

  `p.col`

  :   Point border color. Only applicable if `type = "boxpnt"`.

  `p.fill`

  :   Point fill color. Only applicable if `type = "boxpnt"`.

  `size`

  :   Point size. Only applicable if `type = "boxpnt"`.

  `alpha`

  :   Transparency level for points (0 = transparent, 1 = opaque).

  `grey`

  :   Numeric. Controls grayscale coloring for plot elements and axes.

  `title`

  :   Plot title. If title is to be omitted, set to `NULL`.

## Value

A plot

## Examples

``` r
# Compare regression model residuals to fit
M0 <- lm(mpg ~ hp + cyl, mtcars)
eda_vd(M0)


# By default, points sharing a identical value are "stacked"
# To jitter:
eda_vd(M0, overlap = "jitter")


# To overplot:
eda_vd(M0, overlap = "overplot")


# To represent fit using a boxplot
eda_vd(M0, type = "box")


# Decompose variability in response variable for a univariate dataset.
# Add labels to each level.
eda_vd(chickwts, weight, feed, label = TRUE)


# Add response variable (bisque colored boxplot)
eda_vd(chickwts, weight, feed, label = TRUE, show.resp = TRUE)

```
