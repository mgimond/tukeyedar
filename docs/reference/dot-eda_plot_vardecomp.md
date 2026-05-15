# Variability decomposition plots

This is an internal helper function used to generate plots that
visualize the effects and residuals from the analysis of n-way tables or
a regression model. The function is not intended for direct use by the
end-user.

## Usage

``` r
.eda_plot_vardecomp(
  dat,
  response,
  type = "boxpnt",
  input = "nway",
  eff = NULL,
  rotate = FALSE,
  padding = 0.1,
  show.resp = FALSE,
  outliers = TRUE,
  label = FALSE,
  order = TRUE,
  cex.txt = 1,
  lim = NULL,
  overlap = c("stack", "overplot", "jitter"),
  pch = 16,
  p.col = "grey50",
  p.fill = "grey80",
  size = 1,
  alpha = 0.5,
  grey = 0.6,
  title = "Variability decomposition",
  ...
)
```

## Arguments

- dat:

  A data frame in long form, containing the data to be plotted.

- response:

  A character string specifying the name of the response variable column
  in the `dat` data frame.

- type:

  A character string specifying the type of plot to generate. Must be
  either `"boxpnt"` or `"box"`.

- input:

  A character string. `"reg"` = bivariate model input. `"nway"` =
  univariate model or N-way table input.

- eff:

  A list of effect values. Required when `input = "nway"`.

- rotate:

  Logical. If `TRUE`, rotates the plot orientation.

- padding:

  Numeric. Controls padding for plot limits.

- show.resp:

  Logical. If `TRUE`, includes a boxplot for the response variable.

- outliers:

  Logical. If `TRUE`, outliers are displayed in boxplots.

- label:

  Logical. Controls whether labels are displayed.

- order:

  Logical. Controls ordering (likely of factors or effects).

- cex.txt:

  Numeric. Controls text size.

- lim:

  Numeric. Explicit limits for the plot axes.

- overlap:

  Character. Specifies how to handle overlapping points, must be one of
  `"stack"`, `"overplot"`, or `"jitter"`.

- pch:

  Point symbol type. Only applicable if `type = "boxpnt"`.

- p.col:

  Point border color. Only applicable if `type = "boxpnt"`.

- p.fill:

  Point fill color. Only applicable if `type = "boxpnt"`.

- size:

  Point size. Only applicable if `type = "boxpnt"`.

- alpha:

  Transparency level for points (0 = transparent, 1 = opaque).

- grey:

  Numeric. Controls grayscale coloring for plot elements and axes.

- title:

  Plot title. If title is to be omitted, set to `NULL`.

- ...:

  Additional arguments passed to underlying plotting functions.

## Value

Primarily called for producing a plot.
