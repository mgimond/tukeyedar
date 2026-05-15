# Median polish of two-way tables

`eda_pol` Polishes two-way tables using median, means, or any
customizable functions.

## Usage

``` r
eda_pol(
  x,
  row = NULL,
  col = NULL,
  val = NULL,
  stat = median,
  plot = TRUE,
  eps = 0.01,
  maxiter = 5,
  sort = FALSE,
  p = 1,
  tukey = FALSE,
  offset = 1e-05,
  col.quant = FALSE,
  colpal = "RdYlBu",
  adj.mar = TRUE,
  res.size = 1,
  row.size = 1,
  col.size = 1,
  res.txt = TRUE,
  label.txt = TRUE,
  base = exp(1),
  ...
)
```

## Arguments

- x:

  A three column data frame.

- row:

  Name of column assigned to the row effect.

- col:

  Name of column assigned to the column effect.

- val:

  Name of column assigned to the response variable.

- stat:

  Polishing statistic (default is median).

- plot:

  Boolean determining if an output plot should be generated.

- eps:

  Convergence tolerance parameter.

- maxiter:

  Maximum number of iterations.

- sort:

  Boolean determining if the effects row/columns should be sorted.

- p:

  Re-expression power parameter.

- tukey:

  Boolean determining if Tukey's power transformation should used. If
  FALSE, the Box-Cox transformation is adopted.

- offset:

  Offset to add to values if at leat one value is 0 and the power is
  negative.

- col.quant:

  Boolean determining if a quantile classification scheme should be
  used.

- colpal:

  Color palette to adopt.

- adj.mar:

  Boolean determining if margin width needs to accomodate labels.

- res.size:

  Size of residual values in plot `[0-1]`.

- row.size:

  Size of row effect values in plot `[0-1]`.

- col.size:

  Size of column effect values in plot `[0-1]`.

- res.txt:

  Boolean determining if values should be added to plot.

- label.txt:

  Boolean determining if margin and column labels should be plotted.

- base:

  Base used with the log() function if `p` is `0`.

- ...:

  Not used

## Value

A list of class `eda_polish` with the following named components:

- `input` The median polish residuals with three columns: Column levels,
  row levels and residual values.

- `wide` The median polish residuals table in wide form.

- `row` Row effects table

- `col` Column effects table

- `global` Overall value (common value)

- `iter` Number of iterations before polish stabilizes.

- `long` Table of residuals, row effects, column effects and CV values
  in long form.

- `power` Transformation power applied to values prior to polishing.

- `IQ_row` Ratio between interquartile row effect values and 80th
  quantile of residuals.

- `IQ_col` Ratio between interquartile column effect values and 80th
  quantile of residuals.

## Details

The function performs a polish on a two way table. By default, it
applies a median polish, but other statistical summaries such as the
mean can be passed to the function via the `stat = ` argument. The
function returns a list of row/column effects along with global and
residual values. It will also generate a colored table if
`plot = TRUE`.  
For an N-way table with more than two factors, see
[`eda_npol`](https://mgimond.github.io/tukeyedar/reference/eda_npol.md).

## References

- Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985).
  Exploring data tables, trends, and shapes.

- Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley

- [Median polish
  article](https://mgimond.github.io/tukeyedar/articles/polish.md)

## Examples

``` r
M <- eda_pol(inf_mort, row = region, col = edu, val = perc)
```
