# Multi-panel pooled values QQ plots

`eda_qqpool` generates multi-panel pooled values QQ plots for a
continuous variable conditioned on a grouping variable. This plot is
most frequently used to compare residuals to pooled residuals.

## Usage

``` r
eda_qqpool(
  dat,
  x,
  fac,
  p = 1L,
  tukey = FALSE,
  base = exp(1),
  q.type = 5,
  resid = TRUE,
  stat = mean,
  show.par = FALSE,
  nsim = 0,
  sim.type = "lines",
  sim.col = "coral",
  sim.alpha = 0.2,
  sim.pch = 16,
  plot = TRUE,
  grey = 0.6,
  pch = 21,
  nrow = 1,
  p.col = "grey40",
  p.fill = "grey60",
  size = 1,
  text.size = 0.8,
  tail.pch = 21,
  tail.p.col = "grey70",
  tail.p.fill = NULL,
  tic.size = 0.8,
  alpha = 0.8,
  q = FALSE,
  tails = FALSE,
  med = FALSE,
  inner = 0.75,
  title = FALSE,
  ylab = NULL,
  ...
)
```

## Arguments

- dat:

  Data frame.

- x:

  Continuous variable.

- fac:

  Categorical variable.

- p:

  Power transformation to apply to the continuous variable.

- tukey:

  Boolean determining if a Tukey transformation should be adopted
  (`FALSE` adopts a Box-Cox transformation).

- base:

  Base used with the log() function if `p = 0`.

- q.type:

  An integer between 4 and 9 selecting one of the six quantile
  algorithms (See `eda_fval` for a list of quantile algorithms).

- resid:

  Boolean determining if residuals should be plotted. Residuals are
  computed using the `stat` parameter.

- stat:

  Statistic to use if residuals are to be computed. Currently `mean`
  (default) or `median`.

- show.par:

  Boolean determining if power transformation should be displayed in the
  plot.

- nsim:

  Integer. Number of random permutations (factor label shuffles) to
  generate for constructing an empirical reference envelope across
  panels (default is `0`, which turns off simulation).

- sim.type:

  Format of the simulation display—either `"lines"` (default) or
  `"points"`.

- sim.col:

  Color of the simulated reference cloud (default is `"coral"`).

- sim.alpha:

  Transparency setting (`0` to `1`, default is `0.2`) allowing
  overlapping simulation runs to form a dense reference band.

- sim.pch:

  Plotting symbol used when `sim.type = "points"` (default is `16`).

- plot:

  Boolean determining if plot should be generated.

- grey:

  Grey level to apply to plot elements (0 to 1 with 1 = black).

- pch:

  Point symbol type.

- nrow:

  Define the number of rows for panel layout.

- p.col:

  Color for point symbol.

- p.fill:

  Point fill color passed to `bg` (Only used for `pch` ranging from
  21-25).

- size:

  Point symbol size (0-1).

- text.size:

  Size for category text above the plot.

- tail.pch:

  Tail-end point symbol type (See `tails`).

- tail.p.col:

  Tail-end color for point symbol (See `tails`).

- tail.p.fill:

  Tail-end point fill color passed to `bg` (Only used for `tail.pch`
  ranging from 21-25).

- tic.size:

  Size of tic labels (defaults to 0.8).

- alpha:

  Point transparency (0 = transparent, 1 = opaque). Only applicable if
  [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) is not used to define
  point colors.

- q:

  Boolean determining if grey box highlighting the `inner` region should
  be displayed.

- tails:

  Boolean determining if points outside of the `inner` region should be
  symbolized differently. Tail-end points are symbolized via the
  `tail.pch`, `tail.p.col` and `tail.p.fill` arguments.

- med:

  Boolean determining if median lines should be drawn.

- inner:

  Fraction of mid-values to highlight in `q` or `tails`. Defaults to the
  inner 75 percent of values.

- title:

  Title to display. If set to `TRUE`, defaults to
  `"Pooled residuals QQ plot"`. If set to `FALSE`, omits title from
  output. Custom title can also be passed to this argument.

- ylab:

  Y and X axes labels.

- ...:

  Not used

## Value

Returns a dataframe with `x` (or its residuals if `resid = TRUE`),
`fac`, the f-value (by `fac` group), and the matching pooled residuals.

## Details

The function will generate a multi-panel pooled values QQ plots. The
function defaults to a pooled residuals QQ plots which is the most
common use for this kind of plot.\
\
To assess whether observed departures in individual group panels reflect
genuine group-specific distributional shifts rather than natural
within-group sampling variability, `eda_qqpool()` includes an optional
permutation simulation routine triggered by setting `nsim > 0`.

## References

- William S. Cleveland. Visualizing data. (1993)

## Examples

``` r

# Default output
singer <- lattice::singer
eda_qqpool(singer, height, voice.part, nrow = 2)


# Pooled QQ plot with a 50-run permutation reference line envelope
eda_qqpool(singer, height, voice.part, nrow = 2, nsim = 100)

```
