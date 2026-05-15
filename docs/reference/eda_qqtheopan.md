# Multi-panel theoretical QQ plots

`eda_qqmulti` Generates multi-panel theoretical QQ plots for a
continuous variable conditioned on a grouping variable.

## Usage

``` r
eda_qqtheopan(
  dat,
  x,
  fac,
  p = 1L,
  tukey = FALSE,
  q.type = 5,
  dist = "norm",
  dist.l = list(),
  ylim = NULL,
  resid = FALSE,
  stat = mean,
  show.par = FALSE,
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
  tic.size = 0.7,
  alpha = 0.8,
  q = FALSE,
  tails = FALSE,
  med = FALSE,
  inner = 0.75,
  iqr = TRUE,
  title = FALSE,
  xlab = NULL,
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

  Boolean determining if a Tukey transformation should be adopted (FALSE
  adopts a Box-Cox transformation).

- q.type:

  An integer between 1 and 9 selecting one of the nine quantile
  algorithms (See `quantile` function). Used for the fitting the line to
  the points and for computing the boundaries for the shaded region.

- dist:

  Theoretical distribution to use. Defaults to Normal distribution.

- dist.l:

  List of parameters passed to the distribution quantile function.

- ylim:

  Y axes limits.

- resid:

  Boolean determining if residuals should be plotted. Residuals are
  computed using the `stat` parameter.

- stat:

  Statistic to use if residuals are to be computed. Currently `mean`
  (default) or `median`.

- show.par:

  Boolean determining if power transformation should be displayed in the
  plot.

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
  inner 75% of values.

- iqr:

  Boolean determining if an IQR line should be fitted to the points.

- title:

  Title to display. If set to `TRUE`, defaults to `"Normal QQ plot"`. If
  set to `FALSE`, omits title from output. Custom title can also be
  passed to this argument.

- xlab:

  X-axis label.

- ylab:

  Y-axis label.

- ...:

  Not used

## Value

Returns a list with the following components:

- `data`: List with input `x` and `y` values for each group. May be
  interpolated to smallest quantile batch if batch sizes don't match.
  Values will reflect power transformation defined in `p`

## Details

The function will generate a multi-panel theoretical QQ plot. Currently,
only the Normal QQ plot (`dist="norm"`), exponential QQ plot
(`dist="exp"`), and the uniform QQ plot (`dist="unif"`) are supported.

## References

- William S. Cleveland. Visualizing data. (1993)

## Examples

``` r

# Default output
singer <- lattice::singer
eda_qqtheopan(singer, height, voice.part)

# Split into two rows
eda_qqtheopan(singer, height, voice.part, nrow = 2, title = TRUE)

# Compare to a uniform distribution
eda_qqtheopan(singer, height, voice.part, nrow = 2, dist = "unif")

# A uniform QQ plot is analogous to a Q(f) plot
eda_qqtheopan(singer, height, voice.part, nrow = 2, dist = "unif",
              iqr = FALSE, xlab = "f-value")

# Normal QQ plots of Waterville daily averages. Mean monthly values are
# subtracted from the data to recenter all batches around 0.  Color and point
# symbols are used to emphasize the inner core of the data (here set to the
# inner 80% of values)
wat <- tukeyedar::wat05
wat$month <- format(wat$date,"%b")
eda_qqtheopan(wat,avg, month, resid = TRUE, nrow = 4, inner = 0.8 ,
                    tails = TRUE, tail.pch = 3, p.fill = "coral")
```
