# Theoretical QQ plot

`eda_theo` generates a theoretical QQ plot for common distributions
(Normal, Exponential, Uniform, Gamma, Chi-Squared, Weibull) with an
optional null reference simulation envelope.

## Usage

``` r
eda_theo(
  x,
  p = 1L,
  tukey = FALSE,
  base = exp(1),
  q.type = 5,
  dist = "norm",
  dist.l = list(),
  resid = FALSE,
  stat = mean,
  nsim = 0,
  sim.alpha = 0.2,
  sim.col = "coral",
  sim.pch = 16,
  sim.type = "points",
  plot = TRUE,
  show.par = TRUE,
  grey = 0.6,
  pch = 21,
  p.col = "grey50",
  p.fill = "grey80",
  size = 1,
  alpha = 0.8,
  med = TRUE,
  q = FALSE,
  qcol = rgb(0, 0, 0, 0.05),
  iqr = TRUE,
  grid = FALSE,
  tails = FALSE,
  inner = 0.75,
  tail.pch = 21,
  tail.p.col = "grey70",
  tail.p.fill = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  t.size = 1.2,
  ...
)
```

## Arguments

- x:

  Vector of continuous values.

- p:

  Power transformation to apply to `x`.

- tukey:

  Boolean determining if a Tukey transformation should be adopted (FALSE
  adopts a Box-Cox transformation).

- base:

  Base used with the log() function if `p = 0`.

- q.type:

  An integer between 4 and 9 selecting one of the nine quantile
  algorithms. (See the
  [`eda_fval`](https://mgimond.github.io/tukeyedar/reference/eda_fval.md)
  function).

- dist:

  Choice of theoretical distribution.

- dist.l:

  List of parameters passed to the distribution quantile function.

- resid:

  Boolean determining if residuals should be plotted. Residuals are
  computed using the `stat` parameter.

- stat:

  Statistic to use if residuals are to be computed. Currently `mean`
  (default) or `median`.

- nsim:

  Integer. Number of random draws to generate for constructing the null
  reference envelope. Default is `0` (off).

- sim.alpha:

  Numeric. Transparency value (`0` to `1`) for simulated cloud. Default
  `0.2`.

- sim.col:

  Character. Color for simulated cloud. Default `"coral"`.

- sim.pch:

  Integer or character. Plotting symbol for simulated points. Default
  `16`.

- sim.type:

  Character. Format of simulation display: `"points"` or `"lines"`.

- plot:

  Boolean determining if plot should be generated.

- show.par:

  Boolean determining if parameters such as power transformation or
  formula should be displayed.

- grey:

  Grey level to apply to plot elements (0 to 1 with 1 = black).

- pch:

  Point symbol type.

- p.col:

  Color for point symbol.

- p.fill:

  Point fill color passed to `bg` (Only used for `pch` ranging from
  21-25).

- size:

  Point size (0-1)

- alpha:

  Point transparency (0 = transparent, 1 = opaque). Only applicable if
  [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) is not used to define
  point colors.

- med:

  Boolean determining if median lines should be drawn.

- q:

  Boolean determining if `inner` data region should be shaded.

- qcol:

  Fill color of inner quantile box.

- iqr:

  Boolean determining if an IQR line should be fitted to the points.

- grid:

  Boolean determining if a grid should be added.

- tails:

  Boolean determining if points outside of the `inner` region should be
  symbolized differently. Tail-end points are symbolized via the
  `tail.pch`, `tail.p.col` and `tail.p.fill` arguments.

- inner:

  Fraction of the data considered as "mid values". Defaults to 75%. Used
  to define shaded region boundaries, `q`, or to identify which of the
  tail-end points are to be symbolized differently, `tails`.

- tail.pch:

  Tail-end point symbol type (See `tails`).

- tail.p.col:

  Tail-end color for point symbol (See `tails`).

- tail.p.fill:

  Tail-end point fill color passed to `bg` (Only used for `tail.pch`
  ranging from 21-25).

- xlab:

  X label for output plot.

- ylab:

  Y label for output plot.

- title:

  Title to add to plot.

- t.size:

  Title size.

- ...:

  Not used

## Value

A dataframe with the input vector elements and matching theoretical
quantiles. Any transformation applied to `x` is reflected in the output.

## Details

The function generates a theoretical QQ plot. Currently, only the Normal
QQ plot (`dist="norm"`), exponential QQ plot (`dist="exp"`), uniform QQ
plot (`dist="unif"`), gamma QQ plot (`dist="gamma"`), chi-squared QQ
plot (`dist="chisq"`), and the Weibull QQ plot (`dist="weibull"`) are
supported. By default, the Normal QQ plot maps the unit Normal quantiles
to the x-axis (i.e. centered on a mean of 0 and standard deviation of 1
unit).\
\
Note that arguments can be passed to the respective quantile functions
via the `d.list` argument. Some quantile functions require at least one
argument. For example, the `qgamma` function requires that the shape
parameter be specified and the `qchisq` function requires that the
degrees of freedom, `df`, be specified. See examples.\
\
To help evaluate whether departures from the theoretical reference line
reflect true non-conformance to the target distribution rather than
natural sampling variability, setting `nsim > 0` generates an empirical
null envelope. Under the null hypothesis that the data arise from the
specified theoretical distribution, synthetic samples of size \\N\\ are
repeatedly drawn and scaled to match the sample's quantiles. The
resulting simulation runs are plotted behind the observed data with
alpha transparency (`sim.alpha`). If the observed points remain inside
the concentration of the simulated cloud, the apparent deviations are
consistent with random sampling variation for a sample of size \\N\\.

## References

- John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
  Graphical Methods for Data Analysis (1983)

- [Quantile-Quantile plot
  article](https://mgimond.github.io/tukeyedar/articles/qq.md)

## Examples

``` r

 singer <- lattice::singer
 bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )

 # Generate a normal QQ plot
 eda_theo(bass2)


 # Normal QQ plot with 20 null reference simulations
 eda_theo(bass2, nsim = 20)


 # Normal QQ plot with simulation envelope rendered as spaghetti lines
 eda_theo(bass2, nsim = 50, sim.type = "lines")


 # Generate a chi-squared QQ plot. The distribution requires that the degrees
 # of freedom be specified. The inner 70% shaded region is added.
 set.seed(271); x <- rchisq(100, df =3)
 eda_theo(x, dist = "chisq", dist.l = list(df = 3), q = TRUE)


 # Generate a gamma QQ plot. Note that gamma requires at the very least the
 # shape parameter. The chi-squared distribution is a special case of the
 # gamma distribution where shape = df/2 and rate = 1/2.
 eda_theo(x, dist = "gamma", dist.l = list(shape = 3/2, rate = 1/2), q = TRUE)


 # Generate a uniform QQ plot for a Normally derived variable
 set.seed(218)
 x <- rnorm(60)
 eda_theo(x, dist = "unif")


 # Compare x to null distribution envelope
 eda_theo(x, dist = "unif", nsim=40)

 eda_theo(x, dist = "unif", nsim=50, sim.type="lines")


 # Do the tails deviate much from a theoretical Normal distribution?
 eda_theo(x, dist = "norm", nsim=50, sim.type="lines")


 # The uniform QQ plot can double as a quantile plot
 eda_theo(bass2, dist = "unif", q = FALSE, med = FALSE,
          iqr = FALSE, grid = TRUE, xlab = "f-value")
```
