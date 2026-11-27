# Residual-Fit Spread Plot

`eda_rfs` generates a Cleveland residual-fit spread plot with an
optional null reference permutation envelope.

## Usage

``` r
eda_rfs(
  dat,
  x = NULL,
  grp = NULL,
  p = 1L,
  tukey = FALSE,
  base = exp(1),
  nsim = 0,
  sim.alpha = 0.2,
  sim.col = "coral",
  sim.pch = 16,
  sim.type = "points",
  show.par = TRUE,
  stat = mean,
  grey = 0.7,
  pch = 21,
  p.col = "grey50",
  p.fill = "grey80",
  inner = 0.9,
  q = FALSE,
  qtype = 5,
  size = 0.8,
  alpha = 0.7,
  ylim = NULL,
  bar = FALSE,
  ...
)
```

## Arguments

- dat:

  Univariate data frame or model object (`lm`, `eda_lm`, or
  `eda_rline`).

- x:

  Continuous variable (if `dat` is a data frame).

- grp:

  Grouping variable (if `dat` is a data frame).

- p:

  Power transformation to apply to `x`. Applies to univariate scenario
  only.

- tukey:

  Logical determining if a Tukey transformation should be adopted.
  Applies to univariate scenario only.

- base:

  Base used with [`log()`](https://rdrr.io/r/base/Log.html) if `p = 0`.
  Applies to univariate scenario only.

- nsim:

  Integer. Number of random permutations to generate for constructing
  null reference envelopes. Default is `0` (off).

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

- show.par:

  Logical determining if transformation parameters are displayed.

- stat:

  Summary statistic function (`mean` or `median`).

- grey:

  Grey level for plot elements (0 to 1).

- pch:

  Point symbol type.

- p.col:

  Color for point symbol.

- p.fill:

  Point fill color.

- inner:

  Fraction of input data considered as "mid values" (default 0.9).

- q:

  Logical determining if inner quantile region should be shaded.

- qtype:

  Integer specifying quantile algorithm type (1-9).

- size:

  Point size.

- alpha:

  Point transparency.

- ylim:

  Y-axis limits.

- bar:

  Logical determining if a proportion-of-spread stacked bar is added.

- ...:

  Additional arguments.

## Details

The `eda_rfs` function generates a residual-fit spread plot for
univariate and bivariate data. Input can be a dataframe with one column
storing the continuous variable and another column storing the
categorical (grouping) variable or, for a bivariate dataset, a model
output from an [`lm()`](https://rdrr.io/r/stats/lm.html),
[`eda_lm()`](https://mgimond.github.io/tukeyedar/reference/eda_lm.md) or
[`eda_rline()`](https://mgimond.github.io/tukeyedar/reference/eda_rline.md)
function.\
\
The `stat` argument only applies to univariate data and allows the user
to choose the summary statistic to fit to the data (either mean or
median). This statistic is also used to recenter the fitted values in
the rfs plot.\
\
The `q` argument, when set to `TRUE`, will add a shaded region to the
residual quantile plot highlighting the mid portion of the data defined
by the `inner` argument (set to 90 percent of the mid values, by
default). The range defined by the mid portion of the data is
highlighted in the left plot for comparison with the the full range
defined by the fitted values.\
\
The `bar` option, when set to `TRUE`, adds a narrow stacked barplot that
compares the spread covered by the residuals (red bar) with the spread
covered by the fitted values (green bar). The residual spread is
computed for the portion of the residuals defined by the `inner`
argument. The values outputted in the console are those used in
computing the vertical bars. The red bar is the relative spread of the
residuals and the green bar is the relative spread of the fitted values.
The stacked bar plot can be helpful in quickly gauging the effect the
fitted values have in explaining the variability in the data. The longer
the green bar relative to the red bar, the greater the grouping
variable's (for univariate data) or linear model's (for bivariate data)
effect in minimizing the uncertainty in the estimated value.\
\
Setting `nsim > 0` generates an empirical null envelope for both the
fitted values and residual panels. For univariate grouping models, group
labels are repeatedly permuted across observations using
[`sample`](https://rdrr.io/r/base/sample.html). For linear models,
response values are permuted relative to predictors. Comparing observed
curves to the null clouds allows direct visual assessment of whether the
model explains variation beyond random chance.

## References

- William S. Cleveland. Visualizing Data. Hobart Press (1993)

- [Residual-fit spread
  article](https://mgimond.github.io/tukeyedar/articles/rfs.md)

## Examples

``` r

# Generate a basic residual-fit spread plot
eda_rfs(mtcars,mpg, cyl)
#> The mid 90.0% of residuals covers about 10.69 units.
#> The fitted values cover a range of 11.56 units.
#> Relative spread breakdown: 52.0% explained (fitted) vs 48.0% unexplained (residuals).


# Add inner 90% region to the residuals plot (shaded area in plot)
# Vertical grey box shows matching y-values
eda_rfs(mtcars,mpg, cyl, q = TRUE)
#> The mid 90.0% of residuals covers about 10.69 units.
#> The fitted values cover a range of 11.56 units.
#> Relative spread breakdown: 52.0% explained (fitted) vs 48.0% unexplained (residuals).


# Change guide to encompass mid 75% of residual values
eda_rfs(mtcars,mpg, cyl, q = TRUE, inner = 0.75)
#> The mid 75.0% of residuals covers about 8.02 units.
#> The fitted values cover a range of 11.56 units.
#> Relative spread breakdown: 59.1% explained (fitted) vs 40.9% unexplained (residuals).


# Use median instead of the mean to compute group summaries and to
# recenter the fitted values around 0.
eda_rfs(mtcars,mpg, cyl, stat = median)
#> The mid 90.0% of residuals covers about 10.98 units.
#> The fitted values cover a range of 10.80 units.
#> Relative spread breakdown: 49.6% explained (fitted) vs 50.4% unexplained (residuals).


# Apply power transformation of -1 to mpg. Defaults to box-cox method.
eda_rfs(mtcars,mpg, cyl, p = -1)
#> The mid 90.0% of residuals covers about 0.04 units.
#> The fitted values cover a range of 0.03 units.
#> Relative spread breakdown: 42.5% explained (fitted) vs 57.5% unexplained (residuals).


# Display a stacked bar plot showing relative importance in spreads
# between fitted values and residuals.
eda_rfs(mtcars,mpg, cyl, bar = TRUE)
#> The mid 90.0% of residuals covers about 10.69 units.
#> The fitted values cover a range of 11.56 units.
#> Relative spread breakdown: 52.0% explained (fitted) vs 48.0% unexplained (residuals).


# Generate rfs plot for bivariate model output. Model can be generated from
# lm(), eda_lm() or eda_rline()
M1 <- lm(hp ~ mpg, mtcars)
eda_rfs(M1,q =TRUE)
#> The mid 90.0% of residuals covers about 123.58 units.
#> The fitted values cover a range of 207.50 units.
#> Relative spread breakdown: 62.7% explained (fitted) vs 37.3% unexplained (residuals).


M2 <- eda_lm(mtcars, mpg, hp)

#>        int      mpg^1 
#> 324.082314  -8.829731 
eda_rfs(M2,q =TRUE)
#> The mid 90.0% of residuals covers about 123.58 units.
#> The fitted values cover a range of 207.50 units.
#> Relative spread breakdown: 62.7% explained (fitted) vs 37.3% unexplained (residuals).


M3 <- eda_rline(mtcars, mpg, hp)
eda_rfs(M3, q =TRUE)
#> The mid 90.0% of residuals covers about 125.51 units.
#> The fitted values cover a range of 252.28 units.
#> Relative spread breakdown: 66.8% explained (fitted) vs 33.2% unexplained (residuals).


# Add simulaton envelopes to plots
eda_rfs(M1,q =TRUE,nsim=60, sim.type="lines")
#> The mid 90.0% of residuals covers about 123.58 units.
#> The fitted values cover a range of 207.50 units.
#> Relative spread breakdown: 62.7% explained (fitted) vs 37.3% unexplained (residuals).

eda_rfs(mtcars,mpg, cyl, nsim=60)
#> The mid 90.0% of residuals covers about 10.69 units.
#> The fitted values cover a range of 11.56 units.
#> Relative spread breakdown: 52.0% explained (fitted) vs 48.0% unexplained (residuals).

```
