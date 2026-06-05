# Ladder of powers transformation on bivariate data with three-point summary plot

Re-expresses a vector on the ladder of powers. Requires
[`eda_3pt()`](https://mgimond.github.io/tukeyedar/reference/eda_3pt.md)
function.

## Usage

``` r
eda_bipow(
  dat,
  x,
  y,
  p = c(-1, 0, 0.5, 1, 2),
  tukey = FALSE,
  base = exp(1),
  ...
)
```

## Arguments

- dat:

  Dataframe.

- x:

  Variable assigned to the x axis.

- y:

  Variable assigned to the y axis.

- p:

  Vector of powers.

- tukey:

  If set to TRUE, then adopt Tukey's power transformation. If FALSE,
  adopt Box-Cox transformation.

- base:

  Base used with the [`log()`](https://rdrr.io/r/base/Log.html) function
  if `p = 0`.

- ...:

  Other parameters passed to the graphics::plot function.

## Value

No return value

## Details

Generates a matrix of scatter plots and boxplots of various
re-expressions of both x and y values. The 3-point summary and
associated half-slopes are also plotted (this function makes use of the
eda_3pt function). The values are re-expressed using either the Tukey
power transformation or the Box-Cox transformation (the default). See
`eda_re` for more information on these transformation techniques. Axes
labels are omitted to reduce plot clutter.

## References

- Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley.

## Examples

``` r

data(cars)
# Example 1
eda_bipow(dat = cars, x = speed, y = dist)

# Custom powers
eda_bipow(dat = cars, x = speed, y = dist, p = c(-1, -0.5, 0, 0.5, 1))

# Adopt Tukey transformation
eda_bipow(dat = cars, x = speed, y = dist, tukey = TRUE, p = c(-1, -0.5, 0, 0.5, 1))

```
