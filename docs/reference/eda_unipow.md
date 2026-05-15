# Ladder of powers transformation on a single vector

`eda_unipow` re-expresses a vector on the ladder of powers and plots the
results using a histogram and a density function. Either the Tukey or
Box-Cox transformation is used in computing the re-expressed values.

## Usage

``` r
eda_unipow(
  x,
  p = c(2, 1, 1/2, 0.33, 0, -0.33, -1/2, -1, -2),
  tukey = FALSE,
  base = exp(1),
  bins = 5,
  cex.main = 1.3,
  col = "#DDDDDD",
  border = "#AAAAAA",
  title = "Re-expressed data via ladder of powers",
  ...
)
```

## Arguments

- x:

  Vector

- p:

  Vector of powers

- tukey:

  If TRUE, apply Tukey's power transformation. If FALSE adopt Box-Cox
  transformation

- base:

  Base used with the [`log()`](https://rdrr.io/r/base/Log.html) function
  if `p = 0`.

- bins:

  Number of histogram bins

- cex.main:

  Histogram title size (assigned to each histogram plot)

- col:

  Histogram fill color

- border:

  Histogram border color

- title:

  Overall plot title (set to NULL for no title)

- ...:

  Other parameters passed to the graphics::hist function.

## Value

No return value

## Details

The output is a lattice of descriptive plots showing the transformed
data across different powers.  

## References

- Tukey, John W. 1977. Exploratory Data Analysis. Addison-Wesley.

## Examples

``` r
data(mtcars)
eda_unipow(mtcars$mpg, bins=6, tukey = TRUE)
```
