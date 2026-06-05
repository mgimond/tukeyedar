# Calculate quantile f-values

Calculates Fractional Values (f-Values).

## Usage

``` r
eda_fval(x, q.type = 5)
```

## Arguments

- x:

  Vector whose f-values are to be computed.

- q.type:

  An integer specifying the algorithm to use for computing F-values.

## Value

A numeric vector of the same length as `x`, containing the f-values for
each input value. The order of the returned f-values matches the input
vector.

## Details

This function computes the fractional value (f-value) for each element
in a numeric vector `x`. The f-value provides a measure of the position
of a data point relative to the rest of the data, scaled to the range
\[0,1\]. This fraction is sometimes reported as the probability or
cumulative frequency.  
  
Different algorithms are used to compute the f-value. `eda_fval` offers
as optional algorithm type `4` through `9` documented in
[`stats::quantile`](https://rdrr.io/r/stats/quantile.html). The
algorithms used are:  
  

- `4`: f = i / n,

- `5`: f = (i - 0.5) / n,

- `6`: f = i / (n + 1),

- `7`: f = (i - 1) / (n - 1),

- `8`: f = (i - 1/3) / (n + 1/3),

- `9`: f = (i - 3/8) / (n + 1/4).

Where f is the fraction of values that lies below index i, and n is the
total number values.

## References

- John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
  Graphical Methods for Data Analysis (1983)

## See also

- [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) for
  quantile calculations

- [`eda_qq`](https://mgimond.github.io/tukeyedar/reference/eda_qq.md)
  for QQ plots

## Examples

``` r

 set.seed(321)
 z <- round(runif(10, 1, 20))
 z
#>  [1] 19 19  6  6  8  7 10  7 10 16

 # William Cleveland's f-values algorithm
 eda_fval(z)
#>  [1] 0.85 0.95 0.05 0.15 0.45 0.25 0.55 0.35 0.65 0.75

 # Algorithm used by the stats::quantile() function
 eda_fval(z, q.type = 7)
#>  [1] 0.8888889 1.0000000 0.0000000 0.1111111 0.4444444 0.2222222 0.5555556
#>  [8] 0.3333333 0.6666667 0.7777778
```
