# Generate Data with Specific Skewness and Kurtosis

This function generates a dataset with a specified skewness and excess
kurtosis using Fleishman's polynomial transformation.

## Usage

``` r
eda_sim_defunct(n, skewness = 0, kurtosis = 0, ...)
```

## Arguments

- n:

  Integer. Number of data points to generate.

- skewness:

  Numeric. Desired skewness of the generated data. Default is 0
  (symmetric distribution).

- kurtosis:

  Numeric. Desired excess kurtosis of the generated data. Default is 0
  for a Normal distribution.

- ...:

  Not used.

## Value

A numeric vector of length `n` containing the generated data.

## Details

The function uses Fleishman's polynomial transformation of the form:
\$\$Y = a + bX + cX^2 + dX^3\$\$ where `a`, `b`, `c`, and `d` are
coefficients determined to approximate the specified skewness and excess
kurtosis. An excess kurtosis is defined as the kurtosis of a Normal
distribution (k=3) minus 3. Hence, an excess kurtosis of 0 is that of a
Normal distribution. The coefficients are solved using a numerical
optimization approach based on minimizing the residuals of Fleishman's
equations. The simulated values have a mean of 0, but the variance can
range from 1 for normal distributions to a variance greater than 1 for
non-normal distributions.

## References

- Fleishman, A. I. (1978). A method for simulating non-normal
  distributions. Psychometrika, 43, 521–532.

## Examples

``` r

 # A normal distribution
 x <- eda_sim(5000, 0, 0)
#> [1] "Skew/kurtosis combination is valid."
 hist(x)


 # A right-skewed distribution
 x <- eda_sim(5000, 2, 0)
#> Excess kurtosis is below the recommended value of  5.2218 for a skew of  2 .
#>  This may result in a distribution that does not reflect the desired 
#> skewness/kurtosis combination.
#> Warning: 
 hist(x)


 # A left-skewed distribution
 x <- eda_sim(5000, -2, 0)
#> Excess kurtosis is below the recommended value of  5.2218 for a skew of  -2 .
#>  This may result in a distribution that does not reflect the desired 
#> skewness/kurtosis combination.
#> Warning: 
 hist(x)


 # A uniform distribution
 # Note that this is unbounded which may result in outliers
 set.seed(21)
 x <- eda_sim(5000, 0, -10)
#> Excess kurtosis is below the recommended value of  -1.13168 for a skew of  0 .
#>  This may result in a distribution that does not reflect the desired 
#> skewness/kurtosis combination.
#> Warning: 
 hist(x, breaks = 20)


 # A "peaky" distribution
  set.seed(12)
  x <- eda_sim(5000, 0, 10)
#> [1] "Skew/kurtosis combination is valid."
  hist(x, breaks = 20)

```
