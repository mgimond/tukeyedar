# Simulate a given dataset using fleishman transformation

**\[experimental\]**  
  
Generates simulated data with the same mean, standard deviation,
skewness, and kurtosis as the input data.

## Usage

``` r
eda_simdata(x, n)
```

## Arguments

- x:

  A numeric vector representing the dataset to match moments.

- n:

  An integer specifying the number of simulated data points to generate.

## Value

A numeric vector of simulated data.

## References

- Fleishman, A. I. (1978). A method for simulating non-normal
  distributions. Psychometrika, 43, 521–532.

- Wicklin, R. (2013). Simulating Data with SAS (Appendix D: Functions
  for Simulating Data by Using Fleishman’s Transformation). Cary, NC:
  SAS Institute Inc. Retrieved from https://tinyurl.com/4tustnph

## See also

- [`eda_sim`](https://mgimond.github.io/tukeyedar/reference/eda_sim.md)
  for simulating a distribution

## Examples

``` r
set.seed(4321)
nile  <- as.vector(Nile)
simnile <- eda_simdata(nile, 1000)
eda_qq(nile, simnile)

#> [1] "Suggested offsets:y = x * 1.0419 + (-36.2075)"
```
