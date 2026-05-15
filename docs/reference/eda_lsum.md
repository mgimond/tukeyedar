# Tukey's letter value summary

`eda_lsum` The letter value summary was introduced by John Tukey and
extends the boxplot's 5 number summary by exploring the symmetry of the
batch for depth levels other than the half (median) or the fourth
(quartiles).

## Usage

``` r
eda_lsum(x, l = 5, all = TRUE)
```

## Arguments

- x:

  Vector

- l:

  Number of levels (max = 9)

- all:

  Generate upper, lower and mid summaries if TRUE or just generate mid
  summaries if FALSE

## Value

Returns a dataframe of letter value summary.

## Details

Outputs a data frame of letter value summary.

## References

Exploratory Data Analysis, John Tukey, 1973.

## See also

<https://mgimond.github.io/ES218/letter_values.html>

## Examples

``` r
x <- c(22, 8, 11, 3, 26, 1, 14, 18, 20, 25, 24)
eda_lsum(x)
#>   letter depth lower   mid upper spread
#> 1      M   6.0  18.0 18.00  18.0    0.0
#> 2      H   3.5   9.5 16.25  23.0   13.5
#> 3      E   2.0   3.0 14.00  25.0   22.0
#> 4      D   1.5   2.0 13.75  25.5   23.5
#> 5      C   1.0   1.0 13.50  26.0   25.0
```
