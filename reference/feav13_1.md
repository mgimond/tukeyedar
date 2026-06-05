# Bekk Smoothness of Paper

A dataset containing 160 measurements of paper smoothness across five
different material groups. This data was originally reported by Lashof
and Mandel (1960) and is used in Exploratory Data Analysis to illustrate
heteroscedasticity and the need for re-expression.

## Usage

``` r
feav13_1
```

## Format

A `data.frame` with 160 rows and 3 variables:

- Lab:

  Lab that measured the smoothness.

- Material:

  The material group identifier.

- Smoothness:

  The measured smoothness of the paper using the Bekk method.

## Source

Mandel, J. (1964). *The Statistical Analysis of Experimental Data*. New
York: Wiley. (Data from Table 13.3, p. 325).

## References

Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991). *Fundamentals of
Exploratory Analysis of Variance*. Wiley.

## Examples

``` r
M0 <- eda_mean_sweep(feav13_1, Smoothness, Lab,Material,p =0, base=10, max_order = 2)
#> Warning: Power transformation (eda_re) not applied as its definition is missing.
plot(M0)

eda_anova_table(M0)
#>         Effect          SS  df           MS          F             p
#> 1       Common 390.7042135   1 3.907042e+02         NA            NA
#> 2          Lab   0.1961390   3 6.537966e-02   27.37374  5.420466e-14
#> 3     Material  54.8089942   4 1.370225e+01 5736.97904 4.372050e-154
#> 4 Lab:Material   0.1392811  12 1.160676e-02    4.85962  1.175348e-06
#> 5     Residual   0.3343772 140 2.388408e-03         NA            NA
```
