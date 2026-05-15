# Re-expression function

`eda_re` re-expresses a vector following the Tukey or box-cox
transformation.

## Usage

``` r
eda_re(x, p = 0, tukey = FALSE, base = exp(1))
```

## Arguments

- x:

  Vector

- p:

  Power transformation

- tukey:

  If set to TRUE, then adopt Tukey's power transformation, if FALSE,
  adopt Box-Cox transformation

- base:

  Base used with the [`log()`](https://rdrr.io/r/base/Log.html) function

## Value

Returns a vector of same length as input `x`

## Details

The function is used to re-express data using one of two transformation
techniques: Box-Cox transformation (`tukey = FALSE`)or Tukey's power
transformation (`tukey = TRUE`).

## Examples

``` r
x <- c(15, 28, 17, 73,  8, 83,  2)
eda_re(x, p=-1/3)
#> [1] 1.7835596 2.0120494 1.8332666 2.2821832 1.5000000 2.3122532 0.6188984
```
