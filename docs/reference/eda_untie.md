# Resolve ties in a numeric vector

Adjusts tied values in a numeric vector by adding or subtracting a small
fraction of the range.

## Usage

``` r
eda_untie(dat, x = NULL, fac = NULL, f = 0.01, rand = TRUE, ...)
```

## Arguments

- dat:

  A data frame or a numeric vector.

- x:

  Numeric column. Ignored if `dat` is a numeric vector.

- fac:

  Column of categorical values. Ignored if `dat` is a numeric vector.

- f:

  A numeric value specifying the fraction of the range of `x` to use for
  perturbing tied values. Must be between 0 and 1.

- rand:

  A logical value. If `FALSE`, all adjustments are of fixed size based
  on `f`. If `TRUE`, the adjustments are randomized within the range
  specified by `f`.

- ...:

  not used.

## Value

Returns the input numeric data with ties resolved. If `dat` is a vector,
a modified vector is returned. If `dat` is a data frame, a modified
vector corresponding to the column specified by `x` is returned.

## Details

The function identifies tied values in the input vector `x` and perturbs
them slightly to break the ties. If `rand = TRUE`, the adjustment for
each tied value is randomized uniformly with the lower and upper bounds
defined by `[0, f * diff(range(x))]`. If `rand = FALSE`, the adjustment
is deterministic and equal to `+/- f * diff(range(x))`. Alternating
signs (`-1` and `1`) are used to distribute adjustments symmetrically.
The deterministic approach may not eliminate all ties. For example, if
four values are tied, the output will split the values into two tied
values. Repeating the process on the output as needed will eliminate all
remaining ties.

## Examples

``` r
set.seed(42)
x <- c(1, 2, 2, 2, 3, 4, 4, 5)
# Randomized adjustments
x1 <- eda_untie(x, f = 0.01, rand = TRUE)
#> [1] "There were 5 input ties."
x1
#> [1] 1.000000 1.963408 2.037483 1.988554 3.000000 4.033218 3.974330 5.000000

# Deterministic adjustments. Given that there are three elements sharing the
# same value (a value of 2 in this example), the data will need to be
# processed twice.
x2 <- eda_untie(x, f = 0.01, rand = FALSE)
#> [1] "There were 5 input ties."
x2
#> [1] 1.00 1.96 2.04 1.96 3.00 4.04 3.96 5.00
x3 <- eda_untie(x2, f = 0.01, rand = FALSE)
#> [1] "There were 2 input ties."
x3
#> [1] 1.00 1.92 2.04 2.00 3.00 4.04 3.96 5.00

# Random adjustments. Add up to +/- 0.5 inches to singer height values
set.seed(17)
singer <- lattice::singer
factor <- 0.5 / diff(range(singer$height)) # Get fraction that covers 0.5 inches
eda_jitter(singer, height, voice.part)

singer$notie <- eda_untie(singer, height, voice.part, f = factor)
#> [1] "Bass 2 had 23 ties."
#> [1] "Bass 1 had 38 ties."
#> [1] "Tenor 2 had 18 ties."
#> [1] "Tenor 1 had 16 ties."
#> [1] "Alto 2 had 26 ties."
#> [1] "Alto 1 had 31 ties."
#> [1] "Soprano 2 had 28 ties."
#> [1] "Soprano 1 had 32 ties."
eda_jitter(singer, notie, voice.part)
```
