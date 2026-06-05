# Shuffle one or more columns in a data frame.

Permutes the values within one or more specified columns of a data
frame, leaving all other columns unchanged.

## Usage

``` r
eda_shuffle(df, ..., replace = FALSE)
```

## Arguments

- df:

  A data frame.

- ...:

  One or more unquoted column names to be shuffled.

- replace:

  A logical value indicating whether to sample with replacement.
  Defaults to `FALSE` (permutation).

## Value

A data frame with the same dimensions as the input `df`, but with the
values in the specified columns permuted.

## Details

This function shuffles each specified column independently. It is a
convenient wrapper around
[`sample()`](https://rdrr.io/r/base/sample.html) for use in permutation
tests or for generating randomized datasets. If a shuffled column is a
factor, the original factor levels and their order are preserved in the
output.

## Examples

``` r
# Load the yarn dataset
data(yarn)

# Shuffle a single column ('Load')
shuffled_df1 <- eda_shuffle(yarn, Load)

# Shuffle two columns independently ('Load' and 'Amplitude')
shuffled_df2 <- eda_shuffle(yarn, Load, Amplitude)

# Shuffle a column with replacement (bootstrap sampling)
shuffled_df3 <- eda_shuffle(yarn, Cycles, replace = TRUE)
```
