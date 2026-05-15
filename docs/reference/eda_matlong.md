# Convert 2-way data tables Between Long and Matrix Formats

Reshapes a 2-way data table from long format to a matrix, or a matrix
back to a data frame in long format.

## Usage

``` r
eda_matlong(
  data,
  row,
  col,
  response,
  direction = c("long_to_matrix", "matrix_to_long")
)
```

## Arguments

- data:

  A data frame in long format (for `direction = "long_to_matrix"`) or a
  matrix (for `direction = "matrix_to_long"`).

- row:

  A **bare (unquoted)** variable name from `data` (or a desired column
  name for the output) representing the row identifiers in the matrix.

- col:

  A **bare (unquoted)** variable name from `data` (or a desired column
  name for the output) representing the column identifiers in the
  matrix.

- response:

  A **bare (unquoted)** variable name from `data` (or a desired column
  name for the output) representing the values in the matrix.

- direction:

  A character string specifying the direction of the conversion. Must be
  one of `"long_to_matrix"` or `"matrix_to_long"`. Defaults to
  `"long_to_matrix"`.

## Value

If `direction = "long_to_matrix"`, returns a matrix. If
`direction = "matrix_to_long"`, returns a data frame in long format.

## Examples

``` r
eda_matlong(edtts2.12, col = year, row = institution, response = perc)
#>                    1980 1979 1977 1975 1973
#> Organized religion   66   65   64   68   66
#> Banking              60   60   NA   NA   NA
#> Military             52   54   57   58   NA
#> Public schools       51   53   54   NA   58
#> Supreme Court        47   45   46   49   44
#> Newspapers           42   51   NA   NA   39
#> Organized labor      35   36   39   38   30
#> Congress             34   34   40   40   42
#> Television           33   38   NA   NA   37
#> Big business         29   32   22   23   26
```
