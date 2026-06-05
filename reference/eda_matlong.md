# Convert N-way data tables Between Long and Array Formats

Reshapes an N-way data table from a long format data frame to a
multi-dimensional array, or an array back to a long format data frame.

## Usage

``` r
eda_matlong(
  data,
  response,
  ...,
  direction = c("to_array", "to_long"),
  fun = mean
)
```

## Arguments

- data:

  A data frame in long format (for `direction = "to_array"`) or a
  multi-dimensional array/matrix (for `direction = "to_long"`).

- response:

  A bare (unquoted) variable name representing the response/value
  column.

- ...:

  One or more bare (unquoted) variable names representing the
  factor/dimension columns.

- direction:

  A character string specifying the conversion direction. Must be one of
  `"to_array"` or `"to_long"`.

- fun:

  The function to apply when aggregating data in the `to_array`
  conversion, in case of duplicate entries. Defaults to `mean`.

## Value

If `direction = "to_array"`, returns a multi-dimensional array. If
`direction = "to_long"`, returns a data frame in long format.

## Details

This function generalizes the concept of reshaping for multi-way tables.

For `to_array`, it uses `tapply` to create an array where the dimensions
are defined by the factor columns specified in `...` and the cell values
are determined by the `response` column.

For `to_long`, it uses `as.data.frame(as.table(...))` to reshape the
array into a long data frame and renames the columns in the order in
which the column names are passed to the function. It's important that
the order of the column names passed to the function match the order the
factor elements are stored in the array. You can find that order with
the command `names(dimnames(array_name))`.

## Examples

``` r
# Long to 2D array (matrix)
dl2 <- eda_matlong(inf_mort, perc, region, edu)
dl2
#>       edu
#> region  ed8 ed9to11 ed12 ed13to15 ed16
#>     NC 32.1    29.0 18.8     24.3 19.0
#>     NE 25.3    25.3 18.2     18.3 16.3
#>     S  38.8    31.0 19.3     15.7 16.8
#>     W  25.4    21.1 20.3     24.0 17.5

# 2D array to long
# The column names passed to the function must be in the same order
# that the factor elements are listed in the array
names(dimnames(dl2))
#> [1] "region" "edu"   
eda_matlong(dl2, perc, region, edu, direction = "to_long")
#>    region      edu perc
#> 1      NC      ed8 32.1
#> 2      NE      ed8 25.3
#> 3       S      ed8 38.8
#> 4       W      ed8 25.4
#> 5      NC  ed9to11 29.0
#> 6      NE  ed9to11 25.3
#> 7       S  ed9to11 31.0
#> 8       W  ed9to11 21.1
#> 9      NC     ed12 18.8
#> 10     NE     ed12 18.2
#> 11      S     ed12 19.3
#> 12      W     ed12 20.3
#> 13     NC ed13to15 24.3
#> 14     NE ed13to15 18.3
#> 15      S ed13to15 15.7
#> 16      W ed13to15 24.0
#> 17     NC     ed16 19.0
#> 18     NE     ed16 16.3
#> 19      S     ed16 16.8
#> 20      W     ed16 17.5

# Long to 3D array
dl3 <- eda_matlong(yarn, Cycles, Length, Amplitude, Load)
dl3
#> , , Load = 40
#> 
#>       Amplitude
#> Length    8    9   10
#>    250  674  338  170
#>    300 1414 1022  442
#>    350 3636 1568 1140
#> 
#> , , Load = 45
#> 
#>       Amplitude
#> Length    8    9  10
#>    250  370  266 118
#>    300 1198  620 332
#>    350 3184 1070 884
#> 
#> , , Load = 50
#> 
#>       Amplitude
#> Length    8   9  10
#>    250  292 210  90
#>    300  634 438 220
#>    350 2000 566 360
#> 

# 3D array to long
names(dimnames(dl3)) # Get the order of factor elements
#> [1] "Length"    "Amplitude" "Load"     
eda_matlong(dl3, Cycles, Length, Amplitude, Load, direction = "to_long")
#>    Length Amplitude Load Cycles
#> 1     250         8   40    674
#> 2     300         8   40   1414
#> 3     350         8   40   3636
#> 4     250         9   40    338
#> 5     300         9   40   1022
#> 6     350         9   40   1568
#> 7     250        10   40    170
#> 8     300        10   40    442
#> 9     350        10   40   1140
#> 10    250         8   45    370
#> 11    300         8   45   1198
#> 12    350         8   45   3184
#> 13    250         9   45    266
#> 14    300         9   45    620
#> 15    350         9   45   1070
#> 16    250        10   45    118
#> 17    300        10   45    332
#> 18    350        10   45    884
#> 19    250         8   50    292
#> 20    300         8   50    634
#> 21    350         8   50   2000
#> 22    250         9   50    210
#> 23    300         9   50    438
#> 24    350         9   50    566
#> 25    250        10   50     90
#> 26    300        10   50    220
#> 27    350        10   50    360
```
