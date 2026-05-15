# Trims vector and dataframe objects

Removes records from either tail-ends of a sorted dataset. Trimming can
be performed by number of records (specify the `num =` option) or by
quantiles (specify the `prop=` option).  
  
`eda_trim` Trims a vector  
`eda_trim_df` Trims a data frame  
`eda_ltrim` Left-trims a vector  
`eda_rtrim` Right-trims a vector  
`eda_ltrim_df` Left-trims a dataframe  
`eda_rtrim_df` Right-trims a dataframe  

## Usage

``` r
eda_trim(x, prop = 0.05, num = 0)

eda_trim_df(dat, x, prop = 0.05, num = 0)

eda_ltrim(x, prop = 0.05, num = 0)

eda_ltrim_df(dat, x, prop = 0.05, num = 0)

eda_rtrim(x, prop = 0.05, num = 0)

eda_rtrim_df(dat, x, prop = 0.05, num = 0)
```

## Arguments

- x:

  Vector of values (if trimming a vector) or the column whose values are
  used to trim a dataframe (applies to `*_df` functions only)

- prop:

  Fraction of values to trim

- num:

  Number of values to trim

- dat:

  Dataframe (applies to `*_df` functions only)

## Value

Returns the same data type as the input (i.e. vector or dataframe)

## Details

- The input dataset does not need to be sorted (sorting is performed in
  the functions).

- If `num` is set to zero, then the function will assume that the
  trimming is to be done by fraction (defined by the `prop` parameter).

- If `eda_trim` or `eda_trim_df` functions are called, the `num` and
  `prop` values apply to each tail. For example, if `num = 5` then the 5
  smallest AND 5 largest values are removed from the data.

- `NA` values must be stripped from the input vector or column elements
  before running the trim functions.

- Elements are returned sorted on the trimmed elements.

## Examples

``` r

# Trim a vector by 10% (i.e. 10% of the smallest and 10% of the largest
# values)
eda_trim( mtcars[,1], prop=0.1)
#>  [1] 14.7 15.0 15.2 15.2 15.5 15.8 16.4 17.3 17.8 18.1 18.7 19.2 19.2 19.7 21.0
#> [16] 21.0 21.4 21.4 21.5 22.8 22.8 24.4 26.0 27.3

# Trim a data frame by 10% using the mpg column(i.e. 10% of the smallest
# and 10% of the largest mpg values)
eda_trim_df( mtcars, mpg, prop=0.1)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Chrysler Imperial 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Maserati Bora     15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Merc 450SLC       15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> AMC Javelin       15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Dodge Challenger  15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> Ford Pantera L    15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Merc 450SE        16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL        17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 280C         17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Pontiac Firebird  19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Ferrari Dino      19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Volvo 142E        21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> Toyota Corona     21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Porsche 914-2     26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Fiat X1-9         27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
```
