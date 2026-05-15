# Generate an ANOVA Table from `eda_mean_sweep` output

This function generates an analysis-of-variance (ANOVA) table from the
output of the
[`eda_mean_sweep()`](https://mgimond.github.io/tukeyedar/reference/eda_mean_sweep.md)
function. It calculates sums of squares (SS), degrees of freedom (df),
mean squares (MS), F-statistics, and p-values for each decomposed
effect.

## Usage

``` r
eda_anova_table(sweep_result)
```

## Arguments

- sweep_result:

  A list object returned by
  [`eda_mean_sweep()`](https://mgimond.github.io/tukeyedar/reference/eda_mean_sweep.md),
  containing the global mean, effects, residuals, and long-form data.

## Value

A data frame with columns:

- Effect:

  Name of the factor or interaction term

- SS:

  Sum of squares for the effect

- df:

  Degrees of freedom for the effect

- MS:

  Mean square for the effect

- F:

  F-statistic comparing the effect MS to the residual MS

- p:

  p-value from the F-test

## Details

This function assumes that each effect in the `effects` list of the mean
sweep output corresponds to a centered vector of fitted values. The
residuals are used to compute the residual mean square.

## Examples

``` r
# Example of a 3-way table with nested factors
M0 <- eda_mean_sweep(feav1_5, votes, State, Year, Grouping, 
                     nesting = c("Grouping", "State")) 
eda_anova_table(M0)
#>                    Effect         SS  df           MS         F            p
#> 1                  Common 58001249.3   1 58001249.256        NA           NA
#> 2                    Year   182821.1   3    60940.350 29.617235 7.434056e-14
#> 3                Grouping  1745307.6  12   145442.298 70.685494 8.127219e-44
#> 4 State (within Grouping)   611845.2  38    16101.189  7.825237 5.678590e-17
#> 5                Residual   209874.9 102     2057.598        NA           NA

# Example of 3-way table with two way interactions
M0 <- eda_mean_sweep(feav6_8, Hard, Dentist, Method, Alloy, Temp, 
                     max_order = 2)
eda_anova_table(M0)                     
#>            Effect           SS df           MS          F            p
#> 1          Common 49521084.444  1 49521084.444         NA           NA
#> 2         Dentist   157794.556  4    39448.639  5.1510664 1.426389e-03
#> 3          Method   593427.489  2   296713.744 38.7438516 4.990513e-11
#> 4           Alloy   105815.511  1   105815.511 13.8170224 4.940275e-04
#> 5            Temp    82178.022  2    41089.011  5.3652606 7.614399e-03
#> 6  Dentist:Method   306471.844  8    38308.981  5.0022538 1.250716e-04
#> 7   Dentist:Alloy     5687.044  4     1421.761  0.1856486 9.448460e-01
#> 8    Dentist:Temp   134424.311  8    16803.039  2.1940825 4.292912e-02
#> 9    Method:Alloy    54685.089  2    27342.544  3.5702946 3.524196e-02
#> 10    Method:Temp    30652.444  4     7663.111  1.0006225 4.157108e-01
#> 11     Alloy:Temp    21725.356  2    10862.678  1.4184108 2.513089e-01
#> 12       Residual   398233.889 52     7658.344         NA           NA
```
