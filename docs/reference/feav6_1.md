# Simulated Tumor Measurements by Oncologists

This dataset contains measurements of cross-sectional area (in mm²) of
simulated solid tumors. These measurements were estimated by 13
different oncologists. Data are pulled from table 6-1 of the referenced
source.  
  
This dataset represents a three-factor experimental design with fully
crossed factors. The primary factors involved are the Oncologist (the
individual performing the estimate), the Material from which the tumor
model was made (Cork or Rubber), and the Form (or shape) of the tumor
(Small, Oblong, or Large).

## Usage

``` r
feav6_1
```

## Format

A `data.frame` with 78 rows and the following columns:

- Oncologist:

  A character uniquely identifying each oncologist who provided a
  cross-sectional area estimate.

- Material:

  A character indicating the material of the simulated tumor model
  (`Cork or Rubber`).

- Form:

  A character representing the shape of the simulated tumor
  (`Small, Oblong or Large`).

- Area:

  A numeric vector representing the average cross-sectional area (in
  mm²) of the tumor as estimated by the oncologist. This is the response
  variable.

## Source

Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991). *Fundamentals of
Exploratory Analysis of Variance*. Wiley.

## Examples

``` r
M0 <- eda_mean_sweep(feav6_1, Area, Oncologist, Material, Form, max_order = 2)
plot(M0, order = FALSE, rotate = TRUE)

```
