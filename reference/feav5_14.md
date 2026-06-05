# ELISA Test Absorbance for HIV Positive Controls

This dataset contains measurements of light absorbance for positive
control samples from an ELISA (Enzyme-Linked Immunosorbent Assay) test
for HIV. Data are pulled from table 5-14 of the referenced source.  
  
The dataset is an example of a two-factor experimental design with
nested factors. The experiment involved five different production lots.
For each lot, five individual runs of the test were performed. Within
each run, absorbance readings were recorded for three positive control
samples. As such the `Sample` factor is nested under the `Run` factor
which is itself nested under the `lot` factor.

## Usage

``` r
feav5_14
```

## Format

A `data.frame` with 75 rows and the following columns:

- Lot:

  A character indicating the specific production lot of the ELISA test
  `A, B, C, D, or E`.

- Run:

  A character representing the individual test runs. `Run` is nested
  within `Lot`, meaning `Run 1` for `Lot A` is distinct from `Run 1` for
  `Lot B`.

- Sample:

  A character specifying the sample. There are three replicate samples
  per run.

- Absorption:

  A numeric vector representing the light absorbance readings (in
  arbitrary units) for the positive control samples. This is the
  response variable.

## Source

Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991). *Fundamentals of
Exploratory Analysis of Variance*. Wiley.

## Examples

``` r
# Partition response variable across ALL factors. Residuals should be 0.
M0 <- eda_mean_sweep(feav5_14, Absorption, Lot, Run, Sample,
                     nesting = list(c("Lot", "Run"), c("Run","Sample")))
plot(M0, rotate = TRUE)

```
