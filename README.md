
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tukeyedar

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/mgimond/tukeyedar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mgimond/tukeyedar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `tukeyedar` package houses data exploration tools. Many functions
are inspired by work published by Tukey (1977), Hoaglin (1983), Velleman
and Hoaglin (1981), and Cleveland (1993). Note that this package is in
beta mode, so use at your own discretion.

## Installation

You can install the development version of tukeyedar from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("mgimond/tukeyedar")
```

Note that the vignettes will not be automatically generated with the
above command, however, the vignettes are available on this website (see
next section). If you want a local version of the vignettes, add the
`build_vignettes = TRUE` parameter.

``` r
remotes::install_github("mgimond/tukeyedar", build_vignettes = TRUE)
```

If, for some reason the vignettes are not created, you might want to
re-install the package with the `force=TRUE` parameter.

``` r
remotes::install_github("mgimond/tukeyedar", build_vignettes = TRUE, force=TRUE)
```

## Vignettes

It’s strongly recommended that you read the vignettes. These can be
accessed from this website:

- [A detailed rundown of the resistant line
  function](https://mgimond.github.io/tukeyedar/articles/RLine.html)
- [The median
  polish](https://mgimond.github.io/tukeyedar/articles/polish.html)
- [The empirical QQ
  plot](https://mgimond.github.io/tukeyedar/articles/qq.html)
- [The symmetry QQ
  plot](https://mgimond.github.io/tukeyedar/articles/symqq.html)
- [The residual-fit spread
  plot](https://mgimond.github.io/tukeyedar/articles/rfs.html)

If you chose to have the vignettes locally created when you installed
the package, then you can view them locally via
`vignette("RLine", package = "tukeyedar")`. If you use a dark themed
IDE, the vignettes may not render very well so you might opt to view
them in a web browser via the functions
`RShowDoc("RLine", package = "tukeyedar")`.

## Using the functions

All functions start with `eda_`. For example, to generate a three point
summary plot of the `mpg` vs. `disp` from the `mtcars` dataset, type:

``` r
library(tukeyedar)
eda_3pt(mtcars, disp, mpg)
```

Note that most functions are *pipe* friendly. For examples:

``` r
# Using R >= 4.1
mtcars |>  eda_3pt(disp, mpg)

# Using magrittr (or any of the tidyverse packages)
library(magrittr)
mtcars %>% eda_3pt(disp, mpg)
```

------------------------------------------------------------------------

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-visdata1993" class="csl-entry">

Cleveland, William. 1993. *Visualizing Data*. Hobart Press.

</div>

<div id="ref-understanding_eda1983" class="csl-entry">

Hoaglin, Mosteller, D. C. 1983. *Understanding Robust and Exploratory
Data Analysis*. Wiley.

</div>

<div id="ref-eda1977" class="csl-entry">

Tukey, John W. 1977. *Exploratory Data Analysis*. Addison-Wesley.

</div>

<div id="ref-applied_eda1981" class="csl-entry">

Velleman, P. F., and D. C. Hoaglin. 1981. *Applications, Basics and
Computing of Exploratory Data Analysis*. Boston: Duxbury Press.

</div>

</div>
