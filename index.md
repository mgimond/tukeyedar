# tukeyedar

The `tukeyedar` package houses data exploration tools. Many functions
are inspired by work published by Tukey (1977), Hoaglin et al. (1983),
Velleman and Hoaglin (1981), Hoaglin et al. (1985), Hoaglin et al.
(1990), and Cleveland (1993).

> This package was initially developed for an introductory course in
> Exploratory Data Analysis (EDA). As such, many of its functions have
> not undergone the rigorous testing and validation typically required
> for research or production use. Users are advised to exercise caution
> and verify results independently before applying the package in
> critical or high-stakes contexts.

## Installation

The package is available from
[GitHub](https://github.com/mgimond/tukeyedar/). It can be installed
using the following command:

``` r

install.packages("remotes")
remotes::install_github("mgimond/tukeyedar")
```

Note that the vignettes may not be automatically generated with the
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
[`vignette("RLine", package = "tukeyedar")`](https://mgimond.github.io/tukeyedar/articles/RLine.md).
If you use a dark themed IDE, the vignettes may not render very well so
you might opt to view them in a web browser via the functions
`RShowDoc("RLine", package = "tukeyedar")`.

## Using the `tukeyedar` functions

All functions start with `eda_`. For example, to generate a three point
summary plot of the `mpg` vs. `disp` from the `mtcars` dataset, type:

``` r

library(tukeyedar)
eda_3pt(mtcars, disp, mpg)
```

Note that most functions are *pipe* friendly. For example:

``` r

# Using R >= 4.1
mtcars |>  eda_3pt(disp, mpg)

# Using magrittr (or any of the tidyverse packages)
library(magrittr)
mtcars %>% eda_3pt(disp, mpg)
```

------------------------------------------------------------------------

Cleveland, William. 1993. *Visualizing Data*. Hobart Press.

Hoaglin, D. C., F. Mosteller, and J. W. Tukey. 1983. *Understanding
Robust and Exploratory Data Analysis*. Wiley.

Hoaglin, D. C., F. Mosteller, and J. W. Tukey. 1985. *Exploring Data
Tables, Trends, and Shapes*. Wiley.

Hoaglin, D. C., F. Mosteller, and J. W. Tukey. 1990. *Fundamentals of
Exploratory Analysis of Variance*. Wiley.

Tukey, John W. 1977. *Exploratory Data Analysis*. Addison-Wesley.

Velleman, P. F., and D. C. Hoaglin. 1981. *Applications, Basics and
Computing of Exploratory Data Analysis*. Duxbury Press.
