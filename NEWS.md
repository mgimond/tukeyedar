# tukeyedar 0.2.7
*  Added the `eda_rfs` function (the Cleveland residual-fit spread plot).
*  Added the `eda_viol` function that generates a violin plot.
*  Added the `eda_jitter` function that generates a jitter plot from univariate datasets.
* `eda_lm` now outputs an `eda_lm` class.
* `eda_rline` now outputs `fitted.values` class.
* `eda_lm` now accepts `poly = 0`. This generates a flat line. Can be used with `robust = TRUE`.

# tukeyedar 0.2.6
* Added the quantile-difference plot to the `eda_qq` function.
* Removed redundant `outliers` argument from `eda_boxls` (#21).

# tukeyedar 0.2.5
* Added symmetry QQ option to `eda_qq` function.
* Ensure that all re-expressions default to Box-Cox method.

# tukeyedar 0.2.4
* Added polynomial option to `eda_lm` function.
* Added robust fitting option to `eda_lm` function (uses `MASS::rlm`)

# tukeyedar 0.2.3
* Added `show.par` option  to `eda_qq`
* fixed issue #17.

# tukeyedar 0.2.2
* Re-wrote eda_rline function. This also fixed issue #15.

# tukeyedar 0.2.1
* Added the option to plot the density distribution alongside the Normal fit in `eda_normfit`.

# tukeyedar 0.2.0
* Added a Normal QQ plot option to `eda_qq`.
* Added symmetrical Normal fit plot function `eda_normfit`.
* Updated eda_boxls aesthetics.
* Updated median polish diagnostic plot aesthetics.

# tukeyedar 0.1.1

* Introduces the median polish function `eda_pol`.
* Introduces the QQ and Tukey mean-difference plot `eda_qq`.
* Introduces the density plot function `eda_dens`.
* Adds re-expression parameters to `eda_lm` via the parameters `px` and `py`.
* Adds `sd` labels to SD dashed lines in `eda_lm`.
* `eda_lm` will now output `lm` intercept and slope.
* Adds plot method for `eda_rline` object.
* In `eda_re` if `p = 1`, box-cox option is ignored.
* Homogenized plot appearances.
* Added power parameter argument to `eda_boxls`.
* Added power parameter argument to `eda_sl`.
* Added plot option to `eda_sl`.

# tukeyedar 0.1.0

* Initial release of `tukeyedar`

