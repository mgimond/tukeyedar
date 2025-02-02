# tukeyedar 0.4.0
*  Took out the normal and symmetry options from `eda_qq` and created their own functions:
   `eda_qqsym` and `eda_theo`. The latter allows for theoretical distributions other
   than Normal.
*  Added the `eda_fval` function that generates f-values (probabilities) based on one of six algorithms
*  Added the `eda_theopan` function that generates multi-panel theoretical QQ plots.
*  Added the `eda_qqpool` function that generates multi-panel pooled residual plots.
*  Added the `eda_shuffle` function that permutes values across groups.
*  Added the `eda_sim` function that simulates distributions given kurtosis and skewness.
*  `eda_sl` now outputs Cleveland's spread-location plot as well as Tukey's spread-versus-level plot.
*  Added `eda_untie` function that splits ties in a vector by nudging them by a small amount.
*  Fixed bug in `eda_rline` where incorrect residuals were returned.
*  Fixed margin width for `eda_jitter` and `eda_boxls` plots when in horizontal mode.

# tukeyedar 0.3.0
*  Added the `eda_rfs` function (the Cleveland residual-fit spread plot).
*  Added the `eda_viol` function that generates a violin plot.
*  Added the `eda_jitter` function that generates a jitter plot from univariate datasets.
*  Added the `eda_qqmat` function that generates a qq plot matrix.
* `eda_lm` now outputs an `eda_lm` class.
* `eda_rline` now outputs `fitted.values` class.
* `eda_lm` now accepts `poly = 0`. This generates a flat line. Can be used with `robust = TRUE`.
* Following changes were made to the `eda_qq` function:
     *  It now generates different point symbols for values outside of the `inner` region
     *  The `inner` region default is now 75%.
     *  The outer bounds dashed line argument, `l.val` was removed (this to de-clutter the plot)
     *  Removed the quantile-difference plot. Its interpretation is tricky when dealing with a multiplicative offset.  

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

