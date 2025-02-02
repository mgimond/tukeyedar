% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda_sl.R
\name{eda_sl}
\alias{eda_sl}
\title{Spread-location and spread-level plots}
\usage{
eda_sl(
  dat,
  x = NULL,
  fac = NULL,
  type = "location",
  p = 1,
  tukey = FALSE,
  sprd = "frth",
  jitter = 0.01,
  robust = TRUE,
  loess.d = list(family = "symmetric", degree = 1, span = 1),
  loe.col = rgb(0.3, 0.3, 1, 1),
  label = TRUE,
  label.col = "lightsalmon",
  plot = TRUE,
  equal = TRUE,
  grey = 0.6,
  pch = 21,
  p.col = "grey50",
  p.fill = "grey80",
  size = 0.8,
  alpha = 0.8,
  xlab = NULL,
  ylab = NULL,
  labelxbuff = 0.05,
  labelybuff = 0.05,
  show.par = TRUE
)
}
\arguments{
\item{dat}{Dataframe of univariate data or a linear model.}

\item{x}{Continuous variable column (ignored if \code{dat} is a linear model).}

\item{fac}{Categorical variable column (ignored if \code{dat} is a linear
model).}

\item{type}{s-l plot type. \code{"location"} = spread-location,
\code{"level"} = spread-level (only for univariate data).
\code{"dependence"} = spread-dependence (only for bivariate model input).}

\item{p}{Power transformation to apply to variable. Ignored if input is a
linear model.}

\item{tukey}{Boolean determining if a Tukey transformation should be adopted
(FALSE adopts a Box-Cox transformation).}

\item{sprd}{Choice of spreads used in the spread-versus-level plot (i.e.
when \code{type = "level"}). Either
interquartile, \code{sprd = "IQR"} or
fourth-spread, \code{sprd = "frth"} (default).}

\item{jitter}{Jittering parameter for the spread-location plot. A fraction of
the range of location values.}

\item{robust}{Boolean indicating if robust regression should be used on the
spread-level plot.}

\item{loess.d}{Arguments passed to the internal loess function. Applies only
to the bivariate model s-l plots and the spread-level plot.}

\item{loe.col}{LOESS curve color.}

\item{label}{Boolean determining if group labels are to be added to the
spread-location plot.}

\item{label.col}{Color assigned to group labels (only applicable if
\code{type = location}).}

\item{plot}{Boolean determining if plot should be generated.}

\item{equal}{Boolean determining if axes lengths should match (i.e. square
plot).}

\item{grey}{Grey level to apply to plot elements (0 to 1 with 1 = black).}

\item{pch}{Point symbol type.}

\item{p.col}{Color for point symbol.}

\item{p.fill}{Point fill color passed to \code{bg} (Only used for \code{pch}
ranging from 21-25).}

\item{size}{Point size (0-1).}

\item{alpha}{Point transparency (0 = transparent, 1 = opaque). Only
applicable if \code{rgb()} is not used to define point colors.}

\item{xlab}{X label for output plot.}

\item{ylab}{Y label for output plot.}

\item{labelxbuff}{Buffer to add to the edges of the plot to make room for
the labels in a spread-location plot. Value is a fraction of the plot width.}

\item{labelybuff}{Buffer to add to the top of the plot to make room for
the labels in a spread-location plot. Value is a fraction of the plot width.}

\item{show.par}{Boolean determining if the power transformation applied to
the data should be displayed.}
}
\value{
Returns a dataframe of level and spread values.
}
\description{
The \code{eda_sl} function generates William Cleveland's
spread-location plot for univariate and bivariate data. The function will also
generate Tukeys' spread-level plot.
}
\details{
The function generates a few variations of the spread-location/spread-level
plots depending on the data input type and parameter passed to the
\code{type} argument. The residual spreads are mapped to the y-axis and the
levels are mapped to the x-axis. Their values are computed as follows:
\itemize{

\item \code{type = "location"} (univariate data):\cr\cr
William Cleveland's spread-location plot applied to univariate
data.\cr
\eqn{\ spread = \sqrt{|residuals|}} \cr
\eqn{\ location = medians}

\item \code{type = "level"} (univariate data):\cr\cr
Tukey's spread-level plot (aka spread-versus-level plot, Hoaglin et al.,
p 260). If the pattern is close to linear, the plot can help find a power
transformation that will help stabilize the spread in the data by
subtracting one from the fitted slope. This option outputs the slope of
the fitted line in the console. A loess is added to assess linearity.
By default, the fourth spread is used to define the spread. Alternatively,
the IQR can be used by setting \code{spread = "IQR"}. The output will be
nearly identical except for small datasets where the two methods may
diverge slightly in output.\cr
\eqn{\ spread = log(fourth\ spread(residuals))} \cr
\eqn{\ location = log(medians)}

\item \code{type = "location"} if input is a model of class \code{lm},
\code{eda_lm} or \code{eda_rline}:\cr\cr
William Cleveland's spread-location plot (aka scale-location plot)
applied to residuals of a linear model.\cr
\eqn{\ spread = \sqrt{|residuals|}} \cr
\eqn{\ location = fitted\ values}

\item \code{type = "dependence"} if input is a model of class \code{lm},
\code{eda_lm} or \code{eda_rline}:\cr\cr
William Cleveland's spread-location plot applied to residuals of
a linear model.\cr
\eqn{\ spread = \sqrt{|residuals|}} \cr
\eqn{\ dependence = x\ variable}
}
}
\examples{
cars <- MASS::Cars93
# Cleveland's spread-location plot applied to univariate data
eda_sl(cars, MPG.city, Type)

# You can specify the exact form of the spread on the y-axis
# via the ylab argument
eda_sl(cars, MPG.city, Type, ylab = expression(sqrt(abs(residuals))) )

# The function can also generate Tukey's spread-level plot to identify a
# power transformation that can stabilize spread across fitted values
# following power = 1 - slope
eda_sl(cars, MPG.city, Type, type = "level")

# A slope of around 3 is computed from the s-l plot, therefore, a suggested
# power is 1 - 3 = -2. We can apply a power transformation within the
# function via the p argument. By default, a Box-Cox transformation method
# is adopted.
eda_sl(cars, MPG.city, Type, p = -2)

# Spread-location plot can also be generated from residuals of a linear model
M1 <- lm(mpg ~ hp, mtcars)
eda_sl(M1)

# Spread can be compared to X instead of fitted value
eda_sl(M1, type = "dependence")
}
\references{
\itemize{
\item Understanding Robust and Exploratory Data Analysis, Hoaglin,
David C., Frederick Mosteller, and John W. Tukey, 1983.
\item William S. Cleveland. Visualizing Data. Hobart Press (1993)
}
}
