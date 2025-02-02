% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda_qq.R
\name{eda_qq}
\alias{eda_qq}
\title{Quantile-Quantile and Tukey mean-difference plots}
\usage{
eda_qq(
  x,
  y = NULL,
  fac = NULL,
  norm = FALSE,
  sym = FALSE,
  p = 1L,
  tukey = FALSE,
  md = FALSE,
  q.type = 5,
  fx = NULL,
  fy = NULL,
  plot = TRUE,
  show.par = TRUE,
  grey = 0.6,
  pch = 21,
  p.col = "grey50",
  p.fill = "grey80",
  size = 1,
  alpha = 0.8,
  med = TRUE,
  q = TRUE,
  tails = FALSE,
  inner = 0.75,
  tail.pch = 21,
  tail.p.col = "grey70",
  tail.p.fill = NULL,
  switch = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  t.size = 1.2,
  ...
)
}
\arguments{
\item{x}{Vector for first variable, or a dataframe.}

\item{y}{Vector for second variable, or column defining the continuous
variable if \code{x} is a dataframe.}

\item{fac}{Column defining the categorical variable if \code{x} is a
dataframe. The categorical column must be limited to two levels (groups).
dataframe. Ignored if \code{x} and \code{y} are vectors.}

\item{norm}{Defunct. Use \code{eda_theo} instead.}

\item{sym}{Defunct. Use \code{eda_sym} instead.}

\item{p}{Power transformation to apply to continuous variable(s).}

\item{tukey}{Boolean determining if a Tukey transformation should be adopted
(FALSE adopts a Box-Cox transformation).}

\item{md}{Boolean determining if a Tukey mean-difference plot should be
generated.}

\item{q.type}{An integer between 1 and 9 selecting one of the nine quantile
algorithms. (See \code{quantile} function).}

\item{fx}{Formula to apply to x variable before pairing up with y. This is
computed after any transformation is applied to the x variable.}

\item{fy}{Formula to apply to y variable before pairing up with x. This is
computed after any transformation is applied to the y variable.}

\item{plot}{Boolean determining if plot should be generated.}

\item{show.par}{Boolean determining if parameters such as power
transformation and formula should be displayed.}

\item{grey}{Grey level to apply to plot elements (0 to 1 with 1 = black).}

\item{pch}{Point symbol type.}

\item{p.col}{Color for point symbol.}

\item{p.fill}{Point fill color passed to \code{bg} (Only used for \code{pch}
ranging from 21-25).}

\item{size}{Point size (0-1)}

\item{alpha}{Point transparency (0 = transparent, 1 = opaque). Only
applicable if \code{rgb()} is not used to define point color.}

\item{med}{Boolean determining if median lines should be drawn.}

\item{q}{Boolean determining if \code{inner} data region should be shaded.}

\item{tails}{Boolean determining if points outside of the \code{inner} region
should be symbolized differently. Tail-end points are symbolized via the
\code{tail.pch},  \code{tail.p.col} and \code{tail.p.fill} arguments.}

\item{inner}{Fraction of the data considered as "mid values". Defaults to
75\\%. Used  to define shaded region boundaries, \code{q}, or to identify
which of the tail-end points are to be symbolized differently,
\code{tails=TRUE}.}

\item{tail.pch}{Tail-end point symbol type (See \code{tails}).}

\item{tail.p.col}{Tail-end color for point symbol (See \code{tails}).}

\item{tail.p.fill}{Tail-end point fill color passed to \code{bg}
(Only used for \code{tail.pch} ranging from 21-25).}

\item{switch}{Boolean determining if the axes should be swapped in an
empirical QQ plot. Only applies to dataframe input. Ignored if vectors are
passed to the function.}

\item{xlab}{X label for output plot. Ignored if \code{x} is a dataframe.}

\item{ylab}{Y label for output plot. Ignored if \code{x} is a dataframe.}

\item{title}{Title to add to plot.}

\item{t.size}{Title size.}

\item{...}{Not used}
}
\value{
Returns a list with the following components:

\itemize{
\item \code{data}: Dataframe with input \code{x} and \code{y} values.
Data will be interpolated to smallest quantile batch if batch sizes differ.
Values will reflect power transformation defined in \code{p}.
\item \code{p}: Re-expression applied to original values.
\item \code{fx}: Formula applied to x variable.
\item \code{fy}: Formula applied to y variable.}
}
\description{
\code{eda_qq} Generates an empirical QQ plot and a Tukey
mean-difference plot
}
\details{
By default, the QQ plot will highlight the inner 75\\% of the data
for both x and y axes to mitigate the visual influence of extreme values.
The \code{inner} argument controls the extent of this region. For example
\code{inner = 0.5} will highlight the IQR region. \cr
\cr
If the shaded regions are too distracting, you can opt to have the
tail-end points symbolized differently by setting \code{tails = TRUE} and
\code{q = FALSE}. The tail-end point symbols can be customized via the
\code{tail.pch}, \code{tail.p.col} and \code{tail.p.fill} arguments.
\cr
The middle dashed line represents each batch's median value. It can be
turned off by setting \code{med = FALSE}\cr
\cr
Console output prints the suggested multiplicative and additive offsets. It
adopts a resistant line fitting technique to derive the coefficients. The
suggested offsets output applies to the raw or re-expressed data but it
ignores any \code{fx} or \code{fy} transformations applied to the data.
Note that the suggested offsets may not always be the most parsimonious fit.
Eyeballing the offsets may sometimes result in a more satisfactory
characterization of the differences between batches. See the QQ plot
article for an introduction on its use and interpretation.
\cr \cr
To generate a Tukey mean-difference plot, set \code{med = TRUE}.
\cr \cr
For more information on this function and on interpreting a QQ plot see
the \href{../articles/qq.html}{QQ plot article}.
}
\examples{

# Passing data as a dataframe
 singer <- lattice::singer
 dat <- singer[singer$voice.part  \%in\% c("Bass 2", "Tenor 1"), ]
 eda_qq(dat, height, voice.part)

# If the shaded region is too distracting, you can apply a different symbol
# to the tail-end points and different color to the points falling in the
# inner region.
eda_qq(dat, height, voice.part, q = FALSE, tails = TRUE, tail.pch = 3,
       p.fill = "coral", size = 1.2, med = FALSE)

# For a more traditional look to the QQ plot
eda_qq(dat, height, voice.part, med = FALSE, q = FALSE)

# Passing data as two separate vector objects
 bass2 <- subset(singer, voice.part == "Bass 2", select = height, drop = TRUE )
 tenor1 <- subset(singer, voice.part == "Tenor 1", select = height, drop = TRUE )

 eda_qq(bass2, tenor1)

 # The function suggests an offset of the form y = x * 1.04 - 5.2
 eda_qq(bass2, tenor1, fx = "x * 1.04 - 5.2")

 # The suggested offset helps align the points along the x=y line, but we
 # we might come up with a better characterization of this offset.
 # There seems to be an additive offset of about 2 inches. By subtracting 2
 # from the x variable, we should have points line up with the x=y line
 eda_qq(bass2, tenor1, fx = "x - 2")

 # We can fine-tune by generating the Tukey mean-difference plot
 eda_qq(bass2, tenor1, fx = "x - 2", md = TRUE)

 # An offset of another 0.5 inches seems warranted
 # We can say that overall, bass2 singers are 2.5 inches taller than  tenor1.
 # The offset is additive.
 eda_qq(bass2, tenor1, fx = "x - 2.5", md = TRUE)

 # Note that the "suggested offset" in the console could have also been
 # applied to the data (though this formula is a bit more difficult to
 # interpret than our simple additive model)
 eda_qq(bass2, tenor1, fx = "x * 1.04 -5.2", md = TRUE)

 # Example 2: Sepal width
 setosa <- subset(iris, Species == "setosa", select = Petal.Width, drop = TRUE)
 virginica <- subset(iris, Species == "virginica", select = Petal.Width, drop = TRUE)

 eda_qq(setosa, virginica)

 # The points are not completely parallel to the  x=y line suggesting a
 # multiplicative offset. The slope may be difficult to eyeball. The function
 # outputs a suggested slope and intercept. We can start with that
 eda_qq(setosa, virginica, fx = "x *  1.7143")

 # Now let's add the suggested additive offset.
 eda_qq(setosa, virginica, fx = "x *  1.7143  + 1.6286")

 # We can confirm this value via the mean-difference plot
 # Overall, we have both a multiplicative and additive offset between the
 # species' petal widths.
 eda_qq(setosa, virginica, fx = "x *  1.7143 + 1.6286", md = TRUE)

}
\references{
\itemize{
\item John M. Chambers, William S. Cleveland, Beat Kleiner, Paul A. Tukey.
Graphical Methods for Data Analysis (1983)
\item \href{../articles/qq.html}{Quantile-Quantile plot article}}
}
