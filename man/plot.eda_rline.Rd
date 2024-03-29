% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.eda_rline.R
\name{plot.eda_rline}
\alias{plot.eda_rline}
\title{Plot eda_rline model}
\usage{
\method{plot}{eda_rline}(
  x,
  type = "model",
  xlab = NULL,
  ylab = NULL,
  grey = 0.7,
  pch = 21,
  equal = TRUE,
  p.col = "grey50",
  p.fill = "grey80",
  size = 0.8,
  alpha = 0.7,
  model = TRUE,
  pt3 = TRUE,
  fit = TRUE,
  ...
)
}
\arguments{
\item{x}{Object of class \code{eda_rline}.}

\item{type}{Plot type. One of two: "model", "residuals".}

\item{xlab}{Custom x-axis label. Defaults to column name.}

\item{ylab}{Custom y-axis label. Defaults to column name.}

\item{grey}{Grey level to apply to plot elements (0 to 1 with 1 = black).}

\item{pch}{Point symbol type.}

\item{equal}{Boolean determining if axes lengths should match (i.e. square
plot).}

\item{p.col}{Color for point symbol.}

\item{p.fill}{Point fill color passed to \code{bg} (Only used for \code{pch}
ranging from 21-25).}

\item{size}{Point size (0-1).}

\item{alpha}{Point transparency (0 = transparent, 1 = opaque). Only applicable
if \code{rgb()} is not used to define point colors.}

\item{model}{Boolean indicating if the resulting model should be added above
plot. Only applies to \code{type = "model"}.}

\item{pt3}{Boolean indicating if the 3-pt summaries should be added to the
plot. Only applies to \code{type = "model"}.}

\item{fit}{Boolean indicating if the fitted line should be added to the plot.}

\item{...}{Arguments to be passed to subsequent methods.}
}
\value{
Does not return a value.
}
\description{
\code{plot.eda_rline} A plot method for lists of \code{eda_rline}
 class.
}
\details{
The function generates a scatter plot with the fitted model from an
 \code{eda_rline} object.
}
\examples{
r.lm    <- eda_rline(age_height, Months, Height)

plot(r.lm)
plot(r.lm, pt3 = FALSE)
plot(r.lm, type = "residuals")
}
