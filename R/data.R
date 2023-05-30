#' Age vs. height for private and rural school children
#'
#' The data were reproduced from Hoaglin et al.'s book which  were originally
#' sourced from Bernard G. Greenberg (1953) in the American Journal of Public
#' Health (vol 43, pp. 692-699). The dataset tabulate children's
#' height and weight from urban private and rural public schools.
#'
#' @format A data frame with 18 rows and 2 variables:
#' \describe{
#'   \item{Months}{Child's age in months}
#'   \item{Height}{Child's height in cm}
#'   ...
#' }
#' @source Understanding robust and exploratory data analysis, by D.C. Hoaglin,
#'    F. Mosteller and J.W. Tukey. (page 135)
"age_height"

#' Breast cancer mortality vs. temperature
#'
#' The data represent the relationship between mean annual temperature
#' and breast cancer mortality rate.
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'   \item{Temp}{Temperature in degrees Fahrenheit.}
#'   \item{Mortality}{Mortality rate presented as an index.}
#'   ...
#' }
#' @source Applications, Basics and Computing of Exploratory Data Analysis,
#'       P.F. Velleman and D.C. Hoaglin, 1981. (page 127)
"neoplasms"

#' Andrew Siegel's pathological 9-point dataset
#'
#' A synthetic dataset created to test the robustness of fitted lines.
#' Originally published by Andrew Siegel and later adapted in Hoaglin et al.'s
#' book.
#'
#' @format A data frame with 9 rows and 2 variables:
#' \describe{
#'   \item{X}{X values}
#'   \item{Y}{Y values}
#'   ...
#' }
#' @source Robust regression using repeated medians, Andrew F. Siegel, Biometrika,
#'    vol 69, n 1, 1982.
#' @source Understanding robust and exploratory data analysis, by D.C. Hoaglin,
#'    F. Mosteller and J.W. Tukey. 1983 (page 139)
"nine_point"

#' Legacy temperature normals for Waterville Maine (1981-2010)
#'
#' NOAA/NCEI derived normal daily temperatures for the city of Waterville, Maine
#' (USA) for the 1981 to 2010 period.
#'
#' @format A data frame with 365 rows and 5 variables:
#' \describe{
#'   \item{date}{Date centered on the 1981-2010 period. Note that the year is purely symbolic.}
#'   \item{doy}{Day of year}
#'   \item{min}{Typical minimum temperature for the 1981-2010 period.}
#'   \item{avg}{Typical average temperature for the 1981-2010 period.}
#'   \item{max}{Typical maximum temperature for the 1981-2010 period.}
#'   ...
#' }
#' @source https://www.ncei.noaa.gov/
"wat95"

#' Temperature normals for Waterville Maine (1991-2020)
#'
#' NOAA/NCEI derived normal daily temperatures for the city of Waterville, Maine
#' (USA) for the 1991 to 2020 period.
#'
#' @format A data frame with 365 rows and 5 variables:
#' \describe{
#'   \item{date}{Date centered on the 11991-2020 period. Note that the year is purely symbolic.}
#'   \item{doy}{Day of year.}
#'   \item{min}{Typical minimum temperature for the 1991-2020 period.}
#'   \item{avg}{Typical average temperature for the 1991-2020 period.}
#'   \item{max}{Typical maximum temperature for the 1991-2020 period.}
#'   ...
#' }
#' @source https://www.ncei.noaa.gov/
"wat05"
