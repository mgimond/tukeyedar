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
#' @source
#'    Robust regression using repeated medians, Andrew F. Siegel, Biometrika,
#'    vol 69, n 1, 1982.
#'    Understanding robust and exploratory data analysis, by D.C. Hoaglin,
#'    F. Mosteller and J.W. Tukey. 1983 (page 139)
"nine_point"
