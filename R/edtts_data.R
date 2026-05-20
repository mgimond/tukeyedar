#' Confidence in American institutions survey data
#'
#' Table 2-12 in \emph{Exploring Data tables, Trends, and Shapes} listing the
#' percentage of respondents indicating "a great deal" or "quite a lot" of
#' confidence in American institutions between the years 1973 and 1980.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{institution}{American institutions}
#'   \item{year}{Survey year}
#'   \item{perc}{Percentage of respondents indicating "a great deal" or
#'               quite a lot" of confidence in American institutions}
#' }
#'
#' @references
#'   \itemize{
#'      \item{Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985).
#'            Exploring data tables, trends, and shapes. Wiley.}
#'    }
#'
#' @usage data(edtts2.12)
#' @keywords datasets
"edtts2.12"

#' Dataset that is perfectly additive in the square root scale
#'
#' Table 4-25 of \emph{Exploring Data tables, Trends, and Shapes} is a synthetic
#' dataset of a three-way table. The key characteristic of this data is that if
#' one takes the square root of each value in the table, the resulting
#' values would perfectly fit a simple additive model. This means that, in the
#' square root scale, there are no interaction effects between the factors.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{A}{Effect A}
#'   \item{B}{Effect B}
#'   \item{C}{Effect C}
#'   \item{Value}{Response variable}
#' }
#'
#' @details
#' The data is constructed using specific values
#' \itemize{
#'   \item{grand mean:} \eqn{\mu = 20}
#'   \item{factor A:} \eqn{(\alpha_1, \alpha_2, \alpha_3) = (-1, 0, 3)}
#'   \item{factor B:} \eqn{(\beta_1, \beta_2, \beta_3) = (-1, 0, 1)}
#'   \item{factor C:} \eqn{(\gamma_1, \gamma_2, \gamma_3) = (-1, 0, 2)}
#' }
#'
#' @references
#'   \itemize{
#'      \item{Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985).
#'            Exploring data tables, trends, and shapes. Wiley.}
#'    }
#'
#' @usage data(edtts4.25)
#' @keywords datasets
#'
#' @examples
#' # Median polish of raw values
#' M0 <- eda_npol(edtts4.25, Value, A, B, C)
#'
#' # There is evidence of strong interaction as shown in this plot
#' plot(M0, plot = "diagnostic")
#'
#' # Taking the square root eliminates interaction effects
#' # Note that this may throw a warning if loess option is
#' # set to TRUE
#' M1 <- eda_npol(edtts4.25, Value, A, B, C, p = 0.5)
#' plot(M1, plot = "diagnostic", loe = FALSE)
"edtts4.25"

#' Yarn Data
#'
#' Table 4-19 of \emph{Exploring data tables, trends, and shapes} shows data from a
#' textile experiment where specimens of worsted yarn were repeatedly loaded
#' until they broke. The table shows the resulting number of cycles to failure
#' for the yarn under different conditions related to the length of the specimen,
#' the amplitude of the loading, and the load applied.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{Length}{Length effect}
#'   \item{Amplitude}{Amplitude effect}
#'   \item{Load}{Load effect}
#'   \item{Cycles}{Response variable}
#' }
#'
#' @references
#'   \itemize{
#'      \item{Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985).
#'            Exploring data tables, trends, and shapes. Wiley.}
#'    }
#'
#' @usage data(yarn)
#' @keywords datasets
"yarn"

#' Weight Gain in Pigs Data
#'
#' Table 4-2 in \emph{Exploring Data tables, Trends, and Shapes} is
#' derived from an experiment conducted at the Iowa Agricultural Experiment
#' Station. It summarizes the weight gain of male pigs across various combinations
#' of three dietary supplements
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{Methionine}{Methionine offered at 3 levels: 0%, 0.25% amd 0.50%}
#'   \item{Protein}{Soybean meal protein offered at 2 levels: 12% and 14%}
#'   \item{Lysine}{Lysine offered at 4 levels: 0%, 0.05%, 0.1% and 0.15%}
#'   \item{WeightGain}{Treatment totals of average daily weight gains
#'                     (measured in units of .01 lb) for the combined
#'                     results for two pigs}
#' }
#'
#' @references
#'   \itemize{
#'      \item{Hoaglin, David C. and Mosteller, Frederick and Tukey, John W. (1985).
#'            Exploring data tables, trends, and shapes. Wiley.}
#'    }
#'
#' @usage data(edtts4.2)
#' @keywords datasets
"edtts4.2"

