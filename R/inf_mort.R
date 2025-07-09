#' Infant mortality dataset
#'
#' Infant mortality dataset pulled from Table 6-2 of "Understanding 
#' Robust and Exploratory Data Analysis".
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{region}{Regional effect}
#'   \item{edu}{Father's education effect
#'   \itemize{
#'      \item{\code{ed8}: Less than High School Graduate}
#'      \item{\code{ed12}: High School Graduate}
#'      \item{\code{ed13to15}: Some College or Associate’s Degree}
#'      \item{\code{ed16}: Bachelor’s Degree or Greater}
#'     }} 
#'   \item{perc}{Numbers of infant deaths per 1000 live births}
#' }
#'
#' @references
#'   \itemize{
#'      \item{Emerson, John D., and David C. Hoaglin. (1983). Understanding 
#'            Robust and Exploratory Data Analysis. John Wiley & Sons.}
#'     }  
#'
#' @usage data(inf_mort)
#' @keywords datasets
"inf_mort"
