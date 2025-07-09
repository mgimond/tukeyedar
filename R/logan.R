#' Logan airport dataset
#'
#' Median delay times for passenger carriers flying out of Logan
#' International Airport (Boston, USA) by month and am/pm scheduled departure
#' for the 2023 year.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{am_pm}{Morning (\code{am}) or afternoon (\code{pm}) scheduled flight}
#'   \item{carrier}{Passenger carrier. \code{AA} = American Airlines,
#'                  \code{DL} = Delta Airlines, \code{B6} = JetBlue Airways,
#'                  \code{UA} =  United Airlines, \code{WN} = Southwest Airlines.}
#'   \item{month}{Month}
#'   \item{delay}{Delay time in minutes. Negative values indicate an earlier than
#'                scheduled departure.}
#' }
#'
#' @usage data(logan)
#' @keywords datasets
"logan"
