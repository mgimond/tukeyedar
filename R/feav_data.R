#' @title
#' Presidential Election Results for Franklin D. Roosevelt (1932-1944)
#'
#' @description
#' This dataset contains results from presidential elections in which Franklin
#' D. Roosevelt was elected, covering the years 1932, 1936, 1940, and 1944. The
#' data are organized into 13 distinct groupings of states with three states
#' represented within each grouping. Data are pulled from table 1-5 of the
#' referenced source.
#' \cr  \cr
#' Each numerical entry in \code{votes} is the ratio of the votes for
#' Roosevelt to the sum of votes for Roosevelt and his Republican
#' opponent (scaled by x1000).
#' \cr \cr
#' The dataset serves as an example of a two-way, nested, dataset with the
#' \code{State} votes nested under separate \code{Grouping} regions.
#'
#' @format A \code{data.frame} with 156 rows and the following columns:
#'
#' \describe{
#'    \item{Grouping}{A factor variable (1-13) indicating the specific grouping of states.}
#'    \item{State}{A character string representing the name of the state within each grouping.}
#'    \item{Year}{A factor of the election year.}
#'    \item{votes}{An integer of the ratio of votes (x1000) carried by
#'      Roosevelt. This is the primary response variable.}
#'  }
#'
#' @source
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' M0 <- eda_mean_sweep(feav1_5, votes, State, Year, Grouping, nesting = c("Grouping", "State"))
#' plot(M0,  rotate = TRUE)
#'
"feav1_5"

#' @title Weight Gain in Rats on Different Diets
#'
#' @description This dataset contains the weight gain (in grams) of
#' rats subjected to different dietary treatments. Data are pulled from table
#' 5-12 of the referenced source.
#' \cr \cr
#' This dataset serves as an example of a two-way table with (balanced)
#' replicates (10 replicates per \code{Level} and \code{Type} combinations).
#'
#' @format A \code{data.frame} with 60 rows and the following columns:
#'
#' \describe{
#'   \item{Level}{A character indicating the amount of protein.
#'      Either \code{High} or \code{Low}. }
#'   \item{Type}{A character indicating the specific diet administered
#'   to each group of rats.}
#'   \item{Weight}{An integer vector representing the weight gain,
#'    in grams, for each rat. This is the response variable.}
#' }
#'
#' @source
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' # Include second level interactions
#' M0 <- eda_mean_sweep(feav5_12, Weight, Level, Type, max_order = 2)
#' plot(M0, label = TRUE)
#'
"feav5_12"

#' @title ELISA Test Absorbance for HIV Positive Controls
#'
#' @description This dataset contains measurements of light absorbance for
#'   positive control samples from an ELISA (Enzyme-Linked Immunosorbent Assay)
#'   test for HIV. Data are pulled from table 5-14 of the referenced
#'   source.
#'   \cr \cr
#'   The dataset is an example of a two-factor experimental design with nested
#'   factors. The experiment involved five different production lots. For
#'   each lot, five individual runs of the test were performed. Within each run,
#'   absorbance readings were recorded for three positive control samples. As such
#'   the \code{Sample} factor is nested under the \code{Run} factor which is itself
#'   nested under the \code{lot} factor.
#'
#' @format A \code{data.frame} with 75 rows and the following columns:
#'
#' \describe{
#'    \item{Lot}{A character indicating the specific production lot
#'        of the ELISA test \code{A, B, C, D, or E}.}
#'    \item{Run}{A character  representing the individual test runs.
#'        \code{Run} is nested within \code{Lot}, meaning \code{Run 1} for
#'        \code{Lot A} is distinct from \code{Run 1} for \code{Lot B}.}
#'    \item{Sample}{A character specifying the sample. There are
#'        three replicate samples per run.}
#'    \item{Absorption}{A numeric vector representing the light
#'        absorbance readings (in arbitrary units) for the positive control
#'        samples. This is the response variable.}
#' }
#'
#' @source
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' # Partition response variable across ALL factors. Residuals should be 0.
#' M0 <- eda_mean_sweep(feav5_14, Absorption, Lot, Run, Sample,
#'                      nesting = list(c("Lot", "Run"), c("Run","Sample")))
#' plot(M0, rotate = TRUE)
#'
"feav5_14"

#'
#' @docType data
#' @title Simulated Tumor Measurements by Oncologists
#'
#' @description This dataset contains measurements of cross-sectional area (in
#'   mm²) of simulated solid tumors. These measurements were estimated by 13
#'   different oncologists. Data are pulled from table 6-1 of the referenced
#'   source.
#'   \cr \cr
#'   This dataset represents a three-factor experimental design with fully
#'   crossed factors. The primary factors involved are the Oncologist (the
#'   individual performing the estimate), the Material from which the tumor
#'   model was made (Cork or Rubber), and the Form (or shape) of the tumor
#'   (Small, Oblong, or Large).
#'
#' @format A \code{data.frame} with 78 rows and the following columns:
#'
#' \describe{
#'    \item{Oncologist}{A character uniquely identifying each
#'              oncologist who provided a cross-sectional area estimate.}
#'    \item{Material}{A character indicating the material of the
#'              simulated tumor model (\code{Cork or Rubber}).}
#'    \item{Form}{A character representing the shape of the simulated
#'              tumor (\code{Small, Oblong or Large}).}
#'    \item{Area}{A numeric vector representing the average cross-sectional
#'               area (in mm²) of the tumor as estimated by the oncologist. This
#'               is the response variable.}
#'  }
#'
#' @source
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' M0 <- eda_mean_sweep(feav6_1, Area, Oncologist, Material, Form, max_order = 2)
#' plot(M0, order = FALSE, rotate = TRUE)
#'
"feav6_1"

#' @title Replicated Tumor Measurements by Oncologists
#'
#' @description This dataset contains replicated measurements of cross-sectional
#' area (in mm²) of simulated solid tumors, as estimated by 13
#' different oncologists. The data represents a fully crossed three-factor
#' experimental design with replication within cells--for each unique combination
#' of the three factors, two replicate measurements were taken. Data are pulled
#' from table 6-5 of the referenced source.
#' \cr \cr
#' The data are an extension of the data presented in \code{feav6_1}, now including
#' the two replicate observations instead of just their averages.
#' As such, each combination of factors appears twice in the data table
#' (once for each replicate) with potentially different response values.
#'
#' @format A \code{data.frame} with 156 rows and the following columns:
#'\describe{
#'   \item{Oncologist}{A character uniquely identifying each
#'              oncologist who provided a cross-sectional area estimate.}
#'   \item{Material}{A character indicating the material of the
#'              simulated tumor model (\code{Cork or Rubber}).}
#'   \item{Form}{A character representing the shape of the simulated
#'              tumor (\code{Small, Oblong or Large}).}
#'   \item{Area}{A numeric vector representing the cross-sectional
#'               area (in mm²) of the tumor as estimated by the oncologist. This
#'               is the response variable.}
#'  }
#'
#' @source Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' M0 <- eda_mean_sweep(feav6_5, Area, Oncologist, Material, Form, max_order = 3)
#' plot(M0, order = FALSE, rotate = TRUE)
#'
#'
"feav6_5"

#' @title Diamond Pyramid Hardness Number of Dental Fillings
#'
#' @description This dataset contains measurements of the Diamond Pyramid
#'   Hardness Number (DPHN) of simulated dental fillings. The response variable
#'   represents the sum of 10 individual DPHN readings for each sample.
#'   Data are pulled from table 6-8 of the referenced source.
#'   \cr \cr
#'   The data were collected from a study involving five different
#'   dentists. The dental fillings were made from two types of gold alloy
#'   (\code{A1} and \code{AuCa}), sintered at three different temperatures
#'    (\code{1500F}, \code{1600F}, \code{1700F}), and prepared using
#'    three condensation methods (\code{1}, \code{2}, \code{3}).
#'
#' @format A \code{data.frame} with 90 rows and the following columns:
#'
#' \describe{
#'     \item{Dentist}{A factor with 5 levels, each identifying a denstist.}
#'     \item{Method}{A factor denoting the condensation method used.}
#'     \item{Alloy}{A character indicating the type of gold alloy used.}
#'     \item{Temp}{A character representing the sintering temperature.}
#'     \item{Hard}{An integer vector representing the summed Diamond
#'        Pyramid Hardness Number (DPHN) for each combination of factors.
#'        This is the response variable.}
#' }
#'
#' @source Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @examples
#' M0 <- eda_mean_sweep(feav6_8, Hard, Dentist, Method, Alloy, Temp, max_order = 3)
#' plot(M0, rotate = TRUE, order = FALSE)
#'
"feav6_8"


