#' Variability decomposition plot
#'
#' Generate plots that visualize the fit/effect and residuals from \code{eda_*} objects.
#'
#' @param dat A model of type \code{eda_pol}, \code{eda_npol}, \code{eda_lm},
#'   or \code{lm}, or a dataframe with a response variable, \code{y}, and a
#'   categorical variable, \code{x}.
#' @param y response (continuous) variable if \code{dat} is a dataframe, 
#'   \code{NULL} otherwise.
#' @param x categorical variable if \code{dat} is a dataframe, 
#'   \code{NULL} otherwise.
#' @param stat statistical function used to fit \code{y} by \code{x} if 
#'   \code{dat} is a dataframe, ignored otherwise.
#' @param p Power transformation to apply to univariate data. Ignored if 
#'   \code{dat} is not a dataframe.
#' @param tukey	Boolean determining if a Tukey transformation should be adopted 
#'   (TRUE) or if a Box-Cox transformation should be adopted (FALSE). Ignored if 
#'   \code{dat} is not a dataframe.
#' @param base Base used with the \code{log()} function if \code{p = 0}.
#' @param ... Additional arguments passed to the \code{.eda_plot_vardecomp} internal
#'   function.
#'   
#' @inheritDotParams  .eda_plot_vardecomp 
#' 
#' @returns A plot
#' 
#' @importFrom stats ave
#'  
#' @export
#'
#' @examples
#' # Compare regression model residuals to fit
#' M0 <- lm(mpg ~ hp + cyl, mtcars)
#' eda_vd(M0)
#' 
#' # By default, points sharing a identical value are "stacked"
#' # To jitter:
#' eda_vd(M0, overlap = "jitter")
#' 
#' # To overplot:
#' eda_vd(M0, overlap = "overplot")
#' 
#' # To represent fit using a boxplot
#' eda_vd(M0, type = "box")
#' 
#' # Decompose variability in response variable for a univariate dataset.
#' # Add labels to each level.
#' eda_vd(chickwts, weight, feed, label = TRUE)
#' 
#' # Add response variable (bisque colored boxplot)
#' eda_vd(chickwts, weight, feed, label = TRUE, show.resp = TRUE)
#' 
eda_vd <- function(dat, y=NULL, x=NULL, stat = median,
                   p = 1L, tukey = FALSE, base = exp(1), ...) {
  
  # Check for valid arguments
  dots <- list(...)
  dot_names <- names(dots)
  internal_args <- names(formals(.eda_plot_vardecomp))
  
  # Check for invalid names
  invalid <- setdiff(dot_names, internal_args)
  if (length(invalid) > 0) {
    warning(sprintf("Invalid arguments passed to %s: %s",
                    deparse(substitute(.eda_plot_vardecomp)),
                    paste(invalid, collapse = ", ")))
  }
  
  # Validate input
    input_typ <- c("data.frame", "eda_lm", "lm", "eda_rline", "eda_polish",
                   "eda_npol")
    if (!inherits(dat, input_typ)) 
    stop(cat("The input object must be one of the following: ", 
             paste0(input_typ,sep="", collapse=",")) )
    if (inherits(dat, "data.frame")){
      resp <- deparse(substitute(y))
      if (is.null(resp)) stop("The variable y must be specified")
      y <- eda_re(dat[[resp]], p = p, tukey = tukey, base = base)
      cat <- deparse(substitute(x))
      if (is.null(cat)) stop("The variable x must be specified")
      x <- dat[[cat]]
    }

    # Process dataframe (univariate analysis) ----
    if(inherits(dat, "data.frame")){
      fit <- ave(y, x, FUN = stat)
      fit_grp <- tapply(y, x, FUN = stat)
      residuals <- y - fit
      long <- data.frame(y, x, fit, residuals)
      names(long)[c(1,2)] <- c(resp, cat)
      effects <- list( fit = fit_grp -median(fit_grp))
      names(effects) <- cat
      .eda_plot_vardecomp(dat = long, response = resp, eff = effects, ...)
    }
    
    # Process eda_pol model ----
    if(inherits(dat, "eda_polish")){
      .eda_plot_vardecomp(dat = dat$long, response = dat$response, 
                          eff = dat$effects, ...)
    }
    
    # Process eda_npol model ----
    if(inherits(dat, "eda_npol")){
      .eda_plot_vardecomp(dat = dat$long, response = dat$response, 
                          eff = dat$effects, ...)
    }   
    
    # Process eda_lm model ----
    if(inherits(dat, "eda_lm")){
       fit <- dat$fitted.values - median(dat$fitted.values)
      .eda_plot_vardecomp(dat = dat$data, response = names(dat$data)[2], 
                          eff = list(fit = fit), ...)
    }   
    
    # Process eda_lm model ----
    if(inherits(dat, "eda_rline")){
      fit <- dat$fitted.values - median(dat$fitted.values)
      .eda_plot_vardecomp(dat = dat$data, response = names(dat$data)[2], 
                          eff = list(fit = fit), ...)
    }   
    
    # Process lm model ----
    if(inherits(dat, "lm")){
      fit <- dat$fitted.values - median(dat$fitted.values)
      data <- dat$model
      data$residuals <- dat$residuals
      .eda_plot_vardecomp(dat = data, response = names(dat$model)[1], 
                          eff = list(fit = fit), ...)
    } 
}


  