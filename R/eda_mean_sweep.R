#' @title Exploratory ANOVA Decomposition
#'
#' @description Performs an exploratory decomposition of a numeric response variable into
#'   additive components (global mean, main effects, interaction effects, and
#'   residuals) using a sequential mean sweeping algorithm. It supports both
#'   balanced and unbalanced designs, and can account for nested factor structures.
#'
#' @param data A data frame containing the response and factor variables.
#' @param ... Unquoted variable names: The first variable must be the numeric
#'   response, followed by one or more factor variables.
#' @param max_order An integer specifying the maximum order of
#'   interaction effects to include. Main effects are always computed if factors
#'   are provided.
#'   \describe{
#'     \item{\code{max_order = 1}}{ (default) Only main effects are calculated and included
#'           in the decomposition.}
#'     \item{\code{max_order = 2}}{Main effects and all two-way interactions among the
#'     specified factors are included.}
#'     \item{\code{max_order = k}}{Main effects and all interaction terms up to order \code{k}
#'     are included.}
#'    }
#' @param nesting A list of character vectors specifying nested relationships.
#'   Each element should be a pair like \code{c("Parent", "Child")} indicating
#'   that \code{Child} is nested within \code{Parent}. If provided, the function
#'   will automatically reorder the sweeping sequence to ensure that parent
#'   factors are swept before their nested children.
#' @param p Numeric. A power transformation to apply to the response variable
#'   before decomposition.
#' @param tukey Logical. If \code{TRUE}, Tukey's transformation
#'   is applied. If \code{FALSE}, a Box-Cox style transformation
#'   is used.
#' @param base Numeric. The base for the logarithm if a logarithmic
#'   transformation (\code{p=0}) is applied. Defaults to \code{exp(1)}
#'   (natural logarithm).
#'
#' @return A list of class \code{"eda_mean_sweep"} with the following
#'   components:
#' \describe{
#'   \item{global}{The \bold{common} or \bold{global mean}}
#'   \item{response}{The name of the response variable used in the analysis.}
#'   \item{effects}{A named list of \bold{main and interaction effects}. Each element
#'     is a named vector of centered effects, representing the deviations from
#'     the adjusted mean attributable to that factor or interaction.}
#'   \item{residuals}{A numeric vector of \bold{residuals} after the global mean
#'     and all specified effects have been "swept out" (subtracted) from the
#'     response variable.}
#'   \item{long}{The original data frame with an added \code{residuals} column,
#'     which can be useful for further exploratory plotting.}
#' }
#'
#' @details
#' This function implements the value-splitting and sweeping procedure
#' central to Exploratory Data Analysis (EDA) of Analysis of Variance (ANOVA).
#' It systematically decomposes the response variable into additive
#' overlays, which when recombined, recover the original data.
#'
#' The decomposition process is sequential: the global mean is first removed,
#' then main effects are calculated and subtracted, followed by interaction
#' effects up to \code{max_order}. Each effect is
#' calculated as the mean deviation from the previously swept \code{y}
#' for its respective levels, and then subtracted, leaving the remaining \code{y}
#' for subsequent effects or as residuals.
#'
#' Nested factors are specifically handled by computing their effects
#' within each level of their parent factor ensuring appropriate variance
#' attribution.
#' \cr \cr
#' Unbalanced designs are supported by calculating group wise means
#' allowing for a robust decomposition even when cell counts are unequal.
#'
#' \strong{Important consideration for factor order}: The order in which factors are
#' specified can significantly affect the decomposition, particularly when factors
#' are correlated or nested. Factors listed earlier in the arguments are "swept"
#' first and may absorb variation that might otherwise be attributed to factors listed
#' later in the arguments. To manage this, the \code{nesting} argument provides a
#' structured way to enforce a logical sweeping sequence, ensuring parent factors
#' are accounted for before their nested children.
#'
#' @references
#' Hoaglin, D. C., Mosteller, F., & Tukey, J. W. (1991).
#' \emph{Fundamentals of Exploratory Analysis of Variance}. Wiley.
#'
#' @seealso
#' \code{\link{eda_anova_table}} for computing the ANOVA table (e.g., Sums of Squares,
#' Mean Squares, F-statistics) from the output of this function.
#' \code{\link{plot.eda_mean_sweep}} for visualizing the decomposed effects and residuals.
#'
#' @importFrom utils combn
#'
#' @examples
#' # A one-way analysis of chickwts. "weight" is the response and "feed" is
#' # the factor. First column passed to the function must be the response variable
#' # ("weight" in this example)
#' M0 <- eda_mean_sweep(chickwts, weight, feed)
#'
#' # Global (overall) mean weight
#' M0$global
#'
#' # Effect level values
#' M0$effects
#'
#' # Compare residuals' spread to those of the effects
#' plot(M0, label = TRUE)
#'
#' # A two-way analysis without replicates (i.e. one value per cell)
#' M0 <- eda_mean_sweep(inf_mort, perc, region, edu)
#' plot(M0, label = TRUE)
#'
#' # A two -way analysis with replicates (i.e. multiple values per cell)
#' # Include 2-way interaction effects
#' M0 <- eda_mean_sweep(feav5_12, Weight, Level, Type, max_order = 2)
#' plot(M0, label = TRUE)
#'
#' # A three-way analysis with nested factors. There are two embedded nests:
#' # Sample embedded under Run and Run embedded under Lot.
#' # Response variable is decomposed across ALL factors leaving 0 residuals
#' M0 <- eda_mean_sweep(feav5_14, Absorption, Lot, Run, Sample,
#'                      nesting = list(c("Lot", "Run"), c("Run","Sample")))
#' plot(M0, rotate = TRUE)
#'
#' # A traditional ANOVA table can be generated from the eda_mean_sweep object
#' eda_anova_table(M0)
#'
#' # A three-way analysis with one nested factor (State within Grouping)
#' # If there is just one nesting object, the nesting argument can be passed
#' # a c() object without the need of embedding it in a list() object
#' M0 <- eda_mean_sweep(feav1_5, votes, State, Year, Grouping,
#'                      nesting = c("Grouping", "State"))
#' plot(M0, rotate = TRUE)
#'
#' # A three-way analysis with 2-way interactions
#' M0 <- eda_mean_sweep(feav6_8, Hard, Dentist, Method, Alloy, Temp, max_order = 2)
#' plot(M0, rotate = TRUE, order = FALSE) # Preserve factor order as entered in arguments
#' plot(M0, rotate = TRUE) # By default, factors are ordered by range
#'
#' # A three-way analysis with 3-way interactions
#' M0 <- eda_mean_sweep(feav6_8, Hard, Dentist, Method, Alloy, Temp, max_order = 3)
#' plot(M0, rotate = TRUE)

#' @export

eda_mean_sweep <- function(data, ..., max_order = 1, nesting = NULL, p = 1,
                                     tukey = FALSE, base = exp(1)) {
  vars <- substitute(list(...))[-1]
  if (length(vars) < 2) stop("Please provide at least one response and one factor variable.")
  response <- deparse(vars[[1]])

  # Store original factor order for initial checks and interaction generation
  factors_initial <- sapply(vars[-1], deparse)
  # This will hold the sweeping order, potentially reordered by dependency_order
  factors_current_sweep_order <- factors_initial

  # Basic input checks for data frame and variables
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!response %in% names(data)) stop(paste0("Response variable '", response, "' not found in 'data'."))
  if (!is.numeric(data[[response]])) stop("Response variable must be numeric.")
  missing_factors <- setdiff(factors_initial, names(data))
  if (length(missing_factors) > 0) stop(paste0("Missing factor(s): ", paste(missing_factors, collapse = ", ")))

  # Coerce all specified factors to R factor type
  for (f in factors_initial) {
    if (!is.factor(data[[f]])) data[[f]] <- as.factor(data[[f]])
  }

  y <- data[[response]]

  # Apply power transformation if specified
  if(p != 1L){
    y <- eda_re(y, p = p, tukey = tukey, base = base)
    warning("Power transformation (eda_re) not applied as its definition is missing.")
  }

  global_mean <- mean(y, na.rm = TRUE)
  adjusted_y <- y - global_mean
  effects <- list()

  # --- Nesting Pre-processing: Create a map for quick parent lookup and order factors ---
  child_to_parent_map <- list() # Map: child_factor_name -> parent_factor_name
  nesting_list_of_vectors <- list() # Store nesting pairs in a consistent list(c("P","C")) format

  if (!is.null(nesting)) {
    # Handle different input formats for 'nesting' argument
    if (is.character(nesting) && length(nesting) > 0) {
      if (length(nesting) %% 2 != 0) {
        stop("If 'nesting' is a character vector, it must contain an even number of elements (parent-child pairs).")
      }
      # Convert a flat character vector (e.g., c("P1", "C1", "P2", "C2"))
      # into a list of c("Parent", "Child") vectors.
      nesting_list_of_vectors <- split(nesting, ceiling(seq_along(nesting) / 2))
    } else if (is.list(nesting)) {
      nesting_list_of_vectors <- nesting
    } else {
      stop("'nesting' argument must be NULL, a character vector (e.g., c('Parent1','Child1')), or a list of character vectors (e.g., list(c('Parent1','Child1'))).")
    }

    # Populate child_to_parent_map for quick lookup during sweeping
    for (pair in nesting_list_of_vectors) {
      if (length(pair) == 2) {
        child_to_parent_map[[pair[2]]] <- pair[1]
      } else {
        warning(paste("Nesting pair", paste(pair, collapse=","), "is not a 'c(Parent, Child)' vector. Skipping this pair."))
      }
    }

    # Reorder factors for sweeping to ensure parents are swept before their children
    factors_current_sweep_order <- dependency_order(factors_initial, nesting_list_of_vectors)
  }
  # --- End Nesting Pre-processing ---

  # Main effects sweeping loop (incorporates correct nesting logic)
  for (f in factors_current_sweep_order) {
    # Check if the current factor 'f' is a child in any nested relationship
    if (f %in% names(child_to_parent_map)) {
      # Recursively collect all ancestors of the current factor
      get_ancestor_chain <- function(factor_name, parent_map) {
        chain <- c()
        current <- factor_name
        while (!is.null(parent_map[[current]])) {
          current <- parent_map[[current]]
          chain <- c(current, chain)
        }
        return(chain)
      }

      ancestor_chain <- get_ancestor_chain(f, child_to_parent_map)
      grouping_vars <- c(ancestor_chain, f)

      # Compute nested effect within full ancestry
      grouping_factor <- interaction(data[, grouping_vars], drop = TRUE)

      mean_by_ancestors_child <- ave(adjusted_y, grouping_factor, FUN = mean, na.rm = TRUE)
      mean_by_ancestors <- ave(adjusted_y, interaction(data[, ancestor_chain], drop = TRUE), FUN = mean, na.rm = TRUE)


      effect_vector <- mean_by_ancestors_child - mean_by_ancestors

      # Label the effect
      nested_term_name <- paste0(f, " (within ", paste(ancestor_chain, collapse = ":"), ")")

      # Compute effect values
      nested_levels_for_tapply <- interaction(data[, grouping_vars], drop = TRUE)
      effects[[nested_term_name]] <- tapply(effect_vector, nested_levels_for_tapply, mean, na.rm = TRUE)


    } else { # This is a standard main effect (not a nested child)
      # Calculate the main effect as the deviation of group means from the overall mean of `adjusted_y`
      effect_vector <- ave(adjusted_y, data[[f]], FUN = function(x) mean(x, na.rm = TRUE) - mean(adjusted_y, na.rm = TRUE))
      effects[[f]] <- tapply(effect_vector, data[[f]], mean, na.rm = TRUE)
    }

    # Subtract the calculated effect from `adjusted_y` for the next sweep
    adjusted_y <- adjusted_y - effect_vector
  }

  # Interactions sweeping loop
  interaction_effects <- list()
  if (max_order >= 2) {
    # Generate combinations from the factors present in the data for interactions.
    # Crucially, exclude combinations that represent a nested parent-child pair
    # as these are already captured by the 'nested main effect' component.
    all_child_factors <- names(child_to_parent_map) # All factors identified as children in nesting

    for (order in 2:max_order) {
      combos <- combn(factors_initial, order, simplify = FALSE) # Use initial factors for combn

      for (combo in combos) {
        # Check if this combination is a parent-child nested relationship.
        # This helps avoid creating a "crossed interaction" term for what's treated
        # as a nested "main effect"
        is_handled_by_nesting <- FALSE
        if (length(combo) == 2) { # Only check 2-way combos for parent-child relationship
          f1 <- combo[1]
          f2 <- combo[2]
          # If f2 is a child of f1, or f1 is a child of f2, it's a nested pair.
          if ((f2 %in% all_child_factors && child_to_parent_map[[f2]] == f1) ||
              (f1 %in% all_child_factors && child_to_parent_map[[f1]] == f2)) {
            is_handled_by_nesting <- TRUE
          }
        }

        if (is_handled_by_nesting) {
          next # Skip this combination; it's considered covered by nesting
        }

        # For regular crossed interactions
        interaction_name <- paste(combo, collapse = ":")
        interaction_levels <- interaction(data[, combo], drop = TRUE)

        effect_vector <- ave(adjusted_y, interaction_levels,
                             FUN = function(x) mean(x, na.rm = TRUE) - mean(adjusted_y, na.rm = TRUE))
        interaction_effects[[interaction_name]] <- tapply(effect_vector, interaction_levels, mean, na.rm = TRUE)
        adjusted_y <- adjusted_y - effect_vector
      }
    }
  }

  # Residuals: The remaining variability after subtracting all effects
  data$residuals <- adjusted_y

  # Compile and return the results as an "eda_mean_sweep" object
  result <- list(
    global = global_mean,
    response = response,
    effects = c(effects, interaction_effects), # Combine main and interaction effects
    residuals = adjusted_y,
    long = data # Include the data frame with residuals and factor info for ANOVA table
  )
  class(result) <- "eda_mean_sweep"
  return(result)
}


dependency_order <- function(factors, nesting_list_of_vectors) {
  ordered <- character(0)
  remaining <- unique(factors)

  # Create a quick lookup for children and their direct parents from the nesting list
  child_to_parent_map <- list()
  for (pair in nesting_list_of_vectors) {
    if (length(pair) == 2) {
      child_to_parent_map[[pair[2]]] <- pair[1]
    }
  }

  while (length(remaining) > 0) {
    found_a_factor_to_add <- FALSE
    for (f in remaining) {
      # Check if f is a child factor in any nesting relationship
      is_child_factor <- f %in% names(child_to_parent_map)
      parent_of_f <- if (is_child_factor) child_to_parent_map[[f]] else NULL

      # A factor can be processed if it's not a child, or if its parent has already been processed
      if (!is_child_factor || parent_of_f %in% ordered) {
        ordered <- c(ordered, f)
        remaining <- setdiff(remaining, f)
        found_a_factor_to_add <- TRUE
        break # Move to the next iteration of the while loop to check remaining factors
      }
    }
    if (!found_a_factor_to_add && length(remaining) > 0) {
      # This indicates a circular dependency or unresolvable factors
      stop("Circular dependency or unresolvable nesting order detected among factors. Check your 'nesting' argument.")
    }
  }
  ordered
}
