#' Replext Function for ANOVA Simulations in Table 1 Cell 1
#'
#' This function performs repeated simulations for ANOVA to determine minimum
#' sample sizes for given power and effect sizes, as well as calculating Type I error
#' rates. It is designed to replicate and extend the results for Table 1 Cell 1 in
#' Vanbrabant et al. (2015).
#'
#' @param S The number of datasets to generate for each simulation, default is 20000.
#' @param k The number of groups in the ANOVA design.
#' @param fs A vector of effect sizes to consider in the simulations.
#' @param n_start The starting sample size for the simulations.
#' @param constrs A vector of constraint types to be used in the simulations.
#' @param alpha The significance level used in hypothesis testing, default is 0.05.
#' @param pow The desired power for the statistical test, default is 0.80.
#' @param nmax The maximum sample size to consider in the simulations.
#'
#' @return A data frame containing the calculated Type I error rates and the minimum
#'   sample sizes required for each combination of effect size and constraint type.
#'
#' @details The function uses a nested approach, first determining minimum sample
#'   sizes for various combinations of effect size and constraints, and then
#'   calculating Type I error rates. It leverages the 'pj_pow' function for power
#'   calculation and integrates internal function 'find_min_sample_size' for
#'   determining the smallest sample size achieving the desired power.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' replext_t1_c1(S=5, fs = c(0.40), constrs = c(2))
#'
#' @export
replext_t1_c1 <- function(S = 20000, k = 3,
                          fs = c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
                          n_start = 6, constrs = c(0, 1, 2), alpha = 0.05,
                          pow = 0.80, nmax = 1000) {
  # Vectorized function to find the minimum sample size for a given power and effect size
  find_min_sample_size <- function(f, constr) {
    n <- n_start
    current_power <- 0.0
    while (current_power < pow && n <= nmax) {
      datasets <- generate_datasets(S, k, f, n)
      current_power <- pj_pow(datasets, constr, alpha)
      if (current_power >= pow) {
        return(n)
      }
      n <- n + 1
    }
    return(NA)
  }


  # Create a matrix for sample sizes using outer
  sample_sizes <- outer(fs, constrs, Vectorize(find_min_sample_size))
  dimnames(sample_sizes) <- list(paste0("f=", fs), paste0("constr=", constrs))

  # Function to calculate Type I error rates
  calc_type_I_error <- function(constr) {
    largest_effect_size_n <- min(sample_sizes[, paste0("constr=", constr)], na.rm = TRUE)
    S_half <- as.integer(S/2)
    type_I_error_datasets <- generate_datasets(S_half, k, 0, largest_effect_size_n)
    pj_pow(type_I_error_datasets, constr, alpha)
  }

  # Calculate Type I error rates
  type_I_errors <- sapply(constrs, calc_type_I_error)

  # Combine and rearrange the results
  results <- cbind(Type_I_Error = type_I_errors, as.data.frame(t(sample_sizes)))

  return(results)
}

#' Generate Replext Tables for Linear Regression Analysis
#'
#' This function generates replext tables for linear regression, similar to those
#' in Table 2 Cell 1 of the referenced paper. It computes minimum sample sizes for
#' various power and effect size combinations, and calculates Type I error rates.
#'
#' @param S The number of datasets to generate for each simulation, default is 20000.
#' @param p The number of predictors in the regression model.
#' @param f2s A vector of effect sizes to be used in the simulations.
#' @param n_start The starting sample size for the simulations.
#' @param constrs A vector of constraint types (number of inequality constraints)
#'   to be applied in the simulations.
#' @param rho The correlation coefficient between predictors, default is 0.0.
#' @param beta The regression coefficient for predictors, default is 0.1.
#' @param alpha The significance level used in hypothesis testing, default is 0.05.
#' @param pow The desired power for the statistical test, default is 0.80.
#' @param standardize A logical flag to indicate whether to standardize the
#'   predictors in the datasets, default is TRUE.
#' @param nmax The maximum sample size to consider in the simulations.
#'
#' @return A data frame containing Type I error rates and the minimum sample sizes
#'   required for each combination of effect size and constraint type.
#'
#' @details The function uses a nested approach to first determine minimum sample
#'   sizes for different combinations of effect size and constraints, and then
#'   calculates Type I error rates. It leverages the `lr_pow` function for power
#'   calculation and uses `generate_datasets_reg` for dataset generation.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' replext_t2_c1(S = 2, f2s = c(0.35), constrs = c(2))
#'
#' @export
replext_t2_c1 <- function(S = 20000, p = 3,
                          f2s = c(0.02, 0.05, 0.08, 0.10, 0.15, 0.20, 0.25, 0.35),
                          n_start = 6, constrs = c(0, 1, 2, 3), rho = 0.0, beta = 0.1,
                          alpha = 0.05, pow = 0.80, standardize = TRUE,nmax = 1000) {
  # Vectorized function to find the minimum sample size for a given power and effect size
  find_min_sample_size <- function(f2, constr) {
    n <- n_start
    current_power <- 0
    while (current_power < pow && n <= nmax) {
      datasets <- generate_datasets_reg(S, n, p, f2, rho, beta)
      current_power <- lr_pow(datasets, constr, standardize, alpha)
      if (current_power >= pow) {
        return(n)
      }
      n <- n + 1
    }
    return(NA)
  }

  # Create a matrix for sample sizes using outer
  sample_sizes <- outer(f2s, constrs, Vectorize(find_min_sample_size))
  dimnames(sample_sizes) <- list(paste0("f2=", f2s), paste0("constr=", constrs))

  # Function to calculate Type I error rates
  calc_type_I_error <- function(constr) {
    largest_effect_size_n <- min(sample_sizes[, paste0("constr=", constr)], na.rm = TRUE)
    S_half <- as.integer(S/2)
    type_I_error_datasets <- generate_datasets_reg(S_half, largest_effect_size_n, p, .Machine$double.eps, rho, beta)
    lr_pow(type_I_error_datasets, constr, standardize, alpha)
  }

  # Calculate Type I error rates
  type_I_errors <- sapply(constrs, calc_type_I_error)

  # Combine and rearrange the results
  results <- cbind(Type_I_Error = type_I_errors, as.data.frame(t(sample_sizes)))

  return(results)
}

