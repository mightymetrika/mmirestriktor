#' Calculate Equally Spaced Differences
#'
#' This function calculates the equally spaced differences between means in a
#' one-way ANOVA setup. It is based on the formula presented in Vanbrabant et al.
#' (2015) for calculating differences between means, d.
#'
#' @param k An integer representing the number of groups (k = 3, ..., 8).
#' @param f A numeric value representing the effect size (f = 0.10, 0.15, 0.20,
#' 0.25, 0.30, 0.40). Typical values represent small (0.10), medium (0.25), and
#' large (0.40) effects.
#'
#' @return A numeric value representing the equally spaced difference, d, based
#' on the number of groups (k) and the effect size (f).
#' @export
#'
#' @examples
#' d_eq_spaced(4, 0.25) # For k = 4 and f = 0.25
d_eq_spaced <- function(k, f){
  2*f*sqrt(k)/sqrt(sum(sapply(1:k, function(i)(2*i - 1 - k)^2)))
}

#' Calculate Group Means for One-Way ANOVA
#'
#' This function calculates the means of different groups in a one-way ANOVA
#' setting. It uses the equally spaced difference calculated by 'd_eq_spaced' and
#' follows the approach described in Vanbrabant et al. (2015).
#'
#' @param k An integer representing the number of groups (k = 3, ..., 8).
#' @param f A numeric value representing the effect size (f = 0.10, 0.15, 0.20,
#' 0.25, 0.30, 0.40).Typical values represent small (0.10), medium (0.25), and
#' large (0.40) effects.
#'
#' @return A numeric vector containing the means of the groups. Each element of
#' the vector corresponds to a group mean.
#' @export
#'
#' @examples
#' mui(4, 0.25) # For k = 4 and f = 0.25
mui <- function(k, f){
  sapply(1:k, function(i){
    -(k-1)*d_eq_spaced(k, f)/2 + (i - 1)*d_eq_spaced(k, f)
    })
}

#' Generate Datasets for ANOVA Simulation
#'
#' This function generates a specified number of datasets for use in ANOVA
#' simulations.Each dataset is generated based on a specified number of groups,
#' effect size, and sample size per group. The data generation follows the model:
#' yi = μ1xi1 + ... + μkxik + ei, as described in Vanbrabant, Van De Schoot,
#' and Rosseel (2015).
#'
#' @param S Integer, the number of datasets to generate.
#' @param k Integer, the number of groups (k = 3, ..., 8).
#' @param f Numeric, the effect size (f = 0.10, 0.15, 0.20, 0.25, 0.30, 0.40).
#' @param n Integer, the sample size per group.
#'
#' @return A list of data frames, each representing a dataset. Each data frame
#' contains two columns: 'x', indicating group membership, and 'y', representing
#' the dependent variable generated according to the model.
#' @export
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained statistical inference: sample-size tables for ANOVA and regression. Frontiers in Psychology, 5. DOI:10.3389/fpsyg.2014.01565. URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' generate_datasets(S = 2, k = 4, f = 0.25, n = 30)
generate_datasets <- function(S, k, f, n) {
  # Initialize empty list
  datasets <- vector("list", S)

  # Compute group means
  means <- mui(k, f)

  # Generate S data sets
  datasets <- lapply(1:S, function(s) {
    dataset <- lapply(1:k, function(x) {
      y <- means[x] + stats::rnorm(n, mean = 0, sd = 1)  # y_i = μ_j + e_i
      data.frame(
        group = rep(x, n),
        y = y
      )
    })
    do.call(rbind, dataset)
  })

  return(datasets)
}

