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
        x = as.factor(rep(x, n)),
        y = y
      )
    })
    do.call(rbind, dataset)
  })

  return(datasets)
}

pj_pow <- function(df_list, constr = 0, alpha = 0.05){
  # Number of groups
  grps <- length(unique(df_list[[1]]$x))

  # Check if 'constr' is a valid integer within the expected range
  if (!is.numeric(constr) || constr %% 1 != 0 || constr < 0 || constr >= grps) {
    stop("'constr' must be a non-negative integer less than the number of groups")
  }


  # Construct the constraint string based on the number of constraints
  if (constr > 0) {
    # Inequality constraints
    cn <- paste0("x", grps, " > x", 1:constr, collapse = " & ")
  } else {
    # Equality constraints
    cn <- paste0("x", 1:grps, collapse = " == ")
  }

  # Calculate power
  pj_pow <- mean(sapply(df_list, function(df) {
    # Run model
    mod <- mmir_model(y ~ -1 + x, data = df, engine = "lm")
    imod <- restriktor::iht(mod, constraints = cn)

    # Check p-value based on the type of constraint
    pval <- if (constr > 0) {
      # For inequality constraints, use Type A and Type B p-values
      imod[["B"]]$pvalue[1] >= alpha && imod[["A"]]$pvalue[1] <= alpha
    } else {
      # For equality constraints, use the regular F-test p-value
      imod$pvalue
    }

    # Return power (as.integer for inequality, directly for equality)
    if (constr > 0) {
      as.integer(pval)
    } else {
      pval
    }
  }))

  return(pj_pow)
}


sim_reg <- function(n, p, f2, rho, beta = 0.1) {

  # Validate beta
  if(length(beta) == 1) {
    beta <- rep(beta, p)
  } else if(length(beta) != p) {
    stop("Length of beta must equal p")
  }

  # Rest of code remains the same

  R2 <- f2/(1+f2)

  Sigma <- matrix(rho, ncol = p, nrow = p)
  diag(Sigma) <- 1

  sigma2 <- (t(beta) %*% Sigma %*% beta) * (1 - R2)/R2

  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)

  e <- stats::rnorm(n, mean = 0, sd = sqrt(sigma2))

  eta <- drop(X %*% beta)

  y <- eta + e

  return(list(y = y, X = X))
}

generate_datasets_reg <- function(S = 20000, n, p, f2, rho, beta = 0.1) {

  datasets <- vector("list", S)

  datasets <- lapply(1:S, function(x) {

    sim_data <- sim_reg(n, p, f2, rho, beta)
    as.data.frame(cbind(sim_data$y, sim_data$X))

  })

  names <- c("y", paste0("x", 1:p))
  datasets <- lapply(datasets, stats::setNames, nm = names)

  return(datasets)

}

lr_pow <- function(df_list, constr = 0, standardize = TRUE, alpha = 0.05){

  # Number of variables
  p <- ncol(df_list[[1]]) - 1

  # Check if 'constr' is a valid integer within the expected range
  if (!is.numeric(constr) || constr %% 1 != 0 || constr < 0 || constr > p) {
    stop("'constr' must be a non-negative integer less than or equal to the number of independent variables")
  }

  # Standardize if requested
  if(standardize) {
    df_list <- lapply(df_list, function(df) {
      df[,2:p] <- scale(df[,2:p])
      df
    })
  }

  # Construct constraints
  if(constr > 0) {
    cn <- paste0("x", p:constr, " >= 0", collapse=" & ")
  } else {
    cn <- paste0("x", 1:p, " == 0", collapse=" & ")
  }

  # Calculate power
  lr_pow <- mean(sapply(df_list, function(df) {

    mod <- stats::lm(paste0("y ~ ", paste0("x", 1:p, collapse=" + ")), data = df)
    imod <- restriktor::iht(mod, constraints = cn)

    if(constr > 0) {
      imod[["B"]]$pvalue[1] >= alpha &&
        imod[["A"]]$pvalue[1] <= alpha
    } else {
      imod$pvalue
    }

  }))

  return(lr_pow)

}

