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
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
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
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
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
#' yi = mu1xi1 + ... + mukxik + ei, as described in Vanbrabant, Van De Schoot,
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
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
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

#' Power Calculation for ANOVA Simulation
#'
#' This function calculates the power for hypothesis tests in a constrained
#' statistical inference setting, particularly in the context of ANOVA and regression
#' as discussed in Vanbrabant et al. (2015). It is designed to work with a list of
#' data frames, where each data frame represents a different dataset. The function
#' accommodates both equality and inequality constraints.
#'
#' @param df_list A list of data frames, each representing a dataset. Designed to
#'   use results generated from the generate_datasets() function.
#' @param constr An integer indicating the number of inequality constraints.
#'   A value of 0 indicates that all constraints are equality constraints.
#'   The value must be a non-negative integer less than the number of groups.
#' @param alpha The significance level used in the hypothesis testing, with a
#'   default value of 0.05. It should be a numeric value between 0 and 1.
#'
#' @return The function returns the calculated power as a numeric value,
#'   representing the proportion of p-values smaller than the predefined
#'   significance level alpha.
#'
#' @details The function first checks the validity of the 'constr' parameter and
#'   then constructs the constraint string based on the number of constraints.
#'   It runs the model for each dataset in the df_list using the mmir_model function
#'   and applies the constraints using the restriktor::iht function. The power is
#'   calculated based on the proportion of datasets that meet the hypothesis test
#'   criteria defined by the constraints and the significance level.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' generate_datasets(S = 2, k = 4, f = 0.25, n = 30) |> pj_pow(constr=1)
#'
#' @export
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
      imod$pvalue <= alpha
    }
  }))

  return(pj_pow)
}

#' Regression Data Simulation for Linear Models
#'
#' This function simulates data for linear regression analysis, as described in the
#' supplemental material of the referenced paper. It generates datasets with a
#' specified number of predictors and sample size, effect size, and correlation
#' coefficient, considering a linear model with fixed regression coefficients.
#'
#' @param n The total number of observations to generate.
#' @param p The number of predictors (Beta) in the regression model.
#' @param f2 The effect size, calculated as (f^2 = R^2 / (1 - R^2)), where
#'   ( R^2) is the coefficient of determination.
#' @param rho The correlation coefficient between predictors, representing the
#'   off-diagonal elements in the covariance matrix. Should be a numeric
#'   value.
#' @param beta The regression coefficients, either a single value replicated for
#'   each predictor or a vector of length equal to the number of predictors (p).
#'
#' @return A list containing two elements: 'y', the simulated response variable,
#'   and 'X', the matrix of predictors.
#'
#' @details The function validates the length of the beta vector, constructs a
#'   covariance matrix for the predictors, and calculates the variance of the error
#'   term. It then uses the multivariate normal distribution to generate predictor
#'   values and calculates the response variable based on the specified regression
#'   coefficients and effect size.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' # Example usage:
#' # Simulate data for a regression model with 100 observations, 3 predictors,
#' # an effect size of 0.10, and a correlation coefficient of 0.5
#' sim_reg(n = 100, p = 3, f2 = 0.10, rho = 0.5)
#'
#' @export
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

#' Generate Multiple Datasets for Regression Simulation
#'
#' This function generates a specified number of datasets for regression analysis
#' simulations. Each dataset is generated using the `sim_reg` function, based on
#' given parameters like sample size, number of predictors, effect size, and
#' correlation coefficient.
#'
#' @param S The number of datasets to generate, default is 20000.
#' @param n The number of observations in each dataset.
#' @param p The number of predictors in the regression model for each dataset.
#' @param f2 The effect size for each dataset, defined as (f^2 = R^2 / (1 - R^2)).
#' @param rho The correlation coefficient between predictors in each dataset.
#' @param beta The regression coefficients for the predictors in each dataset,
#'   either as a single value or a vector of length (p).
#'
#' @return A list of data frames, each representing a simulated dataset for
#'   regression analysis. Each data frame contains columns for the response
#'   variable 'y' and predictors 'x1', 'x2', ..., 'xp'.
#'
#' @details The function uses `sim_reg` to simulate individual datasets, which
#'   are then combined into a list. Each dataset is a data frame with named
#'   columns for the response variable and predictors.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' datasets <- generate_datasets_reg(S = 2, n = 50, p = 3, f2 = 0.10, rho = 0.5)
#'
#' @export
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

#' Calculate Power for Linear Regression Simulations
#'
#' This function computes the power of hypothesis tests in a linear regression
#' setting, considering constraints on the regression coefficients. It processes a
#' list of data frames, each representing a different dataset, and calculates the
#' power based on specified constraints.
#'
#' @param df_list A list of data frames, each representing a dataset for regression
#'   analysis. Each data frame should contain the response variable 'y' and the
#'   predictor variables 'x1', 'x2', ..., 'xp'.
#' @param constr The number of inequality constraints imposed on the regression
#'   coefficients. It must be a non-negative integer less than or equal to the number
#'   of predictors (p). A value of 0 implies no constraints or equality constraints.
#' @param standardize A logical value indicating whether the predictor variables
#'   should be standardized before fitting the model. Default is TRUE.
#' @param alpha The significance level used in hypothesis testing, default is 0.05.
#'
#' @return A numeric value representing the calculated power, defined as the
#'   proportion of datasets meeting the hypothesis test criteria as defined by
#'   the constraints and significance level.
#'
#' @details The function validates the 'constr' parameter, optionally standardizes
#'   the predictor variables, constructs the necessary constraints, and calculates
#'   power by fitting a linear model to each dataset. It uses the 'iht' function
#'   from the 'restriktor' package to apply the constraints and evaluate the
#'   hypothesis tests.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' generate_datasets_reg(S = 4, n = 30, p = 3, f2 = 0.20, rho = 0.5) |> lr_pow()
#'
#' @export
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
      imod$pvalue <= alpha
    }

  }))

  return(lr_pow)

}

