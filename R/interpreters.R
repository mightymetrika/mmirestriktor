#' Interpret Results of Informative Hypothesis Test
#'
#' This function provides a human-readable explanation of the results of an
#' informative hypothesis test. It interprets the p-values of both Type A and
#' Type B tests and provides an overall conclusion.
#'
#' @param iht_res A 'conTest' object containing the results of an informative
#' hypothesis test.
#' @param alpha The significance level for interpreting the p-values. Default is
#' 0.05.
#'
#' @return A character string providing a detailed interpretation of the
#' hypothesis test results.
#'
#' @references
#' Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor:
#' Evaluating informative hypotheses for linear models. In R. van de Schoot & M.
#' Miocevic (Eds.), Small Sample Size Solutions: A Guide for Applied Researchers
#' and Practitioners (1st ed., pp. 157 -172). Routledge.
#'
#' @examples
#' model <- mmir_model(mpg ~ -1 + hp + wt, data = mtcars, engine = "lm",
#'                     standardize = TRUE)
#' iht_res <- restriktor::iht(model, constraints = 'hp < wt')
#' iht_interpreter(iht_res) |> cat()
#'
#' @export
iht_interpreter <- function(iht_res, alpha=0.05) {

  if (!inherits(iht_res, "conTest")) {
    stop("Input iht_res must be of class 'conTest'.")
  }

  # Initialize result explanation with a brief overview and citation
  explanation <- "Informative hypothesis testing involves two types of tests: Type A and Type B. Type A tests if the parameters involved in the hypothesis are equal against the constraint order (e.g., HA0: a1 = a2 = a3 vs. HA1: a1 < a2 < a3). Type B tests if all restrictions hold in the population against no constraints imposed (e.g., HB0: a1 < a2 < a3 vs. HB1: at least one violation).\n\nA non-significant Type B hypothesis test followed by a significant Type A hypothesis test suggests evidence in favor of the order constrained hypothesis.\n\n[Source: Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor: Evaluating informative hypotheses for linear models. In R. van de Schoot & M. Miocevic (Eds.), Small Sample Size Solutions: A Guide for Applied Researchers and Practitioners (1st ed., pp. 157 -172). Routledge.]\n"

  # Extract p-values for Type B and Type A tests
  pvalue_B <- iht_res$B$pvalue
  pvalue_A <- iht_res$A$pvalue

  # Evaluate Type B test first
  if (pvalue_B > alpha) {
    explanation <- paste(explanation, "\nType B hypothesis test is not significant (p =", round(pvalue_B, 3), "). This means we do not reject the null hypothesis that all restrictions hold in the population.")

    # Evaluate Type A test
    if (pvalue_A > alpha) {
      explanation <- paste(explanation, "\n\nType A hypothesis test is also not significant (p =", round(pvalue_A, 3), "). This means we cannot conclude that at least one inequality constraint is strictly true.")
    } else {
      explanation <- paste(explanation, "\n\nType A hypothesis test is significant (p =", round(pvalue_A, 3), "). This means at least one inequality constraint is strictly true.")
      explanation <- paste(explanation, "\n\nCombining Type B and Type A tests, we have indirect evidence in favor of the order-constrained hypothesis.")
    }

  } else {
    explanation <- paste(explanation, "\nType B test is significant (p =", round(pvalue_B, 3), "). This means at least one order constraint is violated. Therefore, the order-constrained hypothesis is not supported.")
  }

  return(explanation)
}

#' Interpret Results of Restricted Means Analysis
#'
#' This function provides a human-readable interpretation of the results of a
#' restricted means analysis. It compares the original (unconstrained) and
#' reduced (restricted) R-squared values to evaluate the imposed constraints. It
#' also returns the Generalized Order-Restricted Information Criterion (GORIC)
#' which can be used for model comparison.
#'
#' @param rm_res An object of class 'restriktor', typically the result of a call
#' to \code{\link[restriktor]{restriktor}}.
#'
#' @return A character string with an interpretation of the analysis
#' results, including the R-squared values, their reduction, and the Generalized
#' Order-Restricted Information Criterion (GORIC) if available.
#'
#' @seealso \code{\link[restriktor]{restriktor}} for generating 'restriktor' objects.
#'
#' @references
#' Vanbrabant, L., & Rosseel, Y. (2020). An Introduction to Restriktor: Evaluating
#' informative hypotheses for linear models. In Small Sample Size Solutions (1st
#' ed., p. 16). Routledge.
#'
#'
#' @examples
#' model <- mmir_model(mpg ~ -1 + hp + wt, data = mtcars, engine = "lm",
#'                     standardize = TRUE)
#' rm_res <- restriktor::restriktor(model, constraints = 'hp < wt')
#' rm_interpreter(rm_res) |> cat()
#'
#' @export
rm_interpreter <- function(rm_res) {

  if (!inherits(rm_res, "restriktor")) {
    stop("Input rm_res must be of class 'restriktor'.")
  }

  if (inherits(rm_res, "conLM")){
    # Initialize result explanation
    explanation <- "Restricted means analysis focuses on comparing the original (unconstrained) and reduced (restricted) R-squared values.\n\n"

    # Extract R-squared values
    R2_org <- rm_res$R2.org
    R2_reduced <- rm_res$R2.reduced

    # Evaluate R-squared reduction
    R2_diff <- R2_org - R2_reduced

    explanation <- paste(explanation, "The original R-squared is ", round(R2_org, 3),
                         " and the reduced R-squared is ", round(R2_reduced, 3), ".\n\n")

    if (R2_diff > 0) {
      explanation <- paste(explanation, "The R-squared has reduced by ", round(R2_diff, 3),
                           ", suggesting that at least one constraint may be violated.\n\n")
      } else {
        explanation <- paste(explanation, "Both R-squared values are equal, suggesting that all constraints are in line with the data.\n\n")
      }
    } else {
      explanation <- ""
    }

  # Add GORIC information if available
  summary_object <- summary(rm_res)
  if (!is.null(summary_object$goric[[1]])) {
    explanation <- paste(explanation, "The Generalized Order-Restricted Information Criterion (GORIC) is ",
                         round(summary_object$goric[[1]], 3), ", which can be used for model selection.\n")
  }

  return(explanation)
}
