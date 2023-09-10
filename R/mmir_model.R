#' Fit Restriktor Supported Model
#'
#' @description
#' `mmir_model` is a function for the `mmirestriktor` package that fits
#' a model to data using one of the specified engines ('lm', 'glm', or 'rlm').
#' It also provides an option to standardize numeric variables.
#'
#' @param formula An object of class `formula` (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param engine A character string indicating which engine to use for model
#' fitting. Can be one of "lm", "glm", "rlm". Default is "lm".
#' @param standardize Logical. If TRUE, numeric predictor variables in `data` are
#' standardized before fitting the model. Default is TRUE.
#' @param ... Additional arguments to be passed to the model fitting function
#' (lm, glm, or rlm).
#'
#' @return
#' An object representing the fitted model, of class 'lm', 'glm', or 'rlm'
#' depending on the engine used.
#'
#' @details
#' The `mmir_model` function serves as a utility function for fitting models
#' in the `mmirestriktor` package. It supports different modeling engines and
#' allows for variable standardization.
#'
#' @seealso
#' \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, \code{\link[MASS]{rlm}}
#'
#' @examples
#' mod <- mmir_model(mpg ~ hp + wt, data = mtcars, engine = "lm")
#' summary(mod)
#'
#' @export
mmir_model <- function(formula, data, engine = "lm", standardize = TRUE, ...) {
  # Check formula
  if (!inherits(formula, "formula")) {
    stop("formula is not a valid R formula")
  }

  # Check data
  if (!is.data.frame(data)) {
    stop("data is not a valid data frame")
  }

  # Check standardize
  if (!is.logical(standardize) || length(standardize) != 1) {
    stop("standardize is not a single boolean value")
  }

  # Standardize numeric predictors if standardize is set to TRUE
  if (standardize) {
    # Get all variables from the formula
    all_vars <- all.vars(formula)

    # Get the response variable from the formula
    response_var <- as.character(formula)[2]

    # Identify the predictor variables
    predictor_vars <- setdiff(all_vars, response_var)

    # Identify numeric predictors
    numeric_predictors <- predictor_vars[sapply(data[predictor_vars], is.numeric)]

    # Standardize the numeric predictors
    data[numeric_predictors] <- lapply(data[numeric_predictors], scale)
  }

  # Fit the model using the selected engine
  if (engine == "lm") {
    model <- stats::lm(formula, data, ...)
  } else if (engine == "glm") {
    model <- stats::glm(formula, data, ...)
  } else if (engine == "rlm") {
    model <- MASS::rlm(formula, data, ...)
  } else {
    stop(paste("Invalid engine:", engine))
  }

  return(model)
}
