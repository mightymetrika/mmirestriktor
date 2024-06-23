#' Generate UI Parameters for Selected Simulation Type (PostgreSQL Integration)
#'
#' This internal helper function dynamically generates UI elements for setting
#' parameters based on the chosen simulation type in the Replext Shiny application.
#' The *_pgsql version of the function was built for integration with PostgreSQL
#'
#' @param cellBlock A string identifier for the selected simulation type.
#'   Currently supports 'replext_t1_c1' for Order Constrained ANOVA and
#'   'replext_t2_c1' for Inequality Constrained Linear Regression simulations.
#'
#' @return A list of Shiny UI elements appropriate for setting parameters for the
#'   selected simulation type.
#'
#' @details The function uses a switch statement to determine which set of UI
#'   elements to generate based on the `cellBlock` parameter. Each set of UI
#'   elements corresponds to the input parameters required for either the ANOVA
#'   or regression simulations. These UI elements include numeric inputs, text
#'   inputs, and checkbox inputs for various simulation parameters such as number
#'   of iterations, number of groups or variables, effect sizes, constraints, etc.
#'
#' @keywords internal
getUIParams_pgsql <- function(cellBlock) {
  switch(cellBlock,
         "replext_t1_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 10),
                                shiny::numericInput("k", "Number of groups:", 3),
                                shiny::numericInput("fs", "Population mean difference:", 0.10),
                                shiny::numericInput("n_start", "Starting sample size per group:", 6),
                                shiny::textInput("constrs", "Vector of correctly specified order constraints:", "0,1,2"),
                                shiny::numericInput("alpha", "Alpha level:", 0.05),
                                shiny::textInput("pow", "Statistical power:", 0.80),
                                shiny::numericInput("nmax", "Maximum sample size per group:", 1000)),
         "replext_t2_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 10),
                                shiny::numericInput("p", "Number of variables:", 3),
                                shiny::numericInput("f2s", "Effect size:", 0.02),
                                shiny::numericInput("n_start", "Starting sample size per group:", 6),
                                shiny::textInput("constrs", "Vector of correctly specified inequality:", "0,1,2,3"),
                                shiny::numericInput("rho", "Correlation among independent variables:", 0.0),
                                shiny::textInput("beta", "Vector of regression coefficients:", "0.1"),
                                shiny::numericInput("alpha", "Alpha level:", 0.05),
                                shiny::textInput("pow", "Statistical power:", 0.80),
                                shiny::checkboxInput("standardize", "Standardize variables", value = TRUE),
                                shiny::numericInput("nmax", "Maximum sample size per group:", 1000))

  )
}

#' Append Input Parameters to Simulation Results (PostgreSQL Integration)
#'
#' This internal helper function appends the user-specified input parameters to
#' each row of the simulation results data frame. This function is used to provide
#' a complete record of the parameters used for each simulation run. The *_pgsql
#' version of the function was built for integration with PostgreSQL
#'
#' @param df A data frame containing the simulation results.
#' @param input A list containing the user input parameters from the Shiny application.
#'
#' @return A data frame combining the original simulation results with a row-wise
#'   repetition of the input parameters for each simulation run.
#'
#' @details The function first generates a unique identifier for each simulation
#'   run. It then constructs a data frame of the input parameters based on the
#'   selected simulation type (either 'replext_t1_' or 'replext_t2_'). The input
#'   parameters are repeated to match the number of rows in the results data frame
#'   and then appended to it. This augmented data frame provides a comprehensive
#'   view of the results and the parameters used to generate them.
#'
#' @keywords internal
appendInputParams_pgsql <- function(df, input) {

  # Helper function to generate a unique run code
  generateRunCode <- function() {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    random_string <- paste(sample(c(letters, LETTERS, 0:9), 5, replace = TRUE), collapse = "")
    paste0(timestamp, "_", random_string)
  }

  # Generate a unique code for the simulation run
  run_code <- generateRunCode()

  # Create a data frame of input parameters
  if (grepl("^replext_t1_", input$cellBlock)) {

    params_df <- data.frame(
      S = input$S, k = input$k, fs = input$fs,
      n_start = input$n_start, constrs = text_to_vector(input$constrs |> paste(collapse = ",")),
      alpha = input$alpha, pow = input$pow, nmax = input$nmax, #Seed = input$seed,
      RunCode = run_code, stringsAsFactors = FALSE
    )
  } else if (grepl("^replext_t2_", input$cellBlock)) {

    params_df <- data.frame(
      S = input$S, p = input$p, f2s = input$f2s,
      n_start = input$n_start, constrs = text_to_vector(input$constrs) |> paste(collapse = ","),
      rho = input$rho, beta = text_to_vector(input$beta) |> paste(collapse = ","), alpha = input$alpha,
      pow = input$pow, standardize = input$standardize, nmax = input$nmax, #Seed = input$seed,
      RunCode = run_code, stringsAsFactors = FALSE
    )
  } else {
    stop("Must select a supported cell block")
  }

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  result_df <- cbind(df, params_df)

  # Extract row names as a new column nconstr
  result_df$nconstr <- rownames(df)

  # Rename columns
  if(startsWith(names(result_df), 'f2=') |> any()){

    names(result_df)[startsWith(names(result_df), 'f2=')] <- 'nf2'

    # Reorder columns to have nconstr and nf2 at the beginning
    result_df <- result_df[, c("nconstr", "nf2", setdiff(names(result_df), c("nconstr", "nf2")))]

  }

  if(startsWith(names(result_df), 'f=') |> any()){

    names(result_df)[startsWith(names(result_df), 'f=')] <- 'nf'

    # Reorder columns to have nconstr and nf2 at the beginning
    result_df <- result_df[, c("nconstr", "nf", setdiff(names(result_df), c("nconstr", "nf")))]
  }

  return(result_df)
}

#' Execute Simulation Based on User Input (PostgreSQL Integration)
#'
#' This internal helper function runs the appropriate simulation function based
#' on the user-selected cell block in the Replext Shiny application. It handles
#' the dynamic execution of either ANOVA or regression simulations based on the
#' parameters specified by the user. The *_pgsql version of the function was built
#' for integration with PostgreSQL
#'
#' @param input A list containing the user input parameters from the Shiny application.
#'
#' @return The result of the simulation, which can be either from `replext_t1_c1`
#'   (Order Constrained ANOVA) or `replext_t2_c1` (Inequality Constrained Linear
#'   Regression), depending on the selected cell block.
#'
#' @details The function determines which simulation function to call
#'   (`replext_t1_c1` or `replext_t2_c1`) based on the prefix of the `cellBlock`
#'   input parameter. The function passes user-specified parameters to the chosen
#'   simulation function and returns the simulation results.
#'
#' @keywords internal
runSimulation_pgsql <- function(input) {

  # Dynamically call the appropriate function based on the cell block prefix
  if (grepl("^replext_t1_", input$cellBlock)) {
    return(replext_t1_c1(S = input$S, k = input$k, fs = input$fs,
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         alpha = input$alpha, pow = input$pow, nmax = input$nmax
    ))
  } else if (grepl("^replext_t2_", input$cellBlock)) {
    return(replext_t2_c1(S = input$S, p = input$p, f2s = input$f2s,
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         rho = input$rho, beta = text_to_vector(input$beta),
                         alpha = input$alpha, pow = input$pow,
                         standardize = input$standardize, nmax = input$nmax
    ))
  } else {
    stop("Must select a supported cell block")
  }

}
