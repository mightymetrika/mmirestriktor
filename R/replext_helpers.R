getCellBlocks <- function(){
  list("Order Constrained ANOVA" = "replext_t1_c1",
       "Inequality Constrained Linear Regression" = "replext_t2_c1")
}

getUIParams <- function(cellBlock) {
  switch(cellBlock,
         "replext_t1_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 20000),
                                  shiny::numericInput("k", "Number of groups:", 3),
                                  shiny::textInput("fs", "Vector of population mean differences:", "0.10,0.15,0.20,0.25,0.30,0.35,0.40"),
                                  shiny::numericInput("n_start", "Starting sample size per group:", 6),
                                  shiny::textInput("constrs", "Vector of correctly specified order constraints:", "0,1,2"),
                                  shiny::numericInput("alpha", "Alpha level:", 0.05),
                                  shiny::textInput("pow", "Statistical power:", 0.80),
                                  shiny::numericInput("nmax", "Maximum sample size per group:", 1000)),
         "replext_t2_c1" = list(shiny::numericInput("S", "Number of simulation iterations:", 20000),
                                shiny::numericInput("p", "Number of variables:", 3),
                                shiny::textInput("f2s", "Vector of effect sizes:", "0.02,0.05,0.08,0.10,0.15,0.20,0.25,0.35"),
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

appendInputParams <- function(df, input) {
  # Generate a unique code for the simulation run
  run_code <- paste(sample(letters, 10, replace = TRUE), collapse = "")

  print(paste0("aIP: ", input$constrs))
  print(paste0("aIP: ", text_to_vector(input$constrs)))

  # Create a data frame of input parameters
  if (grepl("^replext_t1_", input$cellBlock)) {
    params_df <- data.frame(
      S = input$S, k = input$k, fs = text_to_vector(input$fs),
      n_start = input$n_start, constrs = text_to_vector(input$constrs),
      alpha = input$alpha, pow = input$pow, nmax = input$nmax, Seed = input$seed,
      RunCode = run_code, stringsAsFactors = FALSE
    )
  } else if (grepl("^replext_t2_", input$cellBlock)) {
    params_df <- data.frame(
      S = input$S, p = input$p, f2s = text_to_vector(input$f2s),
      n_start = input$n_start, constrs = text_to_vector(input$constrs),
      rho = input$rho, beta = text_to_vector(input$beta), alpha = input$alpha,
      pow = input$pow, standardize = input$standardize, nmax = input$nmax,
      Seed = input$seed, RunCode = run_code, stringsAsFactors = FALSE
    )
  } else {
    stop("Must select a supported cell block")
  }

  # Repeat the parameters data frame to match the number of rows in df
  params_df <- params_df[rep(1, nrow(df)), ]

  # Combine with the simulation results
  cbind(df, params_df)
}

runSimulation <- function(input) {

  # Set the seed if provided
  if (!is.na(input$seed) && input$seed > 0) {
    set.seed(input$seed)
  }

  # Dynamically call the appropriate function based on the cell block prefix
  if (grepl("^replext_t1_", input$cellBlock)) {
    return(replext_t1_c1(S = input$S, k = input$k, fs = text_to_vector(input$fs),
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         alpha = input$alpha, pow = input$pow, nmax = input$nmax
    ))
  } else if (grepl("^replext_t2_", input$cellBlock)) {
    return(replext_t2_c1(S = input$S, p = input$p, f2s = text_to_vector(input$f2s),
                         n_start = input$n_start, constrs = text_to_vector(input$constrs),
                         rho = input$rho, beta = text_to_vector(input$beta),
                         alpha = input$alpha, pow = input$pow,
                         standardize = input$standardize, nmax = input$nmax
    ))
  } else {
    stop("Must select a supported cell block")
  }

}

#' Convert Comma-Separated String to Numeric Vector
#'
#' This internal function takes a string of comma-separated values and
#' converts it into a numeric vector. It is typically used to process
#' user inputs from the Shiny app's UI where multiple values can be
#' entered as a single string.
#'
#' @param text_input A string containing comma-separated values,
#'        typically a user input from the Shiny app's UI.
#'
#' @return A numeric vector converted from the comma-separated string.
#'         If the input is an empty string, returns an empty numeric vector.
#'
#' @keywords internal
text_to_vector <- function(text_input) {
  as.numeric(unlist(strsplit(text_input, ",")))
}
