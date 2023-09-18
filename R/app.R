#' Mighty Metrika Interface to Restriktor Shiny App
#'
#' This function launches a Shiny app which allows users to fit and analyze
#' models with restrictions using the mmir_model(), restriktor::iht(), and
#' restriktor::restriktor() functions. The app provides a user interface to
#' upload a CSV file, specify a model formula, and define constraints for
#' informative hypothesis testing.
#'
#' The app has the following functionalities:
#' - Upload a CSV file to be used as the dataset for modeling.
#' - View the variables available in the uploaded dataset.
#' - Input a formula to define the model to be fit.
#' - Choose a model fitting engine from "lm", "glm", and "rlm".
#' - Specify extra arguments for the model fitting function.
#' - View the terms available for defining constraints after fitting the model.
#' - Define constraints for hypothesis testing.
#' - Set a significance level (alpha) for hypothesis testing.
#' - Choose the type of analysis to perform: Informative Hypothesis Test and/or
#'   Restricted Means.
#' - View the results and interpretation of the hypothesis tests after running
#'   the analysis.
#'
#' @return This function does not return a value; it launches a Shiny app in the
#' user's default web browser.
#'
#' @export
#'
#' @examples
#' if (interactive()){
#'   mmirestriktor()
#' }
mmirestriktor <- function(){

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
        shiny::textInput("formula", "Formula"),
        shiny::selectInput("engine", "Engine", choices = c("lm", "glm", "rlm")),
        shiny::textInput("args", "Extra arguments"),
        shiny::actionButton("fit_model", "Fit Model"),
        shiny::textAreaInput("constraint", "Constraint", rows = 3),
        shiny::numericInput("alpha", "Significance Level (alpha)", value = 0.05, min = 0.01, max = 1, step = 0.01),
        shiny::checkboxGroupInput("analysis_type", "Analysis Type", choices = c("Informative Hypothesis Test", "Restricted Means")),
        shiny::actionButton("run_analysis", "Run Analysis")
      ),
      shiny::mainPanel(
        shiny::uiOutput("variables_header"),
        shiny::verbatimTextOutput("variables"),
        shiny::uiOutput("model_terms_header"),
        shiny::verbatimTextOutput("model_terms"),
        shiny::conditionalPanel(
          condition = "input.analysis_type.includes('Informative Hypothesis Test')",
          shiny::uiOutput("iht_results_header"),
          shiny::verbatimTextOutput("iht_results"),
          shiny::uiOutput("iht_interpretation_header"),
          shiny::htmlOutput("iht_interpretation")
        ),
        shiny::conditionalPanel(
          condition = "input.analysis_type.includes('Restricted Means')",
          shiny::uiOutput("rm_results_header"),
          shiny::verbatimTextOutput("rm_results"),
          shiny::uiOutput("rm_interpretation_header"),
          shiny::htmlOutput("rm_interpretation")
        )
      )
    )
  )

  server <- function(input, output) {
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath, row.names = NULL)
    })

    model <- NULL
    model_fitted <- shiny::reactiveVal(FALSE)  # Create a reactive value to track the model fitting status

    shiny::observeEvent(input$fit_model, {
      shiny::req(input$formula, data(), input$engine)
      args_list <- list(formula = stats::as.formula(input$formula), data = data(), engine = input$engine, standardize = TRUE)

      # If the user has provided extra arguments, add them to args_list
      if (nzchar(input$args)) {
        extra_args <- tryCatch({
          str2list(input$args)
        }, error = function(e) {
          shiny::showNotification(
            paste("Error in extra arguments:", e$message),
            type = "error",
            duration = NULL
          )
          return(NULL)
        })

        # If extra_args is not NULL, add it to args_list
        if (!is.null(extra_args)) {
          args_list <- c(args_list, extra_args)
        }
      }

      tryCatch({
        model <<- do.call(mmir_model, args_list)
        model_fitted(TRUE)  # Set the reactive value to TRUE after fitting the model
        output$model_terms <- shiny::renderPrint({
          names(stats::coef(model))
        })
      }, error = function(e) {
        # Display the error message in the Shiny app
        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
        # Reset the model fitting status to FALSE
        model_fitted(FALSE)
      })
    })

    output$variables <- shiny::renderPrint({
      names(data())
    })

    output$variables_header <- shiny::renderUI({
      if(!is.null(data())){
        shiny::tags$h2("Data Frame Variables")
      }
    })

    output$model_terms_header <- shiny::renderUI({
      if(model_fitted()){  # Use the reactive value to control the rendering of the header
        shiny::tags$h2("Available Terms for Constraint")
      }
    })

    shiny::observeEvent(input$run_analysis, {
      shiny::req(input$constraint, model, input$analysis_type)

      constraint <- input$constraint

      if ("Informative Hypothesis Test" %in% input$analysis_type) {
        tryCatch({
          iht_res <- restriktor::iht(model, constraints = constraint)
          output$iht_results <- shiny::renderPrint({iht_res})
          output$iht_interpretation <- shiny::renderUI({
            shiny::HTML(nl2br(iht_interpreter(iht_res, alpha = input$alpha)))
          })
          output$iht_results_header <- shiny::renderUI({
            shiny::tags$h2("Informative Hypothesis Test Results")
          })
          output$iht_interpretation_header <- shiny::renderUI({
            shiny::tags$h4("Informative Hypothesis Test Interpretation")
          })
        }, error = function(e) {
          # Display the error message in the Shiny app
          shiny::showNotification(
            paste("Error:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }

      if ("Restricted Means" %in% input$analysis_type) {
        tryCatch({
          rm_res <- restriktor::restriktor(model, constraints = constraint)
          output$rm_results <- shiny::renderPrint({summary(rm_res)})
          output$rm_interpretation <- shiny::renderUI({
            shiny::HTML(nl2br(rm_interpreter(rm_res)))
          })
          output$rm_results_header <- shiny::renderUI({
            shiny::tags$h2("Restricted Means Results")
          })
          output$rm_interpretation_header <- shiny::renderUI({
            shiny::tags$h4("Restricted Means Interpretation")
          })
        }, error = function(e) {
          # Display the error message in the Shiny app
          shiny::showNotification(
            paste("Error:", e$message),
            type = "error",
            duration = NULL
          )
        })
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
