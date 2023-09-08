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
            shiny::HTML(nl2br(iht_interpreter(iht_res)))
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
          output$rm_results <- shiny::renderPrint({rm_res})
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


# mmirestriktor <- function(){
#
#   ui <- shiny::fluidPage(
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#         shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
#         shiny::textInput("formula", "Formula"),
#         shiny::selectInput("engine", "Engine", choices = c("lm", "glm", "rlm")),
#         shiny::textInput("args", "Extra arguments"),
#         shiny::actionButton("fit_model", "Fit Model"),
#         shiny::textAreaInput("constraint", "Constraint", rows = 3),
#         shiny::checkboxGroupInput("analysis_type", "Analysis Type", choices = c("Informative Hypothesis Test", "Restricted Means")),
#         shiny::actionButton("run_analysis", "Run Analysis")
#       ),
#       shiny::mainPanel(
#         shiny::uiOutput("variables_header"),
#         shiny::verbatimTextOutput("variables"),
#         shiny::uiOutput("model_terms_header"),
#         shiny::verbatimTextOutput("model_terms"),
#         shiny::conditionalPanel(
#           condition = "input.analysis_type.includes('Informative Hypothesis Test')",
#           shiny::uiOutput("iht_results_header"),
#           shiny::verbatimTextOutput("iht_results"),
#           shiny::uiOutput("iht_interpretation_header"),
#           shiny::htmlOutput("iht_interpretation")
#         ),
#         shiny::conditionalPanel(
#           condition = "input.analysis_type.includes('Restricted Means')",
#           shiny::uiOutput("rm_results_header"),
#           shiny::verbatimTextOutput("rm_results"),
#           shiny::uiOutput("rm_interpretation_header"),
#           shiny::htmlOutput("rm_interpretation")
#         )
#       )
#     )
#   )
#
#   server <- function(input, output) {
#     data <- shiny::reactive({
#       shiny::req(input$file)
#       utils::read.csv(input$file$datapath, row.names = NULL)
#     })
#
#     model <- NULL
#     model_fitted <- shiny::reactiveVal(FALSE)  # Create a reactive value to track the model fitting status
#
#     shiny::observeEvent(input$fit_model, {
#       shiny::req(input$formula, data(), input$engine)
#       args_list <- list(formula = stats::as.formula(input$formula), data = data(), engine = input$engine, standardize = TRUE)
#       model <<- do.call(mmir_model, args_list)
#       model_fitted(TRUE)  # Set the reactive value to TRUE after fitting the model
#       output$model_terms <- shiny::renderPrint({
#         names(stats::coef(model))
#       })
#     })
#
#     output$variables <- shiny::renderPrint({
#       names(data())
#     })
#
#     output$variables_header <- shiny::renderUI({
#       if(!is.null(data())){
#         shiny::tags$h2("Data Frame Variables")
#       }
#     })
#
#     output$model_terms_header <- shiny::renderUI({
#       if(model_fitted()){  # Use the reactive value to control the rendering of the header
#         shiny::tags$h2("Available Terms for Constraint")
#       }
#     })
#
#     shiny::observeEvent(input$run_analysis, {
#       shiny::req(input$constraint, model, input$analysis_type)
#
#       constraint <- input$constraint
#
#       if ("Informative Hypothesis Test" %in% input$analysis_type) {
#         tryCatch({
#           iht_res <- restriktor::iht(model, constraints = constraint)
#           output$iht_results <- shiny::renderPrint({iht_res})
#           output$iht_interpretation <- shiny::renderUI({
#             shiny::HTML(nl2br(iht_interpreter(iht_res)))
#           })
#           output$iht_results_header <- shiny::renderUI({
#             shiny::tags$h2("Informative Hypothesis Test Results")
#           })
#           output$iht_interpretation_header <- shiny::renderUI({
#             shiny::tags$h4("Informative Hypothesis Test Interpretation")
#           })
#         }, error = function(e) {
#           # Display the error message in the Shiny app
#           shiny::showNotification(
#             paste("Error:", e$message),
#             type = "error",
#             duration = NULL
#           )
#         })
#       }
#
#       if ("Restricted Means" %in% input$analysis_type) {
#         tryCatch({
#           rm_res <- restriktor::restriktor(model, constraints = constraint)
#           output$rm_results <- shiny::renderPrint({rm_res})
#           output$rm_interpretation <- shiny::renderUI({
#             shiny::HTML(nl2br(rm_interpreter(rm_res)))
#           })
#           output$rm_results_header <- shiny::renderUI({
#             shiny::tags$h2("Restricted Means Results")
#           })
#           output$rm_interpretation_header <- shiny::renderUI({
#             shiny::tags$h4("Restricted Means Interpretation")
#           })
#         }, error = function(e) {
#           # Display the error message in the Shiny app
#           shiny::showNotification(
#             paste("Error:", e$message),
#             type = "error",
#             duration = NULL
#           )
#         })
#       }
#     })
#   }
#
#   shiny::shinyApp(ui = ui, server = server)
# }
#
#
