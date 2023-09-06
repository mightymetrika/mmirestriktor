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
        shiny::verbatimTextOutput("variables"),
        shiny::verbatimTextOutput("model_terms"),
        shiny::verbatimTextOutput("iht_results"),
        shiny::verbatimTextOutput("iht_interpretation"),
        shiny::verbatimTextOutput("rm_results"),
        shiny::verbatimTextOutput("rm_interpretation")
      )
    )
  )

  server <- function(input, output) {
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath, row.names = NULL)
    })

    model <- NULL

    shiny::observeEvent(input$fit_model, {
      shiny::req(input$formula, data(), input$engine)
      args_list <- list(formula = stats::as.formula(input$formula), data = data(), engine = input$engine, standardize = TRUE)
      model <<- do.call(mmir_model, args_list)
      output$model_terms <- shiny::renderPrint({
        names(stats::coef(model))
      })
    })

    output$variables <- shiny::renderPrint({
      names(data())
    })

    shiny::observeEvent(input$run_analysis, {
      shiny::req(input$constraint, model, input$analysis_type)

      constraint <- input$constraint

      if ("Informative Hypothesis Test" %in% input$analysis_type) {
        iht_res <- restriktor::iht(model, constraints = constraint)
        output$iht_results <- shiny::renderPrint({iht_res})
        output$iht_interpretation <- shiny::renderPrint({
          iht_interpreter(iht_res)
        })
      } else {
        output$iht_results <- shiny::renderPrint({NULL})
        output$iht_interpretation <- shiny::renderPrint({NULL})
      }

      if ("Restricted Means" %in% input$analysis_type) {
        rm_res <- restriktor::restriktor(model, constraints = constraint)
        output$rm_results <- shiny::renderPrint({rm_res})
        output$rm_interpretation <- shiny::renderPrint({
          rm_interpreter(rm_res)
        })
      } else {
        output$rm_results <- shiny::renderPrint({NULL})
        output$rm_interpretation <- shiny::renderPrint({NULL})
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
