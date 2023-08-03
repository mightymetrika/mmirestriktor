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
        shiny::verbatimTextOutput("results")
      )
    )
  )

  server <- function(input, output) {
    data <- shiny::reactive({
      shiny::req(input$file)
      readr::read_csv(input$file$datapath)
    })

    model <- NULL

    shiny::observeEvent(input$fit_model, {
      shiny::req(input$formula, data(), input$engine, input$args)
      model <<- mmir_model(stats::as.formula(input$formula), data(), input$engine, standardize = TRUE, eval(parse(text = input$args)))
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
      }
      if ("Restricted Means" %in% input$analysis_type) {
        rm_res <- restriktor::restriktor(model, constraints = constraint)
      }
      output$results <- shiny::renderPrint({
        list(iht = iht_res, rm = rm_res)
      })
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
#         shiny::verbatimTextOutput("variables"),
#         shiny::verbatimTextOutput("model_terms"),
#         shiny::verbatimTextOutput("results")
#       )
#     )
#   )
#
#   server <- function(input, output) {
#     data <- shiny::reactive({
#       shiny::req(input$file)
#       readr::read_csv(input$file$datapath)
#     })
#
#     model <- shiny::eventReactive(input$fit_model, {
#       shiny::req(input$formula, data(), input$engine, input$args)
#       mmir_model(as.formula(input$formula), data(), input$engine, standardize = TRUE, eval(parse(text = input$args)))
#     })
#
#     output$variables <- shiny::renderPrint({
#       names(data())
#     })
#
#     output$model_terms <- shiny::renderPrint({
#       names(stats::coef(model()))
#     })
#
#     analysis <- shiny::eventReactive(input$run_analysis, {
#       shiny::req(input$constraint, model(), input$analysis_type)
#       constraint <- input$constraint
#       if ("Informative Hypothesis Test" %in% input$analysis_type) {
#         iht_res <- restriktor::iht(model(), constraints = constraint)
#       }
#       if ("Restricted Means" %in% input$analysis_type) {
#         rm_res <- restriktor::restriktor(model(), constraints = constraint)
#       }
#       list(iht = iht_res, rm = rm_res)
#     })
#
#     output$results <- shiny::renderPrint({
#       analysis()
#     })
#   }
#
#   shiny::shinyApp(ui = ui, server = server)
# }

# mmirestriktor <- function(){
#
#   ui <- shiny::fluidPage(
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#         shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
#         shiny::textInput("formula", "Formula"),
#         shiny::selectInput("engine", "Engine", choices = c("lm", "glm", "rlm")),
#         shiny::textInput("args", "Extra arguments"),
#         shiny::textInput("constraint", "Constraint"),
#         shiny::checkboxGroupInput("analysis_type", "Analysis Type", choices = c("Informative Hypothesis Test", "Restricted Means")),
#         shiny::actionButton("run_analysis", "Run Analysis")
#       ),
#       shiny::mainPanel(
#         shiny::verbatimTextOutput("variables"),
#         shiny::verbatimTextOutput("model_terms"),
#         shiny::verbatimTextOutput("results")
#       )
#     )
#   )
#
#   server <- function(input, output) {
#     data <- shiny::reactive({
#       shiny::req(input$file)
#       readr::read_csv(input$file$datapath)
#     })
#
#     model <- shiny::reactive({
#       shiny::req(input$formula, data(), input$engine, input$args)
#       mmir_model(as.formula(input$formula), data(), input$engine, standardize = TRUE, eval(parse(text = input$args)))
#     })
#
#     output$variables <- shiny::renderPrint({
#       names(data())
#     })
#
#     output$model_terms <- shiny::renderPrint({
#       names(stats::coef(model()))
#     })
#
#     analysis <- shiny::eventReactive(input$run_analysis, {
#       shiny::req(input$constraint, model(), input$analysis_type)
#       constraint <- input$constraint
#       if ("Informative Hypothesis Test" %in% input$analysis_type) {
#         iht_res <- restriktor::iht(model(), constraints = constraint)
#       }
#       if ("Restricted Means" %in% input$analysis_type) {
#         rm_res <- restriktor::restriktor(model(), constraints = constraint)
#       }
#       list(iht = iht_res, rm = rm_res)
#     })
#
#     output$results <- shiny::renderPrint({
#       analysis()
#     })
#   }
#
#   shiny::shinyApp(ui = ui, server = server)
#
# }
