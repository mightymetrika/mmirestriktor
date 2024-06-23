#' Launch Replext Simulation Shiny Application
#'
#' This function creates and launches a Shiny web application for running
#' simulations related to constrained statistical inference in ANOVA and regression
#' settings. The application allows users to set various parameters for
#' `replext_t1_c1` and `replext_t2_c1` functions and view the resulting simulation
#' data. The simulation is based on Vanbrabant et al. (2015).
#'
#' @details The Shiny application consists of a user interface for setting
#'   simulation parameters and a server logic to process the simulations. Users can
#'   select between different simulation settings (cell blocks), specify parameters,
#'   run the simulations, view the results in a table format, and download the
#'   results. The application also handles dynamic UI elements based on user
#'   selections and manages data downloads.
#'
#'   The app's UI includes:
#'   - A sidebar for input parameters and action buttons.
#'   - A main panel for displaying simulation results.
#'
#'   The server logic includes:
#'   - Rendering parameter input UI based on selected cell block.
#'   - Running simulations and storing results.
#'   - Rendering and exporting the results table.
#'
#' @return A Shiny app object which can be run to start the application.
#'
#' @references
#' Vanbrabant, Leonard; Van De Schoot, Rens; Rosseel, Yves (2015). Constrained
#' statistical inference: sample-size tables for ANOVA and regression. Frontiers
#' in Psychology, 5. DOI:10.3389/fpsyg.2014.01565.
#' URL: https://www.frontiersin.org/articles/10.3389/fpsyg.2014.01565
#'
#' @examples
#' # Launch the Replext Simulation Shiny application
#' if(interactive()){
#'   replext()
#' }
#'
#' @export
replext <- function() {

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("seed", "Random Number Seed (Optional):", value = NA, min = 1),
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::downloadButton("downloadBtn", "Download Data")
      ),
      shiny::mainPanel(
        DT::DTOutput("resultsTable")
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      getUIParams(input$cellBlock)
    })

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {
      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation(input)

      # Update the results reactive value
      results(simResults)
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # Download handler for exporting data
    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("Simulation_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Ensure there is data to download
        shiny::req(results())

        # Get appended results
        simResults_exp <- appendInputParams(results(), input)

        # Write the data to a CSV file
        utils::write.csv(simResults_exp, file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
