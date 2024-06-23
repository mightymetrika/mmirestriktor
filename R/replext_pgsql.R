#' Launch Replext Simulation Shiny Application
#'
#' This function creates and launches a Shiny web application for running
#' simulations related to constrained statistical inference in ANOVA and regression
#' settings. The application allows users to set various parameters for
#' `replext_t1_c1` and `replext_t2_c1` functions and view the resulting simulation
#' data. The simulation is based on Vanbrabant et al. (2015). The app includes
#' functionality to interact with a PostgreSQL database. The app includes a user
#' interface for selecting simulation parameters and a server logic to process the
#' simulation and handle user interactions, including saving and retrieving data
#' from a database.
#'
#' @details The Shiny application consists of a user interface for setting
#'   simulation parameters and a server logic to process the simulations and save
#'   to PostgreSQL database. Users can select between different simulation settings
#'   (cell blocks), specify parameters, run the simulations, view the results in
#'   a table format, submit results to PostgreSQL database, and download the database
#'   table. The application also handles dynamic UI elements based on user
#'   selections and manages data downloads.
#'
#' @param dbname The name of the PostgreSQL database to connect to.
#' @param datatable The name of the table in the database where the simulation results will be stored and retrieved.
#' @param host The host address of the PostgreSQL database.
#' @param port The port number for the PostgreSQL database connection.
#' @param user The username for accessing the PostgreSQL database.
#' @param password The password for the specified user to access the PostgreSQL database.
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
#' if (interactive()) {
#'   replext_pgsql(
#'     dbname = "your_db_name",
#'     datatable = "your_data_table",
#'     host = "localhost",
#'     port = 5432,
#'     user = "your_username",
#'     password = "your_password"
#'   )
#' }
#'
#' @export
replext_pgsql <- function(dbname, datatable, host, port, user, password) {

  # Helper function to save data to the database
  saveData <- function(data) {
    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Convert NA to NaN in the data frame
    data[is.na(data)] <- NaN

    # Loop through rows of data and save to databse
    lapply(1:nrow(data), function(i){

      # get row i of the data
      row_data <- data[i, ]

      # Construct the update query by looping over the data fields
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        datatable,
        paste(names(row_data), collapse = ", "),
        paste(row_data, collapse = "', '")
      )

      # Execute the query
      tryCatch({
        pool::dbExecute(pool, query)
      }, error = function(e) {
        print(paste("Error inserting row", i, ":", e))
      })
    })


  }

  # Helper function to load data from database
  loadData <- function() {

    # Connect to the database
    pool <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbname,
      host = host,
      user = user,
      password = password,
      port = port
    )

    # Close pool on stop
    shiny::onStop(function() {
      pool::poolClose(pool)
    })

    # Construct the fetching query
    sql <- sprintf("SELECT * FROM %s", datatable)
    query <- pool::sqlInterpolate(pool, sql)

    # Submit the fetch query and disconnect
    pool::dbGetQuery(pool, query)

  }

  # Define the UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Replext Simulation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("cellBlock", "Select Cell Block:",
                           choices = getCellBlocks()),
        shiny::uiOutput("paramsUI"),
        shiny::actionButton("runSim", "Run Simulation"),
        shiny::actionButton("submit", "Submit"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        shiny::downloadButton("downloadBtn", "Download Data")
      ),
      shiny::mainPanel(
        # Conditionally display the Simulation Results header and table
        shiny::uiOutput("simulation_results_header"),
        DT::DTOutput("resultsTable"),
        shiny::br(),  # Add a line break
        shiny::br(),  # Add a line break
        # Add a header for the responses table
        shiny::div(
          shiny::h4("All Responses"),
          DT::DTOutput("responses")
        )
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {

    # Render the UI for parameters based on the selected cell block
    output$paramsUI <- shiny::renderUI({
      getUIParams_pgsql(input$cellBlock)
    })

    # Reactive value to store the results
    results <- shiny::reactiveVal(data.frame())     #For display
    results_exp <- shiny::reactiveVal(data.frame()) #For export

    # Load data from the database on app start
    output$responses <- DT::renderDT({
      loadData()
    }, options = list(pageLength = 5))

    # Observe event for the run simulation button
    shiny::observeEvent(input$runSim, {
      # Call the simulation function with both user-provided and default parameters
      simResults <- runSimulation_pgsql(input)

      # Update the results reactive value
      results(simResults)
      results_exp(simResults)
    })

    #Output the results table
    output$resultsTable <- DT::renderDT({
      results()
    }, options = list(pageLength = 5))

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {
      # Prevent submitting if results are empty
      if(nrow(results_exp()) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "No results to submit. Please run the simulation first.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      # add additional simulation setting information to the results before
      # exporting to database
      simResults_exp <- appendInputParams_pgsql(results(), input)
      saveData(simResults_exp)

      # Clear the results after submission
      results_exp(data.frame())

      # Update the responses table with new data
      output$responses <- DT::renderDT({
        loadData()
      }, options = list(pageLength = 5))

    })

    # Conditionally display the Simulation Results header
    output$simulation_results_header <- shiny::renderUI({
      if (nrow(results()) > 0) {
        shiny::h4("Simulation Results")
      } else {
        NULL
      }
    })

    # Download handler for exporting data
    output$downloadBtn <- shiny::downloadHandler(
      filename = function() {
        paste0("Simulation_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Ensure there is data to download
        #shiny::req(loadData())

        # Write the data to a CSV file
        utils::write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
