FbarCards <- function(){

  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("FBarCards Game"),

    # Sidebar layout for input components
    shiny::sidebarLayout(

      # Sidebar panel for inputs
      shiny::sidebarPanel(

        # Numeric input for selecting difficulty level
        shiny::numericInput("difficulty", "Select Difficulty Level (n):", min = 2, max = 7, value = 3),

        # Action button to start/restart the game
        shiny::actionButton("start_game", "Start/Restart Game"),

        # Placeholder for swap UI elements (to be added dynamically based on difficulty level)
        shiny::uiOutput("swap_ui")

      ),

      # Main panel for outputs
      shiny::mainPanel(

        # Table output for displaying the grid of cards
        shiny::tableOutput("cards_grid"),

        # Text output for displaying the game score and IHT interpretation
        shiny::verbatimTextOutput("game_score"),
        shiny::htmlOutput("iht_interpretation")

      )
    )
  )

  server <- function(input, output, session) {

    # Reactive value to hold the game state
    game_state <- shiny::reactiveVal()

    # Function to start/restart the game
    shiny::observeEvent(input$start_game, {

      # Get the difficulty level
      n <- input$difficulty

      # Deal cards to the grid
      game_deck <- deal_cards_to_grid(n = n)

      # Set the game state
      game_state(list(cards_grid = game_deck$cards_matrix, deck = game_deck$updated_deck))
    })

    # Dynamic UI for swap controls
    output$swap_ui <- shiny::renderUI({

      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Get the difficulty level
      n <- nrow(state$cards_grid)

      # Create swap UI elements
      swap_ui <- lapply(1:n, function(i) {
        shiny::selectInput(inputId = paste0("swap_row_", i),
                    label = paste("Swap cards in row", i),
                    choices = 1:n,
                    selected = NULL,
                    multiple = TRUE,
                    selectize = FALSE)
      })

      # Return the swap UI elements as a list
      do.call(tagList, swap_ui)
    })

    # Display the grid of cards
    # output$cards_grid <- shiny::renderTable({
    #
    #   # Get the game state
    #   state <- game_state()
    #
    #   # Check if the game has started
    #   if (is.null(state)) return(NULL)
    #
    #   # Display the grid of cards
    #   state$cards_grid
    # }, rownames = TRUE)
    output$cards_grid <- shiny::renderTable({

      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Extract the card names from the cards_grid matrix
      card_names_matrix <- matrix(sapply(state$cards_grid, function(x) x$card),
                                  nrow = nrow(state$cards_grid),
                                  ncol = ncol(state$cards_grid),
                                  byrow = TRUE)

      # Display the grid of card names
      card_names_matrix
    }, rownames = TRUE)


    # Logic to swap cards in the same row
    shiny::observe({

      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Get the difficulty level
      n <- nrow(state$cards_grid)

      # Loop through each row to check if a swap is requested
      for(i in 1:n) {

        # Get the selected columns to swap
        swap_cols <- input[[paste0("swap_row_", i)]]

        # Check if two columns are selected for swapping
        if (length(swap_cols) == 2) {

          # Swap the cards in the selected columns
          state$cards_grid[i, swap_cols] <- state$cards_grid[i, rev(swap_cols)]

          # Update the game state
          game_state(state)
        }
      }
    })

    # Logic to score the game
    shiny::observeEvent(input$score_game, {

      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Create a data.frame from the grid of cards to fit the lm model
      cards_df <- data.frame(do.call(rbind, lapply(1:nrow(state$cards_grid), function(i) {
        sapply(1:ncol(state$cards_grid), function(j) {
          state$cards_grid[[i, j]]$value
        })
      })))

      # Fit the unrestricted linear model
      fit <- stats::lm(cards_df[[ncol(cards_df)]] ~ . - 1, data = cards_df)

      # Construct the constraint string for IHT
      constraint <- paste(rev(colnames(cards_df)[-ncol(cards_df)]), collapse = " < ")

      # Perform IHT
      iht_res <- restriktor::iht(fit, constraints = constraint)

      # Interpret the IHT result to score the game
      score_interpretation <- iht_interpreter(iht_res)

      # Display the score interpretation
      output$score_interpretation <- shiny::renderText({ score_interpretation })
    })
  }

  shiny::shinyApp(ui = ui, server = server)

}
