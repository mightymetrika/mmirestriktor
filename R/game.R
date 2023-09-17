#' FbarCards Shiny App
#'
#' Launches a 'shiny' app for the FbarCards game. In this game, a grid of cards
#' is displayed and the objective is to reorder the cards in each row such that,
#' when the rows are stacked, the columns of cards are in increasing order from
#' left to right. Players can swap the positions of two cards in the same row
#' before finalizing their choices and scoring the game. The game utilizes
#' Informative Hypothesis Testing (IHT) to score the final grid of cards.
#'
#' @description A shiny app for playing the FbarCards game, which is a puzzle
#' game that requires players to arrange a grid of cards in a specific order to
#' win. The game offers different levels of difficulty and provides instant
#' feedback on the outcome after scoring.
#'
#' @return This function launches a shiny app and does not return a value.
#'
#' @examples
#' if (interactive()) {
#'   FbarCards()
#' }
#'
#' @export
FbarCards <- function(){

  ui <- shiny::fluidPage(

    # Add shiny theme to UI
    theme = shinythemes::shinytheme("united"),

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

        # Action button to score the game
        shiny::actionButton("score_game", "Score Game"),

        # Placeholder for swap UI elements (to be added dynamically based on difficulty level)
        shiny::uiOutput("swap_ui")

      ),

      shiny::mainPanel(

        # Table output for displaying the grid of cards
        shiny::tableOutput("cards_grid"),

        # Output for the conditional content (replacing the conditional panel)
        shiny::uiOutput("conditional_content")
      )
    )
  )

  server <- function(input, output, session) {

    # Reactive value to hold the score status of the game
    game_scored <- shiny::reactiveVal(FALSE)

    # Reactive value to hold the swap status of each row
    row_swap_status <- shiny::reactiveVal()

    # Reactive value to hold the game state
    game_state <- shiny::reactiveVal()

    # Define iht_res as a reactive variable at the beginning of your server function
    iht_res <- shiny::reactiveVal(NULL)

    # New reactive expression to hold the score interpretation
    score_interpretation_reactive <- shiny::reactive({
      # Get the IHT interpretation
      if (is.null(iht_res())) return(NULL)

      score_interpretation <- iht_interpreter(iht_res())
      score_interpretation
    })

    # Function to start/restart the game
    shiny::observeEvent(input$start_game, {

      # Get the difficulty level
      n <- input$difficulty

      # Deal cards to the grid
      game_deck <- deal_cards_to_grid(deck = mmcards::i_deck(deck = mmcards::shuffle_deck(),
                                                             i_path = "inst",
                                                             i_names = c("2_of_clubs", "2_of_diamonds", "2_of_hearts", "2_of_spades",
                                                                         "3_of_clubs", "3_of_diamonds", "3_of_hearts", "3_of_spades",
                                                                         "4_of_clubs", "4_of_diamonds", "4_of_hearts", "4_of_spades",
                                                                         "5_of_clubs", "5_of_diamonds", "5_of_hearts", "5_of_spades",
                                                                         "6_of_clubs", "6_of_diamonds", "6_of_hearts", "6_of_spades",
                                                                         "7_of_clubs", "7_of_diamonds", "7_of_hearts", "7_of_spades",
                                                                         "8_of_clubs", "8_of_diamonds", "8_of_hearts", "8_of_spades",
                                                                         "9_of_clubs", "9_of_diamonds", "9_of_hearts", "9_of_spades",
                                                                         "10_of_clubs", "10_of_diamonds", "10_of_hearts", "10_of_spades",
                                                                         "jack_of_clubs", "jack_of_diamonds", "jack_of_hearts", "jack_of_spades",
                                                                         "queen_of_clubs", "queen_of_diamonds", "queen_of_hearts", "queen_of_spades",
                                                                         "king_of_clubs", "king_of_diamonds", "king_of_hearts", "king_of_spades",
                                                                         "ace_of_clubs", "ace_of_diamonds", "ace_of_hearts", "ace_of_spades"
                                                             )),
                                      n = n)

      # Set the game state
      game_state(list(cards_grid = game_deck$cards_matrix, deck = game_deck$updated_deck))

      # Reset the row swap status to allow swapping for each row
      row_swap_status(rep(FALSE, n))

      # Reset the game_scored reactive value to hide the conditional content
      game_scored(FALSE)
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
    output$cards_grid <- shiny::renderUI({
      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Create a grid of image outputs to display the card images
      shiny::fluidRow(
        lapply(1:nrow(state$cards_grid), function(row) {
          shiny::fluidRow(
            lapply(1:ncol(state$cards_grid), function(col) {
              shiny::column(width = floor(12 / nrow(state$cards_grid)),
                            style = 'padding:0px;
                                     margin-bottom:-7em',
                            shiny::imageOutput(outputId = paste0("card_image_", row, "_", col))
              )
            })
          )
        })
      )
    })

    # Set output options to ensure the UI updates correctly
    shiny::outputOptions(output, "cards_grid", suspendWhenHidden = FALSE)

    # Logic to swap cards in the same row
    shiny::observe({

      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Get the row swap status
      swap_status <- row_swap_status()

      # Get the difficulty level
      n <- nrow(state$cards_grid)

      # Loop through each row to check if a swap is requested
      for(i in 1:n) {

        # Check if the row has not been swapped yet
        if (!swap_status[i]) {

          # Get the selected columns to swap
          swap_cols <- as.numeric(input[[paste0("swap_row_", i)]])

          # Check for NULL swap_cols and exit early if found
          if (is.null(swap_cols)) next

          # Check if two columns are selected for swapping
          if (length(swap_cols) == 2) {

            # Create a new matrix to hold the updated game state
            new_cards_grid <- state$cards_grid

            # Perform the swap in the new matrix
            temp <- new_cards_grid[i, swap_cols[1]]
            new_cards_grid[i, swap_cols[1]] <- new_cards_grid[i, swap_cols[2]]
            new_cards_grid[i, swap_cols[2]] <- temp

            # Update the game state with the new matrix
            game_state(list(cards_grid = new_cards_grid, deck = state$deck))

            # Update the row swap status to indicate that the row has been swapped
            swap_status[i] <- TRUE
            row_swap_status(swap_status)
          }
        }
      }
    })

    observe({
      # Get the game state
      state <- game_state()

      # Check if the game has started
      if (is.null(state)) return(NULL)

      # Create individual image outputs for each card in the grid
      for(row in 1:nrow(state$cards_grid)) {
        for(col in 1:ncol(state$cards_grid)) {
          # Capture the current value of the icard attribute
          src <- state$cards_grid[[row, col]]$icard

          # Define a new function to create the image output
          create_image_output <- function(src, row, col) {
            output_id <- paste0("card_image_", row, "_", col)
            force(src)
            force(row)
            force(col)
            output[[output_id]] <- shiny::renderImage({
              list(src = src, contentType = "image/png", width=200, height="auto")
            }, deleteFile = FALSE)
          }

          # Call the function with the src, row, and col arguments to create the image output
          create_image_output(src, row, col)
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

      # Convert the wide-format data frame to a long-format data frame
      cards_df_long <- utils::stack(cards_df)
      names(cards_df_long) <- c("Value", "Column")

      # Convert the "Column" column to a factor
      cards_df_long$Column <- factor(cards_df_long$Column, levels = names(cards_df))

      # Fit the unrestricted linear model
      fit <- stats::lm(Value ~ Column - 1, data = cards_df_long)

      # Construct the constraint string for IHT using the levels of the 'Column' factor
      constraint <- paste(paste0("Column", levels(cards_df_long$Column)), collapse = " < ")

      # Perform IHT
      iht_result <- restriktor::iht(fit, constraints = constraint)
      iht_res(iht_result)  # Update the reactive variable here

      # Get score_interpretation using the reactive expression
      output$score_interpretation <- shiny::renderUI({
        shiny::HTML(nl2br(score_interpretation_reactive()))
      })

      # Display the IHT results
      output$iht_results <- shiny::renderPrint({
        # Get the IHT results
        iht_res()
      })

      # Set game_scored to TRUE indicating the game has been scored
      game_scored(TRUE)

    })

    output$conditional_content <- shiny::renderUI({
      if (game_scored()) {
        # Get the score interpretation
        score_interpretation <- score_interpretation_reactive()

        # Check the winning condition
        if (grepl("evidence in favor of the order-constrained hypothesis", score_interpretation)) {
          game_result <- shiny::tags$h1(style = "color: green;", "You Win!")
        } else {
          game_result <- shiny::tags$h1(style = "color: red;", "You Lose!")
        }

        shiny::tagList(
          shiny::h4("Informative Hypothesis Test Results"),
          game_result, # Display the game result here
          shiny::verbatimTextOutput("game_score"),
          shiny::verbatimTextOutput("iht_results"),
          shiny::h4("Informative Hypothesis Test Interpretation"),
          shiny::htmlOutput("score_interpretation")
        )
      }
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}




# FbarCards <- function(){
#
#   ui <- shiny::fluidPage(
#
#     # Application title
#     shiny::titlePanel("FBarCards Game"),
#
#     # Sidebar layout for input components
#     shiny::sidebarLayout(
#
#       # Sidebar panel for inputs
#       shiny::sidebarPanel(
#
#         # Numeric input for selecting difficulty level
#         shiny::numericInput("difficulty", "Select Difficulty Level (n):", min = 2, max = 7, value = 3),
#
#         # Action button to start/restart the game
#         shiny::actionButton("start_game", "Start/Restart Game"),
#
#         # Action button to score the game
#         shiny::actionButton("score_game", "Score Game"),
#
#         # Placeholder for swap UI elements (to be added dynamically based on difficulty level)
#         shiny::uiOutput("swap_ui")
#
#       ),
#
#       shiny::mainPanel(
#
#         # Table output for displaying the grid of cards
#         shiny::tableOutput("cards_grid"),
#
#         # Output for the conditional content (replacing the conditional panel)
#         shiny::uiOutput("conditional_content")
#       )
#     )
#   )
#
#   server <- function(input, output, session) {
#
#     # Reactive value to hold the score status of the game
#     game_scored <- shiny::reactiveVal(FALSE)
#
#     # Reactive value to hold the swap status of each row
#     row_swap_status <- shiny::reactiveVal()
#
#     # Reactive value to hold the game state
#     game_state <- shiny::reactiveVal()
#
#     # Define iht_res as a reactive variable at the beginning of your server function
#     iht_res <- shiny::reactiveVal(NULL)
#
#     # New reactive expression to hold the score interpretation
#     score_interpretation_reactive <- shiny::reactive({
#       # Get the IHT interpretation
#       if (is.null(iht_res())) return(NULL)
#
#       score_interpretation <- iht_interpreter(iht_res())
#       score_interpretation
#     })
#
#     # Function to start/restart the game
#     shiny::observeEvent(input$start_game, {
#
#       # Get the difficulty level
#       n <- input$difficulty
#
#       # Deal cards to the grid
#       game_deck <- deal_cards_to_grid(n = n)
#
#       # Set the game state
#       game_state(list(cards_grid = game_deck$cards_matrix, deck = game_deck$updated_deck))
#
#       # Reset the row swap status to allow swapping for each row
#       row_swap_status(rep(FALSE, n))
#
#       # Reset the game_scored reactive value to hide the conditional content
#       game_scored(FALSE)
#     })
#
#     # Dynamic UI for swap controls
#     output$swap_ui <- shiny::renderUI({
#
#       # Get the game state
#       state <- game_state()
#
#       # Check if the game has started
#       if (is.null(state)) return(NULL)
#
#       # Get the difficulty level
#       n <- nrow(state$cards_grid)
#
#       # Create swap UI elements
#       swap_ui <- lapply(1:n, function(i) {
#         shiny::selectInput(inputId = paste0("swap_row_", i),
#                            label = paste("Swap cards in row", i),
#                            choices = 1:n,
#                            selected = NULL,
#                            multiple = TRUE,
#                            selectize = FALSE)
#       })
#
#       # Return the swap UI elements as a list
#       do.call(tagList, swap_ui)
#     })
#
#     # Display the grid of cards
#     output$cards_grid <- shiny::renderTable({
#       # Get the game state
#       state <- game_state()
#
#       # Check if the game has started
#       if (is.null(state)) return(NULL)
#
#       # Extract the card names from the cards_grid matrix
#       card_names_matrix <- matrix(sapply(state$cards_grid, function(x) x$card),
#                                   nrow = nrow(state$cards_grid),
#                                   ncol = ncol(state$cards_grid),
#                                   byrow = FALSE, # Set byrow to FALSE
#                                   dimnames = list(NULL, NULL)) # Reset dimnames to avoid the conversion issue
#
#       # Display the grid of card names
#       card_names_matrix
#     }, rownames = TRUE)
#
#
#     # Logic to swap cards in the same row
#     shiny::observe({
#
#       # Get the game state
#       state <- game_state()
#
#       # Check if the game has started
#       if (is.null(state)) return(NULL)
#
#       # Get the row swap status
#       swap_status <- row_swap_status()
#
#       # Get the difficulty level
#       n <- nrow(state$cards_grid)
#
#       # Loop through each row to check if a swap is requested
#       for(i in 1:n) {
#
#         # Check if the row has not been swapped yet
#         if (!swap_status[i]) {
#
#           # Get the selected columns to swap
#           swap_cols <- as.numeric(input[[paste0("swap_row_", i)]])
#
#           # Check for NULL swap_cols and exit early if found
#           if (is.null(swap_cols)) next
#
#           # Check if two columns are selected for swapping
#           if (length(swap_cols) == 2) {
#
#             # Create a new matrix to hold the updated game state
#             new_cards_grid <- state$cards_grid
#
#             # Perform the swap in the new matrix
#             temp <- new_cards_grid[i, swap_cols[1]]
#             new_cards_grid[i, swap_cols[1]] <- new_cards_grid[i, swap_cols[2]]
#             new_cards_grid[i, swap_cols[2]] <- temp
#
#             # Update the game state with the new matrix
#             game_state(list(cards_grid = new_cards_grid, deck = state$deck))
#
#             # Update the row swap status to indicate that the row has been swapped
#             swap_status[i] <- TRUE
#             row_swap_status(swap_status)
#           }
#         }
#       }
#     })
#
#     # Logic to score the game
#     shiny::observeEvent(input$score_game, {
#
#       # Get the game state
#       state <- game_state()
#
#       # Check if the game has started
#       if (is.null(state)) return(NULL)
#
#       # Create a data.frame from the grid of cards to fit the lm model
#       cards_df <- data.frame(do.call(rbind, lapply(1:nrow(state$cards_grid), function(i) {
#         sapply(1:ncol(state$cards_grid), function(j) {
#           state$cards_grid[[i, j]]$value
#         })
#       })))
#
#       # Convert the wide-format data frame to a long-format data frame
#       cards_df_long <- utils::stack(cards_df)
#       names(cards_df_long) <- c("Value", "Column")
#
#       # Convert the "Column" column to a factor
#       cards_df_long$Column <- factor(cards_df_long$Column, levels = names(cards_df))
#
#       # Fit the unrestricted linear model
#       fit <- stats::lm(Value ~ Column - 1, data = cards_df_long)
#
#       # Construct the constraint string for IHT using the levels of the 'Column' factor
#       constraint <- paste(paste0("Column", levels(cards_df_long$Column)), collapse = " < ")
#
#       # Perform IHT
#       iht_result <- restriktor::iht(fit, constraints = constraint)
#       iht_res(iht_result)  # Update the reactive variable here
#
#       # Updated score_interpretation output to use the new reactive expression
#       output$score_interpretation <- shiny::renderText({
#         score_interpretation_reactive()
#       })
#
#       # Display the IHT results
#       output$iht_results <- shiny::renderPrint({
#         # Get the IHT results
#         iht_res()
#       })
#
#       # Set game_scored to TRUE indicating the game has been scored
#       game_scored(TRUE)
#
#     })
#
#     output$conditional_content <- shiny::renderUI({
#       if (game_scored()) {
#         # Get the score interpretation
#         score_interpretation <- score_interpretation_reactive()
#
#         # Check the winning condition
#         if (grepl("evidence in favor of the order-constrained hypothesis", score_interpretation)) {
#           game_result <- shiny::tags$h1(style = "color: green;", "You Win!")
#         } else {
#           game_result <- shiny::tags$h1(style = "color: red;", "You Lose!")
#         }
#
#         shiny::tagList(
#           shiny::h4("Informative Hypothesis Test Results"),
#           game_result, # Display the game result here
#           shiny::verbatimTextOutput("game_score"),
#           shiny::verbatimTextOutput("iht_results"),
#           shiny::h4("Informative Hypothesis Test Interpretation"),
#           shiny::verbatimTextOutput("score_interpretation")
#         )
#       }
#     })
#   }
#
#   shiny::shinyApp(ui = ui, server = server)
# }
