#' Deal Cards to Grid
#'
#' This function deals n^2 cards from a specified or default deck to form
#' an n x n grid. The remaining deck is also returned alongside the grid.
#'
#' @param deck A dataframe representing a deck of cards, with each row being a
#' a card. The parameter is designed to take mmcards::shuffle_deck() or
#' mmcards::i_deck() as input.
#' @param n A single integer representing the number of rows and columns in the
#' grid (i.e., the grid will be n x n). This parameter is required and
#' does not have a default value.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item `cards_matrix`: an n x n matrix where each element is a list
#'   representing a card.
#'   \item `updated_deck`: a list representing the remaining deck after n^2
#'   cards have been dealt.
#' }
#'
#' @examples
#'   # Dealing cards to a 2x2 grid using the default shuffled deck
#'   deal_cards_to_grid(n = 2)
#'
#' @export
deal_cards_to_grid <- function(deck = mmcards::shuffle_deck(), n) {
  # Deal n^2 cards from the deck
  dealt_cards <- vector("list", n^2)

  for(i in 1:(n^2)) {
    deck <- mmcards::deal_card(deck)
    dealt_cards[[i]] <- deck$dealt_card
  }

  # Convert the list of dealt cards into a matrix
  cards_matrix <- matrix(dealt_cards, nrow = n, ncol = n, byrow = FALSE)

  return(list(cards_matrix = cards_matrix, updated_deck = deck$updated_deck))
}
