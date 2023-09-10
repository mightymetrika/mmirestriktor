deal_cards_to_grid <- function(deck = mmcards::shuffle_deck(), n) {
  # Deal n^2 cards from the deck
  dealt_cards <- vector("list", n^2)

  for(i in 1:(n^2)) {
    deck <- mmcards::deal_card(deck)
    dealt_cards[[i]] <- deck$dealt_card
  }

  # Convert the list of dealt cards into a matrix
  cards_matrix <- matrix(dealt_cards, nrow = n, ncol = n, byrow = TRUE)

  return(list(cards_matrix = cards_matrix, updated_deck = deck$updated_deck))
}
