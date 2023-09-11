test_that("deal_cards_to_grid works", {
  card_grid <- deal_cards_to_grid(n = 3)
  expect_equal(length(card_grid), 2)
  expect_true(inherits(card_grid[[1]], "list"))
  expect_true(inherits(card_grid[[1]][[1]], "list"))
  expect_true(inherits(card_grid[[1]][[2]], "list"))
  expect_true(inherits(card_grid[[1]][[3]], "list"))
  expect_false(inherits(card_grid[[1]][[1]], "data.frame"))
  expect_s3_class(card_grid[[2]], "StandardDeck")
  expect_s3_class(card_grid[[2]], "ShuffledDeck")
  expect_s3_class(card_grid[[2]], "data.frame")
})


# Swap the cards in the selected columns
#state$cards_grid[[i, swap_cols]] <- state$cards_grid[[i, rev(swap_cols)]]
# temp <- state$cards_grid[i, swap_cols[1]]
# state$cards_grid[i, swap_cols[1]] <- state$cards_grid[i, swap_cols[2]]
# state$cards_grid[i, swap_cols[2]] <- temp
# temp <- state$cards_grid[[i, swap_cols[1]]]
# state$cards_grid[[i, swap_cols[1]]] <- state$cards_grid[[i, swap_cols[2]]]
# state$cards_grid[[i, swap_cols[2]]] <- temp
