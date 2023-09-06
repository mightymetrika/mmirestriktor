test_that("iht_interpreter works", {

  model <- mmir_model(mpg ~ -1 + hp + wt, data = mtcars, engine = "lm", standardize = TRUE)
  iht_res <- restriktor::iht(model, constraints = 'hp < wt')
  res <- iht_interpreter(iht_res)


  expect_true(is.character(res))
  expect_error(iht_interpreter(model))
})

test_that("rm_interpreter works", {

  model <- mmir_model(mpg ~ -1 + hp + wt, data = mtcars, engine = "lm", standardize = TRUE)
  rm_res <- restriktor::restriktor(model, constraints = 'hp < wt')
  res <- rm_interpreter(rm_res)


  expect_true(is.character(res))
  expect_error(iht_interpreter(model))
})


