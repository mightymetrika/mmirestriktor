test_that("mmir_model works correctly for lm engine", {
  model <- mmir_model(mpg ~ hp + wt, data = mtcars, engine = "lm", standardize = TRUE)
  expect_s3_class(model, "lm")
})

test_that("mmir_model works correctly for glm engine", {
  model <- mmir_model(mpg ~ hp + wt, data = mtcars, engine = "glm",
                      standardize = TRUE, family = gaussian)
  expect_s3_class(model, "glm")
})

test_that("mmir_model works correctly for rlm engine", {
  model <- mmir_model(mpg ~ hp + wt, data = mtcars, engine = "rlm", standardize = TRUE)
  expect_s3_class(model, "rlm")
})

test_that("mmir_model returns error for invalid engine", {
  expect_error(mmir_model(mpg ~ hp + wt, data = mtcars, engine = "invalid_engine", standardize = TRUE),
               "Invalid engine: invalid_engine")
})

test_that("mmir_model returns error for invalid formula", {
  expect_error(mmir_model("invalid_formula", data = mtcars, engine = "lm", standardize = TRUE),
               "formula is not a valid R formula")
})

test_that("mmir_model returns error for invalid data", {
  expect_error(mmir_model(mpg ~ hp + wt, data = "invalid_data", engine = "lm", standardize = TRUE),
               "data is not a valid data frame")
})

test_that("mmir_model returns error for invalid standardize value", {
  expect_error(mmir_model(mpg ~ hp + wt, data = mtcars, engine = "lm", standardize = "invalid_standardize"),
               "standardize is not a single boolean value")
})
