# test_that("replext_t1_c1 works", {
#
#   res <- replext_t1_c1(S=5, fs = c(0.1, 0.15), constrs = c(1,2))
#
#   expect_true(is.data.frame(res))
#   expect_equal(nrow(res), 2)
#   expect_equal(ncol(res), 3)
# })

test_that("replext_t2_c1 works", {

  res <- replext_t2_c1(S=5, f2s = c(0.25, 0.35), constrs = c(1,2))

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 3)
})
