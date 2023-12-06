test_that("d_eq_spaced works: 'For example, if k = 4 and the effect size f = 0.25,
          then d = sqrt(1/20)'", {
  expect_equal(d_eq_spaced(4, 0.25), sqrt(1/20))
})

test_that("mui works: 'For example, if k = 4 and the effect size f = 0.25,
          then d = sqrt(1/20) and mu1 = -0.335. Then, mu2 = mu1 + d,
          mu3 = mu1 + 2d and mu4 = mu1 + 3d.'",{

            #Compute d and mui
            d <- d_eq_spaced(4, 0.25)
            mui <- mui(4, 0.25)


            expect_equal(round(mui[1], 3), -0.335)
            expect_equal(mui[2], mui[1] + d)
            expect_equal(mui[3], mui[1] + 2*d)
            expect_equal(mui[4], mui[1] + 3*d)
            })

test_that("generate_datasets works", {

  # Generate s datasets
  s <- 2
  k <- 4
  n <- 30
  dsets <- generate_datasets(S = s, k = k, f = 0.25, n = n)

  expect_equal(length(dsets), s)
  expect_type(dsets, "list")
  expect_true(is.data.frame(dsets[[1]]))
  expect_equal(nrow(dsets[[1]]), k*n)
          })

test_that("pj_pow works", {
  pow <- generate_datasets(S = 10, k = 4, f = 0.25, n = 30) |> pj_pow(constr=1)
  expect_type(pow, "double")
  expect_true(pow >= 0)
  expect_true(pow <= 1)
})

test_that("sim_reg works", {
  res <- sim_reg(n = 20, p = 3, f2 = 0.10, rho = 0.5)
  expect_equal(length(res), 2)
  expect_equal(length(res$y), 20)
  expect_true(is.matrix(res$X))
})

test_that("generate_datasets_reg works", {
  s <- 2
  p <- 4
  n <- 30

  dsets <- generate_datasets_reg(S = s, n = n, p = p, f2 = 0.10, rho = 0.5)

  expect_equal(length(dsets), s)
  expect_type(dsets, "list")
  expect_true(is.data.frame(dsets[[1]]))
  expect_equal(nrow(dsets[[1]]), n)
})

test_that("lr_pow works", {
  pow <- generate_datasets_reg(S = 4, n = 30, p = 3, f2 = 0.20, rho = 0.5) |> lr_pow()
  expect_type(pow, "double")
  expect_true(pow >= 0)
  expect_true(pow <= 1)
})
