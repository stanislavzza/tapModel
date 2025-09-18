# tests/testthat/test-list_columns.R
library(tidyverse)

test_that("lc_vsum adds vectors vertically", {
  lc <- list(c(1, 2), c(3, 4), c(5, 6))
  expect_equal(lc_vsum(lc), c(9, 12))
})

test_that("lc_add and lc_subtract work elementwise", {
  lc1 <- list(c(1, 2), c(3, 4))
  lc2 <- list(c(5, 6), c(7, 8))
  expect_equal(lc_add(lc1, lc2), list(c(6, 8), c(10, 12)))
  expect_equal(lc_subtract(lc2, lc1), list(c(4, 4), c(4, 4)))
})

test_that("lc_mpy handles scalars and list-columns", {
  lc <- list(c(1, 2), c(3, 4))
  scalars <- c(2, 3)
  lc2 <- list(c(2, 2), c(0, 1))
  expect_equal(lc_mpy(lc, scalars), list(c(2, 4), c(9, 12)))
  expect_equal(lc_mpy(lc, lc2), list(c(2, 4), c(0, 4)))
})

test_that("lc_zero and lc_one generate correctly", {
  expect_equal(lc_zero(2, 3), list(c(0, 0, 0), c(0, 0, 0)))
  expect_equal(lc_one(2, 3), list(c(1, 1, 1), c(1, 1, 1)))
})

test_that("lc_indicator builds one-hot vectors", {
  idx <- c(1, 3, 2)
  out <- lc_indicator(idx, K = 3)
  expect_equal(out[[1]], c(1, 0, 0))
  expect_equal(out[[2]], c(0, 0, 1))
  expect_equal(out[[3]], c(0, 1, 0))
})

test_that("lc_fn applies a function elementwise", {
  lc <- list(c(1, 2), c(3, 4))
  out <- lc_fn(lc, function(x) x^2)
  expect_equal(out, list(c(1, 4), c(9, 16)))
})

test_that("lc_prob clamps and normalizes", {
  lc <- list(c(-1, 2), c(0.1, 0.1))
  out <- lc_prob(lc)
  expect_equal(out[[1]], c(0, 1))          # clamped and normalized
  expect_equal(out[[2]], c(0.5, 0.5))      # normalized
})

test_that("lc_dot computes rowwise dot products", {
  lc1 <- list(c(1, 2), c(3, 4))
  lc2 <- list(c(2, 2), c(0, 1))
  expect_equal(lc_dot(lc1, lc2), c(6, 4))
})

test_that("lc_norm computes l1 and l2 norms", {
  lc <- list(c(3, 4), c(1, -1))
  expect_equal(lc_norm(lc, "l2"), c(5, sqrt(2)))
  expect_equal(lc_norm(lc, "l1"), c(7, 2))
})

test_that("lc_bind collapses list-column into a matrix", {
  lc <- list(c(1, 2), c(3, 4))
  mat <- lc_bind(lc)
  expect_equal(mat, matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
})

test_that("lc_logsumexp is numerically stable", {
  lc <- list(c(-1000, -1001), c(0, log(2)))
  out <- lc_logsumexp(lc)
  expect_true(is.finite(out[1]))
  expect_equal(round(out[2], 6), log(3), tolerance = 1e-6)
})

test_that("lc_pull extracts elements correctly", {
  lc <- list(c(0.7, 0.3), c(0.4, 0.6))
  expect_equal(lc_pull(lc, 1), c(0.7, 0.4))
  expect_equal(lc_pull(lc, 2), c(0.3, 0.6))
})

test_that("lc_clone replicates selected element across vectors", {
  lc <- list(c(0.1, 0.2, 0.3, 0.4),
             c(5, 6, 7, 8))
  idx <- c(3, 2)
  out <- lc_clone(lc, idx)
  expect_equal(out[[1]], rep(0.3, 4))
  expect_equal(out[[2]], rep(6, 4))
})

