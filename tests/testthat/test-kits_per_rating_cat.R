# tests/testthat/test-kits_per_rating_cat.R
library(tidyverse)

test_that("kits_per_rating_cat returns 0 for deterministic ratings", {
  cat_ratings <- expand.grid(rater_id = 1:3, subject_id = 1:5) |>
        dplyr::mutate(rating = 1) |>
        as_cat_ratings(K = 3) |>
        set_cat_params(t = c(1,0,0), a = 1, p = c(1,0,0))

  kits <- kits_per_rating_cat(cat_ratings )
  expect_equal(kits, 0) # no entropy for perfect accuracy
})

test_that("kits_per_rating_cat ~ 1 for uniform random ratings", {
  set.seed(123)

  cat_ratings <- expand.grid(rater_id = 1:5, subject_id = 1:50) |>
    dplyr::mutate(rating = sample.int(3, 250, replace = TRUE, prob = rep(1/3, 3))) |>
    as_cat_ratings(K = 3) |>
    set_cat_params(t = c(1,1,1), a = 0, p = c(1,1,1))

  kits <- kits_per_rating_cat(cat_ratings)
  expect_gt(kits, 0.98) # should be close to 1
})

test_that("kits_per_rating_cat reduces to bits_per_rating for K = 2", {
  set.seed(123)
  cat_ratings <- generate_sample_ratings_cat(K = 2)

  kits <- kits_per_rating_cat(cat_ratings)

  bits <- cat_ratings |>
          as_rating_params_cat() |>
          dplyr::mutate(rating = rating - 1,  # it expects 0,1 values
                 t = lc_pull(t, 2),    # downsample from list to first element
                 p = lc_pull(p, 2)) |>
          bits_per_rating()

  expect_equal(round(kits, 4), round(bits, 4))
})
