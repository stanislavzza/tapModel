# tests/testthat/test-krits_per_rating_cat.R
library(tidyverse)

test_that("krits_per_rating_cat returns 0 for deterministic ratings", {
  cat_ratings <- expand.grid(rater_id = 1:3, subject_id = 1:5) |>
        dplyr::mutate(rating = 1) |>
        as_cat_ratings(K = 3) |>
        set_cat_params(t = c(1,0,0), a = 1, p = c(1,0,0))

  krits <- krits_per_rating_cat(cat_ratings )
  expect_equal(krits, 0) # no entropy for perfect accuracy
})

test_that("krits_per_rating_cat ~ 1 for uniform random ratings", {
  set.seed(123)

  cat_ratings <- expand.grid(rater_id = 1:5, subject_id = 1:50) |>
    dplyr::mutate(rating = sample.int(3, 250, replace = TRUE, prob = rep(1/3, 3))) |>
    as_cat_ratings(K = 3) |>
    set_cat_params(t = c(1,1,1), a = 0, p = c(1,1,1))

  krits <- krits_per_rating_cat(cat_ratings)
  expect_gt(krits, 0.98) # should be close to 1
})

test_that("krits_per_rating_cat reduces to bits_per_rating for K = 2", {
  set.seed(123)
  cat_ratings <- generate_sample_ratings_cat(K = 2)

  krits <- krits_per_rating_cat(cat_ratings)

  bits <- cat_ratings |>
          as_rating_params_cat() |>
          dplyr::mutate(rating = rating - 1,  # it expects 0,1 values
                 t = lc_pull(t, 2),    # downsample from list to first element
                 p = lc_pull(p, 2)) |>
          bits_per_rating()

  expect_equal(round(krits, 3), round(bits, 3))
})

test_that("a^2 ~ fleiss kappa for unbiased ratings", {
  set.seed(123)
  cat_ratings <- generate_sample_ratings_cat(N_s = 200, N_r = 10,
                                             K = 4,
                                             params = list(t = c(1,1,1,1), a = .5, p = c(1,1,1,1)),
                                             details = FALSE)

  a_f <- fleiss_kappa_cat(cat_ratings)$a

  expect_equal(.5, round(a_f, 1))
})

test_that("fit_counts_cat recovers parameters", {
  set.seed(123)

  # set params
  true_t = c(.1, .2, .3, .4)
  true_a = .5
  true_p = c(.4, .3, .2, .1)

  # generate ratings
  cat_ratings <- generate_sample_ratings_cat(N_s = 300, N_r = 10,
                                             K = 4,
                                             params = list(t = true_t,
                                                           a = true_a,
                                                           p = true_p),
                                             details = FALSE)

  # get estimates
  avg_params <- cat_ratings |>
                fit_counts_cat()

  p_l2_diff <- lc_norm(lc_subtract(list(true_p), list(avg_params$p)), "l2")
  t_l2_diff <- lc_norm(lc_subtract(list(true_t), list(avg_params$t)), "l2")

  expect_equal(true_a, round(avg_params$a, 1))
  expect_lt(p_l2_diff, .1)
  expect_lt(t_l2_diff, .1)
})

test_that("binary solution matches cat solution with K = 2", {
  # create a binary sample using cat methods and see if the extimated
  # t_i values match when we estimate using cat methods and bin methods

  set.seed(123)

  true_t <- c(.2, .8)
  true_a <- .5
  true_p <- c(.8, .2)

  params_cat <- list(t = true_t, a = true_a, p = true_p)
  params_bin <- list(t = true_t[2], a = true_a, p = true_p[2])

  # generate ratings
  cat_ratings <- generate_sample_ratings_cat(
    N_s = 100, N_r = 5, K = 2,
    params = params_cat,
    details = TRUE
  )

  t1 <- cat_ratings |>
    estimate_ti_cat() |>
    pluck("subjects") |>
    mutate(cat = lc_pull(t,2)) # t[2] is the class 1 coef


  ratings <- cat_ratings$ratings |>
    mutate(rating = rating - 1) # binary is 0, 1 not 1, 2

  t2 <- ratings |>
    as_rating_params(params_bin) |>
    estimate_ti() |>
    pull_rating_params() |>
    pluck("subjects")

  t_out <- data.frame(subject_id =t1$subject_id,
                      cat = t1$cat,
                      bin = t2$t)
  # squared error
  e2 <- sum((t_out$cat - t_out$bin)^2)
  expect_lt(e2, 1e-4)

  ####### Do the confusion matrices match? ##################
  # confusion matrices
  c_cat <- cat_ratings |>
    as_rating_params_cat() |>
    tapModel:::estimate_C(K = 2, normalize = TRUE)

  rater_stats <- cat_ratings$ratings |>
    mutate(rating = rating -1) |>
    as_rating_params(params_bin) |>
    #group_by(rater_id) |>
    summarize(tpr  = mean(t*rating),         # ta + ta'p
              tnr  = mean((1-t)*(1-rating)), # t'a + t'a'p'
              fpr  = mean((1-t)*rating),     # t'a'p
              fnr  = mean(t*(1-rating)),
              total = tpr + tnr + fpr + fnr)

  c_bin <- matrix(c(rater_stats$tnr,
                    rater_stats$fnr,
                    rater_stats$fpr,
                    rater_stats$tpr),
                  nrow = 2, ncol = 2)

  expect_equal(c_cat, c_bin)


})
