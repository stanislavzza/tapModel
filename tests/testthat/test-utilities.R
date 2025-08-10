suppressMessages(library(tidyverse))

#' Raw ratings data for testing
raw_ratings <- expand.grid(
  Subject = 1:10,
  Rater = 1:6
) |>
  dplyr::mutate(Rating = rep(1:5,12)) |>
  dplyr::select(Subject, Rating, Rater)

#' as_ratings(raw_ratings, in_class)
test_that("as_ratings works", {

  ratings <- as_ratings(raw_ratings, 1:3)

  # 60 ratings total
  expect_equal(nrow(ratings), 60)

  # 36 ratings of class 1
  expect_equal(sum(ratings$rating), 36)

})

#' verify_count_index(count_index)
test_that("verify_count_index works", {

  count_index <- count_ratings(raw_ratings, 1:3, summarize = FALSE)

  # one row per subject with summarize = FALSE
  expect_equal(nrow(count_index), 10)

  # 60 ratings total
  expect_equal(sum(count_index$N_r), 60)

  # 36 ratings of class 1
  expect_equal(sum(count_index$N_c), 36)

  # should return the count_index if it's right
  expect_equal(verify_count_index(count_index), count_index)

  # should throw an error if it's wrong
  expect_error(verify_count_index(count_index %>% select(-N_r)))
  expect_error(verify_count_index(count_index %>% select(-N_c)))

  # should error if we send counts
  counts <- count_ratings(raw_ratings, 1:3, summarize = TRUE)
  expect_error(verify_count_index(counts))

})

#' count_ratings(raw_ratings, in_class, summarize = FALSE)
test_that("count_ratings works", {

  count_index <- count_ratings(raw_ratings, 1:3, summarize = FALSE)

  # one row per subject with summarize = FALSE
  expect_equal(nrow(count_index), 10)

  # 60 ratings total
  expect_equal(sum(count_index$N_r), 60)

  # 36 ratings of class 1
  expect_equal(sum(count_index$N_c), 36)

  counts <- count_ratings(raw_ratings, 1:3, summarize = TRUE)

  expect_equal(nrow(counts), 2)

  # 60 ratings total
  expect_equal(sum(counts$N_r*counts$n), 60)

  # 36 ratings of class 1
  expect_equal(sum(counts$N_c*counts$n), 36)

})

# as_count_index(ratings)
test_that("as_count_index works", {

  count_index <- raw_ratings |> as_ratings(1:3) |> as_count_index()

  # one row per subject
  expect_equal(nrow(count_index), 10)

  # 60 ratings total
  expect_equal(sum(count_index$N_r), 60)

  # 36 ratings of class 1
  expect_equal(sum(count_index$N_c), 36)

})

#' verify_counts(counts)
test_that("verify_counts works", {

  counts <- count_ratings(raw_ratings, 1:3, summarize = TRUE)

  # 60 ratings total
  expect_equal(sum(counts$N_r*counts$n), 60)

  # 36 ratings of class 1
  expect_equal(sum(counts$N_c*counts$n), 36)

  # should return the counts if it's right
  expect_equal(verify_counts(counts), counts)

  # should throw an error if it's wrong
  expect_error(verify_counts(counts %>% select(-N_r)))
  expect_error(verify_counts(counts %>% select(-N_c)))

  # should error if we send count_index
  count_index <- count_ratings(raw_ratings, 1:3, summarize = FALSE)
  expect_error(verify_counts(count_index))

})

rm(raw_ratings)
