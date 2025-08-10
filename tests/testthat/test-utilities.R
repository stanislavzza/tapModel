suppressMessages(library(tidyverse))

#' Raw ratings data for testing
raw_ratings <- expand.grid(
  Subject = 1:10,
  Rater = 1:6
) |>
  dplyr::mutate(Rating = rep(1:5,12)) |>
  dplyr::select(Subject, Rating, Rater)

#' as_binary_ratings(raw_ratings, in_class)
test_that("as_binary_ratings works", {

  ratings <- as_binary_ratings(raw_ratings, 1:3)

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
  expect_equal(verify_count_index(count_index), 0L)

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

  count_index <- raw_ratings |> as_binary_ratings(1:3) |> as_count_index()

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
  expect_equal(verify_counts(counts), 0L)

  # should throw an error if it's wrong
  expect_error(verify_counts(counts %>% select(-N_r)))
  expect_error(verify_counts(counts %>% select(-N_c)))

  # should error if we send count_index
  count_index <- count_ratings(raw_ratings, 1:3, summarize = FALSE)
  expect_error(verify_counts(count_index))

})

rm(raw_ratings)

################## params #####################

#' verify_params(params)
test_that("verify_params accepts valid parameter sets", {
  valid_sets <- list(
    list(t = 0.8, a = 0.9, p = 0.6),
    list(t = 0.7, a0 = 0.85, a1 = 0.9, p = 0.6),
    list(t = 0.7, a = 0.85, p0 = 0.5, p1 = 0.6),
    list(t = 0.7, a0 = 0.85, a1 = 0.9, p0 = 0.5, p1 = 0.6)
  )

  for (params in valid_sets) {
    expect_silent({
      df <- verify_params(params)
      expect_s3_class(df, "data.frame")
      expect_true(all(names(params) %in% names(df)))
    })
  }
})

test_that("verify_params accepts named vectors", {
  vec <- c(t = 0.8, a = 0.9, p = 0.6)
  result <- verify_params(vec)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("t", "a", "p"))
})

test_that("verify_params throws error on missing parameter sets", {
  expect_error(verify_params(list(t = 0.8, a = 0.9)),
               "params must be list or named vector containing one of the following sets")
})

test_that("verify_params throws error on non-list/vector input", {
  expect_error(verify_params(matrix(c(0.8, 0.9, 0.6), nrow = 1)),
               "params must be a list or named vector")
})

test_that("verify_params throws error on NA or out-of-bounds values", {
  expect_error(verify_params(list(t = 0.8, a = 0.9, p = NA)),
               "All parameters must be between 0 and 1")
  expect_error(verify_params(list(t = 0.8, a = 0.9, p = 1.1)),
               "All parameters must be between 0 and 1")
  expect_error(verify_params(list(t = 0.8, a = -0.1, p = 0.6)),
               "All parameters must be between 0 and 1")
})

test_that("verify_params expands (t, a, p) to full form", {
  params <- list(t = 0.8, a = 0.9, p = 0.6)
  result <- verify_params(params, expand = TRUE)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(result$a0, 0.9)
  expect_equal(result$a1, 0.9)
  expect_equal(result$p0, 0.6)
  expect_equal(result$p1, 0.6)
})

test_that("verify_params expands (t, a, p0, p1) correctly", {
  params <- list(t = 0.8, a = 0.9, p0 = 0.5, p1 = 0.6)
  result <- verify_params(params, expand = TRUE)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(result$a0, 0.9)
  expect_equal(result$a1, 0.9)
})

################## rating_params ##############

test_that("verify_rating_params accepts correct data frame", {
  df <- data.frame(
    subject_id = 1:3,
    rater_id = c("A", "B", "C"),
    rating = c(1, 0, 1),
    t = c(0.8, 0.8, 0.8),
    a = c(0.9, 0.9, 0.9),
    p = c(0.7, 0.7, 0.7)
  )

  expect_silent({
    out <- verify_rating_params(df)
    expect_identical(out, 0)
  })
})

test_that("verify_rating_params catches missing t/a/p columns", {
  df <- data.frame(
    subject_id = 1:3,
    rater_id = c("A", "B", "C"),
    rating = c(1, 0, 1)
    # t/a/p columns missing
  )
  expect_error(verify_rating_params(df), "must have columns t, a, p")
})

test_that("verify_rating_params catches non-binary ratings", {
  df <- data.frame(
    subject_id = 1:3,
    rater_id = c("A", "B", "C"),
    rating = c(1, 0, 2),  # invalid rating
    t = c(0.8, 0.8, 0.8),
    a = c(0.9, 0.9, 0.9),
    p = c(0.7, 0.7, 0.7)
  )
  expect_error(verify_rating_params(df), "Ratings must be binary")
})

