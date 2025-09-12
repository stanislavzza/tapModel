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
test_that("verify_params accepts valid parameter sets", {
  valid_sets <- list(
    list(t = 0.8, a = 0.9, p = 0.6),
    list(t = 0.7, a0 = 0.85, a1 = 0.9, p = 0.6),
    list(t = 0.7, a = 0.85, p0 = 0.5, p1 = 0.6),
    list(t = 0.7, a0 = 0.85, a1 = 0.9, p0 = 0.5, p1 = 0.6)
  )

  for (params in valid_sets) {
    expect_identical(verify_params(params), 0)
  }
})

test_that("verify_params throws error on invalid sets", {
  expect_error(verify_params(list(t = 0.8, a = 0.9)),
               "params must be list or named vector containing")
  expect_error(verify_params(list(t = 0.8, a = 0.9, p = NA)),
               "between 0 and 1")
  expect_error(verify_params(list(t = 0.8, a = 1.1, p = 0.9)),
               "between 0 and 1")
  expect_error(verify_params(matrix(c(0.8, 0.9, 0.6), nrow = 1)),
               "params must be a list")
})

# expand params
test_that("expand_params converts (t, a, p) to full form", {
  params <- list(t = 0.8, a = 0.9, p = 0.6)
  expanded <- expand_params(params)

  expect_s3_class(expanded, "data.frame")
  expect_named(expanded, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(expanded$a0, 0.9)
  expect_equal(expanded$a1, 0.9)
  expect_equal(expanded$p0, 0.6)
  expect_equal(expanded$p1, 0.6)
})

test_that("expand_params works when only p needs expanding", {
  params <- list(t = 0.8, a0 = 0.85, a1 = 0.9, p = 0.6)
  expanded <- expand_params(params)

  expect_named(expanded, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(expanded$p0, 0.6)
  expect_equal(expanded$p1, 0.6)
})

test_that("expand_params works when only a needs expanding", {
  params <- list(t = 0.8, a = 0.9, p0 = 0.5, p1 = 0.6)
  expanded <- expand_params(params)

  expect_named(expanded, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(expanded$a0, 0.9)
  expect_equal(expanded$a1, 0.9)
})

test_that("expand_params leaves fully expanded params unchanged", {
  params <- list(t = 0.8, a0 = 0.85, a1 = 0.9, p0 = 0.5, p1 = 0.6)
  expanded <- expand_params(params)

  expect_named(expanded, c("t", "a0", "a1", "p0", "p1"))
  expect_equal(expanded$a0, 0.85)
  expect_equal(expanded$p1, 0.6)
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

