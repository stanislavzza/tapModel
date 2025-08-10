library(tidyverse)
set.seed(123)
# utility function for testing
as_list <- function(x){
  x <- as.list(x)
  # remove any names
  names(x) <- NULL
  return(x)
}

############################## SAMPLE DATA ###################################
rating_sample <- function(){
  sample(c("pass","fail","incomplete"), 5, prob = c(.4,.3,.3), replace = TRUE)
}
# raters in columns ########################################
raw_ratings_rater <- data.frame(rater1 = rating_sample(),
                                rater2 = rating_sample(),
                                rater3 = rating_sample(),
                                my_category = "portfolio",
                                my_subject = 1:5)

# subjects in columns ########################################
raw_ratings_subject <- data.frame(subject1 = rating_sample(),
                                  subject2 = rating_sample(),
                                  subject3 = rating_sample(),
                                  my_category = "portfolio",
                                  my_rater = str_c("rater",1:5))

# categories in columns
raw_ratings_category <- data.frame(category1 = rating_sample(),
                                   category2 = rating_sample(),
                                   category3 = rating_sample(),
                                   my_subject = str_c("subject",1:5),
                                   my_rater = 1:5)

# single column for ratings--the long data format
raw_ratings_long <- data.frame(my_subject = str_c("subject",rep(1:5, each = 3)),
                               my_rater = str_c("rater",rep(1:3, 5)),
                               my_rating = rating_sample())

#################################### TESTS ####################################

###############################################################################
##################### raters ##################################################
###############################################################################
test_that("raw ratings with rater headers", {

  ratings <- format_raw_ratings(raw_ratings_rater,
                                rating_cols = c("rater1", "rater2", "rater3"),
                                rating_colnames_type = "rater",
                                subject_id_col = "my_subject",
                                rater_id_col = NULL,
                                prune = FALSE)

  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id", "my_category"))

  # value check of first row
  expect_identical(as_list(ratings[1,]), list(1L,"pass","rater1","portfolio"))

 # with raw ratings
 ratings<- format_raw_ratings(raw_ratings_rater,
                                rating_cols = c("rater1", "rater2", "rater3"),
                                rating_colnames_type = "rater",
                                subject_id_col = "my_subject",
                                rater_id_col = NULL,
                                prune = TRUE)

 # column check
 expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id"))

 # value check, should be binary
 expect_identical(as_list(ratings[1,]), list(1L,"pass","rater1"))

})

###############################################################################
##################### subjects#################################################
###############################################################################
test_that("raw ratings with subject headers", {

  ratings  <- format_raw_ratings(raw_ratings_subject,
                                rating_cols = c("subject1", "subject2", "subject3"),
                                rating_colnames_type = "subject",
                                subject_id_col = NULL,
                                rater_id_col = "my_rater",
                                prune = FALSE)

  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id", "my_category"))

  # value check, should be binary
  expect_identical(as_list(ratings[1,]), list("subject1","fail","rater1","portfolio"))

  # with raw ratings
  ratings  <- format_raw_ratings(raw_ratings_subject,
                                rating_cols = c("subject1", "subject2", "subject3"),
                                rating_colnames_type = "subject",
                                subject_id_col = NULL,
                                rater_id_col = "my_rater",
                                prune = TRUE)

  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id"))

  # value check across cols
  expect_identical(as_list(ratings[1,]), list("subject1","fail","rater1"))

})

###############################################################################
##################### categories ##############################################
###############################################################################
test_that("raw ratings with category headers", {

  # with binary ratings
  ratings <- format_raw_ratings(raw_ratings_category,
                                rating_cols = c("category1", "category2", "category3"),
                                rating_colnames_type = "category",
                                subject_id_col = "my_subject",
                                rater_id_col = "my_rater",
                                prune = FALSE)

  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id", "category"))

  # value check, should be binary
  expect_identical(as_list(ratings[1,]), list("subject1","fail",1L,"category1"))

  # with raw ratings
  ratings <- format_raw_ratings(raw_ratings_category,
                                          rating_cols = c("category1", "category2", "category3"),
                                          rating_colnames_type = "category",
                                          subject_id_col = "my_subject",
                                          rater_id_col = "my_rater",
                                          prune = FALSE)

  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id", "category"))

  # value check across cols
  expect_identical(as_list(ratings[1,]), list("subject1","fail",1L,"category1"))

})

###############################################################################
##################### long format #############################################
###############################################################################
test_that("raw ratings with single rating column", {

  # with binary ratings
  ratings <- format_raw_ratings(raw_ratings_long,
                                          rating_cols = "my_rating",
                                          rating_colnames_type = "rating",
                                          subject_id_col = "my_subject",
                                          rater_id_col = "my_rater",
                                          prune = FALSE)
  # column check
  expect_equal(colnames(ratings), c( "subject_id",  "rating", "rater_id"))

  # value check, should be binary
  expect_identical(as_list(ratings[1,]), list("subject1","pass","rater1"))

  # with raw ratings, no rater_id column
  ratings <- format_raw_ratings(raw_ratings_long |>
                                  select(-my_rater),
                                          rating_cols = "my_rating",
                                          rating_colnames_type = "rating",
                                          subject_id_col = "my_subject",
                                          rater_id_col = NULL,
                                          prune = TRUE)

    expect_equal(colnames(ratings), c( "subject_id",  "rating"))

  # value check across cols
  expect_identical(as_list(ratings[1,]), list("subject1","pass"))

})

################################## CLEAN UP ###################################
rm(rating_sample, raw_ratings_rater, raw_ratings_subject, raw_ratings_category, raw_ratings_long)
