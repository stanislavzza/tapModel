#' Create a ratings data frame
#' @description The package functions require a specific format for the ratings
#' data, e.g. ratings must be binary and the column names must be subject_id,
#' rating, and (optionally) rater_id.
#' @param raw_ratings A data frame where the first column is the subject ID,
#' the second column is a rating for that subject, and an optional third
#' column gives the subject ID. Usually this is created by using a select
#' statement on an original data frame.
#' @param in_class A vector with the rating values that comprise the in-class. For
#' example, for a 1-5 scale, you might pick 1:3 to be the in-class, so that
#' the result is a binary distinction between {1,2,3} and {4,5}.
#' @return A data frame with columns subject_id, rating, and rater_id if
#' that information is included.
#' @export
as_ratings <- function(raw_ratings, in_class){
  message("as_ratings() expects subject_id in first column, rating in second column,
          and rater_id in third column if present. They can have any name.")

  # validity check on rating column. Should only have a few values
  if(length(unique(raw_ratings[[2]])) > 5){
    warning("Rating column should have only a few values. May want to check the column order.")
  }

  # create the standardized binary data
  ratings <- raw_ratings |>
    select(1:2) |>
    na.omit() |> # no blanks
    rename(subject_id = 1, rating = 2) |>
    mutate(rating = as.integer(rating %in% in_class)) |>
    select(subject_id, rating)

  # if we have a third column, assume it's the rater ID
  if(ncol(raw_ratings) == 3){
    rater_id <- raw_ratings |>
      na.omit() |>
      select(rater_id = 3)

    ratings <- cbind(ratings, rater_id)
  }


  return(ratings)
}

#' Verify count_index is in the correct format
#' @param counts A data frame with columns N_r, N_c for each subject
#' There shouldn't be a column n, which indicates multiplicity.
#' @return The original data frame if it's right or an error message.
#' @export
verify_count_index <- function(count_index){
  # make sure we have columns N_r and N_c
  if(!all(c("N_r", "N_c") %in% colnames(count_index))) {
    stop("counts must have columns N_r and N_c")
  }

  # we shouldn't have column n
  if("n" %in% colnames(count_index)){
    stop("There's a column 'n', which implies that this is a summarized count.
         There should be one row per subject for a count_index")
  }

  return(count_index)
}

#' Verify counts are in the correct format
#' @param counts A data frame with columns N_r, N_c, and n
#' @return The original data frame if it's right, fix it if
#' we can if not, or an error message.
#' @export
verify_counts <- function(counts){
  # make sure we have columns N_r and N_c
  if(!all(c("N_r", "N_c") %in% colnames(counts))) {
    stop("counts must have columns N_r and N_c")
  }

  # if we don't have column n, throw an error
  if(!("n" %in% colnames(counts))){
    stop("verify_counts() expects a column n that tallies up the (N_r, N_c) pairs.")
  }

  return(counts)
}


#' Verify ratings are in the correct format
#' @param ratings A data frame with columns subject_id, rater_id, and rating
#' @return The original data frame if it's right, or an error message
#' @export
verify_ratings <- function(ratings){
  # make sure we have columns subject_id, rater_id, and rating
  if(!all(c("subject_id", "rater_id", "rating") %in% colnames(ratings))) {
    stop("A ratings data frame must have columns subject_id, rater_id, and rating")
  }

  # make sure rating column is binary, with no NAs
  if(!all(ratings$rating %in% c(0,1))) {
    stop("Ratings must be binary")
  }

  return(ratings)
}


#' Count ratings
#' @description The functions in this package require a special form for the
#' ratings data. This function wraps `as_ratings` and either
#' `as_counts` or `as_count_index` to properly format the raw data.
#' There are two ways to summarize this data: one is to
#' provide row-wise counts of N_r (number of raters) and N_c (number of class 1
#' ratings), which optionally includes a RaterIR__. The other is to summarize
#' that information with a column n to indicate how many times that pair of N_r,
#' N_c occurs, which will drop any rater ID. The longer form is
#' referred to as a `count_index`, and the summarized form as `counts`. There
#' are support functions included to check validity and convert as needed.
#' @param summarize If FALSE returns the count_index, which has one row per
#' subject, with N_r, N_c. If TRUE, returns counts, comprising the unique
#' pairs of N_r,N_c and how many times they occur n. Defaults to FALSE, since
#' some functions expect the count_index.
#' @return A data frame with one row per subject, with the number of raters N_r,
#' and N_c the  number of raters who rated in the in-class range. If summarize
#' is TRUE, then the condensed form is returned.
#' @export
count_ratings <- function(raw_ratings, in_class, summarize = FALSE){

  ratings <- as_ratings(raw_ratings, in_class)

  if(summarize){
     as_counts(ratings)
  } else {
    as_count_index(ratings)
  }

}

#' convert from long form to count index
#' @param ratings A dataframe with subject_id and rating
#' @return A data frame with columns subject_id, N_r, and N_c
#' @export
as_count_index <- function(ratings){

  ratings |>
    group_by(subject_id) |>
    summarize(N_r = n(),
              N_c = sum(rating))

}

#' convert from long form to counts of N_r, N_c pairs
#' @param ratings A dataframe with subject_id and rating
#' @return A data frame with columns N_r, and N_c, and n
#' @details Unlike `as_count_index`, this function does not associate
#' counts with subject IDs. Instead it counts up the pairs of (N_r, N_c)
#' with a column n. This is useful for efficiently fitting the t-a-p model.
#' @export
as_counts <- function(ratings){

  ratings |>
    group_by(subject_id) |>
    summarize(N_r = n(),
              N_c = sum(rating)) |>
    count(N_r, N_c, name = "n")

}

