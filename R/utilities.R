#' Convert to binary ratings
#' @description The package functions require a specific format for the ratings
#' data, e.g. ratings must be binary and the column names must be subject_id,
#' rating, and (optionally) rater_id, followed by any other columns.
#' @param raw_ratings A data frame where the first column is subject_id,
#' the second column is a rating for that subject, and an optional third
#' column gives the rater_id
#' @param in_class A vector with the rating values that comprise the in-class. For
#' example, for a 1-5 scale, you might pick 1:3 to be the in-class, so that
#' the result is a binary distinction between {1,2,3} and {4,5}.
#' @return A data frame with columns subject_id, rating, and rater_id if
#' that information is included.
#' @export
as_binary_ratings <- function(raw_ratings, in_class){
  message("as_binary_ratings() expects subject_id in first column, rating in second column,
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
  if(ncol(raw_ratings) > 2){
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
#' @return Zero on success
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

  return(0)
}

#' Verify counts are in the correct format
#' @param counts A data frame with columns N_r, N_c, and n
#' @return Zero on success
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

  return(0)
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

  return(0)
}

#' Verify rating params are in the correct format
#' @param ratings A data frame with columns subject_id, rater_id, and rating
#' @return Zero on success
#' @export
verify_rating_params <- function(rating_params){

  # must be a ratings data frame
  verify_ratings(rating_params)

  # make sure we have columns for parameters
  if(!all(c("t", "a", "p") %in% colnames(rating_params))) {
    stop("A rating_params data frame must have columns t, a, p")
  }

  # make sure rating column is binary, with no NAs
  if(!all(rating_params$rating %in% c(0,1))) {
    stop("Ratings must be binary")
  }

  return(0)
}

#' verify params
#' @description Verify that the t-a-p parameters are in the right format.
#' @details The params must contain one of the following sets:
#' - t, a, p
#' - t, a0, a1, p
#' - t, a, p0, p1
#' - t, a0, a1, p0, p1
#' @param params A list with the t-a-p parameters
#' @param expand If TRUE, the function will expand the parameters to the full
#' list. This can be convenient for LL calculations to simplify the formula.
#' @return The original params as a data frame if formatted correctly, or an
#' error message.
#' @export
verify_params <- function(params, expand = FALSE){
  # check that it's a list or named vector
  if(!is.list(params) && !is.vector(params)) {
    stop("params must be a list or named vector")
  }

  # check that we have the right parameters
  if(!all(c("t", "a", "p") %in% names(params)) &&
     !all(c("t", "a0", "a1", "p") %in% names(params)) &&
     !all(c("t", "a", "p0", "p1") %in% names(params)) &&
     !all(c("t", "a0", "a1", "p0", "p1") %in% names(params))) {
    stop("params must be list or named vector containing one of the following sets: (t, a, p); (t, a0, a1, p); (t, a, p0, p1); or (t, a0, a1, p0, p1)")
  }

   # if it's a named vector, convert to data frame
  if(is.list(params)){
    df_params <- as.data.frame(t(unlist(params)))
  } else if(is.vector(params)){
    df_params <- as.data.frame(t(as.list(params)))
  } else {
    stop("params must be a list or named vector")
  }

  # all values must be between zero and one with no missing values
  if(any(is.na(df_params)) || any(df_params < 0) || any(df_params > 1)){
    stop("All parameters must be between 0 and 1, with no missing values")
  }

 # expand if requested. This will convert a to a0,a1, and p to p0,p1 if
  # neccessary
  if(expand) {
    if("a" %in% names(df_params)) {
      df_params$a0 <- df_params$a
      df_params$a1 <- df_params$a
      df_params$a  <- NULL
    }
    if("p" %in% names(df_params)) {
      df_params$p0 <- df_params$p
      df_params$p1 <- df_params$p
      df_params$p <- NULL
    }
    df_params <- df_params |>
      select(t, a0, a1, p0, p1) # reorder the columns
  }

  # standardize the format
  return(df_params)
}


#' Count ratings
#' @description The functions in this package require a special form for the
#' ratings data. This function wraps `as_binary_ratings` and either
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

  ratings <- as_binary_ratings(raw_ratings, in_class)

  if(summarize){
     as_counts(ratings)
  } else {
    as_count_index(ratings)
  }

}

#' convert from long form to count index
#' @param ratings A dataframe with subject_id and binary rating
#' @return A data frame with columns subject_id, N_r, and N_c
#' @export
as_count_index <- function(ratings){

  # make sure it's the right format
  verify_ratings(ratings)

  ratings |>
    group_by(subject_id) |>
    summarize(N_r = n(),
              N_c = sum(rating))

}

#' convert from ratings to rating_params
#' @param ratings A dataframe with subject_id and binary rating
#' @param params A `params` object with t, a, p parameters
#' @description This is a convenience function for adding average
#' parameters to a ratings set, e.g. for use with the calibration functions.
#' Currently this only works with the basic t-a-p model.
#' @return A data frame with columns subject_id, rating, t, a, p
#' @export
as_rating_params <- function(ratings, params){
  # make sure it's the right format
  verify_ratings(ratings)
  verify_params(params)

  # add the parameters to the ratings
  ratings |>
    mutate(t = params$t,
           a = params$a,
           p = params$p)

}

#' convert from long form to counts of N_r, N_c pairs
#' @param ratings A dataframe with subject_id and binary rating
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

#' Is the t-a-p model degenerate?
#' @description given t-a-p parameters, return TRUE if the model is degenerate
#' @param params vector of t-a-p parameters
#' @param threshold threshold for degeneracy, defaults to x < 1e-6
#' @return TRUE if the model is degenerate
#' @export
is_degenerate <- function(params, threshold = 1e-6){
  eps1 <- params[1]*params[2]*params[3]
  eps2 <- (1-params[1])*(1-params[2])*(1-params[3])

  if(eps1 > threshold & eps2 > threshold) {return(FALSE)}
  return(TRUE)
}

#' Log-sum-exponential
#' @param vec a vector of numerics
#' @return a scalar log(exp(vec[1]) + exp(vec[2]) ...)
#' @details See https://rpubs.com/FJRubio/LSE, where I got the code from.
#' Note that this isn't vectorized, so you need to use rowwise()
#' @export
LSE_R <- function(vec){
  n.vec <- length(vec)
  vec <- sort(vec, decreasing = TRUE)
  Lk <- vec[1]
  for (k in 1:(n.vec-1)) {
    Lk <- max(vec[k+1], Lk) + log1p(exp(-abs(vec[k+1] - Lk)))
  }
  return(Lk)
}

