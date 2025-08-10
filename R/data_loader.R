#' Format raw rating data for analysis
#' @description
#' Standardize rater data to ratings and subject ID, with optional columns.
#' @param raw_ratings A data frame with one or more columns for raters and one or
#' more columns for ratings.
#' @param ratings_cols The required names of the column(s) that contain ratings.
#' @param rating_colname_type The header names of the ratings column(s) must be one of
#' "rating", rater", "subject", or "category." The first, "rating" is for simple
#' cases with a single column for rating data. The others are for "wide" data
#' cases where the ratings are spread over two or more columns For example,
#' if you have columns named rater1, rater2, and rater3, each with ratings in them,
#' the value of this parameter would be "rater."
#' @param subject_id_col The optional name for the column with the subject ID, or
#' NULL or "[None]" if there is not one. Defaults to NULL.
#' @param rater_id_col The optional name for the column with the rater ID, or
#' NULL or "[None]" if there is not one. Defaults to NULL.
#' @param prune If TRUE, remove any columns except for subject_id, rating, and
#' rater_id, if present. Defaults to FALSE, which will allow other descriptive columns
#' to be included, e.g. categories of ratings. You shouldn't prune the ratings
#' if you have more than one category, because normally you'll want to analyze
#' one category at a time.
#' @return A list with the formatted ratings and a way to map these abstracted
#' values back to the original data. In the list, the element `ratings` is a
#' data frame in long format with one rating per row, identified by
#' subject (required), rater (optional) and other descriptive columns that may
#' be present, like category of ratings. The function
#' `as_binary_ratings(ordinal_ratings, in_class = ...)` can be used
#' to convert from nominal or ordinal to binary.
#' The data frame has columns with standardized names: in order
#' `subject_id`, `rating`, and `rater_id` (if present), followed by any other
#' columns that may be present.
#' @details This function assumes that the data has ratings in columsn not rows.
#' If your ratings are in rows, then you'll need to transpose and rename the
#' columns appropriately. There must be a subject identifier in the raw_data.
#' This can come from the names of the ratings columns, e.g. "subject1",
#' "subject2" as columns with ratings below, or the subject identifier can be a
#' separate column, e.g. "subject". If neither of these are present, the
#' formatting function will create a subject_id column from the row numbers.
#' If the ratings are in categories, then there must be a subject ID column
#' since there will presumably be a rater_id column (otherwise you can only have
#'  one rating per subject).
#' @export
format_raw_ratings <- function(raw_ratings,
                               rating_cols,
                               rating_colnames_type,
                               subject_id_col = NULL,
                               rater_id_col = NULL,
                               prune = FALSE) {

  ######################################################
  # validate arguments         #########################
  ######################################################

  # if the rating_colnames_type = rating, there must be exactly one
  # value in rating_cols
  if (rating_colnames_type == "rating" && length(rating_cols) != 1)
    stop("rating_cols must have exactly one value if rating_colnames_type = 'rating'")

  # if the rating_colnames_type = rating, there must be a subject_id column
  if (rating_colnames_type == "rating" && is.null(subject_id_col))
    stop("subject_id_col must be provided if rating_colnames_type = 'rating'")

  # validate the rating_colnames_type
  if (!rating_colnames_type %in% c("rating","rater", "subject", "category"))
    stop("rating_colnames_type must be one of 'rater', 'subject', or 'category'")

  # if rating_colnames_type = category, there must be a subject_id_col
  if (rating_colnames_type == "category" && is.null(subject_id_col))
    stop("subject_id_col must be provided if rating_colnames_type = 'category'")

  # data must include rating_cols in colnames
  if (!all(rating_cols %in% colnames(raw_ratings)))
    stop("rating_cols must be in colnames of raw_ratings")

  # data must include subject_id_col in colnames if not NULL
  if (!is.null(subject_id_col) && !subject_id_col %in% colnames(raw_ratings))
    stop("subject_id_col must be in colnames of raw_ratings")

  # same for rater_id_col
  if (!is.null(rater_id_col) && !rater_id_col %in% colnames(raw_ratings))
    stop("rater_id_col must be in colnames of raw_ratings")

  # if the subject_id_col is "[None]", set it to NULL
  # this is for shiny apps
  if (!is.null(subject_id_col) && subject_id_col == "[None]") subject_id_col <- NULL

  # the rating_colnames_type can't be the same as provided subject_id_col
  if (rating_colnames_type == "subject" && !is.null(subject_id_col))
    stop("rating_colnames_type = subject can't coexist with subject_id_col")


  ######################################################
  # set up output values       #########################
  ######################################################


  # create the data frame to become the ratings output
  ratings <- raw_ratings

  ######################################################
  # single rating column ###############################
  ######################################################

  # long data case with a single column for ratings
  if (rating_colnames_type == "rating") {
    message("Ratings are in a single column")

    # rename the provided ratings column to 'rating'
    colnames(ratings)[colnames(ratings) == rating_cols] <- "rating"

    # rename subject_id_col to subject_id
    colnames(ratings)[colnames(ratings) == subject_id_col] <- "subject_id"

    # if rater_id_col is provided, rename it to rater_id
    if(!is.null(rater_id_col)) {
      colnames(ratings)[colnames(ratings) == rater_id_col] <- "rater_id"
    }
  }

  ######################################################
  # subject IDs as rating cols #########################
  ######################################################

  if (rating_colnames_type == "subject") {
    message("Ratings are in subject columns")

    ratings <- ratings  |>
      gather(subject_id, rating, !!rating_cols)

    if(!is.null(rater_id_col)) {
      # rename subject_id_col to subject_id
      colnames(ratings)[colnames(ratings) == rater_id_col] <- "rater_id"
    }
  }

  ######################################################
  # rater IDs as rating cols ###########################
  ######################################################

  if (rating_colnames_type == "rater") {
    message("Ratings are in rater columns")

    if(!is.null(subject_id_col)) {
      # rename subject_id_col to subject_id
      colnames(ratings)[colnames(ratings) == subject_id_col] <- "subject_id"
    } else {
      # create a subject_id column
      message("No subject_id_col provided, creating one from row numbers")
      ratings <- ratings |>
        mutate(subject_id = row_number())
    }

    # pivot to long format
    ratings <- ratings  |>
      gather(rater_id, rating, !!rating_cols)
  }

  ######################################################
  # categories as rating cols ##########################
  ######################################################
  if (rating_colnames_type == "category") {
    message("Ratings are in category columns")
    ratings <- ratings  |>
      gather(category, rating, !!rating_cols)

    # rename subject_id_col to subject_id
    colnames(ratings)[colnames(ratings) == subject_id_col] <- "subject_id"

    # if rater_id_col is provided, rename it to rater_id
    if(!is.null(rater_id_col)) {
      colnames(ratings)[colnames(ratings) == rater_id_col] <- "rater_id"
     }
  }

  ######################################################
  # final modifications       ##########################
  ######################################################

    # put the columns in the correct order
  ratings <- ratings |>
    relocate(subject_id, rating)

  # if rater_id exists, it should be third position
  if("rater_id" %in% colnames(ratings)){
    message("Found rater_id column")
    ratings <- ratings |>
      relocate(rater_id, .after = rating)
  }

  # return just the basics?
  if(prune == TRUE){
    message("Pruning columns to essentials since prune = TRUE")
    if("rater_id" %in% colnames(ratings)){
      ratings <- ratings |>
        select(subject_id, rating, rater_id)
    } else {
      ratings <- ratings |>
        select(subject_id, rating)
    }
  }

  return(ratings)
}

