#' format rater data
#' @description
#' Standardize rater data to have a column with subject ID and one or more
#' columns for aspects being rated for that subject.
#' @param df A data frame with one or more columns for raters and one or
#' more columns for ratings.
#' @param subject_id The optional name for the column with the subject ID, or
#' NULL the default, if there is none. The long format setting for
#' data_type requires a subject ID.
#' @param category The optional name for the column with the category of the
#' rating, or NULL, the default, if all the ratings are of a single thing.
#' @param ratings The names of the columns with the ratings. If the data is in
#' @param data_type What do the columns comprise? There are three data types
#' supported.
#' Use "categories" when each row is a subject ID and one or more categories,
#' with an optional rater ID. For example, a rating set on a service might have
#' columns for friendliness, cleanliness, and speed, with ratings in each of
#' those columns. Use "raters" when each row is a subject ID and two or more
#' raters of that subject, with an optional category column. Here, the ratings
#' for each rater appears under the column for that rater. The "narrow" format
#' is when each row is a unique combination of subject and rater, with an
#' optional category column. This requires the subject ID to exist.
#' '
#' @return A data frame with a column for subject ID and one or more columns for
#' @details For the subject_id and category, the string "[None]" can be substituted
#' for NULL. This is included for compatability with the app.
#' @export

format_data <- function(df,
                        subject_id = NULL,
                        category = NULL,
                        ratings,
                        data_type = "categories") {

  #browser()
  # need this in different places below
  fix_id <- function(df){
    # "[None]" is included for compatibility with the app
    if (is.null(subject_id)  | shiny::isTruthy(subject_id == "[None]" )) {
      df$SubjectID__ <- 1:nrow(df)  # create an ID manually
    } else {
      df <- df |> rename(SubjectID__ = subject_id)
    }
    return(df)
  }

  # format for the chosen data input type
  # requires a subject_id
  if (data_type == "narrow"){
    if(is.null(subject_id)) stop("narrow format requires a subject ID")
    # we also need a category var, but can create one if it doesn't exist
    if(is.null(category)) {
      df$category <- "Rating"
      category <- "Type"
    }

    df$ID__temp__ <- 1:nrow(df) # needed to make each row unique
    df <- spread(df, !!category, !!ratings)
    df$ID__temp__ <- NULL # get rid of the temp ID
    df <- fix_id(df) # standardize name of the subject ID column

    # cf http://stackoverflow.com/questions/21390141/specify-dplyr-column-names
  } else if(data_type == "raters"){

    df <- fix_id(df) # standardize name of the subject ID column

    # if there's a column for the dimension, leave it alone
    if(shiny::isTruthy(category == "[None]") | is.null(category)) {
      df <- gather(df, Rater, Rating, -SubjectID__)
    } else {
      df <- gather(df, Rater, Rating, -SubjectID__, -!!category) |>
        spread(!!category,Rating)
    }

    df$Rater <- NULL # don't need this

  } else {
    df <- fix_id(df) # standardize name of the subject ID column
  }
  return(df)
}

#' Count ratings
#' @description Count the number of raters and the number of raters who rated
#' in a given range.
#' @param ratings A data frame with two columns where the left column is the subject ID
#' the right column is a rating for that subject.
#' @param inClass A vector with the rating values that comprise the in-class. For
#' example, for a 1-5 scale, you might pick 1:3 to be the in-class, so that
#' the result is a binary distinction between {1,2,3} and {4,5}.
#' @return A data frame with one row per subject, with the number of raters N_r,
#' and N_c the  number of raters who rated in the in-class range.
#' @export
count_ratings <- function(ratings, inClass){
  ratings |>
    rename(SubjectID__ = 1, rating = 2) |>
    group_by(SubjectID__) |>
    summarise(N_r = n(),
              N_c = sum(rating %in% inClass)) |>
    filter(N_r > 1) |> # need at least pairs of raters
    select(-SubjectID__)
}
